from __future__ import print_function

import numpy as np
from math import cos
import math
import math
from ortools.constraint_solver import routing_enums_pb2
from ortools.constraint_solver import pywrapcp

# width & height of grid square in m
GRID_SQ_L = 0.04
SHIFT = (GRID_SQ_L / 6378.1370) * (180 / math.pi)
geo_arr = np.loadtxt('coords', delimiter=',')
print(geo_arr.shape)
print(geo_arr)


def sort_by_col(arr, col_id):
    """
    assumes arr has two columns of numbers.
    :param arr:
    :param col_id:
    :return:
    """
    # return arr.view('i8,i8').sort(order=['f2'], axis=0)
    return np.flip(np.sort(arr.view('i8,i8'), order=['f' + str(col_id)], axis=0).view(np.float), 0)


def shift_lat(lat):
    return lat + SHIFT


def shift_long(lat, long):
    return long + SHIFT / cos(lat * math.pi / 180)


def gen_grid(geo_arr):
    grid = []
    lat_sorted = sort_by_col(geo_arr, 0)
    print("----------lat sort------------")
    print(lat_sorted)
    long_sorted = sort_by_col(geo_arr, 1)

    print("----------long sort------------")

    print(long_sorted)
    lat_min = lat_sorted[0][0]
    lat_max = lat_sorted[len(lat_sorted) - 1][0]
    if lat_min>lat_max:
        tmp=lat_max
        lat_max=lat_min
        lat_min=tmp
        lat_sorted=np.flip(lat_sorted,axis=0)

    long_min = long_sorted[0][1]
    long_max = long_sorted[len(long_sorted) - 1][1]
    if long_min>long_max:
        tmp=long_max
        long_max=long_min
        long_min=tmp
        long_sorted=np.flip(long_sorted,axis=0)
    print(lat_min, lat_max, long_min, long_max)
    lat_max+=0.0005
    long_max+=0.0005
    grid_lat_start = lat_min
    grid_long_start = long_min
    i = 0
    while grid_lat_start <= lat_max:
        grid.append((grid_lat_start, []))
        while grid_long_start <= long_max:
            grid[i][1].append((grid_long_start, []))
            grid_long_start = shift_long(grid_lat_start, grid_long_start)
        grid_lat_start = shift_lat(grid_lat_start)
        grid_long_start = long_min
        i += 1

    sublists = []
    lat_ind = -1

    for index, p in enumerate(lat_sorted):
        while lat_ind + 1 < len(grid) and p[0] >= grid[lat_ind + 1][0]:
            lat_ind += 1
            sublists.append([])
        sublists[lat_ind].append(p)


    s_i = 0
    for i, (lat, li) in enumerate(grid):
        if i >= len(sublists):
            # out of bounds
            break
        subl_lat = sort_by_col(np.array(sublists[i]), 1)
        if subl_lat[0][1]>subl_lat[len(subl_lat)-1][1]:
            subl_lat=np.flip(subl_lat,axis=0)
        subl_lat_i = 0
        for j, (long, lj) in enumerate(li):
            if subl_lat_i >= len(subl_lat):
                break
            while (subl_lat_i < len(subl_lat) and subl_lat[subl_lat_i][1] < grid[i][1][j + 1][0]):
                grid[i][1][j][1].append(subl_lat[subl_lat_i])
                subl_lat_i += 1

    return grid

    # print(grid)


grid = gen_grid(geo_arr)
lengths=[]
points = []
for indi, (i, l) in enumerate(grid):
    for indj, (j, lj) in enumerate(l):
        if len(lj) > 0:
            lengths.append(len(lj))
            points.append((i + (i - grid[indi + 1][0]) / 2, j + (j - grid[indi][1][indj + 1][0]) / 2))
print(len(points))

points=(np.array(points)*1000000).astype(int)

import math




def create_data_model():
    """Stores the data for the problem."""
    data = {}
    # Locations in block units
    data['locations'] = points # yapf: disable
    data['num_vehicles'] = 1
    data['depot'] = 0
    return data


def compute_euclidean_distance_matrix(locations):
    """Creates callback to return distance between points."""
    distances = {}
    for from_counter, from_node in enumerate(locations):
        distances[from_counter] = {}
        for to_counter, to_node in enumerate(locations):
            if from_counter == to_counter:
                distances[from_counter][to_counter] = 0
            else:
                # Euclidean distance
                distances[from_counter][to_counter] = (int(
                    math.hypot((from_node[0] - to_node[0]),
                               (from_node[1] - to_node[1]))))
    return distances


def print_solution(manager, routing, solution):
    """Prints solution on console."""
    print('Objective: {}'.format(solution.ObjectiveValue()))
    index = routing.Start(0)
    plan_output = 'Route:\n'
    route_distance = 0
    f=open('res',mode='w')
    while not routing.IsEnd(index):
        plan_output += ' {} ->'.format(manager.IndexToNode(index))
        st=points[manager.IndexToNode(index)][0]/1000000, ',', points[manager.IndexToNode(index)][1]/1000000
        f.write(st)
        print(st)
        previous_index = index
        index = solution.Value(routing.NextVar(index))
        route_distance += routing.GetArcCostForVehicle(previous_index, index, 0)
    plan_output += ' {}\n'.format(manager.IndexToNode(index))
    print(plan_output)
    plan_output += 'Objective: {}m\n'.format(route_distance)


def main():
    """Entry point of the program."""
    # Instantiate the data problem.
    data = create_data_model()

    # Create the routing index manager.
    manager = pywrapcp.RoutingIndexManager(len(data['locations']),
                                           data['num_vehicles'], data['depot'])

    # Create Routing Model.
    routing = pywrapcp.RoutingModel(manager)

    distance_matrix = compute_euclidean_distance_matrix(data['locations'])

    def distance_callback(from_index, to_index):
        """Returns the distance between the two nodes."""
        # Convert from routing variable Index to distance matrix NodeIndex.
        from_node = manager.IndexToNode(from_index)
        to_node = manager.IndexToNode(to_index)
        return distance_matrix[from_node][to_node]

    transit_callback_index = routing.RegisterTransitCallback(distance_callback)

    # Define cost of each arc.
    routing.SetArcCostEvaluatorOfAllVehicles(transit_callback_index)

    # Setting first solution heuristic.
    search_parameters = pywrapcp.DefaultRoutingSearchParameters()
    search_parameters.local_search_metaheuristic = (
        routing_enums_pb2.LocalSearchMetaheuristic.GUIDED_LOCAL_SEARCH)
    search_parameters.time_limit.seconds = 60*1
    search_parameters.log_search = True
    # Solve the problem.
    solution = routing.SolveWithParameters(search_parameters)

    # Print solution on console.
    if solution:
        print_solution(manager, routing, solution)


if __name__ == '__main__':
    main()
