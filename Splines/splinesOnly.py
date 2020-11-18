import numpy
import matplotlib.pyplot as plt
plt.rcParams.update({'font.size': 15})
from mpl_toolkits.mplot3d import Axes3D
##################
#functions

#spline for give x
def spline(x, c, d):
    a = 0
    b = 0
    g = -19 *c - 12 *d 
    h = 8 *c + 5 *d
    e = g + 2*h 
    f = -2*g -3*h
    if x <= 1/2:
        y = a + b * x + c * (x * x) + d * (x * x * x)
    else:
        y = e + f * x + g * (x * x) + h * (x * x * x)
    return y

#spline for all x
def splineAll(c, d):
    X = [i/100 for i in range(100)]
    Y = []
    for x in X:
        Y.append(spline(x, c, d))
    return X, Y

#second derivative of f
def secondDerivative(x, c, d):
    a = 0
    b = 0
    g = -19 *c - 12 *d 
    h = 8 *c + 5 *d
    e = g + 2*h 
    f = -2*g -3*h
    if x <= 1/2:
        y = 2 * c  + 6 * d * x 
    else:
        y = 2 * g  + 6 * h * x 
    return y

#penalization term
def secondDerivativeIntegral(c, d):
    X = [i/100 for i in range(100)]
    I = 0
    for x in X:
        s = secondDerivative(x, c, d)
        I = I + s * s
    return I

#objective function
def objective(c, d, alpha, y):
    I = secondDerivativeIntegral(c, d)
    s = y - spline(1/2, c, d)
    #print(alpha)
    #print(I)
    ell = s * s + alpha * I
    return ell

#gradient descent
def computeGradient(c, d, alpha, y):
    s = y - spline(1/2, c, d)
    g = -19 *c - 12 *d 
    h = 8 *c + 5 *d
    gr = [
        2 * s * (-1/4), 
        2 * s * (1/8) 
        ]
    I1c = 4 * c + 3 * d
    I2c = 4 * g * 19 + 21 * h * 8 - 9 * (g * 8 + 19 * h)
    gr[0] = gr[0] + alpha * (I1c + I2c)
    
    I1d = 3 * d + 3 * c
    I2d = 4 * g * 12 + 21 * h * 5 - 9 * (g * 5 + 12 * h)
    gr[1] = gr[1] + alpha * (I1d + I2d)
    return gr

#gradient descent algorithm
def optimizeObjective(y, alpha, T, eta):
    c, d =  1 * numpy.random.randn(2)
    obj = []
    abPath = []
    for t in range(T):
        abPath.append([c, d])
        obj.append(objective(c, d, alpha, y))
        if t > 2 and obj[-2] < obj[-1]:
            eta = eta * .9 #reduce eta  if the objective increases
        g = computeGradient(c, d, alpha, y)
        c = c - eta * g[0]
        d = d - eta * g[1]
    return [c, d], obj, abPath



##################
#setup
y = 1/2
alpha = .00001
data = [[0, 0], [1/2, y], [1, 0]]

#try different spline parameters
CD = [[1, 1], [-1, 1], [1, -1], [-1, -1]]
fileName = 'pics/points.pdf'
colors = {'b-', 'g-', 'y-', 'k-'}
legend = []
for cd in CD:
    c = cd[0]
    d = cd[1]
    X, Y = splineAll(c, d)
    plt.plot(X, Y)
    legend.append('[c, d]=' + str(cd))
for xy in data:
    plt.plot(xy[0], xy[1], 'xr', markersize=18)
plt.legend(legend)
plt.savefig(fileName)
plt.show()

#grid definition and exhausitve search (alpha = e^-10)
alpha0 = numpy.exp(-10)
C = numpy.linspace(-2, 2, 10)
C1, C2 = numpy.meshgrid(C, C)
Z = objective(C1, C2, alpha0, 1/2)
iD, iC = numpy.unravel_index(numpy.argmin(Z), Z.shape)
cZa0 = C[iC]
dZa0 = C[iD]
Z0 = Z[iD, iC]
Z0obj=objective(cZa0, dZa0, alpha0, 1/2)

#plot surface
fileName = 'pics/objective.pdf'
ax = plt.axes(projection='3d')
ax.scatter3D(cZa0, dZa0, Z0, c='r', marker='*', s=13**2)
ax.plot_wireframe(C1, C2, Z, color='y')
plt.title('(c*, d*, min)=' + str((numpy.round(cZa0, 2), numpy.round(dZa0, 2), 
    numpy.round(Z0, 2))))
plt.savefig(fileName)
plt.show()

#gd optimization for log alpha = -10
numpy.random.seed(1234)
eta = .01
T = 5000
t = [x for x in range(T)]
[cg, dg], obj, cdPath = optimizeObjective(y, alpha0, T, eta)
plt.plot(t, obj)
plt.title('gradient descent optimization ')
plt.xlabel('# of iterations')
plt.ylabel('ell(c, d)')
plt.show()

#plot gradient descent path
fileName = 'pics/GDpath.pdf'
ax = plt.axes(projection='3d')
ax.plot_wireframe(C1, C2, Z, color='y')
zline = numpy.array(obj)
xline = numpy.array([x[0] for x in cdPath])
yline = numpy.array([x[1] for x in cdPath])
ax.plot3D(xline, yline, zline, 'b*')
ax.scatter3D(cZa0, dZa0, Z0, c='r', marker='*', s=13**2)
plt.title('(c*, d*, min)=' + 
        str((numpy.round(cZa0, 2), numpy.round(dZa0, 2), 
    numpy.round(Z0, 2))) + '\n'
       '(c_gd, d_gd, min)=' + 
        str((numpy.round(cg, 2), numpy.round(dg, 2), 
    numpy.round(obj[-1], 2))) )
plt.savefig(fileName)
plt.show()


#plot best spline for different alpha's
tryAlpha = [-16 + 4 * i for i in range(3)]
fileName = 'pics/optimalSpline.pdf'
tryAlpha = numpy.exp([-10] + tryAlpha)
legend = []
for alpha in tryAlpha:
    Z = objective(C1, C2, alpha, y)
    iDa, iCa = numpy.unravel_index(numpy.argmin(Z), Z.shape)
    cZa = C[iCa]
    dZa = C[iDa]
    X, Y = splineAll(cZa, dZa)
    if alpha != alpha0:
        plt.plot(X, Y, '-')
        legend.append('log a='+str(numpy.round(numpy.log(alpha), 1)))
    else:
        plt.plot(X, Y, '--')
        legend.append('log a='+str(numpy.round(numpy.log(alpha), 1)))
        X, Y = splineAll(cg, dg)
        plt.plot(X, Y, 'k.-')
        legend.append('(GD) log a='+str(numpy.round(numpy.log(alpha), 1)))
plt.legend(legend)
for xy in data:
    plt.plot(xy[0], xy[1], 'xr', markersize=18)
plt.savefig(fileName)
plt.show()

