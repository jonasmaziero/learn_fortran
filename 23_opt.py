import numpy as np
from matplotlib import pyplot as plt
import scipy.interpolate
N = 1000  # number of points for plotting/interpolation
x, y, z = np.genfromtxt(r'opt_f.dat', unpack=True)
xll = x.min()
xul = x.max()
yll = y.min()
yul = y.max()
xi = np.linspace(xll, xul, N)
yi = np.linspace(yll, yul, N)
zi = scipy.interpolate.griddata(
    (x, y), z, (xi[None, :], yi[:, None]), method='linear', rescale=False)
contours = plt.contour(xi, yi, zi, 6, colors='black')
plt.clabel(contours, inline=True, fontsize=7)
plt.imshow(zi, extent=[xll, xul, yll, yul], origin='lower', cmap=plt.cm.jet, alpha=0.9)
plt.xlabel(r'$x$')
plt.ylabel(r'$y$')
# plt.clim(0, 600)
plt.colorbar()
x, y = np.genfromtxt(r'opt_x.dat', unpack=True)
plt.plot(x, y, color='magenta')
plt.savefig('opt.eps', format='eps', dpi=100)
#plt.show()
