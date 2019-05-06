"""Test with colormaps"""
#-------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt


#-------------------------------------------------------------------------------
def test(size, colormap):
    """Create a colormap from [0..1] with colormap
    colormap can be 'hot, 'gray', 'winter', 'Oranges', 'Greens', 'brg' ...
    http://matplotlib.org/examples/color/colormaps_reference.html
    """

    # create random points from [0..1]
    #x = np.random.rand(N)
    xvals = np.linspace(0, 10, num=size)
    yvals = np.random.rand(size)

    # create random areas from 0 to 10
    areas = np.pi * (10 * np.random.rand(size))**2

    cmap = plt.get_cmap(colormap)
    scalar = plt.cm.ScalarMappable(cmap=cmap)
    colors = scalar.to_rgba(yvals)

    plt.scatter(xvals, yvals, s=areas, c=colors, alpha=0.5)
    plt.show()

#-------------------------------------------------------------------------------
if __name__ == "__main__":
    test(100, 'hot')

#-------------------------------------------------------------------------------
