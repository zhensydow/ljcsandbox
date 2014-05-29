#-------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt

#-------------------------------------------------------------------------------
N = 100
# create random points from [0..1]
#x = np.random.rand( N )
x = np.linspace( 0, 10, num=N )
y = np.random.rand( N )

# create random areas from 0 to 10
area = np.pi * (10 * np.random.rand(N))**2

def test( colormap ):
    # create a colormap from [0..1] with colormap
    # colormap can be 'hot, 'gray', 'winter', 'Oranges', 'Greens', 'brg' ...
    # http://matplotlib.org/examples/color/colormaps_reference.html
    cmap = plt.get_cmap( colormap )
    scalar = plt.cm.ScalarMappable( cmap=cmap )
    color = scalar.to_rgba( y )
    
    plt.scatter( x, y, s=area, c=color, alpha=0.5 )
    plt.show()

#-------------------------------------------------------------------------------
if __name__=="__main__":
    test( 'hot' )

#-------------------------------------------------------------------------------
