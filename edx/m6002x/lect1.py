import pylab

class MortagePlots( object ):
    def plotPayments( self, style ):
        pylab.plot( self.paid[1:], style, label=self.legend )


    def plotTotPd( self, style ):
        totPd = [self.paid[0]]
        for i in range( 1, len( self.paid ) ):
            totPd.append( totPd[-1] + self.paid[i] )

        pylab.plot( totPd, style, label=self.legend )

class Mortage( object ):
    """Abstract class for building different kinds of mortages"""
    def __init__( self, loan, annRate, months ):
        pass
    
    def __str__( self ):
        return self.legend
