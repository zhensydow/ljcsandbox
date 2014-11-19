# 6.00.2x Problem Set 4

import numpy
import random
import pylab
from ps3b import *

#
# PROBLEM 1
#
def simulationDelayedTreatment(numTrials):
    """
    Runs simulations and make histograms for problem 1.

    Runs numTrials simulations to show the relationship between delayed
    treatment and patient outcome using a histogram.

    Histograms of final total virus populations are displayed for delays of 300,
    150, 75, 0 timesteps (followed by an additional 150 timesteps of
    simulation).

    numTrials: number of simulation runs to execute (an integer)
    """

    numViruses = 100
    maxBirthProb = 0.1
    clearProb = 0.05
    resistances = {'guttagonol':True}
    mutProb = 0.005
    maxPop = 1000
    delayTime = 150

    for k,delayTime in enumerate([0,75,150,300]):
        print "Doing ", delayTime
        maxTime = delayTime + 150
        virusPop = [0]*maxTime
        virusFinalPop = [0]*numTrials
        numCured = 0

        for j in xrange(numTrials):
            viruses = []
            for i in xrange(numViruses):
                viruses.append( ResistantVirus( maxBirthProb, clearProb,
                                                resistances, mutProb ) )

            patient = TreatedPatient( viruses, maxPop )

            for i in xrange(delayTime):
                virusPop[i] = virusPop[i] + patient.update()

            patient.addPrescription( 'guttagonol' )

            for i in xrange(150):
                virusPop[i+delayTime] = virusPop[i+delayTime] + patient.update()

            virusFinalPop[j] = patient.getTotalPop()
            if virusFinalPop[j] <=50:
                numCured = numCured + 1

        print "Cured ", numCured / float(numTrials)

        for i in range(maxTime):
            virusPop[i] = virusPop[i] / float(numTrials)

        pylab.figure(1)
        pylab.subplot( 2, 2, k+1 )
        pylab.hist( virusFinalPop, 50, label='Virus Pop' )
        pylab.title( 'Delay time '+str(delayTime) )
        pylab.legend()
        v = pylab.axis()
        pylab.axis( [0,150+300,v[2],v[3]] )

        pylab.figure(2)
        pylab.subplot( 2, 2, k+1 )
        pylab.plot( virusPop, label='Virus Pop' )
        pylab.title( 'Delay time '+str(delayTime) )
        pylab.legend()
        v = pylab.axis()
        pylab.axis( [0,150+300,v[2],v[3]] )

    pylab.show()




#
# PROBLEM 2
#
def simulationTwoDrugsDelayedTreatment(numTrials):
    """
    Runs simulations and make histograms for problem 2.

    Runs numTrials simulations to show the relationship between administration
    of multiple drugs and patient outcome.

    Histograms of final total virus populations are displayed for lag times of
    300, 150, 75, 0 timesteps between adding drugs (followed by an additional
    150 timesteps of simulation).

    numTrials: number of simulation runs to execute (an integer)
    """
    numViruses = 100
    maxBirthProb = 0.1
    clearProb = 0.05
    resistances = {'guttagonol':False,'grimpex':False}
    mutProb = 0.005
    maxPop = 1000
    delayTime = 150

    for k,delayTime in enumerate([0,75,150,300]):
        print "Doing ", delayTime

        maxTime = 150 + delayTime + 150
        virusPop = [0]*maxTime
        virusFinalPop = [0]*numTrials
        numCured = 0

        for j in xrange(numTrials):
            viruses = []
            for i in xrange(numViruses):
                viruses.append( ResistantVirus( maxBirthProb, clearProb,
                                                resistances, mutProb ) )

            patient = TreatedPatient( viruses, maxPop )

            for i in xrange(150):
                virusPop[i] = virusPop[i] + patient.update()

            patient.addPrescription( 'guttagonol' )

            ofs = 150
            for i in xrange(delayTime):
                virusPop[i+ofs] = virusPop[i+ofs] + patient.update()

            patient.addPrescription( 'grimpex' )

            ofs = 150 + delayTime
            for i in xrange(150):
                virusPop[i+ofs] = virusPop[i+ofs] + patient.update()

            virusFinalPop[j] = patient.getTotalPop()
            if virusFinalPop[j] <=50:
                numCured = numCured + 1

        print "Cured ", numCured / float(numTrials)

        for i in range(maxTime):
            virusPop[i] = virusPop[i] / float(numTrials)

        pylab.figure(1)
        pylab.subplot( 2, 2, k+1 )
        pylab.hist( virusFinalPop, 50, label='Virus Pop' )
        pylab.title( 'Delay time '+str(delayTime) )
        pylab.legend()
        #v = pylab.axis()
        #pylab.axis( [0,150+300,v[2],v[3]] )

        pylab.figure(2)
        pylab.subplot( 2, 2, k+1 )
        pylab.plot( virusPop, label='Virus Pop' )
        pylab.title( 'Delay time '+str(delayTime) )
        pylab.legend()
        #v = pylab.axis()
        #pylab.axis( [0,150+300,v[2],v[3]] )

    pylab.show()

