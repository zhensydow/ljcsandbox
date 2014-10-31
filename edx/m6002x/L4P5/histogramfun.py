import pylab

# You may have to change this path
WORDLIST_FILENAME = "words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of uppercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # wordList: list of strings
    wordList = []
    for line in inFile:
        wordList.append(line.strip().lower())
    print "  ", len(wordList), "words loaded."
    return wordList

def vowelsProp(str):
    count = 0
    for i in range(len(str)):
        if str[i] in ['a','e','i','o','u']:
            count += 1
    return count / float(len(str))

def stdDev( xs ):
    if len(xs)==0:
        return float('NaN')
    
    mean = sum(xs) / float(len(xs))

    total = 0
    for i in range(len(xs)):
        total += (xs[i] - mean)**2

    return (total/float(len(xs)))**0.5

def CV( xs ):
    try:
        mean = sum(xs) / float(len(xs))
        return stdDev( xs ) / mean;
    except ZeroDivisionError:
        return float('NaN')
        
def plotVowelProportionHistogram(wordList, numBins=15):
    """
    Plots a histogram of the proportion of vowels in each word in wordList
    using the specified number of bins in numBins
    """
    vp = map( vowelsProp, wordList )
    mean = sum(vp) / float(len(vp))
    std = stdDev( vp )
    
    pylab.figure(1)
    pylab.hist(vp,numBins)
    xmin, xmax = pylab.xlim()
    ymin, ymax = pylab.ylim()
    pylab.text(xmin + (xmax-xmin)*0.02, (ymax-ymin)/2,
               "Mean = " + str(round(mean,4)) + 
               "\nStdDev = " + str(round(std,4)) )
    pylab.show()

if __name__ == '__main__':
    wordList = loadWords()
    plotVowelProportionHistogram(wordList)
