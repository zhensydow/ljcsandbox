def isAlphabeticalWord(word, wordList=None):
    if (len(word) > 0):
        curr = word[0]
    for letter in word:
        if (curr > letter):
            return False
        else:
            curr = letter
    if wordList is None:
        return True
    return word in wordList

def lotsOfParameters1(a,b,c,d,e):
    print a
    print b
    print c
    print d
    print e

def lotsOfParameters2(a=1,b=2,c=3,d=4,e=5):
    print a
    print b
    print c
    print d
    print e

def lotsOfParameters3(a,b,c=3,d=4,e=5):
    print a
    print b
    print c
    print d
    print e
