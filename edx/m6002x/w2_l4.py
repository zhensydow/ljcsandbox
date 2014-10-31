def stdDevOfLengths(L):
    if len(L)==0:
        return float('NaN')

    ls = [len(s) for s in L]
    mean = sum(ls) / float(len(ls))

    total = 0
    for i in range(len(ls)):
        total += (ls[i] - mean)**2

    return (total/float(len(ls)))**0.5

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
        
def test():
    print stdDevOfLengths( ['a', 'z', 'p'] ), " - 0 "
    print stdDevOfLengths( ['apples', 'oranges', 'kiwis', 'pineapples'] ), " - 1.8708"

