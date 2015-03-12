def quicksort( l ):
    if len(l) <= 1:
        return l
    else:
        pivot = l[0]
        lessers = quicksort( [x for x in l[1:] if x <= pivot] )
        greaters = quicksort( [x for x in l[1:] if x > pivot] )
        return lessers + [pivot] + greaters
