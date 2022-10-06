# 中值选取会影响算法复杂度，介于O(nlogn)和O(n2)之间

def quickSort(alist):
    quickSortHelper(alist,0,len(alist)-1)

def quickSortHelper(alist, first, last):
    if first<last:
        splitpoint = partition(alist,first,last)
        quickSortHelper(alist,first,splitpoint-1)
        quickSortHelper(alist,splitpoint+1,last)

def partition(alist,first,last):
    pivotvalue = alist[first]

    leftmark = first+1
    rightmark = last

    done = False
    while not done:
        while leftmark <= rightmark and alist[leftmark] <= pivotvalue:
            leftmark = leftmark+1
        
        while alist[rightmark]>=pivotvalue and rightmark>=leftmark:
            rightmark = rightmark-1
        
        if rightmark < leftmark:
            done = True
        else:
            alist[rightmark],alist[leftmark]=alist[leftmark],alist[rightmark]
    
    alist[first],alist[rightmark]=alist[rightmark],alist[first]

    return rightmark

alist = [54,26,93,17,77,31,44,55,20]
quickSort(alist)
print(alist)