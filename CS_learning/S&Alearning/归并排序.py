# 使用而外一倍的存储空间 但时间复杂度稳定在nlogn

def merge_sort(lst):
    if len(lst) <= 1:
        return lst

    middle = len(lst)//2
    left = merge_sort(lst[:middle])
    right = merge_sort(lst[middle:])

    merged = []
    while left and right:
        if left[0] <= right[0]:
            merged.append(left.pop(0))
        else:
            merged.append(right.pop(0))
        
    merged.extend(right if right else left)
    return merged