def reverse_recursive(lst):
    """Returns the reverse of the given list.

    >>> reverse_recursive([1, 2, 3, 4])
    [4, 3, 2, 1]
    >>> import inspect, re
    >>> cleaned = re.sub(r"#.*\\n", '', re.sub(r'"{3}[\s\S]*?"{3}', '', inspect.getsource(reverse_recursive)))
    >>> print("Do not use lst[::-1], lst.reverse(), or reversed(lst)!") if any([r in cleaned for r in ["[::", ".reverse", "reversed"]]) else None
    """
    "*** YOUR CODE HERE ***"
    # a = []
    # for i in range(len(lst)):
    #     val = lst.pop()
    #     a.append(val)
    # return a
    ans = []
    for x in lst:
        ans = [x] + ans
    return ans

print(reverse_recursive([1,2,3,4]))