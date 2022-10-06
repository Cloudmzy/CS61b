# 中缀表达式转后缀表达式
from pythonds.basic.stack import Stack

def infixToPosfix(infixexper):
    prec = {}
    prec["*"] = 3
    prec["/"] = 3
    prec["+"] = 2
    prec["-"] = 2
    prec["("] = 1
    opStack = Stack()
    posfixList = []
    tokenList = list(infixexper)

    for token in tokenList:
        if token in "abcdefghijklmnopqrstuvwxyz" or token in "0123456789":
            posfixList.append(token)
        elif token == '(':
            opStack.push(token)
        elif token == ')':
            topToken = opStack.pop()
            while topToken != '(':
                posfixList.append(topToken)
                topToken = opStack.pop()
        else:
            while (not opStack.isEmpty()) and (prec[opStack.peek()] >= prec[token]):
                posfixList.append(opStack.pop())
            opStack.push(token)

    while not opStack.isEmpty():
        posfixList.append(opStack.pop())
    return " ".join(posfixList)

print(infixToPosfix("(a+b)*(c+d)"))

