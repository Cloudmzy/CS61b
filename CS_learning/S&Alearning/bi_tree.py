import turtle

def tree(branch_len):
    if branch_len > 5:
        t.forward(branch_len)
        t.right(20)
        tree(branch_len - 30)
        t.left(40)
        tree(branch_len - 30)
        t.right(20)
        t.backward(branch_len)
    

t = turtle.Turtle()
t.left(90)
t.penup()
t.backward(100)
t.pendown()
tree(75)
t.hideturtle()
turtle.done()