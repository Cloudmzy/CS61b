from re import A
import torch
import numpy as np
a = torch.rand(2,3)
b = torch.rand(2,3)
print(a)
print(b)
print(torch.eq(a, b))
print(torch.equal(a, b))

###

a = torch.tensor([[2,6,3,1,7],[5,2,7,3,4]])
print(torch.topk(a, k=1, dim=1))