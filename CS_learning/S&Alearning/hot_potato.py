#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun  9 23:20:13 2022

@author: cloudzy
"""

from pythonds.basic.queue import Queue

def hotPotato(namelist, num):
    simqueue = Queue()
    for name in namelist:
        simqueue.enqueue(name)
        
    while simqueue.size() > 1:
        for i in range(num):
            simqueue.enqueue(simqueue.dequeue())
        
        simqueue.dequeue()
        
    return simqueue.dequeue()

print(hotPotato(["bill","David","Sam","Jane"], 5))
        
