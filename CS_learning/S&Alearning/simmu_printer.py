from pythonds.basic.queue import Queue
import random

from scipy import rand

# 定义打印机类
class Printer:
    def __init__(self,ppm):
        self.pagerate = ppm
        self.currenTask = None
        self.timeRemainng = 0
    
    def tick(self): 
        if self.currenTask != None:
            self.timeRemainng = self.timeRemainng - 1
            if self.timeRemainng <= 0:
                self.currenTask = None
    
    def busy(self):
        if self.currenTask != None:
            return True
        else:
            return False

    def starNext(self,newtask):
        self.currenTask = newtask
        self.timeRemainng = newtask.getPages()*60/self.pagerate

# 定义任务类
class Task:
    def __init__(self,time):
        self.timestamp = time
        self.pages = random.randrange(1,21)

    def getStamp(self):
        return self.timestamp

    def getPages(self):
        return self.pages
    
    def waitTime(self, currenttime):
        return currenttime - self.timestamp

def newPrintTask():
    num = random.randrange(1,181)
    if num == 180:
        return True
    else: 
        return False

def simulation(numSeconds, pagesPerMinute):

    labprinter = Printer(pagesPerMinute)
    printQueue = Queue()
    waitingtimes = []

    for currentSecond in range(numSeconds):

        if newPrintTask():
            task = Task(currentSecond)
            printQueue.enqueue(task)
        
        if (not labprinter.busy()) and (not printQueue.isEmpty()):
            nexttask = printQueue.dequeue()
            waitingtimes.append(nexttask.waitTime(currentSecond))
            labprinter.starNext(nexttask)

        labprinter.tick()

    averageWait = sum(waitingtimes)/len(waitingtimes)
    print("Avg Wait %6.2f secs %3d tasks remaining."%(averageWait,printQueue.size()))

for i in range(10):
    simulation(3600,15)

