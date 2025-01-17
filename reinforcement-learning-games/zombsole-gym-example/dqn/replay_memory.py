'''
Created on Mar 25, 2018

@author: ywz
'''
import numpy, random
from collections import deque

import numpy as np

class ReplayMemory:
    
    def __init__(self, history_len=4, capacity=1000000, batch_size=32, input_scale=255.0):
        
        self.capacity = capacity
        self.history_length = history_len
        self.batch_size = batch_size
        self.input_scale = input_scale
        
        self.frames = deque([])
        self.others = deque([])
    
    def add(self, frame, action, r, termination):
        
        if len(self.frames) == self.capacity:
            self.frames.popleft()
            self.others.popleft()
        self.frames.append(frame)
        self.others.append((action, r, termination))
        
    def add_nullops(self, init_frame):
        for _ in range(self.history_length):
            self.add(init_frame, 0, 0, 0)
    
    def phi(self, new_frame):
        assert len(self.frames) > self.history_length
        images = [new_frame] + [self.frames[-1-i] for i in range(self.history_length-1)]
        return numpy.concatenate(images, axis=0)
    
    def _phi(self, index):
        images = [self.frames[index-i] for i in range(self.history_length)]
        return numpy.concatenate(images, axis=0)
    
    def sample(self):
        
        # weights = [self.others[idx][1] for idx in range(self.history_length-1, len(self.frames)-1)]
        # weights = np.array([-1.0*min(wgt,0)/200 + max(wgt,0) + 0.25*int(wgt==0) for wgt in weights])
        # twght = weights.sum()
        # cwghts = (weights.cumsum() / twght).tolist()
        while True:
            
            # index = random.choices(list(range(self.history_length-1, len(self.frames)-1)), cum_weights=cwghts, k=1)[0]
            index = random.randint(a=self.history_length-1, b=len(self.frames)-2)
            infos = [self.others[index-i] for i in range(self.history_length)]
            # Check if termination=1 before "index"
            flag = False
            for i in range(1, self.history_length):
                if infos[i][2] == 1:
                    flag = True
                    break
            if flag:
                continue
            
            state = self._phi(index)
            new_state = self._phi(index+1)
            action, r, termination = self.others[index]
            state = numpy.asarray(state / self.input_scale, dtype=numpy.float32)
            new_state = numpy.asarray(new_state / self.input_scale, dtype=numpy.float32)
                
            return (state, action, r, new_state, termination)

