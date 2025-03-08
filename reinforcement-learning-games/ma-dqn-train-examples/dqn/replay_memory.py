'''
Created on Mar 25, 2018

@author: ywz
'''
import numpy, random
from collections import deque


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
    
    def append_memory(self, rm):
        self.frames.extend(rm.frames)
        self.others.extend(rm.others)
        if len(self.frames) >= self.capacity:
            for _ in range(len(self.frames) - self.capacity):
                self.frames.popleft()
                self.others.popleft()

    def clear(self):
        self.frames.clear()
        self.others.clear()

    def phi(self, new_frame):
        assert len(self.frames) > self.history_length
        images = [new_frame] + [self.frames[-1-i] for i in range(self.history_length-1)]
        return numpy.concatenate(images, axis=0)
    
    def _phi(self, index):
        images = [self.frames[index-i] for i in range(self.history_length)]
        return numpy.concatenate(images, axis=0)
    
    def available_sample_size(self):
        return len(self.frames) - self.history_length

    def sample(self):
        
        while True:
            
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

class MultiAgentReplayMemory:
    def __init__(self, agent_ids, history_len=4, capacity=1000000, batch_size=32, input_scale=255.0):
        self.agent_ids = agent_ids
        self.agent_id_indices = { agent_id: idx for idx, agent_id in enumerate(self.agent_ids) }
        
        self.main_replay_memory = ReplayMemory(history_len=history_len, capacity=capacity, batch_size=batch_size, input_scale=input_scale)

        self.agent_replay_memories = [
            ReplayMemory(history_len=history_len, capacity=capacity, batch_size=batch_size, input_scale=input_scale)
            for _ in self.agent_ids
        ]

    def append_to_main(self):
        for agent_idx in range(len(self.agent_replay_memories)):
            self.main_replay_memory.append_memory(
                    self.agent_replay_memories[agent_idx]
            )
            self.agent_replay_memories[agent_idx].clear()

    def add(self, frame, action, r, termination):
        # We assume that if a frame is returned for an agent, then so is an action, reward, and termination value
        for agent_id in frame:
            agent_idx = self.agent_id_indices[agent_id]
            agent_frame = frame[agent_id]
            agent_action = action[agent_id]
            agent_r = r[agent_id]
            agent_termination = termination[agent_id]

            self.agent_replay_memories[agent_idx].add(agent_frame, agent_action, agent_r, agent_termination)
        
    def add_nullops(self, init_frame):
        for agent_id in init_frame:
            agent_idx = self.agent_id_indices[agent_id]
            agent_frame = init_frame[agent_id]
            self.agent_replay_memories[agent_idx].add_nullops(agent_frame)

    def phi(self, new_frame):
        ret = {}
        for agent_id in new_frame:
            agent_idx = self.agent_id_indices[agent_id]
            ret[agent_id] = self.agent_replay_memories[agent_idx].phi(new_frame[agent_id])
        return ret

    def _phi(self, agent_idx, index):
        if agent_idx >= len(self.agent_replay_memories):
            return self.main_replay_memory._phi(index)
        else:
            return self.agent_replay_memories[agent_idx]._phi(index)
    
    def sample(self):
        weights_and_indices = [
                (idx, replay_memory.available_sample_size()) for idx, replay_memory in enumerate(self.agent_replay_memories + [self.main_replay_memory])
        ]
        weights_and_indices = [ (idx, sample_size) for idx, sample_size in weights_and_indices if sample_size > 0 ]

        index_choices = [ idx for idx, _ in weights_and_indices ]
        weights = numpy.array([sample_size for _, sample_size in weights_and_indices])

        cwghts = (weights.cumsum() / weights.sum()).tolist()
        agent_idx = random.choices(index_choices, cum_weights=cwghts, k=1)[0]
        if agent_idx >= len(self.agent_replay_memories):
            return self.main_replay_memory.sample()
        else:
            return self.agent_replay_memories[agent_idx].sample()
 
