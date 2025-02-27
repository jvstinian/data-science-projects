#!/usr/bin/python
from itertools import product
import numpy as np
from zombsole.game import Map
from zombsole.things import ObjectiveLocation, Wall


class Maze(object):
    def __init__(self, width, height, barrier_coords, end_coords): 
        self.width = width
        self.height = height
        self.barrier_coords = barrier_coords
        self.end_coords = end_coords

        if self.width <= 0: 
            raise ValueError('Width must be positive')
        if self.height <= 0: 
            raise ValueError('Height must be positive')

    def display(self): 
        print( '-' * (self.width + 2) )
        for j in reversed( range(self.height) ):  
            s = '|'
            for i in range(self.width):  
                if (i,j) in self.barrier_coords: 
                    s += 'x'
                elif (i,j) in self.end_coords: 
                    s += 'E'
                else: 
                    s += ' '
            s += '|'
            print(s)
        print( '-' * (self.width + 2) )
   
    def display_policy(self, policy_map): 
        print( '-' * (self.width + 2) )
        for j in reversed( range(self.height) ):  
            s = '|'
            for i in range(self.width):  
                if (i,j) in self.barrier_coords: 
                    s += 'x'
                elif (i,j) in self.end_coords: 
                    s += 'E'
                elif (i,j) in policy_map:
                    s += policy_map.get((i,j), ' ')
                else: 
                    s += ' '
            s += '|'
            print(s)
        print( '-' * (self.width + 2) )


class MazeState(object):
    move_right = (1,0)
    move_left  = (-1,0)
    move_up    = (0,1)
    move_down  = (0,-1)

    def __init__(self, maze, location): 
        self.maze = maze
        self.location = location
    def apply_action(self,action): 
        orig_state = self.location
        new_state = tuple( map( sum, zip( self.location, action ) ) )
        
        if new_state in self.maze.barrier_coords: 
            # Landed on the barrier, revert back to the original state
            self.location = orig_state
        else: 
            x = new_state[0] 
            y = new_state[1]
            # If out-of-range, then choose closest valid coordinate
            if x >= self.maze.width: 
                x = self.maze.width - 1
            if x < 0: 
                x = 0
            if y >= self.maze.height: 
                y = self.maze.height - 1
            if y < 0: 
                y = 0
            self.location = (x,y)
        
        if self.location in self.maze.end_coords: 
            return (1.0, self.location)
        else: 
            return (0.0, self.location)

# This implements the policy iteration algorithm from section 3.2.2 of 
# "Reinforcement Learning: A Survey" by Kaelbling, Littman, and Moore (1996).
def iterate_policy(maze,policy,gamma,coords,action_ids,action_map,verbose=True):
    id_minus_T = np.eye( len( coords ) )
    R = np.zeros( (len(coords),1) )
    for index, (coord, action_id) in enumerate( zip(coords,policy) ): 
        ms = MazeState(maze, coord)
        reward, new_location = ms.apply_action( action_map[ action_id ]  )
        if new_location in coords:
            # No need to throw if new_location is outside the permissible coordinates, which 
            # we take to indicate that new_location is an exit or terminal state
            try: 
                new_index = coords.index( new_location )
                id_minus_T[index,new_index] += -1.0 * gamma 
            except Exception as e:
                print(f"Caught exception when updating applying {action_id} to go from {coord} to {new_location}")
                # Don't update id_minus_T
                pass
        R[index,0] = reward
    
    if verbose: 
        print('Matrix: %s' % (id_minus_T))
        print('Determinant: %s' % (np.linalg.det(id_minus_T)))
        print('R: %s' % (R))
    V = np.linalg.solve(id_minus_T,R)

    V2 = np.zeros( (len(coords),len(action_ids)) )
    for coord_index, coord in enumerate( coords ): 
        for action_index, action_id in enumerate(action_ids): 
            ms = MazeState(maze, coord)
            reward, new_location = ms.apply_action( action_map[ action_id ]  )
            try: 
                new_location_index = coords.index( new_location )
                V2[ coord_index, action_index ] = reward + gamma * V[ new_location_index, 0 ]
            except Exception as e: 
                V2[ coord_index, action_index ] = reward

    if verbose: 
        print('argmax: %s' % (np.argmax(V2,axis=1)))
    updated_policy = [ action_ids[ idx ] for idx in np.argmax(V2,axis=1) ]
    if verbose: 
        print('updated policy: %s' % (updated_policy))
    return updated_policy, V


def navigate_zombsole_map(map_name: str, verbose=False): 
    zmap = Map.from_map_name(map_name)

    width = zmap.size[0] + 1
    height = zmap.size[1] # + 1 # Need to check why the map sizes are off in zombsole
    
    end_coords = []
    barrier_coords = []
    for thing in zmap.things:
        if isinstance(thing, (ObjectiveLocation,)):
            end_coords.append(thing.position)
        elif isinstance(thing, (Wall,)):
            barrier_coords.append(thing.position)

    if verbose:
        print("zombsole map ", map_name, " size: ", (width, height))
        print("zombsole map end coordinates")
        print(end_coords)
        print("zombsole map barrier coordinates")
        print(barrier_coords)

    maze = Maze(width, height, barrier_coords, end_coords)
    if verbose:
        maze.display()
    gamma = 0.99
    coords, policy = find_policy_to_navigate_maze(maze, gamma, max_iters=50)
    maze.display_policy({coord: policy_action_id[0] for coord, policy_action_id in zip(coords, policy)})


def find_policy_to_navigate_maze(maze: Maze, gamma: float, initial_policy = None, max_iters = None, verbose: bool = False):
    coords = [coord for coord in product(range(maze.width), range(maze.height)) if coord not in ( maze.barrier_coords + maze.end_coords )]
    if verbose:
        print('Printing all combination of state and actions')
        for coord in coords: 
            for action, action_name in [ (MazeState.move_left, 'left'), (MazeState.move_up, 'up'), (MazeState.move_right, 'right'), (MazeState.move_down, 'down') ]: 
                ms = MazeState(maze, coord)
                ms.apply_action(action)
                print('%s, action %s -> %s' % (coord, action_name, ms.location))
    
    policy = initial_policy if initial_policy is not None else ['up' for coord in coords]
    action_ids = [ 'left', 'up', 'right', 'down' ]
    action_map = dict( [ ('left', MazeState.move_left), ('up', MazeState.move_up), ('right', MazeState.move_right), ('down', MazeState.move_down) ] )
    
    iters = 0

    while True:
        updated_policy, policy_value = iterate_policy(maze,policy,gamma,coords,action_ids,action_map,verbose=verbose)
        iters += 1
        if all( [ a == b for a, b in zip( updated_policy, policy ) ] ): 
            if verbose:
                print('Optimal policy:')
                for coord, action in zip( coords, updated_policy ): 
                    print('%s; %s' % (coord, action))
                print('policy_value: %s' % (policy_value))
            policy = updated_policy
            break
        else:
            policy = updated_policy
            if (max_iters is not None) and (iters >= max_iters):
                break

    return coords, policy

def navigate_simple_maze(verbose=False):
    width = 3
    height = 3
    barrier_coords = [ (0,1), (1,1) ]
    end_ccords = [ (0,2) ]
    maze = Maze(width, height, barrier_coords, end_ccords)
    if verbose:
        maze.display()
    gamma = 0.9
    coords, policy = find_policy_to_navigate_maze(maze, gamma, verbose=verbose)
    maze.display_policy({coord: policy_action_id[0] for coord, policy_action_id in zip(coords, policy)})

if __name__ == "__main__":
    verbose = False
    print("Simple maze: ")
    navigate_simple_maze(verbose=verbose)
    
    print("Zombsole map: ")
    navigate_zombsole_map("bridge", verbose=verbose)

