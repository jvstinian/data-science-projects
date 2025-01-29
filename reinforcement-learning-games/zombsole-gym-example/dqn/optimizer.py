'''
Created on 4 Sep 2017

@author: ywz
'''
import numpy
# import tensorflow as tf
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()
from dqn.utils import flatten_tensor_variables
from dqn.utils import unflatten_tensors, get_param_values
from dqn.utils import get_param_assign_ops, set_param_values
from dqn.krylov import Krylov
from dqn.utils import create_optimizer

class Hvp:
    
    def __init__(self):
        pass
    
    def build(self, f, params, inputs, reg_coeff):
        
        self.f = f                      # Constraint function
        self.params = params            # Parameters (theta)
        self.inputs = inputs            # Neural network inputs
        self.reg_coeff = reg_coeff
        
        grads = tf.gradients(self.f, self.params)
        # Ensure that the constraint involves all the params.
        # Otherwise, H is not p.s.d.
        for grad in grads:
            assert grad is not None
        
        # vector v, e.g., Hv
        self.xs = [tf.placeholder(dtype=tf.float32, shape=param.get_shape()) for param in self.params]
        # Take care of Hx
        Hx = tf.gradients(tf.reduce_sum(tf.stack([tf.reduce_sum(g * x) for g, x in zip(grads, self.xs)])), self.params)
        for hx in Hx:
            assert hx is not None
        
        self.Hx = flatten_tensor_variables(Hx)
    
    def build_eval(self, sess, inputs):
        
        def get_feed_dict(inputs, xs):
            return dict(list(zip(self.inputs + self.xs, inputs + xs)))
            
        def Hx(x):
            shapes = [p.get_shape().as_list() for p in self.params]
            xs = unflatten_tensors(x, shapes)
            y = sess.run(self.Hx, feed_dict=get_feed_dict(inputs, xs))
            return y + self.reg_coeff * x
        
        return Hx
    
class ConjugateOptimizer:
    
    def __init__(self, 
                 cg_iters=10, 
                 reg_coeff=1e-5,
                 backtrack_ratio=0.8,
                 max_backtracks=15,
                 hvp_approach=None):
        
        self.cg_iters = cg_iters
        self.reg_coeff = reg_coeff
        self.backtrack_ratio = backtrack_ratio
        self.max_backtracks = max_backtracks
        self.accept_violation = True
        
        if hvp_approach is None:
            self.hvp_approach = Hvp()
        else:
            self.hvp_approach = hvp_approach
        
    def build(self, loss, leq_constraint, params, inputs):
        
        self.loss_fn = loss
        self.params = params
        self.inputs = inputs
        self.leq_constraint = leq_constraint
        
        constraint_term, constraint_value = leq_constraint
        grads = tf.gradients(loss, params)
        for grad in grads:
            assert grad is not None
        self.flat_grad = flatten_tensor_variables(grads)
        self.constraint_term = constraint_term
        self.constraint_value = constraint_value
        
        self.hvp_approach.build(f=constraint_term, 
                                params=params, 
                                inputs=inputs, 
                                reg_coeff=self.reg_coeff)
        
        self.param_assign_op, self.param_assign_tensor = get_param_assign_ops(params)
        
    def loss(self, sess, input_vals):
        return sess.run(self.loss_fn, feed_dict=dict(list(zip(self.inputs, input_vals))))
    
    def grad(self, sess, input_vals):
        return sess.run(self.flat_grad, feed_dict=dict(list(zip(self.inputs, input_vals))))
        
    def constraint_value(self, sess, input_vals):
        return sess.run(self.constraint_term, feed_dict=dict(list(zip(self.inputs, input_vals))))
    
    def loss_and_constraint_value(self, sess, input_vals):
        return sess.run([self.loss_fn, self.constraint_term], feed_dict=dict(list(zip(self.inputs, input_vals))))
    
    def optimize(self, sess, input_vals, use_init_step_size=True):
        
        old_param = numpy.copy(get_param_values(sess, self.params, flatten=True))
        
        loss = self.loss(sess, input_vals)
        grad = self.grad(sess, input_vals)
        Hx = self.hvp_approach.build_eval(sess, input_vals)
        
        descent_direction = Krylov().cg(Hx, grad, cg_iters=self.cg_iters)
        if use_init_step_size:
            initial_step_size = numpy.sqrt(2.0 * self.constraint_value * (1.0 / (descent_direction.dot(Hx(descent_direction)) + 1e-8)))
        if use_init_step_size is False or numpy.isnan(initial_step_size):
            initial_step_size = 1.0
        descent_step = initial_step_size * descent_direction
        expected_improve_rate = descent_step.dot(grad)
        
        for ratio in self.backtrack_ratio ** numpy.arange(self.max_backtracks):
            new_param = old_param - ratio * descent_step
            set_param_values(sess, self.param_assign_op, self.param_assign_tensor, new_param, flatten=True)
            
            l, v = self.loss_and_constraint_value(sess, input_vals)
            ratio = (loss - l) / (expected_improve_rate * ratio)
            
            assert numpy.isnan(v) == False
            if ratio > 0.1 and l < loss:
                if self.accept_violation:
                    if v <= self.constraint_value * 2: break
                else:
                    if v <= self.constraint_value: break
            
        if (numpy.isnan(l) or numpy.isnan(v) or l >= loss or (v > self.constraint_value and not self.accept_violation)):
            print("Line search condition violated. Rejecting the step!")
            if numpy.isnan(loss):
                print("Violated because loss is NaN")
            if numpy.isnan(v):
                print("Violated because constraint is NaN")
            if l >= loss:
                print("Violated because loss not improving, old_loss {}, new_loss {}".format(loss, l))
            if v >= self.constraint_value:
                print("Violated because constraint is violated")
            set_param_values(sess, self.param_assign_op, self.param_assign_tensor, old_param, flatten=True)

class Optimizer:
    
    def __init__(self, config, feedback_size, 
                 q_network, target_network, replay_memory):
        
        self.feedback_size = feedback_size
        self.q_network = q_network
        self.target_network = target_network
        self.replay_memory = replay_memory
        self.summary_writer = None
        
        self.gamma = config['gamma']
        self.num_frames = config['num_frames']
        
        optimizer = create_optimizer(config['optimizer'], 
                                     config['learning_rate'], 
                                     config['rho'], 
                                     config['rmsprop_epsilon'])
        
        self.train_op = optimizer.apply_gradients(
                 zip(self.q_network.gradient, 
                 self.q_network.vars))
        
    def set_summary_writer(self, summary_writer=None):
        self.summary_writer = summary_writer
        
    def sample_transitions(self, sess, batch_size):
        
        w, h = self.feedback_size
        states = numpy.zeros((batch_size, self.num_frames, w, h), 
                             dtype=numpy.float32)
        new_states = numpy.zeros((batch_size, self.num_frames, w, h), 
                                 dtype=numpy.float32)
        targets = numpy.zeros(batch_size, dtype=numpy.float32)
        actions = numpy.zeros(batch_size, dtype=numpy.int32)
        terminations = numpy.zeros(batch_size, dtype=numpy.int32)
        
        for i in range(batch_size):
            state, action, r, new_state, t = self.replay_memory.sample()
            states[i] = state
            new_states[i] = new_state
            actions[i] = action
            targets[i] = r
            terminations[i] = t

        targets += self.gamma * (1 - terminations) * self.target_network.get_q_value(sess, new_states)
        return states, actions, targets    

    def train_one_step(self, sess, step, batch_size):
        
        states, actions, targets = self.sample_transitions(sess, batch_size)
        feed_dict = self.q_network.get_feed_dict(states, actions, targets)
        
        if self.summary_writer and step % 1000 == 0:
            # TODO: Revert the following
            # summary_str, loss_str, _ = sess.run([self.q_network.summary_op, self.q_network.print_op,
            #                             self.train_op], 
            #                            feed_dict=feed_dict)
            sess.run(self.train_op, feed_dict=feed_dict)
            summary_str, _ = sess.run([self.q_network.summary_op, self.q_network.print_op], feed_dict=feed_dict)
            self.summary_writer.add_summary(summary_str, step)
            self.summary_writer.flush()
            
            # print info about memories
            nzrew = len([r for _, r, _ in self.replay_memory.others if r != 0])
            posrew = len([r for _, r, _ in self.replay_memory.others if r > 0])
            memlen = len(self.replay_memory.others)
            posrewpct = 100.0 * posrew / max(memlen, 1)
            nzrewpct = 100.0 * nzrew / max(memlen, 1)
            local_print_op = tf.print(
                "Percent of memories with non-zero reward: ", nzrewpct, "\n",
                "Percent of memories with positive reward: ", posrewpct,
            )
            sess.run([local_print_op], feed_dict=feed_dict) # TODO: Is feed_dict needed here?
        else:
            sess.run(self.train_op, feed_dict=feed_dict)

    
if __name__ == "__main__":
    
    n = 5
    m = 50
    
    w = tf.get_variable(dtype=tf.float32, shape=(1, n), initializer=tf.truncated_normal_initializer(mean=0.0, stddev=0.01), name='w')
    b = tf.get_variable(dtype=tf.float32, shape=(1,), name='b')
    
    X = tf.placeholder(dtype=tf.float32, shape=(n, m))
    y = tf.placeholder(dtype=tf.float32, shape=(1, m))
    z = tf.matmul(w, X) + b
    loss = tf.reduce_mean(tf.square(y - z))
    leq_constraint = (0.5 * tf.reduce_mean(tf.matmul(w, w, transpose_a=False, transpose_b=True) + b * b), 1)
    
    optimizer = ConjugateOptimizer()
    optimizer.build(loss, leq_constraint, params=[w, b], inputs=[X, y])
    
    # Generate data
    from numpy.linalg import norm
    nw = numpy.random.rand(1, n)
    nw = nw / (norm(nw) + 0.1)
    nb = 0.5
    nX = numpy.random.rand(n, m)
    ny = nw.dot(nX) + nb
    
    print([nw, nb])
    print("----------------------------------------")
    
    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())
        
        print("Initial solution:")
        print(sess.run([w, b]))
        for i in range(100):
            loss = optimizer.loss(sess, input_vals=[nX, ny])
            if i % 10 == 0:
                print("Iteration {}, loss {}".format(i, loss))
            optimizer.optimize(sess, input_vals=[nX, ny])
        print("Final solution:")
        print(sess.run([w, b]))
    

    
