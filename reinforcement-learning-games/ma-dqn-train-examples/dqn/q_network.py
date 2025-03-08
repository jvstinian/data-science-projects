'''
Created on Mar 25, 2018

@author: ywz
'''
import tensorflow as tf
from dqn.layers import Conv2dLayer, DenseLayer


class QNetwork(tf.Module):
    def __init__(self, input_shape=(84, 84, 4), n_outputs=4, 
                 network_type='cnn', scope='q_network'):
        
        self.width = input_shape[0]
        self.height = input_shape[1]
        self.channel = input_shape[2]
        self.n_outputs = n_outputs
        self.network_type = network_type
        self.scope = scope
        
        self.build()
        
    def build(self):
        
        self.net = {}
        # self.net['input'] = tf.transpose(self.x, perm=(0, 2, 3, 1)) # TODO: Not sure if this is the way to go
        self.layers = [ ]
            
        if self.network_type == 'cnn':
            self.net['conv1'] = Conv2dLayer(
                32, kernel=(8, 8), stride=(4, 4), init_b=tf.constant_initializer(0.01), name='conv1'
            )
            self.net['conv2'] = Conv2dLayer(
                64, kernel=(4, 4), stride=(2, 2), init_b=tf.constant_initializer(0.01), name='conv2'
            )
            self.net['conv3'] = Conv2dLayer(
                    64, kernel=(3, 3), stride=(1, 1), init_b=tf.constant_initializer(0.01), name='conv3'
            )
            self.net['feature'] = DenseLayer(
                    512, init_b=tf.constant_initializer(0.01), name='fc1'
            )
            self.layers.extend(
                [ self.net['conv1'], self.net['conv2'], self.net['conv3'], self.net['feature']  ]
            )
        elif self.network_type == 'cnn_nips':
            self.net['conv1'] = Conv2dLayer(
                16, kernel=(8, 8), stride=(4, 4), init_b=tf.constant_initializer(0.01), name='conv1'
            )
            self.net['conv2'] = Conv2dLayer(
                32, kernel=(4, 4), stride=(2, 2), init_b=tf.constant_initializer(0.01), name='conv2'
            )
            self.net['feature'] = DenseLayer(
                    256, init_b=tf.constant_initializer(0.01), name='fc1'
            )
            self.layers.extend(
                [ self.net['conv1'], self.net['conv2'], self.net['feature']  ]
            )
        elif self.network_type == 'mlp':
            self.net['fc1'] = DenseLayer(50, 
                                         # init_W=tf.constant_initializer(0.0), init_b=tf.constant_initializer(0.0), 
                                         name='fc1')
            self.net['feature'] = DenseLayer(50, 
                                             # init_W=tf.constant_initializer(0.0), init_b=tf.constant_initializer(0.0), 
                                             name='fc2')
            self.layers.extend(
                [ self.net['fc1'], self.net['feature']  ]
            )
        else:
            raise NotImplementedError('Unknown network type: {}'.format(self.network_type))
            
        self.net['values'] = DenseLayer(
                self.n_outputs, activation=None, 
                # init_W=tf.constant_initializer(0.0), 
                init_b=tf.constant_initializer(0.0), 
                name='values'
        )
        self.layers.append(self.net['values'])

    # TODO: Revert this as needed.  The following decorator does appear to allow us to save the model
    # @tf.function
    @tf.compat.v1.keras.utils.track_tf1_style_variables
    def call(self, inputs):
        with tf.compat.v1.variable_scope(self.scope):
            ret = tf.transpose(inputs, perm=(0, 2, 3, 1)) # TODO: Not sure if this is the way to go
            for layer in self.layers:
                ret = layer(ret)
            return ret

    # TODO: Revert this as needed.  The following decorator does appear to allow us to save the model
    # @tf.function
    def get_q_value(self, inputs):
        return tf.reduce_max(self.call(inputs), axis=1, name='q_value')

    def get_q_action(self, inputs):
        return tf.argmax(self.call(inputs), axis=1, name='q_action', output_type=tf.int32)

    # TODO: No idea what to do here
    def clone_op(self, network):
        new_vars = {v.name.replace(network.scope, ''): v for v in network.variables}
        print("new vars: ", new_vars)
        for v in self.variables:
            v.assign(new_vars[v.name.replace(self.scope, '')], read_value=True)
    
    def get_gradients_and_loss(self, inputs, actions, ys):
        with tf.GradientTape() as tape:
            indices = tf.transpose(tf.stack([tf.range(tf.shape(actions)[0]), actions], axis=0))
            value = tf.gather_nd(self.call(inputs), indices, name='action_value')
        
            loss = 0.5 * tf.reduce_mean(tf.square((value - ys)))

        gradient = tape.gradient(loss, self.trainable_variables)

        print("loss: ", loss)

        return gradient, loss
        

if __name__ == "__main__":
    import numpy
    
    num_actions = 4
    batch_size = 5
    network = QNetwork(n_outputs=num_actions)
    
    state = numpy.random.rand(batch_size, 4, 84, 84)
    values = numpy.random.rand(batch_size)
    actions = numpy.random.randint(num_actions, size=batch_size)
    
    with tf.Session() as sess:
        summary_writer = tf.summary.FileWriter('log/', sess.graph)
        sess.run(tf.global_variables_initializer())
        
        q_values = sess.run(network.net['values'], feed_dict={network.x: state})
        q_value = network.get_q_value(sess, state)
        q_action = network.get_q_action(sess, state)
        
        print(q_values)
        print(q_value)
        print(q_action)
    
