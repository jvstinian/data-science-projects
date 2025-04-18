'''
Created on Mar 25, 2018

@author: ywz
'''
import numpy
import tensorflow as tf


def get_variable(shape, initializer, name, dtype=tf.float32, trainable=True):
    var = tf.compat.v1.get_variable(shape=shape, initializer=initializer, 
                                    dtype=dtype, name=name, trainable=trainable)
    return var


def HeUniform(shape):
    
    if len(shape) > 2:
        w = shape[0]
        h = shape[1]
        input_channels  = shape[2]
        d = 1.0 / numpy.sqrt(input_channels * w * h)
    else:
        d = 1.0 / numpy.sqrt(shape[0])
    
    init_W = tf.random_uniform_initializer(-d, d)
    init_b = tf.random_uniform_initializer(-d, d)
    return init_W, init_b


class Conv2dLayer(tf.Module):
    def __init__(self, output_dim, kernel=(5, 5), stride=(2, 2), activation=tf.nn.relu, init_W=None, init_b=None, name='conv', padding='VALID'):
        super().__init__(name=name)
        self.output_dim = output_dim
        self.kernel = kernel
        self.stride = stride
        self.activation = activation
        self.init_W = init_W
        self.init_b = init_b
        self.padding = padding

    @tf.compat.v1.keras.utils.track_tf1_style_variables
    def __call__(self, x):
        assert len(x.get_shape().as_list()) == 4
        shape = (
                self.kernel[0], 
                self.kernel[1], 
                x.get_shape().as_list()[-1], 
                self.output_dim
        )
        _W, _b = HeUniform(shape)
        if self.init_W is None: self.init_W = _W
        if self.init_b is None: self.init_b = _b

        with tf.compat.v1.variable_scope(self.name):
            W = get_variable(shape=shape, initializer=self.init_W, dtype=tf.float32, name='weight')
            b = get_variable(shape=(output_dim,), initializer=self.init_b, dtype=tf.float32, name='bias')
            
            conv = tf.nn.conv2d(input=x, filter=W, strides=(1, self.stride[0], self.stride[1], 1), padding=self.padding)
            if self.activation:
                conv = self.activation(tf.nn.bias_add(conv, b))
            else:
                conv = tf.nn.bias_add(conv, b)
    
        return conv


class DenseLayer(tf.Module):
    def __init__(self, output_dim, activation=tf.nn.relu, init_W=None, init_b=None, name='dense'):
        super().__init__(name=name)
        self.output_dim = output_dim
        self.activation = activation
        self.init_W = init_W
        self.init_b = init_b

    @tf.compat.v1.keras.utils.track_tf1_style_variables
    def __call__(self, x):
    
        if len(x.get_shape().as_list()) > 2:
            shape = x.get_shape().as_list()
            x = tf.reshape(x, shape=(-1, numpy.prod(shape[1:])))

        shape = (x.get_shape().as_list()[-1], self.output_dim)
        _W, _b = HeUniform(shape)
        if self.init_W is None: self.init_W = _W
        if self.init_b is None: self.init_b = _b

        with tf.compat.v1.variable_scope(self.name):
            W = get_variable(shape=shape, initializer=self.init_W, dtype=tf.float32, name='weight')
            b = get_variable(shape=(self.output_dim,), initializer=self.init_b, dtype=tf.float32, name='bias')
        
            output = tf.matmul(x, W) + b
            if self.activation:
                output = self.activation(output)
    
        return output


