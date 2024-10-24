'''
Created on 4 Sep 2017

@author: ywz
'''
import numpy
import cv2
import scipy.signal
#import tensorflow as tf
import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()


def flatten_tensor_variables(ts):
    return tf.concat(axis=0, values=[tf.reshape(x, [-1]) for x in ts])

def flatten_tensors(tensors):
    if len(tensors) > 0:
        return numpy.concatenate([numpy.reshape(x, [-1]) for x in tensors])
    else:
        return numpy.asarray([])
    
def unflatten_tensors(flattened, tensor_shapes):
    tensor_sizes = list(map(numpy.prod, tensor_shapes))
    indices = numpy.cumsum(tensor_sizes)[:-1]
    return [numpy.reshape(pair[0], pair[1]) for pair in zip(numpy.split(flattened, indices), tensor_shapes)]

def get_param_values(sess, params, flatten=True):
    values = sess.run(params)
    if flatten:
        values = flatten_tensors(values)
    return values

def get_param_assign_ops(params):
    
    assign_ops = []
    input_tensors = []
    
    for param in params:
        v = tf.placeholder(dtype=param.dtype, shape=param.get_shape())
        assign_ops.append(tf.assign(param, v))
        input_tensors.append(v)
        
    return assign_ops, input_tensors

def set_param_values(sess, assign_ops, input_tensors, values, flatten=True):
    
    if flatten:
        shapes = [p.get_shape().as_list() for p in input_tensors]
        values = unflatten_tensors(values, shapes)
    
    feed_dict = dict(list(zip(input_tensors, values)))
    sess.run(assign_ops, feed_dict=feed_dict)

def discount_cumsum(x, discount):
    # See https://docs.scipy.org/doc/scipy/reference/tutorial/signal.html#difference-equation-filtering
    # Here, we have y[t] - discount*y[t+1] = x[t]
    # or rev(y)[t] - discount*rev(y)[t-1] = rev(x)[t]
    return scipy.signal.lfilter([1], [1, float(-discount)], x[::-1], axis=0)[::-1]

def iterate_minibatches(input_list=None, batch_size=None, shuffle=False):
    
    if batch_size is None:
        batch_size = len(input_list[0])
    assert all(len(x) == len(input_list[0]) for x in input_list)

    if shuffle:
        indices = numpy.arange(len(input_list[0]))
        numpy.random.shuffle(indices)
        
    for start_idx in range(0, len(input_list[0]), batch_size):
        idx = indices[start_idx:start_idx + batch_size] if shuffle else slice(start_idx, start_idx + batch_size)
        yield [r[idx] for r in input_list]


def create_optimizer(method, learning_rate, rho, epsilon):
    
    if method == 'rmsprop':
        opt = tf.train.RMSPropOptimizer(learning_rate=learning_rate, 
                                        decay=rho,
                                        epsilon=epsilon)
    elif method == 'adam':
        opt = tf.train.AdamOptimizer(learning_rate=learning_rate,
                                     beta1=rho)
    elif method == 'momentum':
        opt = tf.train.MomentumOptimizer(
            learning_rate=learning_rate, 
            momentum=rho)
    else:
        raise ValueError('optimizer method %s not supported' % (method))
    
    return opt
 
def cv2_resize_image(image, resized_shape=(84, 84), method='crop', crop_offset=8):
        
        height, width = image.shape
        resized_height, resized_width = resized_shape
        
        if method == 'crop':
            h = int(round(float(height) * resized_width / width))
            resized = cv2.resize(image, (resized_width, h), interpolation=cv2.INTER_LINEAR)
            crop_y_cutoff = h - crop_offset - resized_height
            cropped = resized[crop_y_cutoff : crop_y_cutoff + resized_height, :]
            return numpy.asarray(cropped, dtype=numpy.uint8)
        elif method == 'scale':
            return numpy.asarray(cv2.resize(image, (resized_width, resized_height), 
                                            interpolation=cv2.INTER_LINEAR), dtype=numpy.uint8)
        else:
            raise ValueError('Unrecognized image resize method.')