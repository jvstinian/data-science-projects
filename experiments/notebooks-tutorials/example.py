import tensorflow as tf

opt1 = tf.keras.optimizers.Adam(learning_rate = 1e-2)
opt2 = tf.keras.optimizers.Adam(learning_rate = 1e-3)

@tf.function
def train_step(w, x, y, optimizer):
   with tf.GradientTape() as tape:
       L = tf.reduce_sum(tf.square(w*x - y))
   gradients = tape.gradient(L, [w])
   optimizer.apply_gradients(zip(gradients, [w]))

w = tf.Variable(2.)
x = tf.constant([-1.])
y = tf.constant([2.])

with tf.device('/GPU:0'):
    train_step(w, x, y, opt1)
# print("Calling `train_step` with different optimizer...")
# with assert_raises(ValueError):
#   train_step(w, x, y, opt2)
print(w.numpy())
