# coding: utf-8

# In[1]:

from __future__ import division, print_function, absolute_import

import tensorflow as tf
import numpy as np
import pandas as pd


# In[2]:

def MinMaxScalar1(data):
    numerator = data - np.min(data, 0)               #numerator 分子
    denominator = np.max(data, 0) - np.min(data, 0)  #denominator 分母
    return numerator / (denominator + 1e-5)       #？？？应该放分子上？？？


# In[4]:

# Training Parameters
learning_rate = 0.001
seq_length = 1

# Network Parameters
num_hidden_1 = 1  # 1st layer num features
num_hidden_2 = 1  # 2nd layer num features (the latent dim)
num_hidden_3 = 1
num_hidden_4 = 1
num_hidden_5 = 1
num_input = 14

# In[45]:

xy = np.loadtxt('c:/users/lenovo/desktop/16s14.csv', delimiter=',')    #delimiter定界符，分隔符
xy = MinMaxScalar1(xy)
x = xy
#y = xy[:, [0]]

# In[46]:

dataX = []   #-------------------?这一部分
#dataY = []
for i in range(0, len(x)):
     _x = np.concatenate((x[i:i + seq_length]))   #adj.连结的  vt.连结concatenate
    # _y = y[i + seq_length]
    # print(_x, "->", _y)
     dataX.append(_x)
    # dataY.append(_y)

# In[47]:

#train_size = int(len(dataY) * 0.7)
#test_size = len(dataY) - train_size
trainX = np.array(dataX)

#trainY = np.array(dataY)


# In[8]:

# tf Graph input
X = tf.placeholder("float", [None, num_input])

weights = {
    'encoder_h1': tf.Variable(tf.random_normal([num_input, num_hidden_1])),
    'encoder_h2': tf.Variable(tf.random_normal([num_hidden_1, num_hidden_2])),
    'encoder_h3': tf.Variable(tf.random_normal([num_hidden_2, num_hidden_3])),
    'decoder_h1': tf.Variable(tf.random_normal([num_hidden_2, num_hidden_1])),
    'decoder_h2': tf.Variable(tf.random_normal([num_hidden_1, num_input])),
}
biases = {
    'encoder_b1': tf.Variable(tf.random_normal([num_hidden_1])),
    'encoder_b2': tf.Variable(tf.random_normal([num_hidden_2])),
    'encoder_b3': tf.Variable(tf.random_normal([num_hidden_3])),
    'encoder_b4': tf.Variable(tf.random_normal([num_hidden_4])),
    'encoder_b5': tf.Variable(tf.random_normal([num_hidden_5])),
    'decoder_b1': tf.Variable(tf.random_normal([num_hidden_1])),
    'decoder_b2': tf.Variable(tf.random_normal([num_input])),
}


# In[9]:

# Building the encoder
def encoder(x):
    # Encoder Hidden layer with sigmoid activation #1
    layer_1 = tf.nn.sigmoid(tf.add(tf.matmul(x, weights['encoder_h1']),#matrix multiply矩阵相乘
                                   biases['encoder_b1']))
    # Encoder Hidden layer with sigmoid activation #2
    layer_2 = tf.nn.sigmoid(tf.add(tf.matmul(layer_1, weights['encoder_h2']),
                                   biases['encoder_b2']))
    layer_3 = tf.nn.sigmoid(tf.add(tf.matmul(layer_2, weights['encoder_h3']),
                                   biases['encoder_b3']))
    return layer_3


# Building the decoder
def decoder(x):
    # Decoder Hidden layer with sigmoid activation #1
    layer_1 = tf.nn.sigmoid(tf.add(tf.matmul(x, weights['decoder_h1']),
                                   biases['decoder_b1']))
    # Decoder Hidden layer with sigmoid activation #2
    layer_2 = tf.nn.sigmoid(tf.add(tf.matmul(layer_1, weights['decoder_h2']),
                                   biases['decoder_b2']))
    return layer_1, layer_2


# In[10]:

# Construct model
encoder_op = encoder(X)
features, decoder_op = decoder(encoder_op)
# Prediction
y_pred = decoder_op
# Targets (Labels) are the input data.
y_true = X

# In[11]:

# Define loss and optimizer, minimize the squared error
aeloss = tf.reduce_mean(tf.pow(y_true - y_pred, 2))
aeoptimizer = tf.train.AdamOptimizer(learning_rate=learning_rate)
aetrain = aeoptimizer.minimize(aeloss)
# Initialize the variables (i.e. assign their default value)
with tf.Session() as sess:
    init = tf.global_variables_initializer()
    # Run the initializer
    sess.run(init)
    i = 1
    rsquare_val1 = -100
    step_loss1 = 10000000000000000
    step_loss2 = 0.1
    # Training
    while True:
        # Prepare Data
        # Get the next batch of MNIST data (only images are needed, not labels)
        _, step_loss = sess.run([aetrain, aeloss], feed_dict={
            X: dataX})

        if step_loss1 < step_loss and step_loss2 > step_loss:
            break
        step_loss1 = step_loss
        i=i+1
        print("[step: {}] loss: {}".format(i, step_loss))

    features1 = sess.run(features, feed_dict={X: trainX})

features1 = pd.DataFrame(features1)
features1.to_csv("c:/users/lenovo/desktop/16s1.csv",index=False,sep=',')#3.45%loss


# In[49]:


# In[14]:



# In[ ]:



