import ssl

ssl._create_default_https_context = ssl._create_unverified_context

import tensorflow as tf
from tensorflow.examples.tutorials.mnist import input_data

mnist = input_data.read_data_sets('MNIST_data', one_hot=True)



def compute_accuracy(v_xs, v_ys):
    global prediction
    y_pre = sess.run(prediction, feed_dict={xs: v_xs, keep_prob: 1})
    correct_prediction = tf.equal(tf.arg_max(y_pre, 1), tf.arg_max(v_ys, 1))
    accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))
    result = sess.run(accuracy, feed_dict={xs: v_xs, ys: v_ys, keep_prob: 1})
    return  result


def weight_variable(shape):
    initial = tf.truncated_normal(shape, stddev=0.1)# 产生随即变量
    return tf.Variable(initial)


def bias_variable(shape):
    initial = tf.constant(0.1, shape=shape)
    return tf.Variable(initial)


def conv2d(x, W):
    #stride[1,x_movement,y_movement,1]
    #must have strides[0]=strides[3] 为1
    return tf.nn.conv2d(x, W, strides=[1, 1, 1, 1], padding='SAME')


def max_pool_2x2(x):
    #ksize 是用一个2*2 的 kernel 去池化 步长为strides 2
    return tf.nn.max_pool(x, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='SAME')
    pass

xs = tf.placeholder(tf.float32, [None, 784])
ys = tf.placeholder(tf.float32, [None, 10])
keep_prob = tf.placeholder(tf.float32)
x_image = tf.reshape(xs, [-1, 28, 28, 1])#-1表示整个所有的sample


#conv1 layer
W_conv1 = weight_variable([5, 5, 1, 32])# patch 5*5 in_size 即输入图片的高度为 1 out_size 输出高度为 32 （32 个神经元filter？）
b_con1 = bias_variable([32])
h_conv1 = tf.nn.relu(conv2d(x_image, W_conv1) + b_con1)#隐藏卷积第一层 output size 28x28x32 padding 是same
h_pool1 = max_pool_2x2(h_conv1)#隐藏池化第一层 output size 14x14x32


#conv2 layer
W_conv2 = weight_variable([5, 5, 32, 64])# patch 5*5 in_size 即输入图片的高度为 32 out_size 输出高度为 64 （64 个神经元filter？）
b_con2 = bias_variable([64])
h_conv2 = tf.nn.relu(conv2d(h_pool1, W_conv2) + b_con2)#隐藏卷积第二层 output size 14x14x64 padding 是same
h_pool2 = max_pool_2x2(h_conv2)#隐藏池化第二层 output size 7x7x64


#fully connectted layer1 全连接神经网络
W_fc1 = weight_variable([7*7*64, 1024])
b_fc1 = bias_variable([1024])
#先把pooling2 输出从三维降到一维 变平 -1 为所有sample
#[n_samples,7,7,64] ->>[n_samples,7*7*64]
h_pool2_flat = tf.reshape(h_pool2, [-1, 7*7*64])
h_fcl1 = tf.nn.relu(tf.matmul(h_pool2_flat, W_fc1)+b_fc1)
h_fcl1_dropout = tf.nn.dropout(h_fcl1, keep_prob)


#fully connectted layer2
W_fc2 = weight_variable([1024, 10]) #判别0-9 输出为10
b_fc2 = bias_variable([10])

prediction = tf.nn.softmax(tf.matmul(h_fcl1_dropout, W_fc2)+b_fc2)

#计算loss
cross_entropy = tf.reduce_mean(-tf.reduce_sum(ys*tf.log(prediction), reduction_indices=[1]))
train_step = tf.train.AdamOptimizer(1e-4).minimize(cross_entropy)
sess = tf.Session()
sess.run(tf.global_variables_initializer())

for i in range(1000):
    batch_xs, batch_ys = mnist.train.next_batch(100)
    sess.run(train_step,feed_dict={xs: batch_xs, ys: batch_ys, keep_prob: 0.5})
    if i % 50 == 0:
        print(compute_accuracy(
            mnist.test.images, mnist.test.labels
        ))
