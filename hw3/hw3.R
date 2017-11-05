#XIAODONG GU (XGU60)

#0. Data Preprocessing
#read data from csv files
train = read.csv("mnist_train.csv", header = FALSE)
test = read.csv("mnist_test.csv", header = FALSE)

#transpose data
train = data.frame(t(train))
test = data.frame(t(test))

#partition training set, test set
train_0_1 = train[train$X785 == 0 | train$X785 == 1, ]
test_0_1 = test[test$X785 == 0 | test$X785 == 1, ]
train_3_5 = train[train$X785 == 3 | train$X785 == 5, ]
test_3_5 = test[test$X785 == 3 | test$X785 == 5, ]

#separate labels
true_label_train_0_1 = train_0_1$X785
true_label_test_0_1 = test_0_1$X785
true_label_train_3_5 = train_3_5$X785
true_label_test_3_5 = test_3_5$X785
train_0_1$X785 = NULL
test_0_1$X785 = NULL
train_3_5$X785 = NULL
test_3_5$X785 = NULL

#generate images
image_0 = matrix(as.numeric(train_0_1[1,]), nrow = 28, ncol = 28)
image_1 = matrix(as.numeric(train_0_1[6000,]), nrow = 28, ncol = 28)
image_3 = matrix(as.numeric(train_3_5[1,]), nrow = 28, ncol = 28)
image_5 = matrix(as.numeric(train_3_5[9000,]), nrow = 28, ncol = 28)
image_0 = apply(image_0, 2, rev)
image_1 = apply(image_1, 2, rev)
image_3 = apply(image_3, 2, rev)
image_5 = apply(image_5, 2, rev)
print(image(t(image_0), col = gray.colors(256)))
print(image(t(image_1), col = gray.colors(256)))
print(image(t(image_3), col = gray.colors(256)))
print(image(t(image_5), col = gray.colors(256)))

#2. Implementation
#implement h theta function
h_fun = function(data, theta){
  theta_x = c(data.matrix(data) %*% theta)
  return (2 / (1.0 + exp(theta_x)) - 1)
}

#implement training function
train = function(data, label, init_theta, alpha, alpha_decay, thres, max_it){
  theta = c(init_theta, 1)
  data$one = rep(1, nrow(data))
  
  #thres3 code
  #temp_accuracy = 0
  
  for(it in 1:max_it){
    pred = h_fun(data, theta)
    temp = theta
    
    #thres3 code
    #accuracy = pred_accuracy(theta, data, label)
    
    #update theta using batch gradient descent
    theta = theta - alpha * c((label - pred) %*% data.matrix(data))
    alpha = alpha * alpha_decay
    
    #thres1 code: check whether theta converges using thres1 as convergence criteria
    if(sum(abs(temp - theta)) / length(theta) < thres){
        break
    }
    
    #thres2 code: using thres2 as convergence criteria
    #if(sum((temp - theta)^2) / length(theta) < thres){
    #  break
    #} 
    
    #thres3 code: using thres3 as convergence criteria
    #if(abs(accuracy - temp_accuracy) < thres) {
    #  break
    #}
    #temp_accuracy = accuracy
    
  }
  return (theta)
}


#3. Training
#implement predict function
predict = function(data, theta){
  data$one = rep(1, nrow(data))
  est = h_fun(data, theta)
  return(as.numeric(est >= 0) + -1 * as.numeric(est < 0))
}

#implement function to predict accuracy
pred_accuracy = function(theta, test_data, test_label){
  pred_test = predict(test_data, theta)
  accuracy = sum(as.numeric(pred_test == test_label)) / length(test_label)
  return(accuracy)
}

#3a.
#format labels to 1, -1
format_label = function(labels, label_1, label_2){
  new_labels = as.numeric(labels == label_1) + -1 * as.numeric(labels == label_2)
  return(new_labels)
}

train_label_0_1 = format_label(true_label_train_0_1, 0, 1)
test_label_0_1 = format_label(true_label_test_0_1, 0, 1)
train_label_3_5 = format_label(true_label_train_3_5, 3, 5)
test_label_3_5 = format_label(true_label_test_3_5, 3, 5)

#compute accuracy for model 1 (0, 1)

init_theta = rep(1, ncol(train_0_1))
theta_0_1 = train(train_0_1, train_label_0_1, init_theta, 0.6, 0.99, 1, 1000)
print("3a. train and test accuracies of _0_1 set.")
print(pred_accuracy(theta_0_1, train_0_1, train_label_0_1))
print(pred_accuracy(theta_0_1, test_0_1, test_label_0_1))

#compute accuracy for model 2 (3, 5)
theta_3_5 = train(train_3_5, train_label_3_5, init_theta, 0.6, 0.99, 1, 1000)
print("3a. train and test accuracies of _3_5 set.")
print(pred_accuracy(theta_3_5, train_3_5, train_label_3_5))
print(pred_accuracy(theta_3_5, test_3_5, test_label_3_5))

#3b
#training on _0_1 set, take 80% random sample
batch_test = function(train_data, train_label, train_perc, test_data, test_label,
                      init_theta,alpha, alpha_decay, thres, max_it){
  train_accuracies = c()
  test_accuracies = c()
  for(i in 1:10){
    set.seed(i)
    new_index = sample(1:nrow(train_data))
    sample_index = new_index[1:(nrow(train_data)*train_perc)]
    theta = train(train_data[sample_index, ], 
                      train_label[sample_index],
                      init_theta, alpha, alpha_decay, thres, max_it )
    train_accuracy = pred_accuracy(theta,train_data[sample_index, ],
                                   train_label[sample_index])
    test_accuracy = pred_accuracy(theta,test_data, test_label)
    train_accuracies = c(train_accuracies, train_accuracy)
    test_accuracies = c(test_accuracies, test_accuracy)
  }
  return(c(mean(train_accuracies), mean(test_accuracies)))
}

result_0_1 = batch_test(train_0_1, train_label_0_1, 0.8, test_0_1,
                        test_label_0_1, init_theta, 0.6, 0.99, 1, 1000)
print("3b. train and test accuracies of _0_1 set, averages of 10 runs")
print(result_0_1)

#training on _3_5 set, take 80% random sample
result_3_5 = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5,
                        test_label_3_5, init_theta, 0.6, 0.99, 1, 1000)
print("3b. train and test accuracies of _3_5 set, averages of 10 runs")
print(result_3_5)


#4. Evaluation
#4a. evaluate theta
#training on _3_5 set, take 80% random sample
#using different theta
set.seed(1)
theta_1 = runif(ncol(train_3_5), 0, 1)
theta_2 = runif(ncol(train_3_5), 0, 100)
theta_3 = runif(ncol(train_3_5), 0, 10000)
theta_4 = rep(1, ncol(train_3_5))
theta_5 = rep(100, ncol(train_3_5))
theta_6 = rep(10000, ncol(train_3_5))

theta_collection = list(theta_1, theta_2, theta_3, theta_4, theta_5, theta_6)
#store training accuracy and test accuracy into dataframe
results = data.frame(Training_accuracy = numeric(0), Testing_accuracy = numeric(0))
for(i in 1:6){
  res = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5, test_label_3_5,
                    theta_collection[[i]], 0.6, 0.99, 1, 1000)
  results[i, ] = res
}
print("4a. train and test accuracies of _3_5 set, using different theta")
print(results)

#using different alpha
alpha_collection = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
results_alpha = data.frame(Training_accuracy = numeric(0), Testing_accuracy = numeric(0))
for(i in 1:10){
  res = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5, test_label_3_5,
                   init_theta, alpha_collection[i], 0.99, 1, 1000)
  results_alpha[i, ] = res
}
print("4a. train and test accuracies of _3_5 set, using different alpha")
print(results_alpha)

#using different alpha decay rate
decay_collection = c(0.99, 0.98, 0.95, 0.90, 0.80)
results_decay = data.frame(Training_accuracy = numeric(0), Testing_accuracy = numeric(0))
for(i in 1:5){
  res = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5, test_label_3_5,
                   init_theta, 0.6, decay_collection[i], 1, 1000)
  
  results_decay[i, ] = res
}
print("4a. train and test accuracies of _3_5 set, using different alpha decay")
print(results_decay)

#4b. evaluate threshold
#implement function monitoring thresholds
monitor_threshold = function(data, label, init_theta, alpha, alpha_decay, max_it){
  data$one = rep(1, nrow(data))
  theta = c(init_theta, 1)
  iterations = 1:max_it
  thres1 = c()
  thres2 = c()
  thres3 = c()
  temp_accuracy = 0
  for(it in 1:max_it){
    pred = h_fun(data, theta)
    accuracy = pred_accuracy(theta, data, label)
    temp = theta
    #update theta using batch gradient descent
    theta = theta - alpha * c((label - pred) %*% data.matrix(data))
    alpha = alpha * alpha_decay
    thres1 = c(thres1, sum(abs(temp - theta)) / length(theta))
    thres2 = c(thres2, sum((temp - theta)^2) / length(theta))
    thres3 = c(thres3, abs(accuracy - temp_accuracy))
    temp_accuracy = accuracy
  }
  df = data.frame(iterations = iterations, thres1 = thres1, 
                  thres2 = thres2, thres3 = thres3)
  return (df)
}

df = monitor_threshold(train_3_5, train_label_3_5, init_theta, 0.6, 0.99, 500)
library(reshape2)
df_melt = melt(df, id = c("iterations"))
library(ggplot2)
plot1 = ggplot(df_melt, aes(iterations, value)) +
  geom_line(size = 1)  +
  facet_wrap(~factor(variable), scales = "free") +
  scale_y_log10()
print(plot1)

#experiment with different convergence criteria
#different convergence criteia are directly implemeted in train function. 

#use convergence criteria 2 (thres2), set thres value = 1.
#please un-comment thres2 code in train function to run this experiment.
result_3_5 = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5,
                        test_label_3_5, init_theta, 0.6, 0.99, 1, 1000)
print(result_3_5)

#use convergence criteria 3(thres3), set thres value = 0.000001
#please un-comment thres3 code in train function to run this experiment.
result_3_5 = batch_test(train_3_5, train_label_3_5, 0.8, test_3_5,
                        test_label_3_5, init_theta, 0.6, 0.99, 0.000001, 1000)
print(result_3_5)

#5. Learning curves
#5a. learning curves for accuracies
df01 = data.frame(train_accuracy = numeric(0), test_accuracy = numeric(0))
df35 = data.frame(train_accuracy = numeric(0), test_accuracy = numeric(0))
train_perc = seq(0.05, 1, 0.05)
#for _0_1 set
for(i in 1:20){
  result = batch_test(train_0_1, train_label_0_1, train_perc[i], test_0_1,
                          test_label_0_1, init_theta, 0.6, 0.99, 1, 1000)
  
  df01[i, ] = result
}
df01$perc = train_perc
df01_melt = melt(df01, id = "perc")
print(df01_melt)
plot2 = ggplot(df01_melt, aes(perc, value, color = factor(variable))) +
  geom_line(size = 1)
print(plot2)

#for _3_5 set
for(i in 1:20){
  result = batch_test(train_3_5, train_label_3_5, train_perc[i], test_3_5,
                      test_label_3_5, init_theta, 0.6, 0.99, 1, 1000)
  
  df35[i, ] = result
}
df35$perc = train_perc
df35_melt = melt(df35, id = "perc")
plot3 = ggplot(df35_melt, aes(perc, value, color = factor(variable))) +
  geom_line(size = 1)
print(plot3)

#5b. learning curves for logistic loss
#logistic loss function
loss_fun = function(theta, data, label){
  data$one = rep(1, nrow(data))
  theta_x = c(data.matrix(data) %*% theta)
  loss = label * theta_x
  index1 = (loss <= 10 & loss >= -10)
  index2 = (loss < -10)
  loss[index1] = log(exp(loss[index1]) + 1)
  loss[index2] = 0
  loss = sum(loss) / length(label)
  return (loss)
}

#batch test for logistic loss
batch_testloss = function(train_data, train_label, train_perc, test_data, test_label,
                      init_theta,alpha, alpha_decay, thres, max_it){
  train_loss = c()
  test_loss = c()
  for(i in 1:10){
    set.seed(i)
    new_index = sample(1:nrow(train_data))
    sample_index = new_index[1:(nrow(train_data)*train_perc)]
    theta = train(train_data[sample_index, ], 
                  train_label[sample_index],
                  init_theta, alpha, alpha_decay, thres, max_it )
    train = loss_fun(theta,train_data[sample_index, ],
                                   train_label[sample_index])
    test = loss_fun(theta,test_data, test_label)
    train_loss = c(train_loss, train)
    test_loss = c(test_loss, test)
  }
  return(c(mean(train_loss), mean(test_loss)))
}


df_loss_01 = data.frame(train_loss = numeric(0), test_loss = numeric(0))
df_loss_35 = data.frame(train_loss = numeric(0), test_loss = numeric(0))
# _0_1 set
for(i in 1:20){
  result = batch_testloss(train_0_1, train_label_0_1, train_perc[i], test_0_1,
                      test_label_0_1, init_theta, 0.6, 0.99, 1, 1000)
 
  df_loss_01[i, ] = result
}
df_loss_01$perc = train_perc
df_loss_01_melt = melt(df_loss_01, id = "perc")

plot4 = ggplot(df_loss_01_melt, aes(perc, value, color = factor(variable))) +
  geom_line(size = 1) 
  
print(plot4)

# _3_5 set
for(i in 1:20){
  result = batch_testloss(train_3_5, train_label_3_5, train_perc[i], test_3_5,
                          test_label_3_5, init_theta, 0.6, 0.99, 1, 1000)
  
  df_loss_35[i, ] = result
}
df_loss_35$perc = train_perc
df_loss_35_melt = melt(df_loss_35, id = "perc")

plot5 = ggplot(df_loss_35_melt, aes(perc, value, color = factor(variable))) +
  geom_line(size = 1)
  
print(plot5)


