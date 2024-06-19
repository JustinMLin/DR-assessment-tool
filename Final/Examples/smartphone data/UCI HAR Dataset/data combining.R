train_X = read.table('train/X_train.txt')
train_y = read.delim('train/y_train.txt', header=FALSE)
train = cbind(train_y, train_X)

test_X = read.table('test/X_test.txt')
test_y = read.delim('test/y_test.txt', header=FALSE)
test = cbind(test_y, test_X)

data = rbind(train, test)

column_headers = c("activity", read.table('features.txt')[,2])

colnames(data) = column_headers

activities = factor(data[,1])

levels(activities) = c("walking", "walking upstairs", "walking downstairs", "sitting", "standing", "laying")
data[,1] = activities

write.csv(data, 'clean_data.csv')