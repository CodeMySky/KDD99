library('class')
# read in data
data_10_percent = read.table("kddcup.data_10_percent", sep="," ,
    col.names=c("duration","protocol_type","service","flag","src_bytes","dst_bytes",
                "land","wrong_fragment","urgent","hot","num_failed_logins","logged_in",
                "num_compromised","root_shell","su_attempted","num_root","num_file_creations",
                "num_shells","num_access_files","num_outbound_cmds","is_host_login",
                "is_guest_login","count","srv_count","serror_rate","srv_serror_rate",
                "rerror_rate","srv_rerror_rate","same_srv_rate","diff_srv_rate",
                "srv_diff_host_rate","dst_host_count","dst_host_srv_count",
                "dst_host_same_srv_rate","dst_host_diff_srv_rate",
                "dst_host_same_src_port_rate","dst_host_srv_diff_host_rate",
                "dst_host_serror_rate","dst_host_srv_serror_rate","dst_host_rerror_rate",
                "dst_host_srv_rerror_rate","label"),
    colClasses=c("label"="character"))

dev_size <- floor(0.1 * nrow(data_10_percent))
dev_index <- sample(seq_len(nrow(data_10_percent)), size = dev_size)
data_10_percent <- data_10_percent[dev_index, ]

# split sample
train_size <- floor(0.3 * nrow(data_10_percent))
# x_train$protocol_type = as.numeric(x_train$protocol_type)

## set the seed to make your partition reproductible
set.seed(1)

train_index <- sample(seq_len(nrow(data_10_percent)), size = train_size)

train_data <- data_10_percent[train_index, ]
test_data <- data_10_percent[-train_index, ]

label_index = ncol(train_data)
x_train <- train_data[, 1:(label_index - 1)]
y_train <- train_data[, label_index]

x_test <- test_data[, 1:(label_index - 1)]
y_test <- test_data[, label_index]
# cleaning up
rm(list=c('test_data','train_data','dev_index','train_index'))

#change the data into binary classification
train_abnormal = y_train != 'normal.'
y_train[train_abnormal] = 'abnormal'
y_train[!train_abnormal] = 'normal'
y_train = factor(y_train)
test_abnormal = y_test != 'normal.'
y_test[test_abnormal] = 'abnormal'
y_test[!test_abnormal] = 'normal'
y_test = factor(y_test)
