println("Testing...")
println("Reading test data...")
data = read.table("nsltest.txt", sep="," ,
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
                              "dst_host_srv_rerror_rate","label","hardness"),
                  colClasses=c("label"="character","service"="character", "hardness" = "NULL"))
println("Generating five kinds of attack labels...")
data$attack.type <- apply(data, 1, function(a){
  label = a['label']
  label = type.map[[label]]
})

for (i in 1:nrow(data)) {
  label = data[i,'service']
  index = service.map[[label]]
  if (is.null(index)) {
    service.map[[label]] = length(service.map) 
  }
  data[i,'service'] = service.map[[label]]
}
data$service <- as.numeric(data$service)
data$attack.type <- as.factor(data$attack.type)

# Predict is normal
println("Detecting abnormal data...")
y.is.attack = as.logical(predict(dt.model, data[,feature.selection], type='vector') - 1)

# Predict attack type
println("Classifying abnormal data..")
y.attack.type = predict(nb.model, data[,feature.selection])

y.hat = y.attack.type

y.hat[y.is.attack == FALSE] = 'normal'
result = confusionMatrix(y.hat, data[,43])
print(result)