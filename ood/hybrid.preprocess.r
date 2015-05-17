hybrid.preprocess <- function (input.file = '') {
  hybrid.preprocess.cleanup()
  hybrid.preprocess.load.libraries()
  data.set = list()
  # Change training data here
  #data.set[['training']] <- hybrid.preprocess.load.data("kddcup.data")
  data.set[['training']] <- hybrid.preprocess.load.data("kddcup.data_10_percent")
  # Change testing data here
  #data.set[['training']] <- hybrid.preprocess.load.data("nslfull.txt")
  data.set[['testing']] <- hybrid.preprocess.load.data("corrected")
  return(data.set)
}

# Clean up
hybrid.preprocess.cleanup <- function() {
  cat("\014")
  cat("Hybrid Anomaly Intrusion Detection System
    Proudly presented by Yiqing Liu, and Xiao Han
    Init...
    Cleanning up environment...\n")
  rm(list=setdiff(ls(envir=.GlobalEnv), lsf.str(envir=.GlobalEnv)), envir=.GlobalEnv)
}

# Load libraries
hybrid.preprocess.load.libraries <- function() {
  println("Loading libraries...")
  library('class')
  library('e1071')
  library('rpart')
  library('caret')
  library('randomForest')
  library('rattle')
  library('lattice')
  println("Libraries loaded.")
}

hybrid.preprocess.load.data <- function(input.file) {
  println(c("Loading ...",input.file))
  data.set = read.table(input.file, sep="," ,
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
            colClasses=c("label"="character","service"="character"))
  println("Removing ending period...")
  data.set$label <- apply(data.set, 1, function(a){
    label = gsub("\\.","",a[['label']])
  })
  
  println("Generating normal and abnormal label...")
  data.set$is.attack <- apply(data.set, 1, function(a){
    label = a['label']
    is.attack = label != 'normal'
  })
  
  println("Generating common and uncommon label...")
  data.set$is.common <- apply(data.set, 1, function(a){
    label = a['label']
    is.common = label %in% c('normal','smurf','neptune')
  })
  
  println("Generating five kinds of attack labels...")
  type.map <- list(
    'neptune'='dos','teardrop'='dos','back'='dos','land'='dos','pod'='dos','smurf'='dos',
    'apache2'='dos','mailbomb'='dos','processtable'='dos','udpstorm'='dos',
    'ftp_write'='r2l','warezclient'='r2l','warezmaster'='r2l','guess_passwd'='r2l',
    'imap'='r2l','multihop'='r2l','phf'='r2l','spy'='r2l',
    'named'='r2l','sendmail'='r2l','snmpgetattack'='r2l','snmpguess'='r2l','worm'='r2l','xlock'='r2l','xsnoop'='r2l',
    'loadmodule'='u2r','buffer_overflow'='u2r','perl'='u2r','rootkit'='u2r',
    'httptunnel'='u2r','ps'='u2r','sqlattack'='u2r','xterm'='u2r',
    'satan'='probe','ipsweep'='probe','nmap'='probe','portsweep'='probe',
    'mscan'='probe','saint'='probe',
    'normal'='normal')
  data.set$attack.type <- apply(data.set, 1, function(a){
    label = a[['label']]
    label = type.map[[label]]
  })
  data.set$attack.type <- as.factor(data.set$attack.type)
  
  println("Converting service type to integer...")
  service.type = levels(as.factor(data.set$service))
  service.map <<- list()
  for (i in 1:length(service.type)) {
    service.map[[service.type[i]]] <<- i
  }
  data.set$service <- apply(data.set, 1, function(a) {
    label = a[['service']]
    label = service.map[[label]]
  })
  data.set$service <- as.numeric(data.set$service)
  
  return (data.set)
}

println <- function (log) {
  cat(paste(log,"\n"))
}

data.set = hybrid.preprocess()