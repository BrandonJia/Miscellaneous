library(recosystem)
setwd("~/Dropbox/Carlson SOM/Teaching/MSBA6430/09")

#Example in the slides
X=matrix(c(1,1,2,2,3,3,3,3,4,5,1,3,4,5,1,2,3,4,4,5,4,5,3,2,5,5,5,5,1,4),10,3)
train_set=X[-c(3,7),]
test_set=X[c(3,7),]

#Training with given parameter
r = Reco()
RS1=r$train(data_memory(train_set[,1],train_set[,2],train_set[,3],index1=TRUE), opts = c(dim = 2, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 1, niter = 20))

#Predicting
pred_rvec = r$predict(data_memory(test_set[,1],test_set[,2],test_set[,3],index1=TRUE), out_memory())

#RMSE
sqrt(mean((test_set[,3]-pred_rvec)^2))

#P and Q latent-factor matrices
P_file = out_file(tempfile())
Q_file = out_file(tempfile())
r$output(P_file, Q_file)
P=read.table(P_file@dest, header = FALSE, sep = " ")
Q=read.table(Q_file@dest, header = FALSE, sep = " ")
P
Q

Pnew=matrix(as.numeric(unlist(P)),dim(P)[1],dim(P)[2])
Qnew=matrix(as.numeric(unlist(Q)),dim(Q)[1],dim(Q)[2])

sum(Pnew[3,]*Qnew[5,])


#A more comprehensive example

X=read.table("sim_data.txt",header=F,sep=":")
n1=max(X[,1])
n2=max(X[,2])
N=dim(X)[1]

train_set=X[1:14300,]
test_set=X[14301:21450,]

r = Reco()
#Tuning
opts = r$tune(data_memory(train_set[,1],train_set[,2],train_set[,3]), opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), 
                                     costp_l2 = c(0.01,0.1,1), costq_l2 = c(0.01,0.1,1),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

#Training: using the tuned parameters
set.seed(123)
RS2=r$train(data_memory(train_set[,1],train_set[,2],train_set[,3],index1=TRUE), opts = c(opts$min, nthread = 1, niter = 20))

#Predicting
pred_rvec = r$predict(data_memory(test_set[,1],test_set[,2],test_set[,3],index1=TRUE), out_memory())

#RMSE
sqrt(mean((test_set[,3]-pred_rvec)^2))

#P and Q latent-factor matrices
P_file = out_file(tempfile())
Q_file = out_file(tempfile())
r$output(P_file, Q_file)
P=read.table(P_file@dest, header = FALSE, sep = " ")
Q=read.table(Q_file@dest, header = FALSE, sep = " ")

Pnew=matrix(as.numeric(unlist(P)),dim(P)[1],dim(P)[2])
Qnew=matrix(as.numeric(unlist(Q)),dim(Q)[1],dim(Q)[2])

sum(Pnew[3,]*Qnew[5,])




