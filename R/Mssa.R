library("Rssa")
library("TED")
set.seed(123) 
# An example of using MSSA for data prediction and anomaly detection
# data("AustralianWine", package = "Rssa")
# wine <- window(AustralianWine, end = time(AustralianWine)[nrow(AustralianWine)-20])
# wine <- wine[,-1]
# total.f.wine = data.frame()
# L = 5
# for (i in 1:(nrow(wine)-L)){
# s.wine <- ssa(head(wine,L+i), L = L, kind = "mssa")
# f.wine <- rforecast(s.wine, groups = list(all = 1:5), len = 1, only.new = TRUE)
# total.f.wine = rbind(total.f.wine, f.wine)
# }
# r.wine = wine[(L+1):nrow(wine),]
# par(mfrow = c(2, 2)) 
# ts.plot(r.wine[, "Drywhite"], col = "black", ylab = "Dry")
# ts.plot(total.f.wine[, "Drywhite"], col = "red", ylab = "f.Dry")
# ts.plot(r.wine[, "Fortified"], col = "black", ylab = "Fort") 
# ts.plot(total.f.wine[, "Fortified"], col = "red", ylab = "f.Fort")
# dis = c()
# for (i in 1:nrow(r.wine)){
# dis = c(dis, (sum(r.wine[i,] - total.f.wine[i,])^2)^0.5)}
# par(mfrow = c(1, 1)) 
# ts.plot(dis)

#Generate RTT time series
period = 128
piece = 21
links.No.list = c(50, 100, 200, 400, 800, 1600, 3200)
links.No = links.No.list[7]
ts.length = period * piece
all.ts = matrix(NA,links.No, ts.length)
for (i in 1:links.No){
  all.ts[i,] = c(arima.sim(n=ts.length,list(ar=runif(1,-1,1),ma=runif(1,-1,1))))
}

#Creates a matrix to record anomalous links
events.No = 10
events.link.No = 50 
pointer = 0
events.links = matrix(0,events.No,links.No)
for (i in 1: events.No){
  anomaly.No = events.link.No
  if (pointer < links.No)
  {anomaly.links = pointer + anomaly.No
  events.links[i, (pointer+1) : anomaly.links] = 1
  pointer = anomaly.links}
  else {
    pointer = 0
    anomaly.links = pointer + anomaly.No
    events.links[i, (pointer+1) : anomaly.links] = 1
    pointer = anomaly.links
  }
}

#Create anomalous links
types <- c('box','rc','cr','sine')
shapes = sample(1:4,10,TRUE)

for (x in 1: links.No){
  for (y in 1:events.No){
    if (events.links[y,x]==1){
      all.ts[x,(period*(2*y-1)+1):(period*(2*y-1)+period)]=c(cbfs(type=types[shapes[y]], A=sample(5:10,1)))
    }
  }
}
#Matrix with separate series in the columns.
all.ts = t(all.ts)

#MSSA-based anomaly detection
total.f = data.frame()
L = 5
t1 = proc.time()
for (i in 1:(nrow(all.ts)-L)){
  s.data <- ssa(all.ts[i:(L+i),], L = L, kind = "mssa")
  f.data <- rforecast(s.data, groups = list(all = 1:5), len = 1, only.new = TRUE)
  total.f = rbind(total.f, f.data)
}
r.data = all.ts[(L+1):nrow(all.ts),]
# display two time series as the exemple
# par(mfrow = c(2, 2)) 
# ts.plot(r.data[, 1], col = "black", ylab = "1")
# ts.plot(total.f[, 1], col = "red", ylab = "f.1")
# ts.plot(r.data[, 2], col = "black", ylab = "2") 
# ts.plot(total.f[, 2], col = "red", ylab = "f.2")
dis = c()
for (i in 1:nrow(r.data)){
  dis = c(dis, (sum(r.data[i,] - total.f[i,])^2)^0.5)}

dis = c(rep(0,L), dis)
ts.plot(dis)
ano.points = c()
for (i in 1:(length(dis)-L+1)){
outlier = boxplot.stats(dis[i:(i+L-1)])$out
if (length(outlier)>0){ano.points = c(ano.points, i+5)}
}
#abline(v=ano.points,lwd=1,col='red')
t2 = proc.time()
cat("detection time cost = ", t2[3] - t1[3], "\n")
# Aggregate adjacent changepoints
point.range=1.5*period
i=1
fix.ano.point=c()
while(i<length(ano.points)-1){
  if (ano.points[i]<50 || ano.points[i]>21*period-50){
    i=i+1
    next}
  point.end = ano.points[i] + point.range
  end=i
  for (j in (i+1):length(ano.points)){
    if (ano.points[j]>point.end){
      end=j
      break}
  }
  
  fix.ano.point=c(fix.ano.point,ano.points[i],ano.points[j-1])
  i=j
}
abline(v=fix.ano.point,lwd=2,col='red')

# Gets the start and end time of the event
all.changepoints = fix.ano.point
detected.events = list()
pointer = 1
for (i in 1:(length(all.changepoints)-1)){
  if (all.changepoints[i+1]-all.changepoints[i]>period){
    detected.events=c(detected.events, list(all.changepoints[i]))
    pointer = pointer+1}
  else {
    detected.events=c(detected.events, list(c(all.changepoints[i],all.changepoints[i+1])))
    pointer = pointer+1
  }
}
for (j in 2:length(detected.events)){
  if (length(unlist(detected.events[j]))==1){
    if(unlist(detected.events[j]) %in% unlist(detected.events[j-1]))
    {detected.events[j]=NULL
    pointer = pointer - 1}
  }}

events.period = matrix(NA,pointer-1,2)
w=10
for (i in 1:(pointer-1)){
  if (length(unlist(detected.events[i]))==1){events.period[i,]=c(unlist(detected.events[i])-2*w,unlist(detected.events[i])+2*w)}
  else{events.period[i,]=c(unlist(detected.events[i])[1]-w, unlist(detected.events[i])[2]+w)}
}

# Get the real exception moment
period = 128
points.start = c()
for (k in 0:9){points.start=c(points.start,(period*(2*k+1)+1))}
points.end = points.start+period

tp = 0
fp = 0
fix.events.period = matrix(NA, nrow=0, ncol=3)
for (i in 1:length(points.start)){
  cnt = 0
  timetick = c()
  rseq = seq(points.start[i], points.end[i])
  for (j in 1:nrow(events.period)){
    dseq = seq(events.period[j,1],events.period[j,2])
    inter = intersect(dseq, rseq)
    if (length(inter)>20){
      timetick = c(timetick, dseq)
      cnt = 1
    }
  }
  tp = tp + cnt
  if (cnt>0){fix.events.period = rbind(fix.events.period, c(min(timetick), max(timetick), i))}
  else {fp = fp + 1}
}

precision = (nrow(events.period)-fp)/nrow(events.period)
recall = tp/events.No
cat("precision = ", precision, "\n")
cat("recall = ", recall, "\n")

# Gets the suspicious links 
realno = 0
avrjac = 0
avrchat = 0
for (no in 1:nrow(fix.events.period)){
t3 = proc.time()
events.period = fix.events.period[no,1:2]
detected.NO=fix.events.period[no,3] 
max.dis = max(dis[events.period[1]:events.period[2]])
max.dis.index = which(dis[events.period[1]:events.period[2]]==max.dis)+events.period[1]-1
difs = r.data[(max.dis.index-L),] - total.f[(max.dis.index-L),]
suspicious.links = order(difs, decreasing = TRUE)[1:50]
t4 = proc.time()
chat = t4[3] - t3[3]
avrchat = avrchat + chat
# Event involved links
event.involved.links=which(events.links[detected.NO,]==1)
jaccard = length(intersect(suspicious.links, event.involved.links))/length(union(suspicious.links, event.involved.links))
cat("event number = ", no, "jaccard =", jaccard, "\n")
avrjac = avrjac + jaccard
realno = realno + 1
}
cat("average jaccard =", avrjac/realno, "\n")
cat("average characterization time cost = ", avrchat/realno, "\n")



# Real data
#Load the data
# data <- read.csv(file="D:/10??????Ŀ/PycharmProjects/Artt/R/TSdata4_20150513_20150514.csv", header=T)
# setwd("/mnt/PycharmProject/Artt/data")
# data <- read.csv(file="TSdata4_20150513_20150514.csv", header=T)
# data <- read.csv(file="TSdata4_20150612_20150613.csv", header=T)
# data <- read.csv(file="TSdata4_20151129_20151203.csv", header=T)
# data = data[,-1]
# col.no = ncol(data)
# data = data[,-col.no]
# row.no = nrow(data)
# total.f = data.frame()
# L = 5
# for (i in 1:(row.no-L)){
#   s.data <- ssa(data[0:L+i,], L = L, kind = "mssa")
#   f.data <- rforecast(s.data, groups = list(all = 1:5), len = 1, only.new = TRUE)
#   total.f = rbind(total.f, f.data)
# }
# r.data = data[(L+1):row.no,]
# dis = c()
# for (i in 1:nrow(r.data)){
#   dis = c(dis, (sum(r.data[i,] - total.f[i,])^2)^0.5)}
# ts.plot(dis)
