library(TED)
library(changepoint)

avrpre = 0
avrre = 0
avrjc = 0

avrdc = 0

for (seed in 1:10){
set.seed(seed) 

#Generate ARMA(1,1) time series
events.No = 10
piece = 21

period = 128
links.No.list = c(50, 100, 150, 200, 300, 400, 600, 800, 1200, 1600, 2400, 3200)
links.No = links.No.list[12]
ts.length = period * piece
all.ts = matrix(NA,links.No, ts.length)
for (i in 1:links.No){
  all.ts[i,] = c(arima.sim(n=ts.length,list(ar=runif(1,-1,1),ma=runif(1,-1,1))))
}

# All changepoints in background data
# all.changes = matrix(0,1,ts.length)
# for (i in 1:links.No){
#   change=cpt.meanvar(all.ts[i,], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=3)
#   location = cpts(change)
#   if (length(location)>0){
#     for (j in location){all.changes[1,j]=all.changes[1,j]+1}
#   }
# }
# ts.plot(all.changes[1,])

# 2nd changepoint detection in all changes of background data
# change=cpt.meanvar(all.changes[1,], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=10)
# all.changepoints = c(cpts.ts(change))
# abline(v=all.changepoints,lwd=2,col='red')

#Creates a matrix to record anomalous links

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

# #Changepoint detection for one time series
# one.test = all.ts[1,]
# #par(mar=c(5,5,3,3))
# plot(one.test,type='l',ylab='',xlab='Time Tick',yaxt='n')
# axis(side=2,at=c(-2,0,2,4,6,8),labels = c(2,4,6,8,10,12))
# change=cpt.meanvar(one.test, test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=3)
# one.changepoints = c(cpts.ts(change))
# abline(v=one.changepoints,lwd=2,col='red')

# 1st changepoint detection
t1 = proc.time()
all.ts.changes = matrix(0,links.No,ts.length)
all.changes = matrix(0,1,ts.length)
for (i in 1:links.No){
change=cpt.meanvar(all.ts[i,], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=3)
location = cpts(change)
if (length(location)>0){
  for (j in location){
    all.ts.changes[i,j]=1
    all.changes[1,j]=all.changes[1,j]+1
    }
}
}

#par(mfrow=c(3,1),mar=c(5,5,2,2))
#ts.plot(all.changes[1,],xlab='Time Tick',ylab='Change-points',sub='(a)')
# Shows the real exception moment
points.start = c()
for (k in 0:(events.No-1)){points.start=c(points.start,(period*(2*k+1)+1))}
points.end = points.start+period
# abline(v=points.start,lwd=2,col='blue')
# abline(v=points.end,lwd=2,col='green')
# ts.plot(all.changes[1,],xlab='Time Tick',ylab='Change-points',sub='(b)')

# 2nd changepoint detection
change=cpt.meanvar(all.changes[1,], test.stat = "Normal", method = "PELT", penalty="BIC", minseglen=5)
all.changepoints = c(cpts.ts(change))
t2 = proc.time()
dc =  t2[3] - t1[3]
#cat("detection time cost = ", dc, "\n")
#abline(v=all.changepoints,lwd=2,col='red')
#ts.plot(all.changes[1,],xlab='Time Tick',ylab='Change-points',sub='(c)')

# Aggregate adjacent changepoints
point.range=1.5*period
i=1
fix.changepoint=c()
while(i<length(all.changepoints)-1){
  if (all.changepoints[i]<50 || all.changepoints[i]>21*period-50){
    i=i+1
    next}
  point.end = all.changepoints[i] + point.range
  end=i
  for (j in (i+1):length(all.changepoints)){
    if (all.changepoints[j]>point.end){
      end=j
      break}
  }

  fix.changepoint=c(fix.changepoint,all.changepoints[i],all.changepoints[j-1])
  i=j
}

#abline(v=fix.changepoint,lwd=2,col='red')
# event detection with TED method, poor effect!
# specify a sliding window size 
# w <- 128 
# specify a significant level 
# alpha <- 0.05 
# events <- eventDetection(all.changes[1,],w,'white',parallel=FALSE,alpha, 'art')

# Gets the start and end time of the event
all.changepoints = fix.changepoint
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
cat("seed = ", seed, "\n")
cat("precision = ", precision, "\n")
cat("recall = ", recall, "\n")
avrpre = avrpre + precision
avrre = avrre + recall
avrdc = avrdc + dc


# Gets the link where the changepoint exists at the moment of the exception
realno = 0
avrjac = 0
avrchat = 0
for (no in 1:nrow(fix.events.period)){
  events.period = fix.events.period[no,1:2]
  detected.NO=fix.events.period[no,3]
  event.period = events.period[2]-events.period[1]+1
  suspicious.links = matrix(NA,1,event.period)
  suspicious.links.index=c()
  for (i in 1:links.No){
    index=which(all.ts.changes[i,]==1)
    if (length(index)>0){
      for (j in index){
        if (j %in% c(events.period[1]:events.period[2])){
          suspicious.links = rbind(suspicious.links,all.ts[i,events.period[1]:events.period[2]])
          suspicious.links.index=c(suspicious.links.index,i)
          break
        }
      }
    }
  }
  suspicious.links=suspicious.links[-1,]

  # Characterization
  # Calculation of the distance
  library("dtwclust")

  t3 = proc.time()
  dis = proxy::dist(suspicious.links, method = "SBD",znorm = TRUE)
  # MDS
  if (nrow(dis)<3){next}
  library("MASS")
  mds = isoMDS(dis)
  #plot(mds$points,xlab = "X", ylab = "Y")
  X = mds$points[,1]
  Y = mds$points[,2]
  # Event involved links
  event.involved.links=which(events.links[detected.NO,]==1)
  Anomaly = ifelse(suspicious.links.index %in% event.involved.links,'Yes','No')

  ts2point = data.frame(X,Y,Anomaly)

  # Display
  library(ggplot2)
  library(gridExtra)
  #p1=ggplot(ts2point, aes(x=X, y=Y, colour=Anomaly,shape=Anomaly)) + geom_point(size=1) + scale_color_manual(values=c("blue", "red")) + scale_shape_manual(values=c(3,1))+ggtitle("(a) MDS")+theme(plot.title = element_text(hjust = 0.5))
  kde = kde2d(X,Y)
  t4 = proc.time()
  chat = t4[3] - t3[3]
  avrchat = avrchat + chat

  # Heatmap
  # library(RColorBrewer)
  # cols = brewer.pal(5,"YlOrRd")
  # pal<-colorRampPalette(cols)
  # image(kde,col=pal(20))
  #p2=ggplot(data = ts2point, mapping = aes(x = X, y = Y)) + geom_point(size=1) +stat_density2d()+ggtitle("(b) KDE")+theme(plot.title = element_text(hjust = 0.5))
  #grid.arrange(p1,p2,nrow=1)
  # Calculate the k points closest to the density center
  density.center.index = which(kde$z == max(kde$z))
  density.center.row = density.center.index%%25
  density.center.col = ceiling(density.center.index/25)
  density.center = c(kde$x[density.center.row], kde$y[density.center.col])
  distance.vector = c()
  for (i in 1:nrow(mds$points)) {
    distance.vector = c(distance.vector, sqrt((density.center[1]-mds$points[i,][1])^2 + (density.center[2]-mds$points[i,][2])^2))
  }
  anomaly.center.index = which(distance.vector == min(distance.vector))
  k=50
  dis2center = dis[anomaly.center.index,]
  if (length(dis2center)>=k) {
  knn.index = order(dis2center,decreasing=FALSE)[1:(k)]
  } else {knn.index = order(dis2center,decreasing=FALSE)[1:length(dis2center)]}
  knn.index.global = suspicious.links.index[knn.index]
  num = knn.index.global %in% event.involved.links
  jaccard = length(intersect(knn.index.global, event.involved.links))/length(union(knn.index.global, event.involved.links))
  #cat("event number = ", no, "jaccard =", jaccard, "\n")
  avrjac = avrjac + jaccard
  realno = realno + 1
}
cat("jaccard = ", avrjac/realno, "\n")
avrjc = avrjc + avrjac/realno
# cat("average characterization time cost = ", avrchat/realno, "\n")

}
cat("avr precision = ", avrpre/10, "\n")
cat("avr recall = ", avrre/10, "\n")
#cat("avrdc = ", avrdc/10, "\n")
cat("avr jac = ", avrjc/10, "\n")