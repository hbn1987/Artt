# subspace method
# example
# hilbert = function(n){i = 1:n; 1/outer(i-1,i,"+")}
# x = hilbert(100)[1:100,1:5]
# s = svd(x)
# nor.no = floor(length(s$d)/5)
# noise.no = length(s$d)-nor.no
# nor.eigen = c(s$d[1:nor.no],rep(0, times = noise.no ))
# noise.eighen = c(rep(0,times = nor.no),s$d[(nor.no+1):length(s$d)])
# D.nor = diag(nor.eigen)
# D.noise = diag(noise.eighen)
# nor.space = s$u %*% D.nor %*% t(s$v)
# noise.space = s$u %*% D.noise %*% t(s$v)
# spe = sum(noise.space^2)
# Q-statistic
# theta.1 = sum(noise.eighen)
# theta.2 = sum(noise.eighen^2)
# theta.3 = sum(noise.eighen^3)
# h0 = 1-(2*theta.1*theta.3)/(3*theta.2^2)
# Q.part1 = (1.645*(2*theta.2*(h0^2))^0.5)/theta.1
# Q.part2 = theta.2*h0*(h0-1)/(theta.1^2)
# Q = theta.1*((Q.part1+1+Q.part2)^(1/h0))

library(TED)
set.seed(123)
#Generate ARMA(1,1) time series
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
all.ts = t(all.ts)
w = 5
spelist = c()
Qlist = c()
t1 = proc.time()
for (i in 1:(nrow(all.ts)-w+1)){
  split.data <- all.ts[i:(i+w-1),]
  s = svd(split.data)
  nor.no = 4
  noise.no = 1
  nor.eigen = c(s$d[1:nor.no],rep(0, times = noise.no ))
  noise.eighen = c(rep(0,times = nor.no),s$d[(nor.no+1):length(s$d)])
  D.nor = diag(nor.eigen)
  D.noise = diag(noise.eighen)
  nor.space = s$u %*% D.nor %*% t(s$v)
  noise.space = s$u %*% D.noise %*% t(s$v)
  spe = sum(noise.space^2)
  spelist = c(spelist, spe)
  # Q-statistic
  theta.1 = sum(noise.eighen)
  theta.2 = sum(noise.eighen^2)
  theta.3 = sum(noise.eighen^3)
  h0 = 1-(2*theta.1*theta.3)/(3*(theta.2^2))
  Q.part1 = (1.645*(2*theta.2*(h0^2))^0.5)/theta.1
  Q.part2 = theta.2*h0*(h0-1)/(theta.1^2)
  Q = theta.1*((Q.part1+1+Q.part2)^(1/h0))
  Qlist = c(Qlist, Q)
  # cat("spe = ", spe, "Q =", Q, "\n")
}

spelist = c(rep(0,(w-1)), spelist)
Qlist = c(rep(0,(w-1)), Qlist)
ts.plot(spelist)
dif = spelist - Qlist
ano.points = c(which(dif>0))
#abline(v=ano.points,lwd=2,col='red')
t2 = proc.time()
cat("detection time cost = ", t2[3] - t1[3], "\n")

# Aggregate adjacent changepoints
period = 128
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

# characterization of the event
library("anomalous")
# example
# z = matrix(rnorm(3000),ncol = 100)
# y = tsmeasures(z)
# biplot.features(y)
# anomaly(y)
realno = 0
avrjac = 0
avrchat = 0
if (nrow(fix.events.period)>0){
for (no in 1:nrow(fix.events.period)){
  t3 = proc.time()
  events.period = fix.events.period[no,1:2]
  detected.NO=fix.events.period[no,3] 
  all.ts.piece = all.ts[events.period[1]:min(events.period[2],nrow(all.ts)),]
  y = tsmeasures(all.ts.piece)
  biplot.features(y)
  suspicious.links = anomaly(y, n=50)$index
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
}