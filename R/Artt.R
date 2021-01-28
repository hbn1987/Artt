library(TED)
library(changepoint)

#Load the data
setwd("/mnt/PycharmProject/Artt/data")
data <- read.csv(file="TSdata4_20150513_20150514.csv", header=T)
# data <- read.csv(file="TSdata4_20150612_20150613.csv", header=T)
# data <- read.csv(file="TSdata4_20151129_20151203.csv", header=T)
row.no = nrow(data)
col.no = ncol(data)

# #Generate ARMA(1,1) time series
# period = 128
# piece = 21
# links.No = 1000
# ts.length = period * piece
# all.ts = matrix(NA,links.No, ts.length)
# for (i in 1:links.No){
#   all.ts[i,] = c(arima.sim(n=ts.length,list(ar=runif(1,-1,1),ma=runif(1,-1,1))))
# }

# # All changepoints in background data
# all.changes = matrix(0,1,ts.length)
# for (i in 1:links.No){
#   change=cpt.meanvar(all.ts[i,], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=3)
#   location = cpts(change)
#   if (length(location)>0){
#     for (j in location){all.changes[1,j]=all.changes[1,j]+1}
#   }
# }
# ts.plot(all.changes[1,])

# # 2nd changepoint detection in all changes of background data
# change=cpt.meanvar(all.changes[1,], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=period)
# all.changepoints = c(cpts.ts(change))
# abline(v=all.changepoints,lwd=2,col='red')

# #Creates a matrix to record anomalous links
# events.No = 10
# events.link.No = sample(65:100,events.No)#Here's a trick, we used a value greater than 50
# pointer = 0
# events.links = matrix(0,events.No,links.No)
# for (i in 1: events.No){
#   anomaly.No = events.link.No[i]
#   anomaly.links = pointer + anomaly.No
#   events.links[i, (pointer+1) : anomaly.links] = 1
#   pointer = anomaly.links
# }

# #Create anamlous links
# types <- c('box','rc','cr','sine')
# shapes = sample(1:4,10,TRUE)
# 
# for (x in 1: links.No){
#   for (y in 1:events.No){
#     if (events.links[y,x]==1){
#       all.ts[x,(period*(2*y-1)+1):(period*(2*y-1)+period)]=c(cbfs(type=types[shapes[y]], A=sample(5:10,1)))
#     }
#   }
# }


links.No = col.no - 1
ts.length = row.no
# 1st changepoint detection
all.ts.changes = matrix(0,links.No,ts.length)
all.changes = matrix(0,1,ts.length)
for (i in 2:links.No){
  change=cpt.meanvar(data[,i], test.stat = "Normal", method = "PELT", penalty="MBIC", minseglen=3)
  location = cpts(change)
  if (length(location)>0){
    for (j in location){
      all.ts.changes[i,j]=1
      all.changes[1,j]=all.changes[1,j]+1
    }
  }
}
par(mar=c(5,5,3,3))
plot(all.changes[1,4:(ts.length-4)],type='l', ylab='Change-points',xlab='Time',xaxt='n')#Delete the beginning and endding time points
axis(side=1,at=c(0,10,20,30,40),labels = c('2:00','6:30','11:00','15:30','20:00'))
# 2nd changepoint detection
change=cpt.meanvar(all.changes[1,4:(ts.length-4)], test.stat = "Normal", method = "PELT", penalty="BIC", minseglen=5)
all.changepoints = c(cpts.ts(change))
abline(v=all.changepoints,lwd=2,col='red')

# # Shows the real exception moment
# points.start = c()
# for (k in 0:9){points.start=c(points.start,(period*(2*k+1)+1))}
# points.end = points.start+period
# abline(v=points.start,lwd=2,col='blue')
# abline(v=points.end,lwd=2,col='green')
# abline(v=all.changepoints,lwd=2,col='red')

# event detection with TED method, poor effect!
# specify a sliding window size 
# w <- 128 
# specify a significant level 
# alpha <- 0.05 
# events <- eventDetection(all.changes[1,],w,'white',parallel=FALSE,alpha, 'art')

# Gets the start and end time of the event
period = 20
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
for (i in 1:(pointer-1)){
  if (length(unlist(detected.events[i]))==1){events.period[i,]=c(unlist(detected.events[i])-10,unlist(detected.events[i])+10)}
  else{events.period[i,]=c(unlist(detected.events[i])[1]-10, unlist(detected.events[i])[2]+10)}
}

# Gets the link where the changepoint exists at the moment of the exception
detected.NO=1#Dynamically changing this value to view other events
event.period = events.period[detected.NO,2]-events.period[detected.NO,1]+1
suspicious.links = matrix(NA,1,event.period)
suspicious.links.index=c()
for (i in 1:links.No){
  index=which(all.ts.changes[i,]==1)
  if (length(index)>0){
    for (j in index){
      if (j %in% c(events.period[detected.NO,1]:events.period[detected.NO,2])){
        suspicious.links = rbind(suspicious.links,data[,(i+1)][events.period[detected.NO,1]:events.period[detected.NO,2]])
        suspicious.links.index=c(suspicious.links.index,i)
        break
      }
    }
  }
}
suspicious.links=-suspicious.links[-1,]
# Characterization
# Calculation of the distance
library("dtwclust")
dis = proxy::dist(suspicious.links, method = "SBD",znorm = TRUE)
if (any(is.infinite(dis))){
dis[is.infinite(dis)] = max(dis[dis!=max(dis)])
}

# MDS
library("MASS")
mds = isoMDS(dis)
#plot(mds$points,xlab = "X", ylab = "Y")
X = mds$points[,1]
Y = mds$points[,2]
# # Event 1 involved links
# event.involved.links=which(events.links[1,]==1)
# anamoly = ifelse(suspicious.links.index %in% event.involved.links,'Yes','No')

ts2point = data.frame(X,Y)

# add for 2 pics.begin
# Display
library(ggplot2)
library(gridExtra)
ggplot(data = ts2point, mapping = aes(x = X, y = Y)) + geom_point(size=1) +stat_density2d()+theme(plot.title = element_text(hjust = 0.5))

# detected.NO=2#Dynamically changing this value to view other events
# event.period = events.period[detected.NO,2]-events.period[detected.NO,1]+1
# suspicious.links.2 = matrix(NA,1,event.period)
# suspicious.links.index.2=c()
# for (i in 1:links.No){
#   index=which(all.ts.changes[i,]==1)
#   if (length(index)>0){
#     for (j in index){
#       if (j %in% c(events.period[detected.NO,1]:events.period[detected.NO,2])){
#         suspicious.links.2 = rbind(suspicious.links.2,data[,(i+1)][events.period[detected.NO,1]:events.period[detected.NO,2]])
#         suspicious.links.index.2=c(suspicious.links.index.2,i)
#         break
#       }
#     }
#   }
# }
# suspicious.links.2=-suspicious.links.2[-1,]
# suspicious.links.2=-suspicious.links.2[-1003,]
# suspicious.links.2=-suspicious.links.2[-3090:-3100,]
# suspicious.links.2=-suspicious.links.2[-3650:-3660,]
# # Characterization
# # Calculation of the distance
# dis.2 = proxy::dist(suspicious.links.2, method = "SBD",znorm = TRUE)
# if (any(is.infinite(dis.2))){
#   dis.2[is.infinite(dis.2)] = max(dis.2[dis.2!=max(dis.2)])
# }
# if (any(dis.2<0)){
#   dis.2[dis.2<0] = 0
# }
# # MDS
# mds.2 = isoMDS(dis.2)
# #plot(mds$points,xlab = "X", ylab = "Y")
# X = mds.2$points[,1]
# Y = mds.2$points[,2]
# ts2point.2 = data.frame(X,Y)
# p2=ggplot(data = ts2point.2, mapping = aes(x = X, y = Y)) + geom_point(size=1) +stat_density2d()+ggtitle("(b) MDS+KDE on SBD (DEC 01)")+theme(plot.title = element_text(hjust = 0.5))+xlim(-10,10)+ylim(-15,15)
# grid.arrange(p1,p2,nrow=1)
# #add for 2 pics.end
kde = kde2d(X,Y)

# Heatmap
# library(RColorBrewer)
# cols = brewer.pal(5,"YlOrRd")
# pal<-colorRampPalette(cols)
# image(kde,col=pal(20))

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

# Most suspect nodes
k=100
dis2center = dis[anomaly.center.index,]
knn.index = order(dis2center,decreasing=FALSE)[2:(k+1)]
knn.index.global = suspicious.links.index[knn.index]
links.name = colnames(data)[knn.index.global+1]
sources.top=c()
targets.top=c()
for (i in links.name){
  sou.tar = strsplit(substr(i,2,nchar(i)), split = '...', fixed = T)
  sources.top=c(sources.top,unlist(sou.tar)[1])
  targets.top=c(targets.top,unlist(sou.tar)[2])
}
part.all.nodes = union(sources.top,targets.top)
nodes.top100 = union(sources.top[81:100],targets.top[81:100])
nodes.top80 = union(sources.top[51:80],targets.top[51:80])
nodes.top50 = union(sources.top[1:50],targets.top[1:50])


# Draw the network picture
library(networkD3)
# For test
# data(MisLinks)
# data(MisNodes)
# forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
#              Target = "target", NodeID = "name",
#              Nodesize = "size", Group = "group", opacity = 1, legend = TRUE,
#              opacityNoHover = TRUE)

# # Node numbering for all nodes
# nodes.string = colnames(data)[2:(length(colnames(data))-1)]
# sources = c()
# targets = c()
# for (i in nodes.string){
#   sou.tar = strsplit(substr(i,2,nchar(i)), split = '...', fixed = T)
#   sources=c(sources,unlist(sou.tar)[1])
#   targets=c(targets,unlist(sou.tar)[2])
#   }
# all.nodes = union(sources,targets)
# nodes.No = length(all.nodes)
# group.init = rep(1,times=nodes.No)
# size.init = rep(1,times=nodes.No)

# Node numbering for particial nodes
nodes.string = colnames(data)[2:(length(colnames(data))-1)]
sources = c()
targets = c()
for (i in nodes.string){
  sou.tar = strsplit(substr(i,2,nchar(i)), split = '...', fixed = T)
  sources=c(sources,unlist(sou.tar)[1])
  targets=c(targets,unlist(sou.tar)[2])
}
nodes.No = length(part.all.nodes)
group.init = rep("top 100",times=nodes.No)
size.init = rep(1,times=nodes.No)

# # Nodes grouping, showing all nodes
# top100.index = match(nodes.top100,all.nodes)
# group.init[top100.index]=2
# size.init[top100.index]=2
# top50.index = match(nodes.top50,all.nodes)
# group.init[top50.index]=3
# size.init[top50.index]=3
# Nodes.value = data.frame(name=all.nodes,group=group.init,size=size.init)

# Nodes grouping for part of the nodes
top80.index = match(nodes.top80,part.all.nodes)
group.init[top80.index]="top 75"
size.init[top80.index]=2
top50.index = match(nodes.top50,part.all.nodes)
group.init[top50.index]="top 50"
size.init[top50.index]=3
Nodes.value = data.frame(name=part.all.nodes,group=group.init,size=size.init)

# # Link numbering, showing all the links
# sources.num = c()
# targets.num = c()
# for (i in 1 : length(sources)){
#   sources.num=c(sources.num,match(sources[i],all.nodes)-1)
#   targets.num = c(targets.num,match(targets[i],all.nodes)-1)
# }
# Links.value = data.frame(source=sources.num,target=targets.num)
#
# forceNetwork(Links = Links.value, Nodes = Nodes.value, Source = "source",
#              Target = "target", NodeID = "name",
#              Nodesize = "size", Group = "group", opacity = 1, legend = TRUE,
#              opacityNoHover = TRUE)

# Link numbering for part of the links
sources.num = c()
targets.num = c()
for (i in 1 : length(sources)){
  if (sources[i] %in% part.all.nodes && targets[i] %in% part.all.nodes){
  sources.num=c(sources.num,match(sources[i],part.all.nodes)-1)
  targets.num = c(targets.num,match(targets[i],part.all.nodes)-1)
}}
Links.value = data.frame(source=sources.num,target=targets.num)

ColourScale <- 'd3.scaleOrdinal()
            .domain(["top 50","top 75", "top 100"])
           .range(["#d62728", "#9467bd", "#7f7f7f"]);'

forceNetwork(Links = Links.value, Nodes = Nodes.value, Source = "source",
             Target = "target", NodeID = "name",
             Nodesize = "size", Group = "group", opacity = 1, legend = TRUE,
             opacityNoHover = TRUE, colourScale = JS(ColourScale))
