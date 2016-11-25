library(maps)
library(mapproj)
library(dplyr)
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/candidatesTWOstates_dens1.png",res=300,units="in",w=20,h=10)

prez<-read.csv("/Users/gregorymatthews/pres16results.csv")
prez$fips<-as.numeric(as.character(prez$fips))
prez$cand<-(as.character(prez$cand))

data(county.fips)
prez$cand[!prez$cand%in%c("Donald Trump","Hillary Clinton")]<-"Other"
prez<-prez[prez$cand!="Other",]
prez<-prez[prez$st!="US",]
prez<-prez[is.na(as.numeric(prez[,2])),]
prez<-prez[order(prez$fips),]
prez$st<-as.character(prez$st)


col<-rep(NA, length(map("state", plot=FALSE)$names))
for (i in c(1:7,9:length(map("state", plot=FALSE)$names))){
  st<-strsplit(map("state", plot=FALSE)$names[i],":")[[1]][1]
  st<-state.abb[tolower(state.name)==st]
  temp<-prez[prez$st==st,]
  pct<-temp$pct[temp$cand=="Donald Trump"]/(temp$pct[temp$cand=="Donald Trump"]+temp$pct[temp$cand=="Hillary Clinton"])
  temp$total_votes[temp$total_votes>5000000]<-5000000
  col[i]<-rgb((pct-0.2)/0.6,0,1-(pct-0.2)/0.6,as.numeric(as.character(temp$total_votes))/5000000)
}


# draw map
map("state", col = col, fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")
title("Election 2016")

colleg<-rgb((seq(0.21,0.79,length=5)-0.2)/0.6,0,1-(seq(0.21,0.79,length=5)-0.2)/0.6,1) 
leg.txt<-paste0(c(round(100*seq(0.2,0.8,length=5),2)),"%")
legend("bottomright", leg.txt, horiz = FALSE, fill = colleg,cex=2)

colleg<-rgb(0.5,0,0.5,seq(0,5000000,length=5)/5000000) 
leg.txt<-paste0(c(round(seq(0,5000000,length=5),2)))
leg.txt[5]<-">5000000"
legend("bottomleft", leg.txt, horiz = FALSE, fill = colleg,cex=2)



dev.off()
