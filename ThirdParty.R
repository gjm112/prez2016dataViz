library(maps)
library(mapproj)
library(dplyr)
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/thirdParty.png",res=300,units="in",w=20,h=10)

prez<-read.csv("/Users/gregorymatthews/pres16results.csv")
prez$fips<-as.numeric(as.character(prez$fips))
prez$cand<-(as.character(prez$cand))
data(county.fips)
prez$cand[!prez$cand%in%c("Donald Trump","Hillary Clinton")]<-"Other"
prez<-prez[prez$cand=="Other",]
prez<-prez[!is.na(as.numeric(prez[,2])),]
prez<-prez[order(prez$fips),]
tots<-aggregate(prez$votes, by=list(Category=prez$fips), FUN=sum)
totsVotes<-aggregate(prez$total_votes, by=list(Category=prez$fips), FUN=min)
prez<-as.data.frame(cbind(tots,totsVotes[,2]))
names(prez)<-c("fips","votes","total_votes")

prez<-merge(prez,county.fips,by.x="fips",by.y="fips",all.x=TRUE)

#prez<-merge(prez[prez$st=="WY",],county.fips,by.x="fips",by.y="fips",all.x=TRUE)
nbin<-100
#prez$colorBuckets <- as.numeric(cut(prez$pct*100, seq(0,100,length=nbin)))
prez$colorBuckets <- as.numeric(cut((prez$pct), seq(0,1,length=nbin)))

# define color buckets
#colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
colors <- rgb(seq(0,1,length=nbin),0,seq(1,0,length=nbin))
leg.txt <- paste0(c(1:nbin))

# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties
cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,prez$polyname)]
cnty.fips<-cnty.fips[!is.na(cnty.fips)]


for (i in 1:length(prez[prez$st=="WY","polyname"])-1){
  temp<-prez[prez$st=="WY","polyname"]
  tempFIPS<-prez[prez$st=="WY","fips"]
  cnty.fips[map("county", plot=FALSE)$names==temp[i]]<-as.numeric(as.character(tempFIPS[i]))
}

for (i in 1:length(prez[prez$st=="WI","polyname"])-1){
  temp<-prez[prez$st=="WI","polyname"]
  tempFIPS<-prez[prez$st=="WI","fips"]
  cnty.fips[map("county", plot=FALSE)$names==temp[i]]<-as.numeric(as.character(tempFIPS[i]))
}

for (i in 1:length(prez[prez$st=="NM","polyname"])-1){
  temp<-prez[prez$st=="NM","polyname"]
  tempFIPS<-prez[prez$st=="NM","fips"]
  cnty.fips[map("county", plot=FALSE)$names==temp[i]]<-as.numeric(as.character(tempFIPS[i]))
}

for (i in 1:length(prez[prez$st=="AZ","polyname"])-1){
  temp<-prez[prez$st=="AZ","polyname"]
  tempFIPS<-prez[prez$st=="AZ","fips"]
  cnty.fips[map("county", plot=FALSE)$names==temp[i]]<-as.numeric(as.character(tempFIPS[i]))
}

colorsmatched <- prez$colorBuckets[match(cnty.fips, prez$fips)]

prez<-prez[prez$polyname%in%map("county", plot=FALSE)$names,]
col<-rep(NA,length(map("county", plot=FALSE)$names))

prez$pct<-prez$votes/prez$total_votes
prez<-prez[!is.na(prez$fips),]

col <- unlist(lapply(map("county", plot=FALSE)$names,function(x){
  temp<-prez[prez$polyname==x,]
  out<-NA
  if (!is.na(temp$pct)){
  out<-rgb((1-2*temp$pct),1-2*temp$pct,1-2*temp$pct,1)  
  }
  return(out)
}))


# draw map
map("county", col = col, fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2,
    projection="polyconic")
title("Election 2016: Third Party Percentage")

colleg<-rgb(1-2*seq(0,0.5,length=9),1-2*seq(0,0.5,length=9),1-2*seq(0,0.5,length=9),1) 
leg.txt<-paste0(c(round(100*seq(0,0.5,length=9),2)),"%")
legend("bottomright", leg.txt, horiz = FALSE, fill = colleg,cex=2)

#colleg<-rgb(0.5,0,0.5,seq(0,50000,length=9)/50000) 
#leg.txt<-paste0(round(seq(0,50000,length=9),2))
#leg.txt[9]<-paste0(">",leg.txt[9])
#legend("bottomleft", leg.txt, horiz = FALSE, fill = colleg,cex=2)


dev.off()
