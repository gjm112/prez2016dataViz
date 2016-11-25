library(RCurl)
x<-read.csv("/Users/gregorymatthews/Dropbox/StatsInTheWild/presPercentHistory.csv")

x$PctEC<-as.character(x$PctEC)
x$PctEC<-as.numeric(gsub("%","",x$PctEC))/100

x$Turnout<-as.character(x$Turnout)
x$Turnout<-as.numeric(gsub("%","",x$Turnout))/100

x$PercentVote<-as.character(x$PercentVote)
x$PercentVote<-substring(x$PercentVote,1,6)
x$PercentVote<-as.numeric(gsub("%","",x$PercentVote))/100

x$MarginPct<-as.character(x$MarginPct)
x$MarginPct<-as.numeric(gsub("%","",x$MarginPct))/100
x$MarginPct[3]<-(-0.013)

x$MarginVote<-as.numeric(as.character(x$MarginVote))
x$MarginVote[3]<-(-1677041)
png("/Users/gregorymatthews/Dropbox/StatsInTheWild/presPct.png",res=300,units="in",w=10,h=10)
col<-rep("black",nrow(x))
col[x$WinnerPart=="Rep."]<-"red"
col[x$WinnerPart=="Dem."]<-"blue"
plot(x$MarginPct,x$PctEC,xlim=c(-.2,.35),ylim=c(0.2,1),col=col,pch=16,ylab="Electoral College Vote Percentage",xlab="Popular Vote Margin")
abline(a=0.5,b=1)
abline(v=0)
abline(h=0.5)
#text(x$MarginPct,x$PctEC,x$Winner,col="black",pos=2)
ind<-x$Winner%in%c("Donald Trump","Franklin Roosevelt","Ronald Reagan","George W. Bush","John Quincy Adams","Abraham Lincoln","Barack Obama","Bill Clinton","Rutherford Hayes")
text(x$MarginPct[ind],x$PctEC[ind],x$Winner[ind],col="black",pos=c(2,2,2,4,4,2,4,2,2,4,4,2,2,2,4,2,4))
dev.off()

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/presVotesAll.png",res=300,units="in",w=10,h=10)
col<-rep("black",nrow(x))
col[x$WinnerPart=="Rep."]<-"red"
col[x$WinnerPart=="Dem."]<-"blue"
plot(x$MarginVote,x$PctEC,,xlim=c(-4000000,18000000),ylim=c(0.2,1),col=col,pch=16,ylab="Electoral College Vote Percentage",xlab="Popular Vote Margin")
abline(v=0)
abline(h=0.5)
#text(x$MarginVote,x$PctEC,x$Winner,col="black",pos=2)
ind<-x$Winner%in%c("Donald Trump","Franklin Roosevelt","Ronald Reagan","George W. Bush","John Quincy Adams","Abraham Lincoln","Barack Obama","Bill Clinton","Rutherford Hayes","Richard Nixon","Lyndon Johnson")
text(x$MarginVote[ind],x$PctEC[ind],x$Winner[ind],col="black",pos=c(2,2,2,4,2,4,2,4,2,2,4,4,2,2,2,4,2,2,2,4))
dev.off()

png("/Users/gregorymatthews/Dropbox/StatsInTheWild/presVotesSmall.png",res=300,units="in",w=10,h=10)
col<-rep("black",nrow(x))
col[x$WinnerPart=="Rep."]<-"red"
col[x$WinnerPart=="Dem."]<-"blue"
plot(x$MarginVote,x$PctEC,,xlim=c(-4000000,4000000),ylim=c(0.2,1),col=col,pch=16,ylab="Electoral College Vote Percentage",xlab="Popular Vote Margin")
abline(v=0)
abline(h=0.5)
#text(x$MarginVote,x$PctEC,x$Winner,col="black",pos=2)
ind<-x$Winner%in%c("Donald Trump","Franklin Roosevelt","Ronald Reagan","George W. Bush","John Quincy Adams","Abraham Lincoln","Barack Obama","Bill Clinton","Rutherford Hayes","Richard Nixon","Lyndon Johnson")
text(x$MarginVote[ind],x$PctEC[ind],x$Winner[ind],col="black",pos=c(2,4,2,2,2,4,2,4,2,2,4,4,2,2,2,4,2,2,2,4))
dev.off()

x<-x[order(x$Year),]
plot(x$Year,x$Turnout,type="l",xlab="Year",ylab="Turnout %")
abline(v=c(1850,1900,1950,2000),lty=3,col=rgb(0.5,0.5,0.5,0.5))
abline(h=c(3:8)/10,lty=2,col=rgb(0.5,0.5,0.5,0.5))



