library(tidyverse)
library(ggplot2)

num_crews<-20
msn_time<-14
msn_req<-8
n_crews<-num_crews
ROM<-14
pre_msn<-1


##This is the CREW Interval
#(msn_time/(msn_req))
##Then each crew take off time:
#(msn_time/(msn_req))*(Crew#-1)+1


#NOW what is the Cycle Interval
##Crew 1 must take one crew interval after when CREW MAX takes off
##(msn_time/(msn_req))*(n_crews-1)+2+(msn_time/(msn_req))

Crew_Int<-(msn_time/(msn_req))
Crew_Cycle<-Crew_Int*(n_crews-1)+Crew_Int
White_Space<-Crew_Cycle-ROM-msn_time-pre_msn

Crew<-seq(1,n_crews,by=1)
Msn_start_1<-Crew_Int*(Crews-1)+2
Msn_stop_1<-Msn_start_1+msn_time
ROM_stop_1<-Msn_stop_1+ROM
White_stop_1<-ROM_stop_1+White_Space

Msn_start_2<-White_stop_1+1+pre_msn
Msn_stop_2<-Msn_start_2+msn_time
ROM_stop_2<-Msn_stop_2+ROM
White_stop_2<-ROM_stop_2+White_Space
Schedule<-data.frame(cbind(Crew, Msn_start_1, Msn_stop_1, ROM_stop_1, White_stop_1, Msn_start_2, Msn_stop_2, ROM_stop_2, White_stop_2))
maxx<-max(Schedule$White_stop_2)+30

###PLot the Schedule


plot(Schedule$Msn_start_1-1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="Day", ylab="Crew")
par(new=TRUE)
plot(Schedule$Msn_start_1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$Msn_stop_1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$ROM_stop_1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$White_stop_1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$Msn_start_2-1, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$Msn_start_2, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$Msn_stop_2, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$ROM_stop_2, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
par(new=TRUE)
plot(Schedule$White_stop_2, Schedule$Crew, col="light gray", xlim=c(0,maxx), pch=20, xlab="", ylab="")
#Segments
segments(Schedule$Msn_start_1-1, Schedule$Crew, Schedule$Msn_start_1, Schedule$Crew,
         col = "red", lty = par("lty"), xpd = FALSE)
segments(Schedule$Msn_start_1, Schedule$Crew, Schedule$Msn_stop_1, Schedule$Crew,
         col = "dark green", lty = par("lty"), xpd = FALSE)
segments(Schedule$Msn_stop_1, Schedule$Crew, Schedule$ROM_stop_1, Schedule$Crew,
         col = "red", lty = par("lty"), xpd = FALSE)
segments(Schedule$ROM_stop_1, Schedule$Crew, Schedule$White_stop_1, Schedule$Crew,
         col = "black", lty = par("lty"), xpd = FALSE)
segments(Schedule$White_stop_1, Schedule$Crew, Schedule$Msn_start_2, Schedule$Crew,
         col = "red", lty = par("lty"), xpd = FALSE)
segments(Schedule$Msn_start_2, Schedule$Crew, Schedule$Msn_stop_2, Schedule$Crew,
         col = "dark green", lty = par("lty"), xpd = FALSE)
segments(Schedule$Msn_stop_2, Schedule$Crew, Schedule$ROM_stop_2, Schedule$Crew,
         col = "red", lty = par("lty"), xpd = FALSE)
segments(Schedule$ROM_stop_2, Schedule$Crew, Schedule$White_stop_2, Schedule$Crew,
         col = "black", lty = par("lty"), xpd = FALSE)
#When do we have 8 up?
abline(v=(msn_time+2)-(msn_time/(msn_req)), lty="dashed", col="dark gray")
#abline(v=40)
legend(maxx-50, 5, legend=c("Rest", "Mission", "White Space", "1st day req msns met"),
       col=c("red", "dark green","black" ,"dark gray"), lty=c(1,1,1,2), cex=0.8, lwd=2)



##Now in GGPLOT.... 
Schedule_tidy<-gather(Schedule,"Event","Day",-Crew)
ggplot(data=Schedule_tidy, aes(Day, Crew))+
  geom_point(color="light gray")



