# merge the salary and performance data

####################
## read performances
perf_goals <- read.csv("https://raw.githubusercontent.com/TaddyLab/hockey/master/results/performance-goals.csv")
rownames(perf_goals) <- paste(perf_goals$player,perf_goals$season,sep="_")
perf_corsi <- read.csv("https://raw.githubusercontent.com/TaddyLab/hockey/master/results/performance-corsi.csv")
rownames(perf_corsi) <- paste(perf_corsi$player,perf_corsi$season,sep="_")
perf_fenwick <- read.csv("https://raw.githubusercontent.com/TaddyLab/hockey/master/results/performance-fenwick.csv")
rownames(perf_fenwick) <- paste(perf_fenwick$player,perf_fenwick$season,sep="_")

colnames(perf_corsi) <- paste(colnames(perf_corsi),"corsi",sep=".")
colnames(perf_fenwick) <- paste(colnames(perf_fenwick),"fenwick",sep=".")

perf <- perf_goals
perf <- cbind(perf, perf_corsi[match(rownames(perf),rownames(perf_corsi)),-(1:2)])
perf <- cbind(perf, perf_fenwick[match(rownames(perf),rownames(perf_fenwick)),-(1:2)])

seasons <- levels(factor(perf$season))

##############
## read salary
salary <- read.table("https://raw.githubusercontent.com/TaddyLab/hockey/master/data/nhlsalaries.txt", 
                     sep="|", header=TRUE,quote="",as.is=TRUE,row.names=1)
rownames(salary)[rownames(salary)=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
rownames(salary)[rownames(salary)=="P. J._AXELSSON"] <- "P.J._AXELSSON"
colnames(salary) <- c(seasons,"total")

sal <- unlist(salary[,seasons])
names(sal) <- paste(rownames(salary), 
                    rep(seasons,each=nrow(salary)),
                    sep="_")
sal <- sal[match(rownames(perf),names(sal))]
names(sal) <- rownames(perf)

## who doesn't have salary? 
nosal <- which(is.na(sal)| sal==0)
(length(nosal))
head(perf[nosal,])
## remove them from calculations
perf <- perf[-nosal,]
sal <- sal[-nosal]

################################
## read-in team info
ft <- read.table("https://raw.githubusercontent.com/TaddyLab/hockey/master/data/firstteam.txt",
                 colClasses=c("factor","factor","numeric","factor"),
                 header=TRUE,sep="|")
rownames(ft) <- paste(ft$player,ft$season,sep="_")
team <- ft[match(rownames(perf),rownames(ft)),"team"]

#################################
## combine the data together
nhldf <- cbind(team,sal,perf)
saveRDS(nhldf, "Comps Hockey",compress=FALSE)

#################################
library(gamlr)
library(tgp)

nhldf <- readRDS("Comps Hockey")
nhldf$season <- factor(nhldf$season)
plot(sal ~ season, data=nhldf)

s <- 1:nrow(nhldf) #which( as.numeric(nhldf$season) <=3 )
plays <- factor(nhldf$player[s])
y <- log(tapply(nhldf$sal[s], plays, mean))

Xpm  <- as.matrix(tapply(nhldf$pm[s], plays, mean))
Xppm <- as.matrix(tapply(nhldf$ppm[s], plays, mean))
Xfp  <- as.matrix(tapply(nhldf$fp[s], plays, mean))
Xprob<- as.matrix(tapply(nhldf$prob[s], plays, mean))

pmgrid <- seq(-30,30,length=100)
fpgrid <- seq(0,1,length=100)

R <- 100

pmfit <- bcart(X=Xpm, Z=y, XX=pmgrid,R=R)
ppmfit <- bcart(X=Xppm, Z=y, XX=pmgrid,R=R)

fpfit <- bcart(X=Xfp, Z=y, XX=fpgrid,R=R)
probfit <- bcart(X=Xprob, Z=y, XX=fpgrid,R=R)

pdf(file="salreg-goals.pdf",width=8,height=3)

par(mfrow=c(1,2),mai=c(.9,.9,.1,.1))
plot(pmgrid[-100], pmfit$ZZ.mean[-100],type="l",lwd=2, bty="n", 
     xlab="plus-minus", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(pmgrid, ppmfit$ZZ.mean,col="green",lwd=2)
legend("topleft", bty="n", legend=c("partial","sample"),lwd=4, 
       col=c("green","grey25"))
plot(fpgrid, fpfit$ZZ.mean,type="l",lwd=2, bty="n",
     xlab="for-probability", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(fpgrid, probfit$ZZ.mean,col="green",lwd=2)
dev.off()

# John Tavares example for Comps talk 
library(dplyr)
JT_Sal <- nhldf %>%
  filter(player == "JOHN_TAVARES") %>%
  group_by(season) %>%
  select(player, season, ppm, sal) %>%
  mutate("gls_per_mil" = ppm/sal)

bargains <- nhldf %>% 
  select(player, season, sal, prob, ppm) %>%
  mutate("Gls/mil" = ppm/sal)
bargains <- bargains[order(-bargains$`Gls/mil`),]

nhldf <- rename(nhldf, PFP = prob, 
                PPM = ppm, 
                PM = pm, 
                FP = fp)

goals <- nhldf %>%
  select(player, team, season, PPM, PM, PFP, FP, beta)

goals %>% select(team) %>% recode(WPG = "WAS")

### same thing, but corsi
Xpm  <- as.matrix(tapply(nhldf$pm.corsi[s], plays, mean))
Xppm <- as.matrix(tapply(nhldf$ppm.corsi[s], plays, mean))
Xfp  <- as.matrix(tapply(nhldf$fp.corsi[s], plays, mean))
Xprob<- as.matrix(tapply(nhldf$prob.corsi[s], plays, mean))

pmgrid <- seq(-400,400,length=100)
fpgrid <- seq(0,1,length=100)

R <- 10

pmfit <- bcart(X=Xpm, Z=y, XX=pmgrid,R=R)
ppmfit <- bcart(X=Xppm, Z=y, XX=pmgrid,R=R)

fpfit <- bcart(X=Xfp, Z=y, XX=fpgrid,R=R)
probfit <- bcart(X=Xprob, Z=y, XX=fpgrid,R=R)

pdf(file="salreg-corsi.pdf",width=8,height=3)
par(mfrow=c(1,2),mai=c(.9,.9,.1,.1))
plot(pmgrid, pmfit$ZZ.mean,type="l",lwd=2, bty="n", 
     xlab="plus minus", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(pmgrid, ppmfit$ZZ.mean,col="darkorange",lwd=2)
legend("topleft", bty="n", legend=c("partial","sample"),lwd=4, 
       col=c("darkorange","grey25"))
plot(fpgrid, fpfit$ZZ.mean,type="l",lwd=2, bty="n",
     xlab="for-probability", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(fpgrid, probfit$ZZ.mean,col="darkorange",lwd=2)
dev.off()

par(mfrow=c(1,2))
plot(pmgrid, pmfit$ZZ.mean,type="l",ylim=c(-1.5,3),lwd=2)
polygon(c(pmgrid,rev(pmgrid)), c(pmfit$ZZ.q1,rev(pmfit$ZZ.q2)), col=rgb(0,0,0,.1),border=FALSE)
plot(pmgrid, ppmfit$ZZ.mean,type="l",ylim=c(-1.5,3),lwd=2)
polygon(c(pmgrid,rev(pmgrid)), c(ppmfit$ZZ.q1,rev(ppmfit$ZZ.q2)), col=rgb(0,0,0,.1),border=FALSE)

var <- "pm"
s <- "20082009"
plot(log(sal) ~ pcut, data=nhldf, subset=which(nhldf$season==s))
## report coefficients on the pm and ppm statistics
fulllm <- lm(log(sal) ~ pm.corsi, data=nhldf)
coef(fulllm)[1:2]