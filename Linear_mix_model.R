
rm(list=ls())
#ass1=read.table("D:/U of A/Courses/590/assignment/ass1.txt",header=T,sep="	")
asstrt=read.table("D:/U of A/Courses/590/assignment/ass1-trt.txt",header=T,sep="	")
assw=read.table("D:/U of A/Courses/590/assignment/ass1-w.txt",header=T,sep="	")
attach(asstrt)
group <- factor(Group)
time <- factor(week)
id <- factor(ID)
Wight<-as.numeric(Weight)
head(asstrt)
par(cex = .6)

with(ass1, interaction.plot(time, group,Wight,
                             #ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                             ylab = "mean of weight", xlab = "week", trace.label = "group"))
lines(type = "h",x=week5)
demo1.aov <- aov(Wight ~ group * time + Error(id), data = asstrt)
summary(demo1.aov)

#############
rm(list=ls())
ass1=read.table("D:/U of A/Courses/590/assignment/ass1.txt",header=T,sep="	")
asstrt=read.table("D:/U of A/Courses/590/assignment/ass1-trt.txt",header=T,sep="	")
#assw=read.table("D:/U of A/Courses/590/assignment/ass1-w.txt",header=T,sep="	")
head(ass1)
library(nlme)
asstrt <- within(asstrt, {
  group <- factor(Group)
  time <- factor(week)
 
   id <- factor(ID)
})

str(asstrt)

longg <- groupedData(Wight ~ as.numeric(group) * as.numeric(time) | id, data = asstrt)
#table(ass1)
fit.cs <- gls(Wight ~ group * time, data = longg,
                               corr = corCompSymm(, form= ~ 1 | id) )
summary(fit.cs)
anova(fit.cs)




###############
#attach(asstrt)
#Wight=as.numeric(Wight)
fit.un <- gls(Wight~ group * time, data = longg,
              corr=corSymm(form = ~ 1 | id),
              weights = varIdent(form = ~ 1 | time),method="REML")
summary(fit.un)
anova(fit.un)
# UN <- gls(y ~ ses + time, data, corr=corSymm(form=~1|id), weights=varIdent(form=~1|time), method="REML", control=lmeControl(msMaxIter = 500, msVerbose = TRUE), na.action=na.omit)
####################
fit.ar1 <- gls(Wight ~ group * time, data = longg,
               corr = corAR1(, form= ~ 1 | id))
summary(fit.ar1)
anova(fit.ar1)
#########
fit.arh1 <- gls(Wight ~ group * time, data = longg,
                corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
anova(fit.arh1)
#################
anova(fit.cs, fit.un)
anova(fit.cs, fit.ar1)
anova(fit.cs, fit.arh1)
############
with(asstrt, pairwise.t.test(Wight, time, p.adjust.method="holm", paired=T))
##friedman.test(Wight ~ time | group , data=longg)
detach(asstrt)
#######################################################################
rm(list=ls())
assw=read.table("D:/U of A/Courses/590/assignment/ass1-w.txt",header=T,sep="	")
attach(assw)
group <- factor(Group)
time <- factor(week)
id <- factor(ID)
Wight<-as.numeric(Weight)
head(assw)
par(cex = .6)

with(assw, interaction.plot(time, group,Wight,
                            #ylim = c(5, 20), lty= c(1, 12), lwd = 3,
                            ylab = "mean of weight", xlab = "week", trace.label = "group"))
lines(type = "h",x=week5)
demo1.aov <- aov(Wight ~ group * time + Error(id), data = assw)
summary(demo1.aov)

#################second way##############
rm(list=ls())
asstrt=read.table("D:/U of A/Courses/590/assignment/ass1-trt.txt",header=T,sep="	")
library(nlme)
head(asstrt)
m0 = lme(Weight ~ Group*week, random = ~1|ID,data = asstrt)
summary(m0)

#####################################################
################
rm(list=ls())
ass1=read.table("D:/U of A/Courses/590/assignment/ass2.txt",header=T,sep="	")
ass1
demo1.aov <- aov(diff7.5 ~Group, data = ass1)
summary( demo1.aov)
TukeyHSD( demo1.aov)
demo1.aov1 <- aov(diff5.6 ~Group, data = ass1)
summary( demo1.aov1)
TukeyHSD( demo1.aov1)
