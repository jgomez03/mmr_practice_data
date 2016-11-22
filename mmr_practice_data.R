library(tidyverse)
library(apaTables)

####MAIN ANALYSIS
###load data
analytic.data <- read_csv("mmr_practice_data.csv")
apa.cor.table(analytic.data, filename="Table 1_APA.doc", table.number=1)

glimpse(analytic.data)

###extract key columns and only keep complete cases
analytic.data %>% select(exam, anxiety, preparation)
na.omit(analytic.data)

#center variables ###run the REGRESSION
#also where you can change what you call x and y

##add column with mean center anxiety
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(anxiety,center=T,scale=F)) )

##add column with mean center preparation
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric( scale(preparation,center=T,scale=F)) )

###run the REGRESSION
interaction.regression <- lm(exam~x.centered+z.centered+I(x.centered*z.centered), data=analytic.data, na.action = na.exclude)

summary(interaction.regression)
apa.reg.table(interaction.regression, filename="Table 2_APA.doc", table.number=2)

##explore interaction
#high prep

sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
simple.slope.plus.1SD <- lm(exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude) 
summary(simple.slope.plus.1SD)

apa.reg.table(simple.slope.plus.1SD)

#low prep
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)
simple.slope.minus.1SD <- lm(exam ~ x.centered + z.centered.at.minus.1SD
                             + I(x.centered*z.centered.at.minus.1SD), data=analytic.data,na.action=na.exclude)
summary(simple.slope.minus.1SD)
apa.reg.table(simple.slope.minus.1SD)

###Graphing
#pick anxiety to be x
sd.x <- sd(analytic.data$x.centered,na.rm=TRUE)

## 3D plot
library(MBESS)
intr.plot(b.0=47.05, b.x=15.01, b.z=9.45, b.xz=22.61,
          x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, 
          
          xlab="Anxiety (mean centered)", zlab="Preparation (mean centered)", ylab="Exam Grade",
          
          expand=1, hor.angle = 60, gray.scale=TRUE,
          
          line.wd = 4, zlim=c(0,100))

##making a 2D graph (part 1)


#choose x axis range
x.axis.range <-seq(-2*sd.x,2*sd.x,by=.25*sd.x)

#values of z that indicate high/low preparation
sd.z<-sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi= 1*sd.z
z.line.lo=-1*sd.z

#create predicted values for each line
#+1SD line
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.hi)

#-1SD line
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression,newdata=predictor.x.range.line.lo)

#pull into data frame
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)

##making a 2D graph (part 2)
#library(ggplot2)
#set default variables
my.plot<- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

#make +1 SD Z line 
my.plot <- my.plot + geom_line(color="black",linetype="dotted",size=1.5)

#make -1 SD Z line 
my.plot <- my.plot + geom_line(aes(x=x.axis.range, y=y.values.at.minus.1SD.z),
                               color="black",linetype="solid",size=1.5)

#set graph as apa
my.plot <- my.plot + theme_classic()

#adjust the axis 
my.plot <- my.plot + coord_cartesian(xlim=c(-2,2),ylim=c(0,100))
print(my.plot)

#label the lines
my.plot <- my.plot + labs(x="Anxiety (mean centered)", y="Exam Grade")
my.plot <- my.plot+annotate("text", x = -1, y = 68.5, label = "+1 SD Preparation")
my.plot <- my.plot+annotate("text", x = -1, y = 43.5, label = "-1 SD Preparation")
print(my.plot)


