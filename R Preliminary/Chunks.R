# Raw Data
LE.raw.data <- read.csv("./IHME-GBD_2015_DATA-0e9d54cc-1/IHME-GBD_2015_DATA-0e9d54cc-1.csv")
HALE.raw.data <- read.csv("./IHME-GBD_2015_DATA-86b12d56-1/IHME-GBD_2015_DATA-86b12d56-1.csv")

# Tidy Data
library(data.table)

attach(LE.raw.data)
attach(HALE.raw.data)

DT.LE <- data.table(LE.raw.data)
DT.HALE <- data.table(HALE.raw.data)

tables()

DT.LE

data.LE.1995 <- DT.LE[DT.LE$year == "1995", ]
data.HALE.1995 <- DT.HALE[DT.HALE$year == "1995", ]
data.YLD.1995 <- data.LE.1995$val - data.HALE.1995$val

data.LE.2000 <- DT.LE[DT.LE$year == "2000", ]
data.HALE.2000 <- DT.HALE[DT.HALE$year == "2000", ]
data.YLD.2000 <- data.LE.2000$val - data.HALE.2000$val

data.LE.2005 <- DT.LE[DT.LE$year == "2005", ]
data.HALE.2005 <- DT.HALE[DT.HALE$year == "2005", ]
data.YLD.2005 <- data.LE.2005$val - data.HALE.2005$val

data.LE.2010 <- DT.LE[DT.LE$year == "2010", ]
data.HALE.2010 <- DT.HALE[DT.HALE$year == "2010", ]
data.YLD.2010 <- data.LE.2010$val - data.HALE.2010$val

data.LE.2015 <- DT.LE[DT.LE$year == "2015", ]
data.HALE.2015 <- DT.HALE[DT.HALE$year == "2015", ]
data.YLD.2015 <- data.LE.2015$val - data.HALE.2015$val

# Preliminary Point Estimates

min.HALE.1995 <- round(min(data.HALE.1995$val), 1)
max.HALE.1995 <- round(max(data.HALE.1995$val), 1)
median.HALE.1995 <- round(median(data.HALE.1995$val),1)
mad.HALE.1995 <- round(mad(data.HALE.1995$val), 1)

min.YLD.1995 <- round(min(data.YLD.1995), 1)
max.YLD.1995 <- round(max(data.YLD.1995), 1)
median.YLD.1995 <- round(median(data.YLD.1995),1)
mad.YLD.1995 <- round(mad(data.YLD.1995), 1)

min.HALE.2015 <- round(min(data.HALE.2015$val), 1)
max.HALE.2015 <- round(max(data.HALE.2015$val), 1)
median.HALE.2015 <- round(median(data.HALE.2015$val),1)
mad.HALE.2015 <- round(mad(data.HALE.2015$val), 1)

min.YLD.2015 <- round(min(data.YLD.2015), 1)
max.YLD.2015 <- round(max(data.YLD.2015), 1)
median.YLD.2015 <- round(median(data.YLD.2015),1)
mad.YLD.2015 <- round(mad(data.YLD.2015), 1)

# Violin Plots
library(vioplot)
x1 <- data.HALE.1995$val[data.HALE.1995$year==1995]
x2 <- data.HALE.2015$val[data.HALE.2015$year==2015]
vioplot(x1, x2, 
        names=c("1995", "2015"), 
        col="gold")
title("Violin Plots of Miles Per Gallon")

x1 <- data.YLD.1995
x2 <- data.YLD.2015
vioplot(x1, x2, 
        names=c("1995", "2015"), 
        col="gold")



# Expansion of morbidity
#Years lived with disability (YLDs)
# Years of life lived with any short-term or long-term health loss.

# Healthy life expectancy, or health-adjusted life expectancy (HALE)
#The number of years that a person at a given age can expect to live in good health, 
# taking into account mortality and disability.


plot(data.HALE.1995$val, 
     data.YLD.1995, 
     col="blue",
     cex=1.5,
     xlim = c(55,75), 
     ylim=c(7, 11), 
     axes=F,
     xlab="Healthy life expectancy (HALE)",
     ylab="Years lived with disability (YLDs)")


points(data.HALE.2015$val, 
       data.YLD.2015, 
       col="red",
       cex=1.5)

axis(1, c(min.HALE.1995, median.HALE.1995, max.HALE.1995), col="blue")
axis(2, c(min.YLD.1995, median.YLD.1995, max.YLD.1995), col="blue")
axis(3, c(min.HALE.2015, median.HALE.2015, max.HALE.2015), col="red")
axis(4, c(min.YLD.2015, median.YLD.2015, max.YLD.2015), col="red")

legend(57, 10.75,  
       c("Year 1995","Year 2015"),
       lwd=c(2,2),
       pch=c(1,1),
       cex=c(1.1,1.1),
       col=c("blue", "red"),
       bty = "n")

# Accelerating expansion of morbidity

abline(lm(data.YLD.1995 ~ data.HALE.1995$val), col="blue", lty=2)
abline(lm(data.YLD.2015 ~ data.HALE.2015$val), col="red", lty=2)

summary(lm(data.YLD.1995 ~ data.HALE.1995$val))
confint(lm(data.YLD.1995 ~ data.HALE.1995$val))

summary(lm(data.YLD.2015 ~ data.HALE.2015$val))
confint(lm(data.YLD.2015 ~ data.HALE.2015$val))


# Healthy life expectancy and disability gap

(gap.HALE.1995 <- round(max.HALE.1995 - min.HALE.1995, 1))
(gap.HALE.2015 <- round(max.HALE.2015 - min.HALE.2015, 1))
gap.HALE.2015 - gap.HALE.1995
((gap.HALE.2015 - gap.HALE.1995) / gap.HALE.1995) * 100

(gap.YLD.1995 <- round(max.YLD.1995 - min.YLD.1995, 1))
(gap.YLD.2015 <- round(max.YLD.2015 - min.YLD.2015, 1))
((gap.YLD.2015 - gap.YLD.1995) / gap.YLD.1995) * 100
gap.YLD.2015 - gap.YLD.1995










