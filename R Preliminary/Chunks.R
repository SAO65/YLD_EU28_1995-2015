# Expansion of morbidity

#Years lived with disability (YLDs)
# Years of life lived with any short-term or long-term health loss.

# Healthy life expectancy, or health-adjusted life expectancy (HALE)
#The number of years that a person at a given age can expect to live in good health, 
# taking into account mortality and disability.

# Raw Data
setwd("~/Desktop/YLD_EU28_1995-2015/R Preliminary")
HALE.raw.data <- read.csv("./IHME-GBD_2015_DATA-7c514cb9-1/IHME-GBD_2015_DATA-7c514cb9-1.csv")
LE.raw.data <- read.csv("./IHME-GBD_2015_DATA-b8689aa2-1/IHME-GBD_2015_DATA-b8689aa2-1.csv")

head(LE.raw.data)
head(HALE.raw.data)

# Tidy Data
library(data.table)
library(ggplot2) 

attach(LE.raw.data)
attach(HALE.raw.data)

DT.LE <- data.table(LE.raw.data)
DT.HALE <- data.table(HALE.raw.data)

tables()

######################################################

# Tidy Data 

data.LE.1995.male <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Male", ]
data.HALE.1995.male <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Male", ]
data.YLD.1995.male <- data.LE.1995.male$val - data.HALE.1995.male$val

data.LE.1995.male.italy <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Male" & DT.LE$location == "Italy", ]
data.HALE.1995.male.italy <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Male" & DT.LE$location == "Italy", ]
data.YLD.1995.male.italy <- data.LE.1995.male.italy$val - data.HALE.1995.male.italy$val

data.LE.2015.male <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Male", ]
data.HALE.2015.male <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Male", ]
data.YLD.2015.male <- data.LE.2015.male$val - data.HALE.2015.male$val

data.LE.2015.male.italy <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Male" & DT.LE$location == "Italy", ]
data.HALE.2015.male.italy <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Male" & DT.LE$location == "Italy", ]
data.YLD.2015.male.italy <- data.LE.2015.male.italy$val - data.HALE.2015.male.italy$val


data.LE.1995.female <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Female", ]
data.HALE.1995.female <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Female", ]
data.YLD.1995.female <- data.LE.1995.female$val - data.HALE.1995.female$val

data.LE.1995.female.italy <- DT.LE[DT.LE$year == "1995" & DT.LE$sex == "Female" & DT.LE$location == "Italy", ]
data.HALE.1995.female.italy <- DT.HALE[DT.HALE$year == "1995" & DT.HALE$sex == "Female" & DT.LE$location == "Italy", ]
data.YLD.1995.female.italy <- data.LE.1995.female.italy$val - data.HALE.1995.female.italy$val

data.LE.2015.female <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Female", ]
data.HALE.2015.female <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Female", ]
data.YLD.2015.female <- data.LE.2015.female$val - data.HALE.2015.female$val

data.LE.2015.female.italy <- DT.LE[DT.LE$year == "2015" & DT.LE$sex == "Female" & DT.LE$location == "Italy", ]
data.HALE.2015.female.italy <- DT.HALE[DT.HALE$year == "2015" & DT.HALE$sex == "Female" & DT.LE$location == "Italy", ]
data.YLD.2015.female.italy <- data.LE.2015.female.italy$val - data.HALE.2015.female.italy$val


##############################################################

# Expansion of female and male morbidity

plot(data.HALE.1995.male$val, 
     data.YLD.1995.male, 
     col="blue",
     cex=1,
     pch=1,
     xlim = c(52,75), 
     ylim=c(6, 12), 
     axes=F,
     xlab="Health-Adjusted Life expectancy (HALE)",
     ylab="Years Lived with Disability (YLD)")

points(data.HALE.2015.male$val, 
       data.YLD.2015.male, 
       col="blue",
       cex=1.2,
       pch=16)

points(data.HALE.1995.female$val, 
       data.YLD.1995.female, 
       col="red",
       cex=1,
       pch=1)

points(data.HALE.2015.female$val, 
       data.YLD.2015.female, 
       col="red",
       cex=1.2,
       pch=16)

axis(1, col.axis="black", las=1, cex.axis=0.7, tck=-.01)
axis(2, col.axis="black", las=2, cex.axis=0.7, tck=-.01)

legend(67, 8,  
       c("Year 1995 Males","Year 2015 Males", "Year 1995 Females","Year 2015 Females"),
       pch=c(1,16,1,16),
       lty=c(2,1, 2,1),
       cex=c(.9,.9,.9,.9),
       col=c("blue", "blue", "red", "red"),
       bty = "n")

arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)


######################################################

# Accelerating rate of increase of female morbidity

abline(lm(data.YLD.1995.male ~ data.HALE.1995.male$val), col="blue", lty=2)
summary(lm(data.YLD.1995.male ~ data.HALE.1995.male$val))
confint(lm(data.YLD.1995.male ~ data.HALE.1995.male$val))


abline(lm(data.YLD.2015.male ~ data.HALE.2015.male$val), col="blue", lty=1)
summary(lm(data.YLD.2015.male ~ data.HALE.2015.male$val))
confint(lm(data.YLD.2015.male ~ data.HALE.2015.male$val))


abline(lm(data.YLD.1995.female ~ data.HALE.1995.female$val), col="red", lty=2)
summary(lm(data.YLD.1995.female ~ data.HALE.1995.female$val))
confint(lm(data.YLD.1995.female ~ data.HALE.1995.female$val))


abline(lm(data.YLD.2015.female ~ data.HALE.2015.female$val), col="red", lty=1)
summary(lm(data.YLD.2015.female ~ data.HALE.2015.female$val))
confint(lm(data.YLD.2015.female ~ data.HALE.2015.female$val))


######################################################




