###Final Project
###CS 555
###Nathan Horak

##Functions
differentiateMeteor <- function (x) { ##function to identify as Undifferentiated/Differentiated
  ifelse (x %in% undiff.list,'Undifferentiated','Differentiated')
}

undifferentiateType <- function (x) { ##function to identify type of Undifferentiated
  if (x %in% carbon.list)
    return ('Carbonaceous')
  if (x %in% ordinary.list)
    return ('Ordinary')
  if (x %in% rumuruti.list)
    return ('Rumuruti (R)')
  if (x %in% enstatite.list)
    return ('Enstatite')
  else
    return (NA)
}

ordinaryType <- function (x) { ##function to identify type of Ordinary
  if (x %in% h.list)
    return ('H Meteorite')
  if (x %in% l.list)
    return ('L Meteorite')
  if (x %in% ll.list)
    return ('LL Meteorite')
  else
    return (NA)
}

##Setup
set.seed(800) ##set seed to 800
library(stringr) ##packages
library(tidyverse)
library(car)
library(emmeans)

##Data Preparation
old_meteor <- as_tibble(read.csv("C:\\Users\\Nathan\\Documents\\Grad School\\CS 555 Summer 02 2021\\project\\Meteorite-Landings.csv"))
meteor <- old_meteor ##save original data set as old_meteor
undiff.list <- unique(c(meteor[str_detect(as.matrix(meteor$recclass),c('^C|^E|^H|^L|[0-9]|OC')),]$recclass)) ##lists for Undifferentiated
undiff.list <- undiff.list[str_detect(undiff.list,c('ite|Lunar|achon|K3'),negate=TRUE)] ##str detect
carbon.list <- undiff.list[str_detect(undiff.list,c('^C|OC'))]
ordinary.list <- undiff.list[str_detect(undiff.list,c('^H|^L'))]
rumuruti.list <- undiff.list[str_detect(undiff.list,c('^R'))]
enstatite.list <- undiff.list[str_detect(undiff.list,c('^E'))]
h.list <- ordinary.list[str_detect(ordinary.list,c('^H'))]
ll.list <- ordinary.list[str_detect(ordinary.list,c('^LL'))]
l.list <- ordinary.list[str_detect(ordinary.list,c('^L'))]
l.list <- l.list[str_detect(l.list,c('^LL'),negate=TRUE)]
meteor <- meteor %>% ##mutate tibble with functions
  mutate(Differentiated=sapply(meteor$recclass, differentiateMeteor),
    Undiff_Type=sapply(meteor$recclass, undifferentiateType),
    Ordinary_Type=sapply(meteor$recclass, ordinaryType))


meteor <- select(meteor, Name=name, Mass=mass..g., Differentiated, Undiff_Type, Ordinary_Type) ##select only needed columns
meteor <- meteor %>% filter(!is.na(Mass)) ##remove meteors with no given mass


meteor$Mass <- log(meteor$Mass) ##log transform the mass
meteor <- data.frame(meteor) ##turn to data frame
meteor <- subset(meteor, Undiff_Type!='Rumuruti (R)' | is.na(Undiff_Type)) ##remove all Rumuruti (R) meteors
meteor <- meteor[sample(nrow(meteor), 1000),] ##take sample of 1000 meteors
m1keep <- c('Name','Mass','Differentiated') ##create array of columns to keep for first scenario
meteor1 <- data.frame(meteor[m1keep]) ##data frame subset for first scenario
m2keep <- c('Name','Mass','Undiff_Type') ##create array of columns to keep for second scenario
meteor2 <- data.frame(meteor[m2keep]) ##data frame subset for second scenario
meteor2 <- na.omit(meteor2)
m3keep <- c('Name','Mass','Ordinary_Type') ##create array of columns to keep for third scenario
meteor3 <- data.frame(meteor[m3keep]) ##data frame subset for third scenario
meteor3 <- na.omit(meteor3)

##Plots
aggregate(meteor1$Mass, by=list(meteor1$Differentiated), sd) ##aggregate sd and boxplot for first scenario
boxplot(meteor1$Mass~meteor1$Differentiated, data=meteor1, main= 'Meteor Mass (g)',
        xlab= 'Meteor Label (Differentiated, Undifferentiated)', ylab= 'Logarithmic Mass (g)')

aggregate(meteor2$Mass, by=list(meteor2$Undiff_Type), sd) ##aggregate sd and boxplot for second scenario
boxplot(meteor2$Mass~meteor2$Undiff_Type, data=meteor2, main= 'Meteor Mass (g)',
        xlab= 'Meteor Label (Carbonaceous, Enstatite, Ordinary)', ylab= 'Logarithmic Mass (g)')

aggregate(meteor3$Mass, by=list(meteor3$Ordinary_Type), sd) ##aggregate sd and boxplot for third scenario
boxplot(meteor3$Mass~meteor3$Ordinary_Type, data=meteor2, main= 'Meteor Mass (g)',
        xlab= 'Meteor Label (H Meteorite, L Meteorite, LL Meteorite)', ylab= 'Logarithmic Mass (g)')

##Analysis
table(meteor1$Differentiated) ##table and aggregate mean for first scenario
aggregate(meteor1$Mass, by=list(meteor1$Differentiated), mean)
m1 <- aov(meteor1$Mass~meteor1$Differentiated, data=meteor1) ##one way ANOVA and summary
summary(m1)

table(meteor2$Undiff_Type) ##table and aggregate mean for second scenario
aggregate(meteor2$Mass, by=list(meteor2$Undiff_Type), mean)
m2 <- aov(meteor2$Mass~meteor2$Undiff_Type, data=meteor2) ##one way ANOVA and summary
summary(m2)

table(meteor3$Ordinary_Type) ##table and aggregate mean for third scenario
aggregate(meteor3$Mass, by=list(meteor3$Ordinary_Type), mean)
m3 <- aov(meteor3$Mass~meteor3$Ordinary_Type, data=meteor3) ##one way ANOVA and summary
summary(m3)
TukeyHSD(m3) ##Tukey adjustment for pairwise T-test

