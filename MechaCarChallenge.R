library(tidyverse)
library(dplyr)

#Deliverable 1
MechaCar_mpg <- read.csv('MechaCar_mpg.csv') #import dataset
head(MechaCar_mpg)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg) #generate multiple linear regression model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg)) #generate summary statistics


#Deliverable 2
Suspension_Coil <- read.csv('Suspension_Coil.csv') #import dataset

total_summary <- Suspension_Coil %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups = 'keep') #create summary table
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups = 'keep') #create summary table


#Deliverable 3
sample_table <- Suspension_Coil %>% sample_n(15) #randomly sample 15 data points
plt <- ggplot(sample_table,aes(x=log10(PSI))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot
t.test(log10(sample_table$PSI),mu=mean(log10(Suspension_Coil$PSI))) #compare sample versus population means

Suspension_Coil_filt <- Suspension_Coil[,c("Manufacturing_Lot","PSI")] #filter columns from mtcars dataset
Suspension_Coil_filt$Manufacturing_Lot <- factor(Suspension_Coil_filt$Manufacturing_Lot)
summary(aov(PSI ~ Manufacturing_Lot,data=Suspension_Coil_filt)) #compare means across multiple levels

