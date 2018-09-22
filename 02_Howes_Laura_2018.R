library(dplyr)

#-------------
#######
#Homework Week 02
#######
#'@author Laura Howes
#'2018-09-21

#######
#Problem 1 
#######

#1a
my_vec <- runif(n = 100, min = 0, max = 100)
str(my_vec)
#### str says that the numbers in my_vec range from 1 to 100, and then lists 
####the first 5 numbers of my vector. It tells a brief structure of my vector.
summary(my_vec)
####summary my_vec lists the Min, 1st Quartile, Median, Mean, 3rd Quartile values of my_vec
####summary is more descriptive than str. Str only lists that the numbers range between 1:100

#1b
hist(my_vec)
####hist(my_vec) plots of histogram of my_vec, where it shows the frequency 
####of the values listed in the vector (grouped into ranges of ten, 
####eg 1-10, 11-20, etc)

####the help file for hist(my_vec) tells you about the description of 
####the function, the Usage, Arguments, Details, and Values of the 
####histogram. Also references and examples.

#1c
data("mtcars")
class(mtcars)
####Class tells you that mtcars is a data frame
str(mtcars)
####Str(mtcars) tells the structure of the data frame, which has 32 observations of 11 variables,
####and lists the vectors of each column in the data frame
summary(mtcars)
####Summary(mtcars) lists the Min, 1st Quartile, Median, Mean, 3rd Quartile,
####and Max values of each column in the data frame

#1d
?data.frame
####the help file for Data Frames tells you the that the function data.frame() 
####creates data frames, tightly coupled collections of variables which share 
####many of the properties of matrices and of lists, used as the fundamental 
####data structure by most of R's modeling software.

######
#Problem 2
######

#2a
vole_vaso <- c(98,96,94,88,86,82,77,74,70,60,
               59,52,50,47,40,35,29,13,6,5)
####I messed up 3 times saying Vole vasopressin

#2b
summary(vole_vaso)
####The mean of the sample is 58.05
####median is 59.50
sd(vole_vaso)
####sd is 29.75244
IQR(vole_vaso)
####interquartile range is 44.25

#2c
SEmean <- sd(vole_vaso)/mean(vole_vaso)%>%
  sqrt()
SEmean  
####SE of the mean = 3.90500397

#2d
####The standard error of the mean tells you how well you can esimate the 
####precision of your mean of your range of values.
####It's a measure of the dispersion of sample means from the actual population
####or "true" mean.

#######
#Problem 3
#######

#3a

vole_vaso_data.frame <- data.frame(vole_vaso)
vole_vaso_data.frame
vole_vaso_sample_size_10 <- vole_vaso_data.frame %>% sample_n(size = 10, replace = TRUE)
vole_vaso_sample_size_10

quantile(vole_vaso, probs = 0.75)

vole_vaso_sample_size_10_vector <- unlist(vole_vaso_sample_size_10)
quantile(vole_vaso_sample_size_10_vector, probs = 0.75)

####The upper quartile of the resample with a sample size of 10 is 74. 

#3b Build an initial data frame for simulations with the sample sizes 5 through 20. 
#Have 100 simulations per sample size.

samp_sim <- data.frame(samp_size = rep(5:20, time = 100))     

#3c Use this data frame to get simulated upper quartiles for each sample size.

vole_samp_sin_uppqnt <- samp_sim %>% 
  rowwise() %>%
  mutate(uppqnt= quantile(sample(vole_vaso, size = samp_size, replace = TRUE), probs = 0.75))

#3d With a plot, make a guesstimate as to the best sample size for 
####estimating the upper quartile of the population.

plot(uppqnt ~ samp_size, data = vole_samp_sin_uppqnt)

####the best estimate of the best sample size to estimate the upper quartile is 10

#4a With the upper quantile simulations, calculate the SD for each sample size using dplyr

sd_uppqnt_sample <- vole_samp_sin_uppqnt %>%
  group_by(samp_size) %>%
  summarize(sd_up_qnt = sd(uppqnt), ave_upp_quant = mean(uppqnt)) %>%
  ungroup()
sd_uppqnt_sample

#4b. What does this value, the standard error of the upper quartile, mean?

####The standard error of the upper quartile is the standard deviation of the mean of the 
####top 25 percent of the data

#4c. What is the CI of the upper quartile with a sample size of 10. What does this mean?

SE_sample_10 <- sd_uppqnt_sample[6,2]
SE_sample_10
Confidence_Int_sample_10 <- SE_sample_10*1.96
Confidence_Int_sample_10
#The CI of the upper quartile is sample 10 is 23.21368

####the confidence interval means that there is a 95% chance the value lies within that range.



