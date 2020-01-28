# Section 1.2 Exercises - page 23: Questions 1, 3, 13
# Section 1.3 Exercises - page 34: Questions 19, 20, 25
# Section 1.4 Exercises - page 44: Questions 30, 32
# Section 2.1 Exercises - page 70: Questions 1, 5, 8, 10
# Section 2.2 Exercises - page 78: Questions 15, 18, 25
# Section 2.3 Exercises - page 87: Question 39
# 
# Section 1.2
# 
# 1. Consider the strength data for beams given in Example 1.2. 
# 1.a. Construct a stem-and-leaf display of the data. 
#   What appears to be a representative strength value? 
#   Do the observations appear to be highly concentrated about the representative value or rather spread out? 

beam_strength = c(5.9, 7.2, 7.3, 6.3, 8.1, 6.8, 
                  7.0, 7.6, 6.8, 6.5, 7.0, 6.3, 
                  7.9, 9.0, 8.2, 8.7, 7.8, 9.7, 
                  7.4, 7.7, 9.7, 7.8, 7.7, 11.6, 
                  11.3, 11.8, 10.7)
stem(beam_strength)

# 1.b. Does the display appear to be reasonably symmetric about a representative value, or would you describe its shape in some other way? 

median(beam_strength)
mean(beam_strength)

# 1.c. Do there appear to be any outlying strength values?

bs_iqr = IQR(beam_strength)
bs_iqr
quantile(beam_strength)

bs_q3 = 8.85
bs_upper_limit = bs_q3 + (1.5 * bs_iqr)
bs_upper_limit

# 1.d. What proportion of strength observations in this sample exceed 10 MPa?

over_ten = sum(beam_strength > 10)
total = sum(beam_strength > 0)
total
over_ten / total

# 3. Construct a stem-and-leaf display using repeated stems, and comment on any interesting features of the display

grav_vals = c(0.31, 0.35, 0.36, 0.36, 0.37, 
              0.38, 0.40, 0.40, 0.40, 0.41,
              0.41, 0.42, 0.42, 0.42, 0.42,
              0.42, 0.43, 0.44, 0.45, 0.46, 
              0.46, 0.47, 0.48, 0.48, 0.48, 
              0.51, 0.54, 0.54, 0.55, 0.58, 
              0.62, 0.66, 0.66, 0.67, 0.68, 
              0.75)
stem(grav_vals)

cluster = sum(grav_vals > 0.39 & grav_vals < 0.478)
total = sum(grav_vals > 0)
cluster/total

median(grav_vals)
mean(grav_vals)

boxplot(grav_vals)
 
# 13.a. What proportion of the sampled herds had just one giraffe? 

herd_size = read.csv("C:\\xfer\\csv\\herd-sizes.csv")
one_horse_herd = herd_size$Freq[herd_size$HerdSize == 1]
herds_obsrvd = 1570
one_horse_herd / herds_obsrvd

# 13.b. What proportion of the sampled herds had six or more giraffes (characterized in the article as "large herds")? 

large_herds = sum(herd_size$Freq[herd_size$HerdSize >= 6])
large_herds
large_herds / herds_obsrvd

# 13.c. What proportion of the sampled herds had between 5 and 10 giraffes inclusive

middle_herd = sum(herd_size$Freq[herd_size$HerdSize >= 5 & herd_size$HerdSize <= 10])
middle_herd
middle_herd / herds_obsrvd

#13.d. Draw a histogram using relative frequency on the vertical axis. How would you describe the shape of this histogram?

## I could not get the hist() function to plot the frequency values on the y-axis, so I used barplot() instead.

barplot(herd_size$Freq, 
        col = "blue", 
        density = 65, 
        main = "Sizes of Giraffe Herds in Nambia",
        xlab = "Herd Size by Number of Giraffes",
        ylab = "Frequency of Herd Size",
        space = 0,
        names.arg = herd_size$HerdSize)

# Section 1.3
# 
# 19. Suppose the time (min) taken by a clerk to process a certain application form has a uniform distribution with a = 4 and b = 6. 
# 19.a. Draw the density curve and verify that the total area under the curve is indeed 1.

a = 4
b = 6

interval = b-a
prob_densf = 1/interval
mins = seq(a, b, length = 100)
prob =  rep(prob_densf, 100)

plot(mins, 
     prob, 
     type = "l", 
     xlim = c(1, 9), 
     ylim = c(0, 1),
     ylab = "Probability of Application Form Processed",
     xlab = "Time (Minutes)",
     main = "Application Processing")
polygon(c(a, mins, b), c(0, prob, 0), col="lightblue")

area_under_curve = prob_densf * interval
area_under_curve

# 19.b. In the long run, what proportion of forms will take between 4.5 min and 5.5 min to process? 

top_min = punif(5.5, a, b)
bottom_min = punif(4.5, a, b)
top_min - bottom_min

# At least 4.5 min to process? 

1 - bottom_min

# 19.c. What value separates the slowest 50% of all processing times from the fastest 50% (the median of the distribution)?

median(c(a,b))

# 19.d. What value separates the best 10% of all processing times from the remaining 90%? 

qunif(0.9, min = a, max = b)

# 20. Suppose that the reaction temperature x (C) in a certain chemical process has a uniform distribution with a = -5 and b = 5
# 20.a. In the long run, what proportion of these reactions will have a negative value of temperature? 

a2 = -5
b2 = 5

interval2 = b2 - a2
density_funct2 = 1/interval2
negative_vals = 0 - a2
negative_vals * density_funct2


# 20.b. In the long run, what proportion of temperatures will be between -2 and 2? 

bottom_temp = punif(-2, a2, b2)
top_temp = punif(2, a2, b2)
top_temp - bottom_temp

# Between -2 and 3?

next_temp = punif(3, a2, b2)
next_temp - bottom_temp

# 20.c. For any number k satisfying -5 < k < k + 4 < 5, what long-run proportion of temperatures will be between k and k + 4?

k = -1
k2 = k + 4

bottom_k = punif(k, a2, b2)
top_k = punif(k2, a2, b2)
top_k - bottom_k

# 25. The actual tracking weight of a stereo cartridge set to track at 3 g can be regarded as a 
#   continuous variable with density function f (x) = c[1 - (x - 3)^2] for 2 < x < 4 and f(x) = 0 otherwise. 

track_wt <- function(x) {
  c = 1 - ((x - 3)^2) / x
  c * (1 - ((x - 3)^2))
  }
plot(track_wt, 2, 4)

# 25.a. Determine the value of c [you might find it helpful to graph f(x)]. 
# 
# 25.b. What proportion of actual tracking weights exceed the target weight?

# 25.c. What proportion of actual tracking weights are within .25 g of the target weight


 
# Section 1.4
# 
# 30. Suppose that values are repeatedly chosen from a standard normal distribution.

std_nd = rnorm(1000)
hist(std_nd)

# 30.a. In the long run, what proportion of values will be at most 2.15? 

dnorm(2.15)

#   Less than 2.15?

1 - dnorm(2.149)

# 30.b. What is the long run proportion of selected values that will exceed 1.50?

dnorm(1.50)

#   That will exceed -2.00?

1 - dnorm(-2)

# 30.c. What is the long run proportion of values that will be between -1.23 and 2.85? 

top_end = dnorm(2.85)
top_end
bottom_end = 1 - dnorm(-1.23)
bottom_end

bottom_end - top_end

# 30.d. What is the long run proportion of values that will exceed 5?

dnorm(5)

#   That will exceed -5?

1 - dnorm(-5)

# 30.e. In the long run, what proportion of selected values z will satisfy |z| < 2.50?
#   
#   
#   
# 32.a. What value z* is such that the area under the standard normal curve to the left of z* is .9082?

qnorm(.9082)

# 32.b. What value z* is such that the area under the standard normal curve to the left of that value is .9080? 

qnorm(.9080)

# 32.c. What value z* is such that the area under the standard normal curve to the right of z* is .121?

left_auc = 1 - 0.121
left_auc
qnorm(left_auc)

# 30.d. What value z* is such that the area under the standard normal curve between -z* and z* is .754?

bottom_z = dnorm(-1.5341)
top_z = 1 - dnorm(1.5341)

top_z - bottom_z


# 32.e. How far to the right of 0 would you have to go to capture an upper-tail z curve area of .002? 

up_tail = 1 - 0.002
qnorm(up_tail) 

#    How far to the left of 0 would you have to go to capture this same lower-tail area?
  
qnorm(0.002)  

# Section 2.1
# 
# 1.The May 1, 2009, issue of The Montclarian reported the following sales figures ($ 1000s) 
#for a sample of homes in Alameda, California, that were sold the previous month:

home_sales = c(590, 815, 575, 608, 350, 1285, 408, 540, 555, 679)

# 1.a. Calculate and interpret the sample mean and median.

mean(home_sales)
median(home_sales)

# 1.b. Suppose the sixth observation had been 985 rather than 1285. How would the mean and median change?

home_sales_edit = replace(home_sales, home_sales == 1285, 985)
mean(home_sales_edit)
median(home_sales_edit)

# 1.c. Calculate a 20% trimmed mean by first trimming the two smallest and two largest observations.

mean(home_sales, trim=.2)

# 1.d. Calculate a 15% trimmed mean

mean(home_sales, trim=.15)

# 8. A target is located at the point 0 on a horizontal axis. Let x be the landing point of a shot aimed at the 
# target, a continuous variable with density function f(x)=.75(1-x^2) for-1<=x<=1. What is the mean value of x?

tgt_prac = function(x) {.75 * (1 -(x^2))}

plot(tgt_prac, -1, 1)
mean(tgt_prac(c(-1, 1)))

# Section 2.2
# 
# 15. In the article "Mechanical Reliability of Devices Subdermally Implanted into the Young of Long-Lived and Endangered Wildlife" 
# (J. of Materials Engr. and Performance, 2012: 1924-1931), researchers examined the mechanical reliability of
# a thin enclosure for a biotelemetry device to be subdermally implanted in young wild animals. 
# Six enclosure specimens were subjected to puncture tests. Each specimen was placed in a test apparatus, and 
# researchers recorded the necessary force (N) for the puncture head to cause initial cracks in the enclosure. 
# Here is the corresponding data: 
#   
#   2006.1	2065.2	2118.9	1686.6	1966.9	1792.5
# 
# 15.a. Calculate x~ and the deviations from the mean. 
# 
# 15.b. Use the deviations calculated in part (a) to obtain the sample variance and the sample standard deviation.

# 15.c. Compute the sample standard deviation using a calculator or software function to 
# confirm the accuracy of your answer in (b).

nec_force = c(1686.6, 1792.5, 1966.9, 2006.1, 2065.2, 2118.9)
sd(nec_force)

 
# 18. Traumatic knee dislocation often requires surgery to repair ruptured ligaments. One measure of recovery is 
# range of motion (measured as the angle formed when, starting with the leg straight, the knee is bent as far 
# as possible). The given data on postsurgical range of motion appeared in the article "Reconstruction of the 
# Anterior and Posterior Cruciate Ligaments After Knee Dislocation" (Amer. J. Sports Med., 1999: 189-197): 
   
 range_of_motion =  c(154, 142, 137, 133, 122, 126, 135, 135, 108, 120, 127, 134, 122)
 
# 18.a. What are the values of the sample mean and sample median?

mean(range_of_motion)
median(range_of_motion) 
 
# 18.b. An alternative computing formula for the numerator of s2 is: 

#Sxx = sum(x - mean(x))^2 = sum(x^2) - 1/n * (sum(x)^2)

#   Using this formula, determine the sample variance of the data.

numr_of_s2 = function(x) {sum(x^2) - (1/length(x)) * (sum(x)^2)}
numr_of_s2(range_of_motion)
n = length(range_of_motion)
n
numr_of_s2(range_of_motion)/(n - 1)

var(range_of_motion) # to verify function returns variance correctly

# 25. Let x represent the number of underinflated tires on an automobile of a certain type, and suppose that 
# p(0) = .4, p(1) = p(2) = p(3) = .1, and p(4) = .3, from which mean = 1.8.

vars = sum(((0 - 1.8)^2) * 0.4, ((1 - 1.8)^2) * 0.1, ((2 - 1.8)^2) * 0.1, ((3 - 1.8)^2) * 0.1, ((4 - 1.8)^2) * 0.3)
vars

# 25. a. Calculate the standard deviation of x. 

tires_sd = sqrt(vars)
tires_sd

# 25.b. For what proportion of such cars will the number of underinflated tires be within 1 standard deviation of the mean value? More than 3 standard deviations from the mean value?

tires_sd + 1.8
tires_sd - 1.8
(tires_sd * 3) + 1.8
