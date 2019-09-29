# LIFE113: Workshop 10
# Inferential statistics - T-tests

# Data for this workshop is available at:
# https://archive.ics.uci.edu/ml/datasets/student+performance

# Citation: P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7. 
# http://www3.dsi.uminho.pt/pcortez/student.pdf

# Learning Objectives.
# Following this workshop you should know how to:
#  1. Construct null (H0) and alternative (HA) hypotheses
#  2. Test these hypotheses using an appropriate statistical test (in this case a T-test)
#  3. Understand the limitations of your statistical test
#  4. Implement procedure for statistical testing using R

# STEP 1:
# ------
#   + Create a new R project and save it somewhere sensible
#   + Start a new R script
#   + Download "student-mat.csv" from link above and save it in your current working directory (cwd)

# STEP 2:
#   + Read the data into a data frame and use inspect the data
#   + How many columns are there?
#   + Do you know what each column refers to?

math <- read.csv("student-mat.csv") # you might need to use sep=";"
# math <- read.csv("student-mat.csv", sep=";")

# STEP 3:
#  + Consider the G3 test scores (end of year score)
#  + Use R to generate a histogram of the G3 test scores (save the graph in your cwd)
#  + What should we do about all the "zero" scores?

help(hist)

hist(math$G3, xlab="G3 test score", main="", col="grey")

# STEP 4:
#  + Let's assume the zero scores are due to students who did not participate
#  + Can you remove these scores from the dataset and re-plot the bar chart?

math_nozeroG3 <- math[which(math$G3 > 0),] # Create a new data frame using conditional statement
hist(math_nozeroG3$G3, xlab="G3 test score (excluding zero scores)", main="", col="red") # Plot a histogram of the G3 scores in the new dataframe

# STEP 5:
#  + Does this distribution look normal to you?
#  + How can we test this?
#  + Run help("shapiro.test") and see what happens
#  + Do G3 test scores (try with and without zero scores) approximate a normal distribution?

help("shapiro.test") # run help on any function to find out more info. RStudio is brill for this reason.
shapiro.test(math$G3) # run the Shapiro normality test on G3 scores.
shapiro.test(math_nozeroG3$G3) # run the Shapiro normality test on G3 scores with zero scores removed.

# STEP 6: 
#  + Consider the G3 test scores of female (F) and male (M) groups
#  + Use R to create a box plot for the two groups (exclude zero scores) (Save to your cwd)
#  + Which group do you think performed better by the look of it?
#  + Can you create two histograms for each group (M & F) and overlay them on the same plot (Save to your cwd)
#  + Construct a null hypothesis

boxplot(G3~sex, data=math_nozeroG3, xlab="Sex", ylab="G3 Score")

F_G3 = math_nozeroG3[which(math_nozeroG3$sex=="F"), ]$G3
M_G3 = math_nozeroG3[which(math_nozeroG3$sex == "M"), ]$G3

# Plotting an overlaid histogram

h1 <- hist(F_G3) # histogram for Females only
h2 <-  hist(M_G3) # histogram for Males only

col_F <- rgb(0,1,1,1/4) # Define a colour for Female bars
col_M <- rgb(1,0,1,1/4) # Define a colour for Male bars

plot(h1, col=col_F) # Create a plot "canvas" and add h1 histogram to it
plot(h2, col=col_M, add=T, xlab="G3 Test Scores (excluding zeros)") # Add h2 histogram using "add=T" parameter. Don't forget to label axes.

legend("topright", c("Male", "Female"), fill=c(col_M, col_F)) # This is how you add a legend. All graphs should have one of these

# OK. Based on the boxplots and the histogram let's construct a hypotheses.

# H_0: There is no significance in the difference between the mean G3 scores of females and males (in Portuguese secondary schools)
# H_A: There is a significance in the difference between the mean G3 scores ...

# STEP 7:
#  + We are going to us a T-test to test our hypotheses
#  + Do you know what a T-test is? Read MFS pp. 115 - 121
#  + Does it matter if the variances of our groups aren't the same? What are the group sizes?
#  + Does it matter whehter our group sizes aren't the same?
#  + Does it matter if scores within each group approximate a normal distribution?
#  + Run help("t.test") and see what happens


shapiro.test(F_G3)
shapiro.test(M_G3)

help("t.test")

# + Perform an independent (unpaired) T-test on the F and M data
# + Use the p-value from the test to determine whether to accept/recject your hypotheses.

t.test(M_G3, F_G3)

# p < 0.05: Reject H_0. Accept H_A. There is a significant difference ...


# CHECKPOINT: Do you know how to ..
#  + Load a .csv file into R?
#  + Generate a histogram?
#  + Generage a box plot for multiple groups?
#  + Determine whether data in a group approximates a normal distribution?
#  + Construct a null hypothesis based on two variables (e.g. sex and score)
#  + Test your hypothesis using a T-test
#  + Intepret the p-value from the test to decide whether to accept/reject your hypothesis?

# EXERCISES:

# Exercise 1 (for week 2 workshop)


boxplot(G3~Walc, data=math, xlab="In a romantic relationship", ylab="G3 Score")
hist(math$Walc)

group_1 = math_nozeroG3[which(math_nozeroG3$Walc <3), ]$G3 # Students who score less than 3 on weekly alcohol rating
group_2 = math_nozeroG3[which(math_nozeroG3$Walc >2), ]$G3 # Students who score less than 2 on weekly alcohol rating

a <- hist(group_1, col='red')
b <- hist(group_2, col='blue')

colour_1 = rgb(1,0,0,0.3)
colour_2 = rgb(0,0,1,0.3)

plot(a, col=colour_1, xlab="G3 Test Score")
plot(b, ,col=colour_2, add=T)

t.test(group_1, group_2)

help(plot)
