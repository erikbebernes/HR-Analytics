#Kaggle HR Analytics
#Why are our best and most experienced employees leaving prematurely?

HR_comma_sep <- read.csv("~/Downloads/HR_comma_sep.csv", header=TRUE)
View(HR_comma_sep)
hr<-HR_comma_sep

library(Amelia)
missmap(hr)

#Making hours a discrete variable
quantile(hr$average_montly_hours, .33)
quantile(hr$average_montly_hours, .67)
hr$Hours_Discrete[hr$average_montly_hours <= 69]<- 'low'
hr$Hours_Discrete[hr$average_montly_hours >69  & hr$average_montly_hours < 134]<- 'average'
hr$Hours_Discrete[hr$average_montly_hours >=134]<- 'high'

#Making Satisfaction a discrete Variable
quantile(hr$satisfaction_level, .33)
quantile(hr$satisfaction_level, .67)
quantile(hr$satisfaction_level, .8)

hr$Sat_Discrete[hr$satisfaction_level <= 43]<- 'low'
hr$Sat_Discrete[hr$satisfaction_level >43  & hr$satisfaction_level < 68]<- 'average'
hr$Sat_Discrete[hr$satisfaction_level >=68]<- 'high'


#Visualizations
library(ggplot2)
boxplot(hr$satisfaction_level~hr$Hours_Discrete)
#visual of discrete hours and leaving
mosaicplot(table(hr$Hours_Discrete, hr$Left), main = 'Work Hours and Employees Leaving', shade=TRUE)
#workers with average hours are less likely to leave than workers with high and low hours
#visual of hours and leaving
ggplot(hr, aes(left, average_montly_hours))+geom_boxplot()
#employees who haven't left have hours between 60-125 a month with an average of 100. Employees who
#have left have an average of 125 hours a month
ggplot(hr, aes(Left, type, fill = Satisfaction))+geom_tile()+ggtitle('Satisfaction and Job Type')
#This heatmap shows satisfaction across different disciplines, and whether or not they left
#Almost all fields have higher satisfaction without leaving, exceptions are marketing and IT
ggplot(hr, aes(Satisfaction, Last_eval, color = Left))+geom_point()+ggtitle("Satisfaction and Last Evaluation")
#Employees that left seem to be grouped into three clusters in this scatterplot
library(corrplot)
cor(hrbestleft[,1:5])
cor(hr[sapply(hr, is.numeric)])
corrplot(cor(hr[sapply(hr, is.numeric)]))
#There is a strong negative correlation between satisfaction and left (lower satisfaction = leaving)
#There is a positive correlation between time at the company and leaving (people who stay longer tend to leave)


#Association Rules Analysis
library(arules)
hr$Work_accident<-as.factor(hr$Work_accident)
hr$left<-as.factor(hr$left)
hr$promotion_last_5years<-as.factor(hr$promotion_last_5years)
hr$Hours_Discrete<-as.factor(hr$Hours_Discrete)
hr$Sat_Discrete<-as.factor(hr$Sat_Discrete)
names(hr)
hrassoc<-hr[,c(6,7,8,9,10,11,12)]
rules<-apriori(hrassoc, parameter = list(support = .2, confidence = .7))
summary(hr$Left)
#since the majority of employees haven't left, it will be a good idea to reduce support and increase confidence
rules<-apriori(hrassoc, parameter = list(support = .05, confidence = .95))
#still not getting any interesting rules, so I'll make a new dataset with only left =1
hrleft<-hr[which(hrassoc$left==1),]
hrleft<-hrleft[,c(6:12)]
rules<-apriori(hrleft, parameter = list(support = .3, confidence = 1))
#interesting rules
#1.) of the people who left, 99% never received a promotion
#2.) 95% never had an accident
#3.) 60% were low salary
#4.) 71.5% had low job satisfaction


#Linear Models
sat<-lm(formula= Left~Satisfaction, data = hr)
#significant, R-Squared = .15
last<-lm(formula= Left~Last_Evaluation, data = hr)
# not significant
num<-lm(formula=Left~Num_Projects, data = hr)
#significant, but very low R Squared (0.00056)
avg<-lm(formula=Left~Avg_Monthly_Hours, data =hr)
#significant, Rsquared = .003912
time<-lm(formula=Left~Time_at_Company, data = hr)
#Significant, R-squared = .05
accident<-lm(formula=Left~Accidents, data = hr)
#Significant, R-squared = .02391
promotion<-lm(formula=Left~Promotion, data =hr)
#significant, R-squared = .003818
type<-lm(formula=Left~Type, data=hr)
#not significant
salary<-lm(formula=Left~Salary, data = hr)
#Medium and low are significant, R-squared = .0254

#Logistic Regression (classification)
hr$Left<-as.factor(hr$Left)
glm<-glm(Left~Satisfaction+Last_eval+Num_projects+Avg_monthly_hrs+Time_at_Company+Accident+Promotion_last_5_years+type+Salary, data=hr, family = binomial )
summary(glm)
#Satisfaction, last_eval, num_projects, avg_monthly hours, having an accident, promotions, type R and D, and low and medium salaries are all significant


#How to approach this problem
# Hypothesis 1: People with low satisfaction levels tend to leave
a, b
# True, a majority of the employees who have left have a satisfaction below .4.
#Most employees that have stayed have a satisfaction above .4

# Hypothesis 2: People with high salaries tend to stay
# This can be proven by simply finding percentages.
#6.62% of high salary employees have left
#29.68% of low salary employees have left
#20.43% of medium salary employees have left

# Hypothesis 3: People who have been promoted tend to stay
# Hypothesis 4: Employees performing well have higher satisfaction levels
hrbestleft<-hr[which(hr$Last_eval>40 & hr$Left == 1),]

#Correlations
cor(hrbestleft[,1:7])
#employees with last eval >40 and who have left
                Satisfaction    Last_eval Num_projects Avg_monthly_hrs Time_at_Company
Satisfaction      1.00000000  0.380986386  -0.74529276     -0.48170568     0.691060410
#high negative correlation between satisfaction and number of projects
#high negative correlation between satisfaction and average monthly hours
#high postive correlation between satisfaction and time at company
#high positive correlation between satisfaction and last eval

#high performing employees leave due to too many projects and too many hours...management may be over utilizing them

cor(hr[,1:7])
#comparison with entire dataset
                Satisfaction    Last_eval Num_projects Avg_monthly_hrs Time_at_Company
Satisfaction      1.00000000  0.105021214 -0.142969586    -0.025228539     -0.13372703
?hist
par(mfrow= c(1,2))
hist(hrbestleft$average_montly_hours, main = "Best Employees", col = "blue4"
     ,xlab = "Avg Monthly Hours")
hist(hr$average_montly_hours, main = "All Employees", col = "red",
     xlab = "Avg Monthly Hours")

mtext(text = "Average Hours Worked per Month",
      side = 3, line = 0, cex = 1, adj = .5)


?hist

par(mfrow = c(1,2))
hist3<-hist(hrbestleft$number_project, main = "Best Employees", col = "aquamarine4"
            ,xlab = "Number of Projects", breaks = 4) #best employees have more projects
hist4<-hist(hr$number_project, main = "All Employees"
            , col = "chartreuse", xlab = "Number of Projects", breaks = 4)



par(mfrow = c(1,2))
d<-density(hrbestleft$time_spend_company)
plot(d, col = "orange", main = "Best Employees", xlab = "Years at Company")
e<-density(hr$time_spend_company)
plot(e, col = "maroon", main = "All Employees", xlab = "Years at Company")
hrbestleft$time_spend_company<-as.numeric(hrbestleft$time_spend_company)




hist5<-hist(hrbestleft$Time_at_Company) # almost all of the best employees have been there four years or more
hist6<-hist(hr$Time_at_Company)
hr$
#Are the best employees being promoted
hr$promotion_last_5years<-gsub(0, "No", hr$promotion_last_5years)
hr$promotion_last_5years<-gsub(1, "Yes", hr$promotion_last_5years)
ggplot(hrbestleft, aes(promotion_last_5years))+geom_bar(color = "blue", fill = "blue")+ggtitle("Employees Promoted in the last 5 Years")
hr[which(hr$promotion_last_5years == 0)]<-"No"

?ggplot





plot1<-plot(hrbestleft$promotion_last_5_years) #uhhh...no. Only 4 of the 1,869 have been promoted .2%
plot2<-plot(hr$Promotion_last_5_years) #overall, only 319 of 14,999 employees have been promoted 2.1%
plot5<-plot(hrbeststay$Promotion_last_5_years)
#What types of jobs are the best employees in and what are they getting paid?
ggplot(hrbestleft, aes(sales, fill = salary))+geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Salary and Job type of the Good Employees Who Have Left')+xlab("Job Type")
#most of the good employees who left were in sales, support and technical roles and a majority had low salaries
#How does this compare with the dataset overall?
plot4<-ggplot(hr, aes(sales, fill = Salary))+geom_bar()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle('Salary and Job type of All Employees')+xlab("Job Type")
#not too much of a difference here. Let's try looking at employees who are high performing and have stayed
hrbeststay<-hr[which(hr$last_evaluation>.72 & hr$left == '0'),]
#not much of a difference here either
#Lets look at job satisfaction between the best employees who have stayed and the best who have left
par(mfrow = c(1,3))
hist(hrbestleft$satisfaction)
hist(hrbeststay$satisfaction) 
hist(hr$satisfaction)

p1<-ggplot()+geom_density(data = hrbestleft, aes(satisfaction_level), fill = 'green', alpha = .3)+
  geom_density(data = hrbeststay, aes(satisfaction_level), fill = 'red', alpha = .3)+
  geom_density(data = hr, aes(satisfaction_level), fill = 'blue', alpha = .3)+theme_light(base_size = 16)+xlab("Satisfaction Level")+ylab("")+
  ggtitle("Satisfaction Levels of Subsets")
p1

# So far it looks like significant variables are satisfaction, number of projects, average monthly hours and time at company
#Let's look a little bit deeper into these
#Are employees who have been at the company a long time being promoted more?
plot5<-ggplot(hr, aes(Time_at_Company, fill = Promotion_last_5_years))+geom_bar()
# There is no evidence suggesting loyal employees are being promoted

# Since good employees that left were working a lot of hours, there should be a strong relationship between hours and satisfaction
plot6<-ggplot(hr, aes(satisfaction_level, average_montly_hours, color = left, alpha = .3))+geom_point()+ggtitle("Hours and Satisfaction")
#very tight distributions
#Looking at good employees that left
plot7<-ggplot(hrbestleft, aes(Satisfaction, Avg_monthly_hrs))+geom_point()
#Surprised that so many with high satisfaction decided to leave
#Adding time at company
ggplot(hrbestleft, aes(satisfaction_level, average_montly_hours, color = time_spend_company))+geom_point()+ggtitle("Hours, Satisfaction and Years at Company of HRBestLeft")
#It looks like people with a high satisfaction that left were at the company longer than people that left with low satisfaction

#Conclusions:
# Good employees are leaving for the following reasons:
# 1.) They are working too many hours. Management is overutilizing employees that perform well.
# 2.) They aren't being promoted. In fact, no one is being promoted. There is limited opportunity for career growth at this company.
# 3.) Most good employees that leave fall into two categories: they are working a lot of hours and have very low satisfaction, or they have high satsifaction and have been at the company 5 or more years.

# I'll run a random forest model to see how accurate these conclusions are.
randindex<- sample(1:dim(hr)[1])
cutpoint2_3<-floor(2*dim(hr)[1]/3)
traindata<-hr[randindex[1:cutpoint2_3],]
testdata<- hr[randindex[(cutpoint2_3+1):dim(hr)[1]],]
library(randomForest)
rfmodel <- randomForest(factor(left) ~ satisfaction_level + number_project + average_montly_hours + 
                          time_spend_company + promotion_last_5years + last_evaluation,
                        data = traindata)

plot9<-plot(rfmodel, ylim=c(0,0.36))
#very low error
par(mfrow = c(1,1))
importance(rfmodel)
 MeanDecreaseGini
 Satisfaction                1218.571858
 Num_projects                 694.195400
 Avg_monthly_hrs              510.740778
 Time_at_Company              692.496195
 Promotion_last_5_years         3.322111
 Last_eval                    449.928179
 
prediction<-predict(rfmodel, testdata)
summary(prediction)
    0    1 
    3861 1139
library(caret)
confusionMatrix(prediction, testdata$left)
# Accuracy: 98.9%

hrbestleft<-hr[which(hr$last_evaluation >= .72 & hr$left == 1),]


perc<-hrbestleft[which(hrbestleft$promotion_last_5years == "1"),]/length(hrbestleft)
str(hrbestleft$promotion_last_5years)

length(which(hrbestleft$promotion_last_5years=="1"))/nrow(hrbestleft)


#Decision Tree Analysis

install.packages("party")
library(party)
set.seed(421)
ind<-sample(2, nrow(hr), replace = TRUE, prob = c(0.02,0.3))
traindata<-hr[ind==1,]
testdata<-hr[ind==2,]
form<-left~satisfaction_level+average_montly_hours+time_spend_company+last_evaluation
hrtree<-ctree(form, data = traindata, controls = ctree_control(maxsurrogate = 3))
table(predict(hrtree), traindata$left)
plot(hrtree)
?ctree
print(hrtree)

