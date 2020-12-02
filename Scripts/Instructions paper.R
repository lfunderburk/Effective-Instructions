# R code for data cleaning and statistical tests for "Instructions" paper
# Hanh Tong
# Joint work with David Freeman, Erik Kimbrough, Garrett Petersen

# packages
library(dplyr)
library(tidyr)
library(tibble)
library(sandwich)
library(stargazer)
library(mediation)
#library(DescTools)
#install.packages('DescTools')
#install.packages('mediation')

# set path and working directory
path = "D:/Code for GitHub"
setwd(path)

# read in data
dat=read.csv("CompiledData-earnings.csv")
dat=dat[which(dat$Treatment=="Low"),]
dat = as.tibble(dat)

# Parameter values
# the range for optimal time (period 22: second 1261 to 1320)
lb = 1260 
ub = 1321 
eps = 0  # For deviation from optimal time, in seconds
# For one of the robustness checks, set eps = 60

# NMB for non-money maximizing behavior
# Create a binary variable for "NMB" (=1 if NMB, 0 otherwise) and "correct"
# NMB - Doing the task and doing it early (before period 22)
# correct - Doing the task during the optimal period - period 22
dat = dat %>% mutate(NMB = ifelse(Time<=lb-eps & Time > 0, 1, 0),
               correct = ifelse(Time< ub +eps & Time>lb - eps, 1, 0))

# Create treatment dummies
dat = dat %>% mutate(
  treat1 = ifelse(Quiz==0, 1, 0),   #NO QUIZ,
  treat2 = ifelse(AnswersGiven==0 & Quiz==1 & Enhanced==0, 1, 0),  #QUIZ, NO ANSWERS
  treat3 = ifelse(Paid==0 & AnswersGiven==1 & instructionsTwice==0 & Paper==0, 1,0), #ANSWERS GIVEN
  treat4 = ifelse(Paid==1, 1,0),   #INCENTIVE
  treat5 = ifelse(instructionsTwice==1, 1,0), #TWICE
  treat6 = ifelse(Paper==1,1,0),  #PAPER
  treat7 = ifelse(AnswersGiven==0 & Quiz==1 & Enhanced==1, 1, 0) #ENHANCED
)

dat = dat%>%  
  mutate(treat  = case_when(treat1 == 1 ~ '1',  
                            treat2 == 1 ~ '2',
                            treat3 == 1 ~ '3',
                            treat4 == 1 ~ '4',
                            treat5 == 1 ~ '5',
                            treat6 == 1 ~ '6',
                            treat7 == 1 ~ '7'))

# -------------------------- Table 3 --------------------------------------
# Average Non-money maximizing behavior by treatment
dat %>%  group_by(treat) %>% summarise(average_NMB = mean(NMB))
# Average Quiz score by treatment
dat %>%  group_by(treat) %>% summarise(average_Quiz_score = mean(QuizScore))

#Fisher's exact test of no association between treatments and NMB
F_table = dat %>%  group_by(treat, NMB) %>% count() %>% spread(NMB,n)
fisher.test(F_table[,2:3], workspace=200000000)

#F_t = F_table %>% filter(treat== 1 | treat == 2) 
#fisher.test(F_t[,2:3],workspace=200000000)[[1]]

for (i in 1:6) {
  for (j in (i+1):7){
    F_t = F_table %>% filter(treat== i | treat == j)
    print(paste('The p-value of Fisher exact test between treatment ',i,'and ',j,'is ',fisher.test(F_t[,2:3],workspace=20000000)[[1]]))
  }
}


#Finding 3 analysis
# Null hypothesis: Quiz scores are identical across treatments
#Using the Kruskal-Wallis Test, we can decide 
#whether the population distributions of quiz scores are identical 
#without assuming them to follow the normal distribution.

# footnote 13:  QUIZ, ANSWERS, TWICE
dat_k = dat %>% filter(treat %in% c('2','3','5'))
kruskal.test(dat_k$QuizScore ~ as.numeric(dat_k$treat), data = dat_k )
typeof(dat$treat)

# Goodman-Kruskal test (Goodman and Kruskal's gamma)
#tests of association between QuizScores and NMB

#tests of association between QuizScore and NMB pooling everything; not reported
GoodmanKruskalGamma(dat$NMB[dat$treat != 1],dat$QuizScore[dat$treat != 1], conf.level=.95)

#tests of association between QuizScore and NMB in each treatment
for (i in 2:7){
  GoodmanKruskalGamma(dat$NMB[dat$treat == i],dat$QuizScore[dat$treat == i], conf.level=.95)
}


#test whether INCENTIVE, ENHANCED, or PAPER improved quiz scores
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==4])
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==6])
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==7])


#-----------------------     Logistic regression    ---------------------------
#dat %>% filter(Quiz == 1 & treat == 7) %>%  nrow()

# Logistic regression, with QUIZ (treat = 2) as the reference level
dat$treat = relevel(as.factor(dat$treat), ref = "2")
# a model of NMB as a logistic function of treatment
Eq1 = glm(NMB ~ treat, family="binomial", data = dat)
summary(Eq1)
# If don't change variable treat to factor and relevel:
#LR1 = glm(NMB ~ relevel(as.factor(treat), ref = "2"), family="binomial", data = dat)


# a model of NMB as a logistic-linear function of quiz score, treatment, and their interactions
Eq2 = glm(NMB ~ treat*QuizScore, family="binomial", data = dat %>% filter(treat!=1))
summary(Eq2)
#LR2 = glm(NMB ~ relevel(as.factor(treat), ref = "2")*QuizScore, family="binomial", data = dat %>% filter(treat!=1))


# a linear regression to model treatment effects on quiz scores
Eq3 = lm(QuizScore ~ treat, data = dat %>% filter(treat!=1))
summary(Eq3)

# Calculate robust (HC1) 95% confidence intervals
CovEq1 = vcovHC(Eq1,"HC1")
robust.se1 = sqrt(diag(CovEq1))
CovEq2 = vcovHC(Eq2,"HC1")
robust.se2 = sqrt(diag(CovEq2))
CovEq3 = vcovHC(Eq3,"HC1")
robust.se3 = sqrt(diag(CovEq3))

# Export to LaTex table
#this gives the first three columns of Table 3.
stargazer(Eq1, Eq2, Eq3, ci=TRUE, se=list(robust.se1,robust.se2,robust.se3))


#--------------------- Mediation analysis ------------
# Reference level is treat  = 2 (QUIZ)

med.fit23 = lm(QuizScore ~ treat, data = dat %>% filter(treat==2 | treat==3))
out.fit23 = glm(NMB ~ treat3*QuizScore, family=binomial, data=dat23)
set.seed(2018)
med.out23 = mediate(med.fit23, out.fit23, treat="treat3", mediator="QuizScore", robustSE=TRUE, type="HC1", sims=1000)


med.out23$z0
med.out23$z0.ci
med.out23$z0.p
med.out23$d0
med.out23$d0.ci
med.out23$d0.p





