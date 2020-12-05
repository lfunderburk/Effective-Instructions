# R code for statistical analysis for "Instructions" paper (JESA)
# Author: David Freeman, Erik Kimbrough, Garrett Petersen, and Hanh Tong

# This code is part of the statistical analysis for "Instructions" paper
# This code contains the following analysis:
# Average Non-money maximizing behavior (NMB) and average quiz score, by treatment
# Fisher's exact test of no association between treatments and NMB
# Kruskal-Wallis tst to compare distribution of quiz score across treatments
# Goodman and Kruskal's gamma: tests of association between quiz scores and NMB
# Wilcoxon rank-sum tests:test whether some treatments improved quiz scores
# Logistic regressions:
#   a model of NMB as a logistic function of treatment
#   a model of NMB as a logistic-linear function of quiz score, treatment, and their interactions
# A linear regression to model treatment effects on quiz scores
# Robust (HC1) 95% confidence intervals
# Mediation analysis

#--------------------------------------------------------------------------
# get the packages
install.packages('pacman') 
pacman::p_load(dplyr, tidyr, tibble, DescTools, mediation, sandwich, stargazer)

# set path and working directory
path = "Effective-Instructions" # replace with the path you choose
setwd(path)

# read in data
dat=read.csv("Data/CompiledData-earnings.csv")
dat=dat[which(dat$Treatment=="Low"),]
dat = as_tibble(dat)

# Parameter values
eps = 0  
# eps is the deviation from optimal time (period 22: second 1260 to 1320), in seconds
# For one of the robustness checks, set eps = 60

# NMB
# NMB for non-money maximizing behavior
# In our experiment, the optimal period to do Task 2 is period 22 (second 1260 to 1320)
# doing Task 2 before period 22 clearly exhibits non-money maximizing behavior
# Create a binary variable for "NMB" (=1 if NMB, 0 otherwise)
# NMB - Doing the task and doing it early (before period 22)
dat = dat %>% mutate(NMB = ifelse(Time<=1260-eps & Time > 0, 1, 0))

# Create a categorical variable for treatments, called "treat"
dat = dat%>%  
  mutate(treat  = case_when(Quiz==0 ~ '1',     #NO QUIZ
                            (AnswersGiven==0 & Quiz==1 & Enhanced==0) ~ '2', #QUIZ, NO ANSWERS
                            (Paid==0 & AnswersGiven==1 & instructionsTwice==0 & Paper==0) ~ '3', #ANSWERS GIVEN
                            Paid==1 ~ '4', #INCENTIVE
                            instructionsTwice==1 ~ '5', #TWICE
                            Paper==1 ~ '6', #PAPER
                            (AnswersGiven==0 & Quiz==1 & Enhanced==1) ~ '7')) #ENHANCED


# ----------------------------Table 3 --------------------------------------
# Average Non-money maximizing behavior by treatment
# and Average Quiz score by treatment
dat %>%  group_by(treat) %>% 
  summarise(average_NMB = mean(NMB), average_Quiz_score = mean(QuizScore))


# Fisher's exact test of no association between treatments and NMB
F_table = dat %>%  group_by(treat, NMB) %>% count() %>% spread(NMB,n)
fisher.test(F_table[,2:3], workspace=200000000)

# Fisher's exact test for each pair of treatments i and j
for (i in 1:6) {
  for (j in (i+1):7){
    F_t = F_table %>% filter(treat==i | treat==j)
    print(paste('The p-value of Fisher exact test between treatment ',i,'and ',j,'is ',fisher.test(F_t[,2:3],workspace=20000000)[[1]]))
  }
}


#--------------------------- Finding 3 analysis -------------------------------
# Null hypothesis: Quiz scores are identical across treatments
# Using the Kruskal-Wallis Test, we can decide 
# whether the population distributions of quiz scores are identical 
# without assuming them to follow the normal distribution.

# footnote 13: compare distribution of quiz scores in 
# QUIZ, ANSWERS, TWICE (treatment 2, 3, 5, respectively)
dat_k = dat %>% filter(treat %in% c('2','3','5'))
kruskal.test(dat_k$QuizScore ~ as.numeric(dat_k$treat), data = dat_k )


# Goodman-Kruskal test (Goodman and Kruskal's gamma)
# tests of association between QuizScores and NMB
# tests of association between QuizScore and NMB pooling all treatments with quiz
GoodmanKruskalGamma(dat$NMB[dat$treat != 1], dat$QuizScore[dat$treat != 1], conf.level=.95)
# tests of association between QuizScore and NMB in each treatment
for (i in 2:7){
  GoodmanKruskalGamma(dat$NMB[dat$treat==i],dat$QuizScore[dat$treat==i], conf.level=.95)
}


# test whether INCENTIVE, ENHANCED, or PAPER improved quiz scores
# rank-sum tests
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==4])
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==6])
wilcox.test(dat$QuizScore[dat$treat %in% c('2','3','5')], dat$QuizScore[dat$treat==7])


# --------------------------------- Table 4 ------------------------------------
#-----------------------     Logistic regressions    ---------------------------
# Logistic regression, with QUIZ (treat = 2) as the reference level
dat$treat = relevel(as.factor(dat$treat), ref = "2")

# a model of NMB as a logistic function of treatment
Eq1 = glm(NMB ~ treat, family="binomial", data = dat)
summary(Eq1)

# a model of NMB as a logistic-linear function of quiz score, treatment, and their interactions
Eq2 = glm(NMB ~ treat*QuizScore, family="binomial", data = dat %>% filter(treat!=1))
summary(Eq2)

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
# this gives the first three columns of Table 4.
# report confidence intervals, to match with Mediation analysis that follows
stargazer(Eq1, Eq2, Eq3, ci=TRUE, se=list(robust.se1,robust.se2,robust.se3))


#------------------------------ Mediation analysis ---------------------------------
# Reference level is QUIZ (treat = 2)
# Below is the mediation analysis between ANSWERS (treat = 3) and QUIZ (treat = 2)
med.fit23 = lm(QuizScore ~ treat, data = dat %>% filter(treat==2 | treat==3))
out.fit23 = glm(NMB ~ treat*QuizScore, family=binomial, data=dat %>% filter(treat==2 | treat==3))
set.seed(2018) 
med.out23 = mediate(med.fit23, out.fit23, treat='treat', mediator="QuizScore", robustSE=TRUE, type="HC1", sims=1000)
summary(med.out23)

# numbers to report in table 4
# Reference level is QUIZ
med.out23$z0 # estimated "direct effects" of treatment ANSWERS
med.out23$z0.ci #Robust (HC1) 95% confidence intervals
med.out23$z0.p # p-value
med.out23$d0 # estimated "mediated effects" of treatment via quiz score
med.out23$d0.ci #Robust (HC1) 95% confidence intervals
med.out23$d0.p # p-value





