library(datashieldclient)

load("logindata4test.rda")

# TODO LAB_GLUC_ADJUSTED does not exist
opals <- datashield.login(logindata, assign=TRUE, variables=list("GENDER","PM_BMI_CATEGORIAL","PM_BMI_CONTINUOUS","DIS_CVA", "DIS_AMI", "DIS_DIAB", "MEDI_LPD","LAB_TSC","LAB_TRIG","LAB_HDL","LAB_GLUC_ADJUSTED"))

# DISPLAY SUMMARY OF THE ASSIGNED DATA FRAMES
datashield.aggregate(opals, quote(summary.ds(D)))

# DISPLAY AVAILABLE AGGREGATE METHODS
datashield.methods(opals, type="aggregate")
# DISPLAY AVAILABLE ASSIGNMENT METHODS
datashield.methods(opals, type="assign")

#LOOK AT UNIVARIATE DISTRIBUTION IN MORE DETAIL
ds.table1d(opals, quote(D$DIS_CVA))
ds.table1d(opals, quote(D$MEDI_LPD))
ds.table1d(opals, quote(D$DIS_DIAB))
ds.table1d(opals, quote(D$DIS_AMI))
ds.table1d(opals, quote(D$GENDER))
ds.table1d(opals, quote(D$PM_BMI_CATEGORIAL))

ds.quantilemean(opals,quote(D$LAB_TSC))
ds.quantilemean(opals,quote(D$LAB_TRIG))
ds.quantilemean(opals,quote(D$LAB_HDL))
ds.quantilemean(opals,quote(D$LAB_GLUC_ADJUSTED))
ds.quantilemean(opals,quote(D$PM_BMI_CONTINUOUS))

# TODO fix warning messages
ds.histogram(opals,quote(D$LAB_HDL))
ds.histogram(opals,quote(D$PM_BMI_CONTINUOUS))

datashield.aggregate(opals, quote(mean.ds(D$PM_BMI_CONTINUOUS)))

# TODO fix errors
ds.table2d(opals,quote(D$PM_BMI_CATEGORICAL),quote(D$GENDER))
# TODO fix errors
ds.t.test(opals,x=quote(D$LAB_HDL),y=quote(D$GENDER))


#Question 1
#To see if there is a difference between numeric variables across two factors we can use a boxplot to view the data.  The R function here is boxplot and the input is a$name of numeric variable~a$name of factor v ariable.
#boxplot(a$LAB_TSC~a$GENDER)

ds.histogram(opals,quote(D$LAB_TSC))
datashield.aggregate(opals, quote(mean.ds(D$LAB_TSC)))
datashield.aggregate(opals, quote(mean.ds(D$GENDER)))
datashield.aggregate(opals, quote(var(D$LAB_TSC)))
datashield.aggregate(opals, quote(var(D$GENDER)))

#This can then be formally tested using a t-test.  The R function we need is t.test, with the same input a$name of numeri c variable~a$name of factor v ariable
#t.test(a$LAB_TSC~a$GENDER)

ds.t.test(opals,x=quote(D$LAB_TSC),y=quote(D$GENDER))

#We can save the outcome to our workspace by adding in <-
q11 <- ds.t.test(opals,x=quote(D$LAB_TSC),y=quote(D$GENDER))

glm.mod1 <- ds.glm(opals, D$LAB_TSC ~ D$GENDER, quote(gaussian),quote(20))
glm.mod1


#To determine if there is a difference across numerical variables between more than one group an ANOVA is used, this is can be viewed again using the boxplot function
#boxplot(a$LAB_GLUC_ADJUSTED~a$PM_BMI_CATERGORIAL)
# TODO fix usage
datashield.assign(opals, 'bmi.f', quote(createfactor.ds(D$PM_BMI_CATEGORICAL)))
datashield.assign(opals, 'bmi.n', quote(as.numeric(D$PM_BMI_CATEGORICAL)))


#The ANOVA test can be carried out using the function aov and the same inputs, saving the outcome as object q21
#q21<-aov(a$LAB_GLUC_ADJUSTED~a$PM_BMI_CATERGORIAL)
#we can see whats in q21 by typing it into the console.

ds.table1d(opals,xvect=quote(bmi.f))

#For a better interpretation of the results we can use the summary function on the outcome of the analysis q21.


glm.mod2 <- ds.glm(opals, D$LAB_GLUC_ADJUSTED ~ bmi.f, quote(gaussian),quote(20))
glm.mod2



#Question 3
#To determine a trend over two categorical factor variables we first create a table of those variables using the function table.
#table(a$DIS_DIAB,a$PM_BMI_CATERGORIAL)
# TODO fix errors
ds.table2d(opals, quote(D$DIS_DIAB), quote(D$PM_BMI_CATEGORICAL))
ds.table2d(opals,quote(D$DIS_DIAB),quote(bmi.f))

glm.mod3<-ds.glm(opals, D$LAB_GLUC_ADJUSTED ~ bmi.f, quote(gaussian),quote(20))
glm.mod3


#We can then formally test the differing proportions across the categories using the saved table object q3 and the chi squared function chisq.test(..) .
#chisq.test(q3)  
# TODO fix errors
glm.mod4 <- ds.glm(opals, D$LAB_GLUC_ADJUSTED ~ bmi.n, quote(gaussian),quote(20))
glm.mod4


#4. To determine if total serum cholesterol levels are associated with HDL cholesterol levels
#For example, this can be carried out by plotting a scatter plot or performing a correlation or
#using linear regression using LAB_TSC as an outcome and LAB_HDL  as a covariate other covariates
#can be added to determine other significant variables correlated with total serum cholesterol.

ds.heatmapplot(opals, quote(D$LAB_TSC),quote(D$LAB_HDL))
ds.heatmapplot(opals, quote(D$LAB_TSC),quote(D$LAB_HDL))

ds.heatmapplot(opals, quote(D$LAB_TSC),quote(D$LAB_GLUC_ADJUSTED))
ds.contourplot(opals, quote(D$LAB_TSC),quote(D$LAB_GLUC_ADJUSTED))


#Question5
#To determine predictors of binary outcomes we need to use the function glm.
#The glm function can be used to analyse any input or output whether it be categorical or numerical.  If the outcome to analyse is numeric then we use the “gaussian” option of the glm.  If the outcome contains two categories we can use the “binomial” option.  So the type of glm function we need to answer research question 5 is the one which has family input “binomial”.  

ds.table2d(opals,quote(D$DIS_DIAB),quote(bmi.f))

glm.mod5 <- ds.glm(opals, D$DIS_DIAB ~ bmi.f, quote(binomial),quote(20))
glm.mod5

datashield.assign(opals, 'sex.f', quote(createfactor.ds(D$GENDER)))

glm.mod6<-ds.glm(opals, D$DIS_DIAB ~ sex.f+D$PM_BMI_CONTINUOUS+D$LAB_HDL, quote(binomial),quote(20))
glm.mod6

glm.mod7<-ds.glm(opals, D$DIS_DIAB ~ sex.f*D$PM_BMI_CONTINUOUS+D$LAB_HDL, quote(binomial),quote(20))
glm.mod7


#6. To determine predictors of taking lipid reducing medications
#For example, a logistic regression can, again, be used to determine whether
#HOP variables are significantly associated with a taking lipid reducing medications, MEDI_LPD. 

glm.mod8<-ds.glm(opals, D$MEDI_LPD ~ sex.f*D$LAB_HDL+D$PM_BMI_CONTINUOUS+D$LAB_TSC, quote(binomial),quote(20))
glm.mod8

ds.quantilemean(opals,quote(D$LAB_HDL))

datashield.assign(opals, 'HDL.1.5', quote(D$LAB_HDL-1.5))

glm.mod9 <- ds.glm(opals, D$MEDI_LPD ~ sex.f*HDL.1.5+D$PM_BMI_CONTINUOUS+D$LAB_TSC, quote(binomial),quote(20))
glm.mod9

datashield.logout(opals)
