# LOAD REQUIRED LIBRARIES
library(datashieldclient)


# LOAD THE TABLE THAT CONTAINS THE LOGIN INFO
load("logindata4bioshare.rda")


# LOGIN TO COLLABORATING SERVERS AND ASSIGN DATA
# the variables to assign
myvar <- list("DIS_CVA","MEDI_LPD","DIS_DIAB","DIS_AMI","GENDER","PM_BMI_CATEGORIAL",
              "LAB_TSC","LAB_HDL","LAB_GLUC_FASTING","PM_BMI_CONTINUOUS")

# run the command to login and assign data
ld <- subset(logindata, server != "hunt")
opals <- datashield.login(logins=ld, assign=TRUE, variables=myvar)

# get the total number of participants
sum(unlist(datashield.aggregate(opals,"length(D$GENDER)")))

# LOOK AT UNIVARIATE DISTRIBUTION IN MORE DETAIL

# tabulate some binary variables
ds.table1d(datasources=opals, xvect=quote(D$DIS_CVA)) 
ds.table1d(datasources=opals, xvect=quote(D$MEDI_LPD)) 
ds.table1d(datasources=opals, xvect=quote(D$DIS_DIAB))
ds.table1d(datasources=opals, xvect=quote(D$DIS_AMI)) 
ds.table1d(datasources=opals, xvect=quote(D$GENDER))
# now see what happens if attempt to tabulate a continuous variable
ds.table1d(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))                    

# display quantile (you can choose to display the combine -default- or separate quantiles)
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_TSC))
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL))
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_GLUC_FASTING))

# plot histograms
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL))
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL), type="split")
ds.histogram(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))
ds.histogram(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), type="split")


# ANSWERING SOME QUESTIONS

# Question 1
# To see if there is a difference across two variables we can use t.test.
ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL))
ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL), type="split")

# We can save the outcome to our workspace by adding in '<-'
q11 <- ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL))


# Question 2
# to explore the relationship between 'LAB_GLUC_FASTING' and 'PM_BMI_CATEGORIAL'
# let us tabulate the factor variable
ds.table1d(datasources=opals, xvect=quote(D$PM_BMI_CATEGORIAL))    

# use the glm function the explore the relationship
glm.mod2 <- ds.glm(datasources=opals, formula=D$LAB_GLUC_FASTING ~ D$PM_BMI_CATEGORIAL, family=quote(gaussian), maxit=quote(20))
# display a summary of the results
glm.mod2


# Question 3
# To determine a trend over two categorical factor variables
# let us cross tabulate those two variables using the function 'ds.table2d'
ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))


# We can formally test the differing proportions across categories by using the function 'ds.table2' 
# and look into the chi-squred results
q3 <- ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))
q3$CHI2.TESTS.FOR.HOMOGENEITY


# Question 4. 
# To determine if total serum cholesterol levels are associated with HDL cholesterol levels
# For example, this can be carried out by plotting a scatter plot or performing a correlation or
# using linear regression using LAB_TSC as an outcome and LAB_HDL  as a covariate other covariates
# can be added to determine other significant variables correlated with total serum cholesterol.
ds.heatmapplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL))
# now let us produce the hetamap plots of the studies separately 
ds.heatmapplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL), type="split")

# generate pooled contour plot
ds.contourplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL))

glm.mod4 <- ds.glm(datasources=opals, formula=D$LAB_TSC~D$LAB_HDL, family=quote(gaussian), maxit=quote(20))
glm.mod4


# Question 5
# To determine predictors of binary outcomes we need to use the function glm.
# The glm function can be used to analyse any input or output whether it be categorical or numerical.  
# If the outcome to analyse is numeric then we use the “gaussian” option of the glm.  
# If the outcome contains two categories we can use the “binomial” option.  
# So the type of glm function we need to answer research question 5 is the one which has family input “binomial”.  

# first let us tabulate the two variables
ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))

# now run glm to predict diabetes status using categorical bmi
glm.mod5_1 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$PM_BMI_CATEGORIAL, family=quote(binomial), maxit=quote(20))
glm.mod5_1

# run glm to predict diabestes using gender, continuous bmi and hdl cholesterol
glm.mod5_2 <- ds.glm(datasources=opals, formula=D$DIS_DIAB~D$GENDER+D$PM_BMI_CONTINUOUS+D$LAB_HDL, family=quote(binomial), maxit=quote(20))
glm.mod5_2


# Question 6 
# To determine predictors of taking lipid reducing medications
# For example, a logistic regression can, again, be used to determine whether
# some HOP variables are significantly associated with a taking lipid reducing medications, MEDI_LPD. 
glm.mod6_1 <- ds.glm(datasources=opals, formula=D$MEDI_LPD~D$GENDER*D$LAB_HDL+D$PM_BMI_CONTINUOUS+D$LAB_TSC, family=quote(binomial), maxit=quote(20))
glm.mod6_1

# quantile of the variable 'LAB_HDL'
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL))
# substract the mean 
datashield.assign(opals, 'HDL.1.5', quote(D$LAB_HDL-1.5))

# run another glm analysis using the above adjusted hdl variable
glm.mod6_2 <- ds.glm(datasources=opals, formula=D$MEDI_LPD~D$GENDER*HDL.1.5+D$PM_BMI_CONTINUOUS+D$LAB_TSC, family=quote(binomial), maxit=quote(20))
glm.mod6_2

datashield.logout(opals)