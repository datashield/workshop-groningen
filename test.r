library(datashieldclient)

load("logindata4test.rda")

opals <- datashield.login(logindata, symbol="H", variables=list("GENDER","PM_BMI_CATEGORIAL"))

datashield.aggregate(opals, "summary.ds(H)")

datashield.logout(opals)
