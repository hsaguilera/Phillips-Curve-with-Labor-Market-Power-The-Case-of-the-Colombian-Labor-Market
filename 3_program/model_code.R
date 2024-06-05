library(plm)
library(lmtest)
library(performance)
library(tseries)
library(urca)
library(magrittr)

data <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data.csv")
data_for <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data_for.csv")
data_inf <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data_inf.csv")

#data = subset(data, period <= as.Date("2019-12-01"))

data$period <- as.Date(data$period)
data_for$period <- as.Date(data_for$period)
data_inf$period <- as.Date(data_inf$period)

data$unem = log(data$unem)
data_for$unem = log(data_for$unem)
data_inf$unem = log(data_inf$unem)

data_sub <- subset(data, high_mkp == 0)
data_sub_for <- subset(data_for, high_mkp == 0)
data_sub_inf <- subset(data_inf, high_mkp == 0)


## Labor market
# Fixed Effects Model
model1_fix <- plm(dln_meansalary_dpto ~ unem, data = data, index = c("dpto", "period"), model = "within")
model2_fix <- plm(dln_meansalary_dpto ~ unem, data = data_sub, index = c("dpto", "period"), model = "within")
model3_fix <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data, model = "pooling")
model3_fix <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data, index = c("period"), model = "within")
model3_fix <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data, index = c("dpto"), model = "within")
model3_fix <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data, index = c("dpto", "period"), model = "within")

summary(model1_fix)
summary(model2_fix)
summary(model3_fix)

stargazer(model3_fix, type = "text")
etable(model3_fix, cluster = ~dpto+unem, tex = TRUE)

#check_model(model1_fix)
#check_model(model2_fix)
#check_model(model3_fix)


# Random Effects Model
model1_ran <- plm(dln_meansalary_dpto ~ unem, data = data, index = c("dpto", "period"), model = "random")
model2_ran <- plm(dln_meansalary_dpto ~ unem, data = data_sub, index = c("dpto", "period"), model = "random")
model3_ran <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data, index = c("dpto", "period"), model = "random")

summary(model1_ran)
summary(model2_ran)
summary(model3_ran)

#check_model(model1_ran)
#check_model(model2_ran)
#check_model(model3_ran)


# Haussman Test
phtest(model1_fix, model1_ran)
phtest(model2_fix, model2_ran)
phtest(model3_fix, model3_ran)


## Formal Labor Market (Da un poquito significativo con el logaritmo del desempleo)
# Fixed Effects Model
model1_fix_for <- plm(dln_meansalary_dpto ~ unem, data = data_for, index = c("dpto", "period"), model = "within")
model2_fix_for <- plm(dln_meansalary_dpto ~ unem, data = data_sub_for, index = c("dpto", "period"), model = "within")
model3_fix_for <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data_for, index = c("dpto", "period"), model = "within")

summary(model1_fix_for)
summary(model2_fix_for)
summary(model3_fix_for)

#check_model(model1_fix_for)
#check_model(model2_fix_for)
#check_model(model3_fix_for)

# Random Effects Model
model1_ran_for <- plm(dln_meansalary_dpto ~ unem, data = data_for, index = c("dpto", "period"), model = "random")
model2_ran_for <- plm(dln_meansalary_dpto ~ unem, data = data_sub_for, index = c("dpto", "period"), model = "random")
model3_ran_for <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data_for, index = c("dpto", "period"), model = "random")

summary(model1_ran_for)
summary(model2_ran_for)
summary(model3_ran_for)

#check_model(model1_ran_for)
#check_model(model2_ran_for)
#check_model(model3_ran_for)

# Haussman Test
phtest(model1_fix_for, model1_ran_for)
phtest(model2_fix_for, model2_ran_for)
phtest(model3_fix_for, model3_ran_for)


## Informal Labor Market (da signifiativo si no se usa el logaritmo del desempleo)
# Fixed Effects Model
model1_fix_inf <- plm(dln_meansalary_dpto ~ unem, data = data_inf, index = c("dpto", "period"), model = "within")
model2_fix_inf <- plm(dln_meansalary_dpto ~ unem, data = data_sub_inf, index = c("dpto", "period"), model = "within")
model3_fix_inf <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data_inf, index = c("dpto", "period"), model = "within")

summary(model1_fix_inf)
summary(model2_fix_inf)
summary(model3_fix_inf)

#check_model(model1_fix_inf)
#check_model(model2_fix_inf)
#check_model(model3_fix_inf)

# Random Effects Model
model1_ran_inf <- plm(dln_meansalary_dpto ~ unem, data = data_inf, index = c("dpto", "period"), model = "random")
model2_ran_inf <- plm(dln_meansalary_dpto ~ unem, data = data_sub_inf, index = c("dpto", "period"), model = "random")
model3_ran_inf <- plm(dln_meansalary_dpto ~ unem * high_mkp, data = data_inf, index = c("dpto", "period"), model = "random")

summary(model1_ran_inf)
summary(model2_ran_inf)
summary(model3_ran_inf)

#check_model(model1_ran_inf)
#check_model(model2_ran_inf)
#check_model(model3_ran_inf)

# Haussman Test
phtest(model1_fix_inf, model1_ran_inf)
phtest(model2_fix_inf, model2_ran_inf)
phtest(model3_fix_inf, model3_ran_inf)
