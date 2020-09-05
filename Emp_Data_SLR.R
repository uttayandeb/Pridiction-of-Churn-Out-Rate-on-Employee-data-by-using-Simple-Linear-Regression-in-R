########## Building a prediction model for churn out rate #####


############### Reading and understanding the data ######

Emp_Data<- read.csv(file.choose()) ### reading the emp data set
View(Emp_Data)
colnames(Emp_Data)
str(Emp_Data)
nrow(Emp_Data)
ncol(Emp_Data)
summary(Emp_Data)


# Scatter Diagram (Plot x,y)
plot(Emp_Data$Salary_hike,Emp_Data$Churn_out_rate)



# Other Exploratory data analysis and Plots

boxplot(Emp_Data)

hist(Emp_Data$Salary_hike)

hist(Emp_Data$Churn_out_rate)

summary(Emp_Data)



# Correlation coefficient value for Salary Hike and Churn_out_Date
churn_Rate<- Emp_Data$Churn_out_rate
salary_hike <- Emp_Data$Salary_hike
cor(churn_Rate,salary_hike)



# If |r| is greater than  0.85 then Co-relation is Strong(Correlation Co-efficient = -0.9117216). 
# This has a strong negative Correlation (since correlation is -ve)




############## Simple linear model ############
reg<-lm(churn_Rate~salary_hike)
summary(reg)#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 




confint(reg,level = 0.95) # confidence interval

# The above code will get you 2 equations 
# 1 to calculate the lower range and other for upper range

# Function to Predict the above model 
predict(reg,interval="predict")


################### LOgarthmic transformation #################
reg_log<-lm(churn_Rate~log(salary_hike))  # Regression using logarthmic transformation
summary(reg_log)#Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297

confint(reg_log,level=0.95)

predict(reg_log,interval="predict")



################# Exponential model ######################
reg_exp<-lm(log(churn_Rate)~salary_hike) # regression using Exponential model
summary(reg_exp)#Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 


confint(reg_exp,level=0.95)


exp(predict(reg_exp,interval="predict"))





##################### Quadratic model ####################
Emp_Data[,"sh_sq"] = salary_hike*salary_hike

############### Quadratic model
quad_mod <- lm(churn_Rate~salary_hike+I(salary_hike^2),data=Emp_Data)
summary(quad_mod)#Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")




######## Quadratic model
qd_model <- lm(churn_Rate~salary_hike+sh_sq,data=Emp_Data)
summary(qd_model)#Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9662 

confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")


###################### Cubic model  ###################

poly_mod <- lm(churn_Rate~salary_hike+I(salary_hike^2)+I(salary_hike^3),data=Emp_Data)
summary(poly_mod) # Multiple R-squared:  0.9893,	Adjusted R-squared:  0.984 


confint(poly_mod,level=0.95)

predict(poly_mod,interval="predict")



model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.8101,0.8297,0.8577,0.9662,0.984)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
View(Final)


######################################################################
##### Cubic  model gives the best Adjusted R-Squared value ###########
######################################################################

predicted_Value <- predict(poly_mod)
predicted_Value



Final <- cbind(Salary_Hike=Emp_Data$Salary_hike,Churn_Rate = Emp_Data$Churn_out_rate,Pred_Chr_rate=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-cr)^2))
rmse#[1] 5142.642


plot(poly_mod)


hist(residuals(poly_mod)) # close to normal distribution
