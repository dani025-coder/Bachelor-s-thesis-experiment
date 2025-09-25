library(knitr)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(zoo)
library(lmtest)
library(readxl)
library(car)
library(estimatr)
library(marginaleffects)

datos_excel <- read_excel("C:/Users/dani0/OneDrive/Escritorio/TFG/Experiments/Exp_1/data/data_1_clean.xlsx")

# dummies into factors
datos_excel$aut_enrol<- as.factor(datos_excel$aut_enrol)
datos_excel$b_gpt <- as.factor(datos_excel$b_gpt)
datos_excel$human<- as.factor(datos_excel$human)
datos_excel$human_p<- as.factor(datos_excel$human_p)
datos_excel$default <- as.factor(datos_excel$default)

#model 1: effects of AE in contribution rates by groups
mlp.1 <- lm_robust(data = datos_excel, participation ~ aut_enrol*(human + human_p))
summary(mlp.1)

#present model

modelsummary(list("OLS with robust errors estimation of  'Model 1'"=mlp.1), gof_map = c("nobs"), title= "Independet variable 'participation'", stars = TRUE)

#hypothesis contrast of the significance of the effect of automatic enrollment for humans and humans
#with tendency to procrastination.
linearHypothesis(mlp.1, c("aut_enrol1 + aut_enrol1:human1=0"))
linearHypothesis(mlp.1, c("aut_enrol1 + aut_enrol1:human_p1=0"))

#Plot bar charts with the effect of AE over participation rate 

coefficients <- coef(mlp.1)

names <- c("Baseline GPT", "Human", "Human Procrastinator")
coef.mlp.1 <- coef(mlp.1)
coef.ef.AE.1B <- c(coef.mlp.1[2], (coef.mlp.1[2] + coef.mlp.1[5]), (coef.mlp.1[2] + coef.mlp.1[6]))*100
standar_errors <- c(0.01472, (0.01472 + 0.01541), (0.01472 + 0.02146))*100

coefficients_df <- data.frame(variable = names, coefficient = coef.ef.AE.1B, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)



ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
  labs(title = "Effects of automatic enrollment over the participation rate with standard errors (in percentage points)", x = "Behavior of agents", y = "Increase in the percentage of participation (in pp)") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add standar error bars
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15))  
  

#Bar chart 

coef.ef.AE.1A <- c(coef.mlp.1[1], (coef.mlp.1[1] + coef.mlp.1[2]), (coef.mlp.1[1] + coef.mlp.1[3]), (coef.mlp.1[1] + coef.mlp.1[3]), (coef.mlp.1[1] + coef.mlp.1[4]), (coef.mlp.1[1] + coef.mlp.1[4] + coef.mlp.1[2] + coef.mlp.1[6]) )*100
standar_errors <- c(0.01335, (0.01335 + 0.01472), (0.01335 + 0.01379), (0.01335 + 0.01379), (0.01335 + 0.01601), (0.01335 + 0.01601 + 0.01472 + 0.02146))*100
names <- c("Baseline GPT (NE)", "Baseline GPT (AE)", "Human (NE)", "Human (AE)", "Human Procrastinator (NE)", "Human Procrastinator (AE)")
color <- c("yellow", "skyblue","yellow", "skyblue","yellow", "skyblue")
coefficients_df <- data.frame(variable = names, coefficient = coef.ef.AE.1A, se = standar_errors)



conf_intervals <- data.frame(variable = names,
                             ymin = coef.ef.AE.1A - 1.96 * standar_errors,
                             ymax = coef.ef.AE.1A + 1.96 * standar_errors)

coefficients_df <- cbind(coefficients_df, conf_intervals[, c("ymin", "ymax")])  

ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = color, color = "black", alpha = 0.5) +
  #geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.4, color = "black") +  # Add se bars
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # add intervals of confidence
  labs(title = "Effects of automatic enrollment over the participation rate with 95% confidence intervals (in percentage)",
      x = "Groups of agents by behavior (AE vs NE)",
      y = "Percentage of agents enrolled in the plan") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),  
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate the text on x-axis
        ) +
  annotate("text", x = 6, y = 105, label = "*AE refers to Automatic Enrolment", size = 4, color = "black")


#model 2: effects of AE in contribution rates by groups 

#we only take the data in which individuals decide to participate in the pension plan
#in order to compare with other research

datos_excel_2 <- datos_excel[datos_excel$cont_rate > 0, ]
mco.2 <- lm(data = datos_excel_2, cont_rate ~ aut_enrol*(human + human_p))

#Check the model for heterocedasticity

test_heterocedasticity <- bptest(mco.2)
print(test_heterocedasticity)

#BP = 1484.6, df = 5, p-value < 2.2e-16 < 0.05 RH0, there is heterocedasticity in the model, therefore
#we use robust errors

mco.2.robust.2 <- lm_robust(data = datos_excel_2, cont_rate ~ aut_enrol*(human + human_p))
summary(mco.2.robust.2)

modelsummary(list("OLS with robust errors estimation of 'Model 2', taking only individuals enrolled (NE and AE)"=mco.2.robust.2), gof_map = c("nobs"), title= "Independet variable 'cont_rate'", stars = TRUE)

#hypothesis contrast of the significance of the effect of automatic enrollment over the contribution rate for humans and humans
#with tendency to procrastination.

linearHypothesis(mco.2.robust.2, c("aut_enrol1 + aut_enrol1:human1=0"))
linearHypothesis(mco.2.robust.2, c("aut_enrol1 + aut_enrol1:human_p1=0"))

#Plot bar charts with the effect of AE over contribution rate

#Bar chart 2

coef.mco.2.robust.2 <- coef(mco.2.robust.2)

coef.ef.AE.2B <- c(coef.mco.2.robust.2[2], (coef.mco.2.robust.2[2] + coef.mco.2.robust.2[5]), (coef.mco.2.robust.2[2] + coef.mco.2.robust.2[6])) 
standar_errors <- c(0.13089, (0.13089 + 0.16922), (0.13089 + 0.30216))
names.coef.ef.AE.2 <- c("Baseline GPT", "Human", "Human Procrastinator")

coefficients_df <- data.frame(variable = names.coef.ef.AE.2, coefficient = coef.ef.AE.2B, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)


ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add se bars
  labs(title = "Effects of AE over the contribution rate of enrolled agents with standard errors (in percentage points)", x = "Behavior of agents", y = "Reduction of the contribution rate (in pp)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15))  
        

# Bar chart 3

cont_vector <- c(coef.mco.2.robust.2[1], (coef.mco.2.robust.2[1] + coef.mco.2.robust.2[2]), (coef.mco.2.robust.2[1] + coef.mco.2.robust.2[3]), (coef.mco.2.robust.2[1] + coef.mco.2.robust.2[2] + coef.mco.2.robust.2[3] + coef.mco.2.robust.2[5]), (coef.mco.2.robust.2[1]), (coef.mco.2.robust.2[1]  + coef.mco.2.robust.2[2] + coef.mco.2.robust.2[6]) )
standar_errors <- c(0.06443, (0.06443 + 0.13089), (0.06443 + 0.07027), (0.06443 + 0.13089 + 0.07027 + 0.16922), 0.06443, (0.06443 + 0.13089 + 0.30216))
names_cont <- c("Baseline GPT (NE)", "Baseline GPT (AE)", "Human (NE)", "Human (AE)", "Human Procrastinator (NE)", "Human Procrastinator (AE)")
color <- c("yellow", "skyblue","yellow", "skyblue", "yellow", "skyblue")

coefficients_df <- data.frame(variable = names_cont, coefficient = cont_vector, se = standar_errors)

conf_intervals <- data.frame(variable = names,
                             ymin = cont_vector - 1.96 * standar_errors,
                             ymax = cont_vector + 1.96 * standar_errors)

coefficients_df <- cbind(coefficients_df, conf_intervals[, c("ymin", "ymax")])  

ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = color, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add intervals of confidence
  labs(title = "Effects of AE over the contribution rate of enrolled agents with 95% confidence intervals (in percentage)", x = "Groups of agents by behavior (AE vs NE)", y = "Contribution rate of agents enrolled in the plan") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate the text on x-axis
  ) +
  annotate("text", x = 6, y = 10, label = "*AE refers to Automatic Enrolment", size = 4, color = "black")


# Data for histogram (only individuals enrolled)

datos <- datos_excel_2$cont_rate

# Make the histogram: Distribution of the variable cont_rate (only individuals enrolled)

histograma <- ggplot(data = NULL, aes(x = datos)) +
  geom_histogram(bins = 15, fill = "cyan", color = "black") +
  labs(
    title = "Distribution of the contribution rate variable for all enrolled agents (NE and AE))",
    x = "contribution rate",
    y = "Frequency"
  ) +
  theme_bw()+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15), labels = c(0, 3, 6, 9, 12, 15))+
  # Make that the graph starts from 0
  coord_cartesian(xlim = c(0, max(datos))) +
  # Highlight value 3 of the x axis
  geom_vline(xintercept = 3.4, linetype = "dashed", color = "red") +
  theme(
    plot.title = element_text(size = 16),  # Adjust the size of the text
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 15),   
    axis.text.y = element_text(size = 15)    
  )

print(histograma)

# Data for histogram (individuals enrolled with AE)

datos <- datos_excel_3$cont_rate

# Make the histogram: Distribution of the contribution rate variable when there is AE (only individuals enrolled)

histograma <- ggplot(data = NULL, aes(x = datos)) +
  geom_histogram(bins = 15, fill = "cyan", color = "black") +
  labs(
    title = "Distribution of the contribution rate variable for agents enrolled with AE",
    x = "contribution rate",
    y = "Frequency"
  ) +
  theme_bw()+
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15), labels = c(0, 3, 6, 9, 12, 15))+
  # Make that x axis starts from 0
  coord_cartesian(xlim = c(0, max(datos))) +
  # Highlight value 3
  geom_vline(xintercept = 3.4, linetype = "dashed", color = "red") +
  theme(
    plot.title = element_text(size = 16),  # Adjust the size of the text
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 15),   
    axis.text.y = element_text(size = 15)    
  )

print(histograma)

# Average savings per treatment (contribution rate including participation)

mco.2.1 <- lm(data = datos_excel, cont_rate ~ aut_enrol*(human + human_p))

#Check the model for heterocedasticity

test_heterocedasticity <- bptest(mco.2.1)
print(test_heterocedasticity)

#BP = 1484.6, df = 5, p-value < 2.2e-16 < 0.05 RH0, there is heterocedasticity in the model, therefore
#we use robust errors

mco.2.1.robust <- lm_robust(data = datos_excel, cont_rate ~ aut_enrol*(human + human_p))
summary(mco.2.1.robust)

modelsummary(list("OLS with robust errors estimation of 'Model 2' (all individuals)"=mco.2.1.robust), gof_map = c("nobs"), title= "Independet variable 'cont_rate'", stars = TRUE)

#hypothesis contrast of the significance of the effect of automatic enrollment over the contribution rate for humans and humans
#with tendency to procrastination.

linearHypothesis(mco.2.1.robust, c("aut_enrol1 + aut_enrol1:human1=0"))
linearHypothesis(mco.2.1.robust, c("aut_enrol1 + aut_enrol1:human_p1=0"))

#Plot bar charts with the effect of AE over contribution rate

#Bar chart 2.1

coef.mco.2.1.robust <- coef(mco.2.1.robust)

coef.ef.AE.2.1 <- c((-1.223), (-2.365), (0.569)) 
standar_errors <- c(0.1794, 0.3918, 0.4)
names.coef.ef.AE.2.1 <- c("Baseline GPT", "Human", "Human Procrastinator")

coefficients_df <- data.frame(variable = names.coef.ef.AE.2.1, coefficient = coef.ef.AE.2.1, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)


ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add se bars
  labs(title = "Effects of AE over the contribution rate of all agents with standard errors (in percentage points)", x = "Behavior of agents", y = "Change in the contribution rate (in pp)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15))  


# Bar chart 3.1

cont_vector <- c(7.323, (coef.mco.2.1.robust[1] + coef.mco.2.1.robust[2]), (coef.mco.2.1.robust[1] + coef.mco.2.1.robust[3]), (coef.mco.2.1.robust[1] + coef.mco.2.1.robust[2] + coef.mco.2.1.robust[3] + coef.mco.2.1.robust[5]), (coef.mco.2.1.robust[1] + coef.mco.2.1.robust[4]), (coef.mco.2.1.robust[1]  + coef.mco.2.1.robust[2] + coef.mco.2.1.robust[4] + coef.mco.2.1.robust[6]) )
standar_errors <- c(0.1366, (0.316), (0.28), (0.6718), 0.2972, (0.6972))
names_cont <- c("Baseline GPT (NE)", "Baseline GPT (AE)", "Human (NE)", "Human (AE)", "Human Procrastinator (NE)", "Human Procrastinator (AE)")
color <- c("yellow", "skyblue","yellow", "skyblue", "yellow", "skyblue")

coefficients_df <- data.frame(variable = names_cont, coefficient = cont_vector, se = standar_errors)

conf_intervals <- data.frame(variable = names,
                             ymin = cont_vector - 1.96 * standar_errors,
                             ymax = cont_vector + 1.96 * standar_errors)

coefficients_df <- cbind(coefficients_df, conf_intervals[, c("ymin", "ymax")])  

ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = color, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add intervals of confidence
  labs(title = "Effects of AE over the contribution rate of all agents with 95% confidence intervals (in percentage)", x = "Groups of agents by behavior (AE vs NE)", y = "Contribution rate") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14), # Adjust the size of the text
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate the text on x-axis
  ) +
  annotate("text", x = 6, y = 10, label = "*AE refers to Automatic Enrolment", size = 4, color = "black")


# model 4: asset allocation statistics

mlp.4 <- lm_robust( data = datos_excel_2, bond_market ~ aut_enrol*(human + human_p))
summary(mlp.4)

linearHypothesis(mlp.4, c("aut_enrol1 + aut_enrol1:human1 = 0"))
linearHypothesis(mlp.4, c("aut_enrol1 + aut_enrol1:human_p1 = 0"))

# plot summary of model

modelsummary(list("OLS with robust errors estimation of 'Model 4'"=mlp.4), gof_map = c("nobs"), title= "Independet variable 'bond_market'", stars = TRUE)

# plot bar chart

coef.mlp.4 <- coef(mlp.4)
model.4.coef <- c(coef.mlp.4[2], (coef.mlp.4[2] + coef.mlp.4[5]), (coef.mlp.4[2] + coef.mlp.4[6])) *100
standar_errors <- c(0.01759, 0.04258, 0.06847)*100
names.4 <- c("Baseline GPT", "Human", "Human Procrastinator")

coefficients_df <- data.frame(variable = names.4, coefficient = model.4.coef, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)


ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add se bars
  labs(title = "Effects of automatic enrollment over the asset distribution of enrolled agents with standard errors (in percentage points)", x = "Behavior of agents", y = "Increase in agents choosing the bond market as a result of AE (in pp)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),    # Adjust the size of the text
        plot.title = element_text(size = 16),  
        axis.title.x = element_text(size = 13),  
        axis.title.y = element_text(size = 13),  
        axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15)    
  )


# Default behavior models


#First, Complete default behavior

#For this model I use only the observations that are automatically enrolled

datos_excel_4 <- datos_excel[datos_excel$aut_enrol == 1,]

#Create variable

datos_excel_4$comp_def <- ifelse(datos_excel_4$participation == 1 & datos_excel_4$cont_rate == 3 & datos_excel_4$bond_market == 1, 1,0)

#Model, as the dependent vairable is binary, we use robust errors

mco.rob.5 <- lm_robust(data = datos_excel_4, comp_def ~ human + human_p)
summary(mco.rob.5)

# plot summary of model

modelsummary(list("OLS with robust errors estimation of 'Model 4'"=mco.rob.5), gof_map = c("nobs"), title= "Independet variable 'comp_def'", stars = TRUE)

# plot bar chart

model.5.coef <- c(0.482, (0.482 - 0.157), (0.482 - 0.395)) *100
standar_errors <- c(0.01581, 0.01581 + 0.02167, 0.01581 + 0.01815)*100
names.4 <- c("Baseline GPT", "Human", "Human Procrastinator")
color <- c("orange","red", "purple")

coefficients_df <- data.frame(variable = names.4, coefficient = model.5.coef, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)


ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = color, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add se bars
  labs(title = "'Complete default behavior' which includes participation (only including AE individuals)", x = "Behavior of individuals", y = "percentage of individuals which exhibit 'complete default behavior'") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),    # Adjust the size of the text
        plot.title = element_text(size = 16),  
        axis.title.x = element_text(size = 13),  
        axis.title.y = element_text(size = 13),  
        axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15)    
  )



#Second, partial default behavior

#For this model I use only the observations that are automatically enrolled and that participate in the pension plan

datos_excel_3 <- datos_excel_2[datos_excel_2$aut_enrol == 1,]

# Create variable

datos_excel_3$part_def <- ifelse(datos_excel_3$cont_rate == 3 & datos_excel_3$bond_market == 1, 1,0)

#Model, as the dependent vairable is binary, we use robust errors

mco.rob.6 <- lm_robust(data = datos_excel_3, part_def ~ human + human_p)
summary(mco.rob.6)

# plot summary of model

modelsummary(list("OLS with robust errors estimation of 'Model 5'"=mco.rob.6), gof_map = c("nobs"), title= "Independet variable 'part_def'", stars = TRUE)

# plot bar chart

model.6.coef <- c(0.5021, (0.5021 - 0.1741), (0.5021 - 0.0878)) *100
standar_errors <- c(0.01615, 0.01615 + 0.02198, 0.01615 + 0.03771)*100
names.4 <- c("Baseline GPT", "Human", "Human Procrastinator")
color <- c("orange","red", "purple")

coefficients_df <- data.frame(variable = names.4, coefficient = model.6.coef, se = standar_errors)

coefficients_df <- transform(coefficients_df,
                             ymin = coefficient - se,
                             ymax = coefficient + se)


ggplot(coefficients_df, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = color, color = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "black") +  # Add se bars
  labs(title = "'Partial default behavior' (only including AE individuals)", x = "Behavior of individuals", y = "percentage of individuals which exhibit 'partial default behavior'") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),    # Adjust the size of the text
        plot.title = element_text(size = 16),  
        axis.title.x = element_text(size = 13),  
        axis.title.y = element_text(size = 13),  
        axis.text.x = element_text(size = 15),   
        axis.text.y = element_text(size = 15)    
  )


