########## Probability & Statistics for Data Analysis, Assignment 2 ###############
########## Vasileios Ilias Drouzas, ID: f3352301 ##################################


############################# Exercise 1 ##########################################

data <- read.table("C:/Users/user/Desktop/assignment 2/data.txt", header = TRUE, sep = " ")


str(data)
data


data$W <- as.factor(data$W)

#a.

anova_Y <- aov(Y ~ W, data = data)
anova_X1 <- aov(X1 ~ W, data = data)
anova_X2 <- aov(X2 ~ W, data = data)
anova_X3 <- aov(X3 ~ W, data = data)
anova_X4 <- aov(X4 ~ W, data = data)


library(ggplot2)


ggplot(data, aes(x = W, y = Y)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Y by W")

ggplot(data, aes(x = W, y = X1)) + 
  geom_boxplot() +
  labs(title = "Boxplot of X1 by W")

ggplot(data, aes(x = W, y = X2)) + 
  geom_boxplot() +
  labs(title = "Boxplot of X2 by W")

ggplot(data, aes(x = W, y = X3)) + 
  geom_boxplot() +
  labs(title = "Boxplot of X3 by W")

ggplot(data, aes(x = W, y = X4)) + 
  geom_boxplot() +
  labs(title = "Boxplot of X4 by W")



summary(anova_Y)
summary(anova_X1)
summary(anova_X2)
summary(anova_X3)
summary(anova_X4)

#Checking normality

# Residuals for the anova models
residuals_Y <- residuals(anova_Y)
residuals_X1 <- residuals(anova_X1)
residuals_X2 <- residuals(anova_X2)
residuals_X3 <- residuals(anova_X3)
residuals_X4 <- residuals(anova_X4)

# QQ plot
qqnorm(residuals_Y)
qqline(residuals_Y)

qqnorm(residuals_X1)
qqline(residuals_X1)

qqnorm(residuals_X2)
qqline(residuals_X2)

qqnorm(residuals_X3)
qqline(residuals_X3)

qqnorm(residuals_X4)
qqline(residuals_X4)

#Shapiro-Wilk test for normality
shapiro.test(residuals_Y)
shapiro.test(residuals_X1)
shapiro.test(residuals_X2)
shapiro.test(residuals_X3)
shapiro.test(residuals_X4)

#Levene's test 
library(car)
leveneTest(anova_Y)
leveneTest(anova_X1)
leveneTest(anova_X2)
leveneTest(anova_X3)
leveneTest(anova_X4)


#b. 

plot_matrix <- ggplot(data, aes(x = Y, y = X1, color = W)) +
  geom_point() +
  labs(title = "Scatter Plot of Y vs X1") +
  theme_minimal()

plot_matrix <- plot_matrix + facet_grid(. ~ W)
plot_matrix

plot_matrix <- plot_matrix + geom_point(aes(x = Y, y = X2, color = W)) 
plot_matrix

plot_matrix <- plot_matrix + geom_point(aes(x = Y, y = X3, color = W)) 

plot_matrix <- plot_matrix + geom_point(aes(x = Y, y = X4, color = W)) +
  labs(title = "Scatter Plot of Y vs X1,X2,X3,X4")

plot_matrix

# Creating scatterplot matrix with different colors for levels of W
plot_matrix <- ggplot(data, aes(x = Y, y = X1, color = W)) +
  geom_point() +
  labs(title = "Scatterplot Matrix by W", y = "X1, X2, X3, X4") +
  theme_minimal() +
  geom_point(aes(x = Y, y = X2), color = "blue") +
  geom_point(aes(x = Y, y = X3), color = "green") +
  geom_point(aes(x = Y, y = X4), color = "red") +
  geom_point(aes(x = X1, y = X2), color = "blue") +
  geom_point(aes(x = X1, y = X3), color = "green") +
  geom_point(aes(x = X1, y = X4), color = "red") +
  geom_point(aes(x = X2, y = X3), color = "green") +
  geom_point(aes(x = X2, y = X4), color = "red") +
  geom_point(aes(x = X3, y = X4), color = "red")

print(plot_matrix)

#c. 

model_x4 <- lm(Y ~ X4, data = data)
summary(model_x4)

#d.

model_all <- lm(Y ~ X1 * W + X2 * W + X3 * W + X4 * W, data = data)
summary(model_all)

#e.


par(mfrow = c(2, 2))

# 1. Residuals vs Fitted values plot (Checking linearity)
plot(model_all, which = 1)

# 2. Normal Q-Q plot of residuals (Checking normality)
plot(model_all, which = 2)

# 3. Scale-location plot (Checking homoscedasticity)
plot(model_all, which = 3)

# 4. Residuals vs Leverage plot (Checking influential points)
plot(model_all, which = 5)


#f.

#Perform stepwise regression (both forward and backward selection)
final_model <- step(model_all, direction = "both")

summary(final_model)

#g. 


new_data <- data.frame(X1 = 120, X2 = 30, X3 = 10, X4 = 90, W = "B")
prediction <- predict(final_model, newdata = new_data, interval = "confidence")


point_estimate <- prediction[1]  #point estimate
confidence_interval <- prediction[2:3]  #95% confidence interval

point_estimate
confidence_interval


#h. 

data$Z <- cut(data$X4, breaks = quantile(data$X4, probs = c(0, 1/3, 2/3, 1)), labels = c("Low", "Medium", "High"))

contingency_table <- table(data$X4, data$W)
contingency_table

#i.

model <- aov(Y ~ W * Z, data = data)

summary(model)

plot(model)  # Diagnostic plots


##################### Exercise 2 ######################

data <- read.table("C:/Users/user/Desktop/assignment 2/weightloss.txt", header = TRUE, sep = " ")


str(data)
data

weight_loss_data <- data.frame(data$workout, data$diet, data$loss)

#a.

library(ggplot2)
#boxplot for weight loss per workout
ggplot(weight_loss_data, aes(x = data$workout, y = data$loss)) +
  geom_boxplot() +
  labs(title = "Weight Loss per Workout", x = "Workout", y = "Weight Loss") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#boxplot for weight loss per diet
ggplot(weight_loss_data, aes(x = data$diet, y = data$loss)) +
  geom_boxplot() +
  labs(title = "Weight Loss per Diet", x = "Diet", y = "Weight Loss") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#boxplot for weight loss for combinations of workout and diet
ggplot(weight_loss_data, aes(x = interaction(data$workout, data$diet), y = data$loss)) +
  geom_boxplot() +
  labs(title = "Weight Loss for Workout-Diet Combinations", x = "Workout-Diet Combination", y = "Weight Loss") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#b. 
data$workout <- as.factor(data$workout) #make sure workout is a factor variable


model <- aov(data$loss ~ data$workout, data = data)


summary(model)

#c. 

#changing the reference level to 'W3' for 'workout' factor
weight_loss_data$workout <- relevel(data$workout, ref = "W3")
new_model <- aov(data$loss ~ data$workout, data = data)
summary(new_model)

w2_data <- subset(data, data$workout == "W2")
w3_data <- subset(data, data$workout == "W3")

#pairwise t-test between W2 and W3
t_test_result <- t.test(w2_data$loss, w3_data$loss)


t_test_result

#d.

model2 <- aov(data$loss ~ data$diet, data = data)
summary(model2)


diet_types <- unique(data$diet)

if (length(diet_types) >= 2) {
  combinations <- combn(diet_types, 2, simplify = TRUE)
  
  non_significant_comparisons <- c()
  
  # Performing t-tests for each pairwise comparison
  for (i in 1:ncol(combinations)) {
    diet1 <- subset(data, data$diet == combinations[1, i])
    diet2 <- subset(data, data$diet == combinations[2, i])
    
    t_test_result <- t.test(diet1$loss, diet2$loss)
    
    if (t_test_result$p.value > 0.05) {
      non_significant_comparisons <- c(non_significant_comparisons, paste(combinations[1, i], combinations[2, i]))
    }
  }
  
  print(non_significant_comparisons)
} 

#e. 


subset_data <- subset(data, data$diet %in% c('D3', 'D4', 'D5'))  #exclude 'D1' and 'D2'

new_model2 <- aov(loss ~ diet, data = subset_data)  
summary(new_model2)


#f. 

#Two-Way ANOVA model
model <- aov(data$loss ~ data$workout + data$diet, data = data)
summary(model)


#g. 

anova_table <- summary(model)
p_values_workout <- anova_table$"Pr(>F)"[1:length(anova_table$"Pr(>F)")-1]

unique_diet_levels <- unique(data$diet)

#check significance for each diet level individually
non_significant_diet <- vector()
for (level in unique_diet_levels) {
  temp_data <- subset(data, diet == level)
  temp_model <- aov(loss ~ workout, data = temp_data)
  temp_anova <- anova(temp_model)
  if (temp_anova$"Pr(>F)"[1] > 0.05) {
    non_significant_diet <- c(non_significant_diet, level)
  }
}


non_significant_workout <- levels(data$workout)[p_values_workout > 0.05]


#filter out non-significant levels from the data
data_filtered <- data[!(data$workout %in% non_significant_workout), ]
data_filtered <- data_filtered[!(data_filtered$diet %in% non_significant_diet), ]


model_simplified <- aov(data$loss ~ data$workout + data$diet, data = data_filtered)
summary(model_simplified)


#h. 

#two-Way ANOVA model with interactions
model_interaction <- aov(data$loss ~ data$workout * data$diet, data = data)
summary(model_interaction)


#i. 
#perform stepwise model selection based on AIC
stepwise_model <- step(model_interaction, direction = "both", trace = FALSE)
summary(stepwise_model)

#j.Graphical representation

#Interaction plot
ggplot(data, aes(x = data$workout, y = data$loss, color = data$diet)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_line(aes(group = data$diet), position = position_dodge(width = 0.5)) +
  labs(x = "Workout", y = "Loss", color = "Diet") +
  ggtitle("Interaction Plot of Workout, Diet, and Loss")


# Boxplot
ggplot(data, aes(x = data$workout, y = data$loss, fill = data$diet)) +
  geom_boxplot() +
  labs(x = "Workout", y = "Loss", color = "Diet") +
  ggtitle("Boxplot of Workout, Diet, and Loss")


#k.
constant_model <- aov(data$loss ~ 1, data = data)

summary(constant_model)            # constant model
summary(model)                     # main effects model
summary(model_interaction)         # interaction model

#compare with ANOVA
anova_result <- anova(constant_model, model, model_interaction)
print(anova_result)
