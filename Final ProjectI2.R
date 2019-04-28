library(tidyverse)
library(GGally)

# Data import from hard drive
# Some data transformation performed in this step on rings and gender columns.
library(readr)
ab.data <- read_csv("Downloads/abalone.data.csv", 
                    col_names = c("sex", "length", "diameter","height", "whole_weight", 
                                "shucked_weight", "viscera_weight", "shell_weight", "rings"),  
                    col_types = cols("rings" = col_integer(), 
                                     "sex" = col_factor(levels = c("M", "F","I"))))
###Data Processing
#Performed in import: rings as integer, gender as factor. 
#Create subsets of data by gender for t test
ab.f <- ab.data  %>%  filter(sex %in% c("F")) %>% select(-sex)
ab.i <- ab.data  %>%  filter(sex %in% c("I")) %>% select(-sex)

#Filter master dataset to relevant genders
ab.data2 <- ab.data %>%  filter(sex %in% c("F", "I"))



###Data Exploration
  # Ring frequency
ab.data2 %>% 
  group_by(as.factor(rings)) %>% 
  summarize(n = n()) %>%
  arrange(n %>% desc())

  #Gender frequency
ab.data2 %>% 
  group_by(sex) %>% 
  summarize(n = n()) %>%
  arrange(n %>% desc())

  # Histogram of rings
ab.data2 %>% ggplot(mapping = aes(x = rings)) +
  geom_histogram(bins = 30, coloer = 'blue', fill = 'red') +
  labs(title = "Histogram of Ring Count")

  # Violin plot of rings by sex
ab.data2 %>% ggplot(mapping = aes(x = sex, y = rings, fill = sex)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+ 
  labs(title = "Violin Plot of Ring Count by Sex")

  # Ggpairs exploration of entire dataset
ggpairs(ab.data)


###Statistical Tests
   # Null hypothesis: Difference of ring means between I and F samples will be 0
t.test(ab.i$rings, ab.f$rings, conf.level = 0.95)


###Sample Subsets
set.seed(12)

#Training sample
ab.train <- ab.data2 %>% 
  sample_frac(0.7)

#Testing sample
ab.test <- ab.data2 %>%  
  setdiff(ab.train)

#Verification
nrow(ab.test) + nrow(ab.train) == nrow(ab.data2)


###Model Construction
#Function to normalize input data
normalize <- function(x) {(x - mean(x))/sd(x)
  }

binom <- ab.train %>% mutate_at(vars (-sex), normalize)

  # Univariate Model 1
mod.uni <- glm(sex ~rings -1, data = binom, family = binomial)

mod.uni %>% summary
mod.uni %>% confint()

binom.uni <- binom %>% 
  mutate (score = predict (mod.uni)) 

binom.uni %>% ggplot(aes(x = sex, y = score)) +
  geom_violin ( draw_quantiles= c(0.25, 0.5, 0.75), 
                fill = 'blue', alpha = 0.3, size = 1.0) +
  labs(title="Univariate Model 1 Score",
       x="Sex", y="Score")

binom.uni <- binom.uni %>% 
  mutate(score.sex = if_else (score > 0 , 'I', 'F') %>% 
           as.factor ()) 


caret::confusionMatrix(data = binom.uni$score.sex, 
                       reference = as.factor(binom.uni$sex),
                       mode = "prec_recall")


  # Multivariate Model 1
mod.multi <- glm(sex ~., data = binom, family = binomial)

mod.multi %>% summary
mod.multi %>% confint()

binom.multi <- binom %>% 
  mutate (score = predict (mod.multi)) 

binom.multi %>% ggplot(aes(x = sex, y = score)) +
  geom_violin (draw_quantiles= c(0.25, 0.5, 0.75), 
                fill = 'blue', alpha = 0.3, size = 1.0) +
  labs(title="Multivariate Model 1 Score",
       x="Sex", y="Score")


binom.multi <- binom.multi %>% 
  mutate(score.sex = if_else (score > 0 , 'I', 'F') %>% 
           as.factor ()) 


caret::confusionMatrix(data = binom.multi$score.sex, 
  reference = as.factor(binom.multi$sex),
  mode = "prec_recall")

  #Updated Multivariate Model (2)
binom2 <- binom %>% select(-height, -shell_weight, -whole_weight, -shucked_weight)

mod.multi2 <- glm(sex ~. -1, data = binom2, family = binomial)

mod.multi2 %>% summary
mod.multi2 %>% confint()

binom.multi2 <- binom2 %>% 
  mutate (score = predict (mod.multi2)) 

binom.multi2 %>% ggplot(aes(x = sex, y = score)) +
  geom_violin ( draw_quantiles= c(0.25, 0.5, 0.75), 
                fill = 'blue', alpha = 0.3, size = 1.0) +
  labs(title="Multivariate Model 2 Score",
       x="Sex", y="Score")

binom.multi2 <- binom.multi2 %>% 
  mutate(score.sex = if_else (score > 0 , 'I', 'F') %>% 
           as.factor ()) 

caret::confusionMatrix(data = binom.multi2$score.sex, 
                       reference = as.factor(binom.multi2$sex),
                       mode = "prec_recall")


#Model Testing
ab.test1 <-ab.test
ab.test2 <-ab.test
ab.test3 <-ab.test

  # Uni Model
ab.test3$score <- predict(mod.uni, newdata = ab.test3)

ab.test3 <-ab.test3 %>% mutate (score = normalize(score))
ab.test3 <- ab.test3 %>% mutate (score.sex = if_else(score > 0, "I", "F") %>% as.factor())

caret::confusionMatrix(data = ab.test3$score.sex, 
                       reference = as.factor(ab.test3$sex),
                       mode = "prec_recall")


  # Original Multi Model
ab.test2$score <- predict(mod.multi, newdata = ab.test2)

ab.test2 <-ab.test2 %>% mutate (score = normalize(score))
ab.test2 <- ab.test2 %>% mutate (score.sex = if_else(score > 0, "I", "F") %>% as.factor())

caret::confusionMatrix(data = ab.test2$score.sex, 
                       reference = as.factor(ab.test2$sex),
                       mode = "prec_recall")

  # Updated Multi Model
ab.test1$score <- predict(mod.multi2, newdata = ab.test)

ab.test1 <-ab.test1 %>% mutate (score = normalize(score))
ab.test1 <- ab.test1 %>% mutate (score.sex = if_else(score > 0, "I", "F") %>% as.factor())

caret::confusionMatrix(data = ab.test1$score.sex, 
                       reference = as.factor(ab.test1$sex),
                       mode = "prec_recall")

###Analysis & Conclusions:
  # Exploring the dataset, we determined there were significant differences in the mean ring count (age) of 
#female (F) and infant (I) abalone. Our objective was to determine whether a statistical model could distinguish 
#between the two labels. To accomplish this we split the data into training (70%) and testing (30%) subsets, and
#used the training subset to construct classification models. We created 3 models. First, a single variable model 
#to predict sex from ring count, then a multivariate model to predict sex from all other variables. Our model 
#summary and confidence intervals indicated several non-significant variables. We created the final multivariate 
#model to leave those variables out. 
  # A comparison of confusion matrices run on the test subset indicated that the single variable model was least 
#accurate, the all-included multivariate model was most accurate, and the updated multivariate model was in between.
#Although the all-inclusive model had the best prediction of the test subset, we felt that including several variables
#with high P values, St. error margins in the same magnitude as our coefficient estimates, and confidence intervals
#that included 0 was poor statistical practice.