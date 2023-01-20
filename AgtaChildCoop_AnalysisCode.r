## Analysis code for Daniel Major-Smith et al. "Cooperation and Partner Choice Among Agta Hunter-Gatherer Children: An Evolutionary Developmental Perspective"

## Author: Daniel Major-Smith
## R version 4.0.4


## Make sure workspace is clear
rm(list = ls())

# Install and load packages
#install.packages("tidyverse")
library(tidyverse)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("dagitty")
library(dagitty)

#install.packages("rstan")
library(rstan)

#install.packages("posterior")
library(posterior)

#install.packages("brms")
library(brms)

#install.packages("ggmcmc")
library(ggmcmc)


# Set working directory
setwd("...")
setwd("C:\\Users\\ds16565\\OneDrive - University of Bristol\\MyFiles-Migrated\\Documents\\KidCoopPaper\\Analysis\\CurrentScriptAndData")


#########################################################
## First analysis explores how much individuals gave to others

# Read in data
data <- read_csv("AgtaChildCoop_AmountGiven.csv")

# Quick summary of the data and check levels of missing data
head(data)

summary(data)


# Only vars with missing data are mother and father levels of cooperation (24 and 34 NA's, respectively)


## Check descriptive stats of all the variables

## Change ID variables to factors
data <- data %>%
  mutate(id = as.factor(id)) %>%
  mutate(camp = as.factor(camp)) %>%
  mutate(camp = fct_relevel(camp, "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11"
                            , "M1", "M2", "M3")) %>%
  mutate(household = as.factor(household))

glimpse(data)


## How much was shared
# Data is definitely non-normal - Is a mode at 0 (30% of games), plus similar peaks at 2 and 3 (23% and 27%, respectively)
summary(data$perGiven); sd(data$perGiven)
table(data$perGiven)
barplot(table(data$perGiven), xlab = "Number of gifts given")

## How many recipients they gave to
summary(data$numRecipients); sd(data$numRecipients)
table(data$numRecipients)
barplot(table(data$numRecipients), xlab = "Number of recipients given to")

# Make a table of this data
(table1a <- data %>%
    group_by(perGiven) %>%
    summarise(n_given = n()) %>%
    mutate(per_given = (n_given / sum(n_given)) * 100) %>%
    mutate(per_given = round(per_given, 1)) %>%
    ungroup() %>%
    rename(number = perGiven))

(table1b <- data %>%
    group_by(numRecipients) %>%
    summarise(n_recip = n()) %>%
    mutate(per_recip = (n_recip / sum(n_recip)) * 100) %>%
    mutate(per_recip = round(per_recip, 1)) %>%
    ungroup() %>%
    rename(number = numRecipients))

(table1 <- full_join(table1a, table1b, by = "number"))

write_csv(table1, file = "../Results/table1.csv", quote = FALSE)


## Age - Ranges from 3 to 18, with a mean of 8.9
summary(data$age)
sd(data$age)
hist(data$age)

## Make a decent-looking histogram for age

(figS1 <- ggplot(data = data, aes(x = age)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "gray70") +
  ylab("Number of children") +
  xlab("Age (years)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks=seq(3,18,1)) +
  scale_y_continuous(limits = c(0, 27), expand = c(0, 0)) +
  geom_vline(aes(xintercept=mean(age)),
             color="black", linetype="dashed", size=1))

# Now save this plot
pdf("../Results/FigS1_AgeHist.pdf", width = 12, height = 8)
figS1
dev.off()

# Summary stats of age by camp (for table S1)
tableS1 <- data %>%
  group_by(camp) %>%
  summarise(n = length(age), n_males = sum(sex == 1), per_males = round((n_males / n) * 100, 1), 
            mean = round(mean(age), 1), sd = round(sd(age), 1), 
            min = round(min(age), 1), max = round(max(age), 1), 
            mean_rel = round(mean(rel), 3), sd_rel = round(sd(rel), 3),
            min_rel = round(min(rel), 3), max_rel = round(max(rel), 3), adult = round(mean(adult), 1))

tableS1


# Also want to add a 'total' column to the bottom
total <- tribble(
  ~camp, ~n, ~n_males, ~per_males, ~mean, ~sd, ~min, ~max, ~mean_rel, ~sd_rel, ~min_rel, ~max_rel, ~adult,
  "Total", nrow(data), sum(data$sex == 1), round((sum(data$sex == 1) / nrow(data)) * 100, 1), 
  round(mean(data$age), 1), round(sd(data$age), 1), round(min(data$age), 1), round(max(data$age), 1), 
  round(mean(data$rel), 3), round(sd(data$rel), 3), round(min(data$rel), 3), round(max(data$rel), 3),
  round(mean(tableS1$adult), 1)
)

total

tableS1 <- bind_rows(tableS1, total)
tableS1

# Save this table
write_csv(tableS1, file = "../Results/tableS1.csv", quote = FALSE)


## Sex - 1 = male and 0 = female
table(data$sex)
round(prop.table(table(data$sex)) * 100, 2)

## Average relatedness to child camp-mates
summary(data$rel)
hist(data$rel)

## Average amount shared by adults in camp
summary(tableS1$adult)
hist(tableS1$adult)

## Amount shared by mother and father
summary(data$mother)
summary(data$father)

# Check all looks sensible
glimpse(data)
head(data)


#### Create directed acyclic graph (DAG) encoding the hypothesised causal structure of the data, with 'child levels of cooperation' as the outcome
figS2 <- dagitty('dag {
                   childCoop [pos = "3,3"]
                   age [pos = "0.5,3"]
                   sex [pos = "1,2.5"]
                   relatedness [pos = "2,2"]
                   adultCoop [pos = "3,2"]
                   repeatedInteractions [pos = "2.5,1.5"]
                   resourceAvailability [pos = "3,1.25"]
                   storytelling [pos = "3.5,1.5"]
                   
                   age -> childCoop
                   sex -> childCoop
                   relatedness -> childCoop
                   adultCoop -> childCoop
                   repeatedInteractions -> adultCoop
                   resourceAvailability -> adultCoop
                   storytelling -> adultCoop
}')
plot(figS2)

# Now save this plot
pdf("../Results/FigS2_DAG.pdf", width = 7, height = 5)
plot(figS2)
dev.off()



### Testing for camp-level differences in cooperation

## Single-level model (no random effects)
null.noml <- brm(perGiven ~ 1, data = data, 
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 590951)
summary(null.noml)

null.noml <- add_criterion(null.noml, "loo")

# Check convergence over time and between chains
null.noml_trans <- ggs(null.noml)

# All chains converged and stable
ggplot(filter(null.noml_trans, Parameter %in% c("b_Intercept", "sigma")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


## Camp-level random effect
null.ml_camp <- brm(perGiven ~ 1 + (1 | camp), data = data, 
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 881927)
summary(null.ml_camp)

null.ml_camp <- add_criterion(null.ml_camp, "loo")

# Check convergence over time and between chains
null.ml_camp_trans <- ggs(null.ml_camp)

# All chains converged and stable
ggplot(filter(null.ml_camp_trans, Parameter %in% c("b_Intercept", "sigma", "sd_camp__Intercept")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


## Household-level random effect
null.ml_hh <- brm(perGiven ~ 1 + (1 | household), data = data, 
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 488054)
summary(null.ml_hh)

null.ml_hh <- add_criterion(null.ml_hh, "loo")

# Check convergence over time and between chains
null.ml_hh_trans <- ggs(null.ml_hh)

# All chains converged and stable
ggplot(filter(null.ml_hh_trans, Parameter %in% c("b_Intercept", "sigma", "sd_household__Intercept")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


## Household-level and camp-level random effects
null.ml_hh_camp <- brm(perGiven ~ 1 + (1 | household) + (1 | camp), data = data, 
                            warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 225559)
summary(null.ml_hh_camp)

null.ml_hh_camp <- add_criterion(null.ml_hh_camp, "loo")

# Check convergence over time and between chains
null.ml_hh_camp_trans <- ggs(null.ml_hh_camp)

# All chains converged and stable
ggplot(filter(null.ml_hh_camp_trans, Parameter %in% c("b_Intercept", "sigma", "sd_camp__Intercept", 
                                                           "sd_household__Intercept")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


## Compare these models - Camp only random effect is the best-fitting model, with no improvement when including household
loo_compare(null.noml, null.ml_camp, null.ml_hh, null.ml_hh_camp, criterion = "loo")

data <- data %>%
  select(-household)


# The MLM is a much better fit to the data than the basic linear model - Camp level differences explain
# ~29% of the variance in child offers
hyp <- "sd_camp__Intercept^2 / (sd_camp__Intercept^2 + sigma^2) = 0"
hypothesis(null.ml_camp, hyp, class = NULL)

# Plot this camp-level variation
(fig1 <- ggplot(data = data,
       aes(x = camp, y = perGiven)) +
  geom_boxplot(fill='#A4A4A4', color="black") +
  ylab("Amount shared") +
  xlab("Camp") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_summary(fun=mean, geom="point", shape=18, size=6) +
  scale_x_discrete(limits=c("P9", "P6", "P10", "P7", "P8", "M1", "P2", "P4", 
                            "P3", "P1", "P5", "M2", "P11", "M3")) +
  geom_hline(aes(yintercept=mean(perGiven)),
             color="black", linetype="dashed", size=1))

# Now save this plot
pdf("../Results/Fig1_CampVariation.pdf", width = 12, height = 8)
fig1
dev.off()

# Get summary stats of camp-level variation
campVar <- data %>%
  group_by(camp) %>%
  summarise(n = length(camp), mean = mean(perGiven), sd = sd(perGiven))

campVar <- campVar %>%
  arrange(mean)

campVar



#######################################################################################
## Now run the cooperation models

# First, run a separate model for each predictor
age.give <- brm(perGiven ~ age + (1 | camp), data = data, 
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 142053)
summary(age.give)

sex.give <- brm(perGiven ~ sex + (1 | camp), data = data, 
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 456988)
summary(sex.give)

rel.give <- brm(perGiven ~ rel + (1 | camp), data = data, 
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 201017)
summary(rel.give)

adult.give <- brm(perGiven ~ adult + (1 | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 16191)
summary(adult.give)

# Now run a combined model which includes all predictors (based on our DAG, as we assume that these variables are relatively independent, the results of full model should be similar to individual models - and they are)
full.give <- brm(perGiven ~ age + sex + rel + adult + (1 | camp), data = data, 
                 warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 687570)
summary(full.give)

# Save these results in a table and export to CSV
table2 <- as.data.frame(cbind(c(rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)),
                         c(rep(c("Univariable", "Multivariable"), each = 4)),
                         c(rep(c("Individual", "Individual", "Individual", "Camp"), 2)),
                         c(fixef(age.give)[2,1], fixef(sex.give)[2,1],
                           fixef(rel.give)[2,1], fixef(adult.give)[2,1],
                           fixef(full.give)[2:5,1]),
                         c(fixef(age.give)[2,2], fixef(sex.give)[2,2],
                           fixef(rel.give)[2,2], fixef(adult.give)[2,2],
                           fixef(full.give)[2:5,2]),
                         c(fixef(age.give)[2,3], fixef(sex.give)[2,3],
                           fixef(rel.give)[2,3], fixef(adult.give)[2,3],
                           fixef(full.give)[2:5,3]),
                         c(fixef(age.give)[2,4], fixef(sex.give)[2,4],
                           fixef(rel.give)[2,4], fixef(adult.give)[2,4],
                           fixef(full.give)[2:5,4])))
colnames(table2) <- c("Variable", "Model", "Level", "Coefficient", "SE", "LCI", "UCI")

# Convert estimates to numeric and round
table2 <- table2 %>%
  mutate(Coefficient = as.numeric(Coefficient)) %>%
  mutate(Coefficient = round(Coefficient, 3)) %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(SE = round(SE, 3)) %>%
  mutate(LCI = as.numeric(LCI)) %>%
  mutate(LCI = round(LCI, 3)) %>%
  mutate(UCI = as.numeric(UCI)) %>%
  mutate(UCI = round(UCI, 3))

table2

write_csv(table2, "../Results/table2.csv", quote = FALSE)



## Check some model assumptions (of normality) - Residuals from the full model look relatively normal - And save these images
pdf("../Results/FigS3_Residuals_FullCoopModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(full.give)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-2, 30, "A", cex = 2.5)

qqnorm(residuals(full.give)[, "Estimate"], main = NULL)
qqline(residuals(full.give)[, "Estimate"])
text(-2.25, 2.5, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's a bit odd, but I don't think it's '*too* bad...(mainly looks weird because the range of values is only between 0 and 5)
pdf("../Results/FigS4_Heteroskedasticity_FullCoopModel.pdf", width = 8, height = 6)

plot(residuals(full.give)[, "Estimate"] ~ fitted.values(full.give)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")

dev.off()


### Want to plot the association between child and adult camp-level cooperation
campData <- data %>%
  add_count(camp) %>%
  rename(size = n) %>%
  select(camp, perGiven, adult, size) %>%
  group_by(camp) %>%
  summarise(childCoop = mean(perGiven), adultCoop = mean(adult), size = mean(size))

head(campData)

## Also make a dataframe with the predicted amount shared for each level of adult cooperation (note that for brms, to get predicted confidence intervals need to use 'fitted', as 'predict' gives prediction intervals)
adult <- 0:70
df_temp <- as.data.frame(cbind(adult))
df_temp$fit <- fitted(adult.give, newdata = df_temp, re_formula = NA)[,1]
df_temp$fit_lci <- fitted(adult.give, newdata = df_temp, re_formula = NA)[,3]
df_temp$fit_uci <- fitted(adult.give, newdata = df_temp, re_formula = NA)[,4]
head(df_temp)

set.seed(1234)
(fig2 <- ggplot(data = NULL) +
    geom_ribbon(data = df_temp, aes(ymin = fit_lci, ymax = fit_uci, x = adult), fill = "gray90") +
    geom_line(data = df_temp, aes(x = adult, y = fit),
              colour = "black", size = 1) +
    ylab("Number of gifts shared by children") +
    xlab("Average adult level of cooperation in camp (%)") +
    theme_bw() +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    geom_point(data = data, aes(x = adult, y = perGiven), colour = "gray70", 
               position = position_jitter(width = 0.75, height = 0.2)) +
    geom_point(data = campData, aes(x = adultCoop, y = childCoop, size = size), shape = 18) +
    scale_x_continuous(breaks=seq(0,70,10)))

pdf("../Results/Fig2_ChildAdultCoop.pdf", width = 12, height = 8)
fig2
dev.off()


## Also want to make a plot of predicted amount given, but using fitted values, rather than raw data (to allow adjustment for other variables). Will use results from the full model here (note that even though levels of cooperation can only take integer values between 0 and 5, as these fitted values are taken from a linear model predicted levels of cooperation are on a continuous scale. While this may be an abuse of the data, it is useful to show the association between age and levels of cooperation [which are more difficult to visualise with integer outcomes here]; additionally, as the results of this linear model are broadly comparable to those of poisson and ordinal models below, this gives confidence to these overall patterns.)
data$fit <- fitted(full.give)[, "Estimate"]
summary(data$fit)

newdata_age <- data.frame(age = 3:18, sex = mean(data$sex), rel = mean(data$rel), adult = mean(tableS1$adult))
newdata_age$fit <- fitted(full.give, newdata = newdata_age, re_formula = NA)[, "Estimate"]
newdata_age$fit_lci <- fitted(full.give, newdata = newdata_age, re_formula = NA)[, "Q2.5"]
newdata_age$fit_uci <- fitted(full.give, newdata = newdata_age, re_formula = NA)[, "Q97.5"]
head(newdata_age)

(age_fit <- ggplot(data = NULL) +
    geom_ribbon(data = newdata_age, aes(x = age, ymin = fit_lci, ymax = fit_uci), fill = "gray90") +
    geom_point(data = data, aes(x = age, y = fit_resp)) +
    geom_line(data = newdata_age, aes(x = age, y = fit), colour = "black", size = 1) +
    ylab("Predicted amount shared by children") +
    xlab("Child age (years)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(-0.2, 3.3)) +
    scale_x_continuous(breaks = seq(3, 18, 3)))



### Conducting sensitivity analysis to take variation in adult levels of cooperation into consideration.

## Our approach is as follows:
# 1.	Sample from a normal distribution using the mean and standard error of adult cooperation from each camp, and use this as the average adult level of cooperation for said camp and store these 1,000 datasets.
# 2.	Run the models (both univariable and multivariable) using the command 'brm_multiple' which runs the model in the 1,000 datasets and combines the results together.


## First, create 1,000 datasets with different adult cooperation values
set.seed(52494)
n <- 1000

df_list <- list() # Initialise an empty list to add dataframes to

# Loop over the number of lists to create, and vary adult levels of cooperation in each
for (i in 1:n) {
  data_temp <- data
  data_temp$adult_samp <- NA
  data_temp$adult_samp[data_temp$camp == "P1"] <- rnorm(n = 1, mean = 59.2, sd = 3.73)
  data_temp$adult_samp[data_temp$camp == "P2"] <- rnorm(n = 1, mean = 31.4, sd = 3.61)
  data_temp$adult_samp[data_temp$camp == "P3"] <- rnorm(n = 1, mean = 52.8, sd = 3.47)
  data_temp$adult_samp[data_temp$camp == "P4"] <- rnorm(n = 1, mean = 56.9, sd = 7.34)
  data_temp$adult_samp[data_temp$camp == "P5"] <- rnorm(n = 1, mean = 38.5, sd = 6.44)
  data_temp$adult_samp[data_temp$camp == "P6"] <- rnorm(n = 1, mean = 21.7, sd = 5.19)
  data_temp$adult_samp[data_temp$camp == "P7"] <- rnorm(n = 1, mean = 28.2, sd = 4.28)
  data_temp$adult_samp[data_temp$camp == "P8"] <- rnorm(n = 1, mean = 6.67, sd = 6.67)
  data_temp$adult_samp[data_temp$camp == "P9"] <- rnorm(n = 1, mean = 0, sd = 0)
  data_temp$adult_samp[data_temp$camp == "P10"] <- rnorm(n = 1, mean = 30.4, sd = 6.61)
  data_temp$adult_samp[data_temp$camp == "P11"] <- rnorm(n = 1, mean = 59, sd = 5.47)
  data_temp$adult_samp[data_temp$camp == "M1"] <- rnorm(n = 1, mean = 69.3, sd = 4.52)
  data_temp$adult_samp[data_temp$camp == "M2"] <- rnorm(n = 1, mean = 49.3, sd = 7.81)
  data_temp$adult_samp[data_temp$camp == "M3"] <- rnorm(n = 1, mean = 60.7, sd = 9.64)
  
  df_list[[i]] <- data_temp
}

#df_list

# Now run the combined brms model (lower the iterations and number of chains as well, to speed up processing) - This takes approx. 45 minutes to run.

# Univariable model first
adult.give_samp <- brm_multiple(perGiven ~ adult + (1 | camp), data = df_list, 
                                warmup = 1000, iter = 2000, chains = 1, init = "random", seed = 561804)
summary(adult.give_samp)

# Now for multivariable model
full.give_samp <- brm_multiple(perGiven ~ age + sex + rel + adult + (1 | camp), data = df_list, 
                               warmup = 1000, iter = 2000, chains = 1, init = "random", seed = 868339)
summary(full.give_samp)

# Extract and save results
table_varyAdult <- as.data.frame(cbind(c(rep(c("Adult coop"), 2)),
                                            c("Univariable", "Multivariable"),
                                            c(fixef(adult.give_samp)[2,1], fixef(full.give_samp)[5,1]),
                                            c(fixef(adult.give_samp)[2,2], fixef(full.give_samp)[5,2]),
                                            c(fixef(adult.give_samp)[2,3], fixef(full.give_samp)[5,3]),
                                            c(fixef(adult.give_samp)[2,4], fixef(full.give_samp)[5,4])))
colnames(table_varyAdult) <- c("Variable", "Model", "Coefficient", "SE", "LCI", "UCI")

# Convert estimates to numeric and round
table_varyAdult <- table_varyAdult %>%
  mutate(Coefficient = as.numeric(Coefficient)) %>%
  mutate(Coefficient = round(Coefficient, 3)) %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(SE = round(SE, 3)) %>%
  mutate(LCI = as.numeric(LCI)) %>%
  mutate(LCI = round(LCI, 3)) %>%
  mutate(UCI = as.numeric(UCI)) %>%
  mutate(UCI = round(UCI, 3))

table_varyAdult

write_csv(table_varyAdult, "../Results/table_varyAdult.csv", quote = FALSE)



## As another sensitivity analysis, will run the same models, but using poisson and ordinal regression/cumulative link models, as the number of gifts given (0 to 5) isn't really continuous - CLMs predict the odds (or log-odds) of being in a higher outcome category (amount shared), for a one-unit increase in the predictor variable. This is quite similar to poisson regression, but as poisson regressions require count data there's no (theoretical) upper value. But in our example there is an upper value (5) in which case an ordinal model may also be appropriate. Also, the distribution of our data is skewed, as there is an over-abundance of '0's for a poisson distribution. However, if all models give qualitatively similar results, this bolsters our confidence in our conclusions, even if some model assumptions are violated.

## Start with poisson distribution, because it (potentially) meets model assumptions better, which is used for count data (which is what we have here)

# The mean and variances are roughly equivalent for  number of gifts, meeting one of the assumptions of a  poisson model - Although there is an excess of '0's, which does violate poisson assumptions.
(mean(data$perGiven)); (var(data$perGiven))
barplot(table(data$perGiven))


# Fit the individual and full models again
age.give.p <- brm(perGiven ~ age + (1 | camp), data = data, family = "Poisson",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 958990)
summary(age.give.p)

sex.give.p <- brm(perGiven ~ sex + (1 | camp), data = data, family = "Poisson",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 499444)
summary(sex.give.p)

rel.give.p <- brm(perGiven ~ rel + (1 | camp), data = data, family = "Poisson",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 667957)
summary(rel.give.p)

adult.give.p <- brm(perGiven ~ adult + (1 | camp), data = data, family = "Poisson",
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 224373)
summary(adult.give.p)

# Now run a combined model which includes all predictors
full.give.p <- brm(perGiven ~ age + sex + rel + adult + (1 | camp), data = data, family = "Poisson",
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 512385)
summary(full.give.p)


### Now for the ordinal regression model
# Construct ordinal variable of number of resources given to others - Will also combine 4 and 5 gifts together, as low number of children gave 5
data$perGiven_ord <- data$perGiven
data$perGiven_ord[data$perGiven_ord == 5] <- 4
data$perGiven_ord <- factor(data$perGiven_ord, ordered = T)
table(data$perGiven_ord)

# Fit the individual and full models again
age.give.o <- brm(perGiven_ord ~ age + (1 | camp), data = data, family = "cumulative",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 574224)
summary(age.give.o)

sex.give.o <- brm(perGiven_ord ~ sex + (1 | camp), data = data, family = "cumulative",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 648189)
summary(sex.give.o)

rel.give.o <- brm(perGiven_ord ~ rel + (1 | camp), data = data, family = "cumulative",
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 512656)
summary(rel.give.o)

adult.give.o <- brm(perGiven_ord ~ adult + (1 | camp), data = data, family = "cumulative",
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 453256)
summary(adult.give.o)

# Now run a combined model which includes all predictors
full.give.o <- brm(perGiven_ord ~ age + sex + rel + adult + (1 | camp), data = data, family = "cumulative",
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 317428)
summary(full.give.o)


## Make a table to save all these results to
model <- rep(c("Separate", "Full"), each = 4)
var <- rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)

tableS2 <- as.data.frame(cbind(model, var))
tableS2

# I'm sure there's a better way of combining results, but at least it works, and once this is set up I wont have to repeat it again if I re-run the models.
tableS2$linear_coef[tableS2$model == "Separate" & var == "Age"] <- round(fixef(age.give)[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Age"] <- round(fixef(age.give)[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Age"] <- round(fixef(age.give)[2, 3], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Age"] <- round(fixef(age.give)[2, 4], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Sex"] <- round(fixef(sex.give)[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Sex"] <- round(fixef(sex.give)[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Sex"] <- round(fixef(sex.give)[2, 3], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Sex"] <- round(fixef(sex.give)[2, 4], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.give)[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.give)[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.give)[2, 3], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.give)[2, 4], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.give)[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.give)[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.give)[2, 3], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.give)[2, 4], 3)

tableS2$linear_coef[tableS2$model == "Full"] <- round(fixef(full.give)[2:5, 1], 3)
tableS2$linear_se[tableS2$model == "Full"] <- round(fixef(full.give)[2:5, 2], 3)
tableS2$linear_lci[tableS2$model == "Full"] <- round(fixef(full.give)[2:5, 3], 3)
tableS2$linear_uci[tableS2$model == "Full"] <- round(fixef(full.give)[2:5, 4], 3)

tableS2


# Poisson results
tableS2$poisson_coef[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.p)[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.p)[2, 3]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.p)[2, 4]), 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.p)[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.p)[2, 3]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.p)[2, 4]), 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.p)[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.p)[2, 3]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.p)[2, 4]), 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.p)[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.p)[2, 3]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.p)[2, 4]), 3)

tableS2$poisson_coef[tableS2$model == "Full"] <- round(exp(fixef(full.give.p)[2:5, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Full"] <- round(exp(fixef(full.give.p)[2:5, 3]), 3)
tableS2$poisson_uci[tableS2$model == "Full"] <- round(exp(fixef(full.give.p)[2:5, 4]), 3)

tableS2


# Ordinal regression results
tableS2$ord_coef[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.o)[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.o)[5, 3]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Age"] <- round(exp(fixef(age.give.o)[5, 4]), 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.o)[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.o)[5, 3]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.give.o)[5, 4]), 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.o)[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.o)[5, 3]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.give.o)[5, 4]), 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.o)[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.o)[5, 3]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.give.o)[5, 4]), 3)

tableS2$ord_coef[tableS2$model == "Full"] <- round(exp(fixef(full.give.o)[5:8, 1]), 3)
tableS2$ord_lci[tableS2$model == "Full"] <- round(exp(fixef(full.give.o)[5:8, 3]), 3)
tableS2$ord_uci[tableS2$model == "Full"] <- round(exp(fixef(full.give.o)[5:8, 4]), 3)

tableS2

# Save this table
write_csv(tableS2, file = "../Results/tableS2.csv", quote = FALSE)



# Next, explore the age association to test whether association is quadratic. Will explore various models, including:
# - Linear age term, camp-level random intercepts only
# - Quadratic age term, camp-level random intercepts only
# - Linear age term, camp-level random slopes and intercepts
# - Quadratic age term, camp-level random slopes and intercepts

data <- data %>%
  mutate(age2 = age^2)

## Linear age term, camp-level random intercepts only
age.noSlopes <- brm(perGiven ~ age + (1 | camp), data = data, 
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 899291)
summary(age.noSlopes)

age.noSlopes <- add_criterion(age.noSlopes, "loo")


## Quadratic age term, camp-level random intercepts only
age2.noSlopes <- brm(perGiven ~ age + age2 + (1 | camp), data = data, 
                     warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 748650)
summary(age2.noSlopes)

age2.noSlopes <- add_criterion(age2.noSlopes, "loo")


## Linear age term, camp-level random slopes and intercepts
age.slopes <- brm(perGiven ~ age + (age | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 959263)
summary(age.slopes)

age.slopes <- add_criterion(age.slopes, "loo")

# Check convergence over time and between chains
age.slopes_trans <- ggs(age.slopes)

# All chains converged and stable
ggplot(filter(age.slopes_trans, Parameter %in% c("b_Intercept", "sigma", "sd_camp__Intercept", 
                                                 "sd_camp__age", "b_age")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


## Quadratic age term, camp-level random slopes and intercepts
age2.slopes <- brm(perGiven ~ age + age2 + (age + age2 | camp), data = data, 
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 959263)
summary(age2.slopes)

age2.slopes <- add_criterion(age2.slopes, "loo")

# Check convergence over time and between chains
age2.slopes_trans <- ggs(age2.slopes)

# All chains converged and stable
ggplot(filter(age2.slopes_trans, Parameter %in% c("b_Intercept", "sigma", "sd_camp__Intercept", 
                                                  "sd_camp__age", "sd_camp__age2", "b_age", "b_age2")),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


# Comparing the models, there is practically no difference in the loo model fit values, suggesting that random slopes and random intercepts is not a better fit than just random slopes (with age and age-squared RE model slightly worse), while age-squared term not improve model fit either. So the linear age term with camp-level random intercepts only appears to be the best fit to the data.
loo(age.noSlopes) 
loo(age2.noSlopes)
loo(age.slopes)
loo(age2.slopes)

# Drop the age-squared variable
data <- data %>%
  select(-age2)



############################################################################################
## Univariable models to explore whether parental cooperation predicts child-level cooperation. Just run simple univariate ML-models for this.

## First just mums data (n=155 - so lost 24 obs)
data_mum <- data %>%
  filter(!is.na(mother))

# Only very weak evidence that mums amount shared is associated with child levels of cooperation
mum_model <- brm(perGiven ~ mother + (1|camp), data = data_mum,
                 warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 417249)

summary(mum_model)

# Check consistency of results if use poisson and ordinal models
mum_model.p <- brm(perGiven ~ mother + (1|camp), family = "poisson", data = data_mum,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 481045)
summary(mum_model.p)

mum_model.o <- brm(perGiven_ord ~ mother + (1|camp), family = "cumulative", data = data_mum,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 35525)
summary(mum_model.o)


## Now for just dads data (n=145 - so lost 34 obs)
data_dad <- data %>%
  filter(!is.na(father))

# No evidence that dads amount shared is associated with child levels of cooperation
dad_model <- brm(perGiven ~ father + (1|camp), data = data_dad,
                 warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 796844)

summary(dad_model)

# Check consistency of results if use poisson and ordinal models
dad_model.p <- brm(perGiven ~ father + (1|camp), family = "poisson", data = data_dad,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 30129)
summary(dad_model.p)

dad_model.o <- brm(perGiven_ord ~ father + (1|camp), family = "cumulative", data = data_dad,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 596473)
summary(dad_model.o)


## Now for both parents, using the average if both parents have data, or just one parent if only have data for one parent (n = 162 - so lost 17 cases)
data_mumdad <- data %>%
  filter(!is.na(mother) | (!is.na(father))) %>%
  select(camp, perGiven, perGiven_ord, mother, father)

# Make an average if is data for both parents (or just mums if missing data, or just dad if missing mum)
data_mumdad <- data_mumdad %>%
  mutate(parents = ifelse(((!is.na(mother)) & (!is.na(father))), (mother + father) / 2, 
         ifelse((!is.na(mother)) & (is.na(father)), mother, father)))

summary(data_mumdad$parents)
sample_n(data_mumdad, size = 20)

# As with separate parents models, no evidence that parents amount shared is associated with child levels of cooperation
parents_model <- brm(perGiven ~ parents + (1|camp), data = data_mumdad,
                     warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 261510)

summary(parents_model)

# Check consistency of results if use poisson and ordinal models
parents_model.p <- brm(perGiven ~ parents + (1|camp), family = "poisson", data = data_mumdad,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 143349)
summary(parents_model.p)

parents_model.o <- brm(perGiven_ord ~ parents + (1|camp), family = "cumulative", data = data_mumdad,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 454760)
summary(parents_model.o)


## Make a table to save these results to
TableS3 <- data.frame(cbind(variable = c("Mother", "Father", "Parents"),
                                 linear_coef = c(fixef(mum_model)[2,1], fixef(dad_model)[2,1],
                                                 fixef(parents_model)[2,1]),
                                 linear_se = c(fixef(mum_model)[2,2], fixef(dad_model)[2,2],
                                               fixef(parents_model)[2,2]),
                                 linear_lci = c(fixef(mum_model)[2,3], fixef(dad_model)[2,3], 
                                                fixef(parents_model)[2,3]),
                                 linear_uci = c(fixef(mum_model)[2,4], fixef(dad_model)[2,4], 
                                                fixef(parents_model)[2,4]),
                                 pois_coef = c(exp(fixef(mum_model.p)[2,1]), exp(fixef(dad_model.p)[2,1]),
                                               exp(fixef(parents_model.p)[2,1])),
                                 pois_lci = c(exp(fixef(mum_model.p)[2,3]), exp(fixef(dad_model.p)[2,3]), 
                                              exp(fixef(parents_model.p)[2,3])),
                                 pois_uci = c(exp(fixef(mum_model.p)[2,4]), exp(fixef(dad_model.p)[2,4]), 
                                              exp(fixef(parents_model.p)[2,4])),
                                 ord_coef = c(exp(fixef(mum_model.o)[5,1]), exp(fixef(dad_model.o)[5,1]), 
                                              exp(fixef(parents_model.o)[5,1])),
                                 ord_lci = c(exp(fixef(mum_model.o)[5,3]), exp(fixef(dad_model.o)[5,3]), 
                                             exp(fixef(parents_model.o)[5,3])),
                                 ord_uci = c(exp(fixef(mum_model.o)[5,4]), exp(fixef(dad_model.o)[5,4]), 
                                             exp(fixef(parents_model.o)[5,4]))
))
TableS3

# Convert estimates to numeric and round
TableS3 <- TableS3 %>%
  mutate(linear_coef = as.numeric(linear_coef)) %>%
  mutate(linear_coef = round(linear_coef, 3)) %>%
  mutate(linear_se = as.numeric(linear_se)) %>%
  mutate(linear_se = round(linear_se, 3)) %>%
  mutate(linear_lci = as.numeric(linear_lci)) %>%
  mutate(linear_lci = round(linear_lci, 3)) %>%
  mutate(linear_uci = as.numeric(linear_uci)) %>%
  mutate(linear_uci = round(linear_uci, 3)) %>%
  mutate(pois_coef = as.numeric(pois_coef)) %>%
  mutate(pois_coef = round(pois_coef, 3)) %>%
  mutate(pois_lci = as.numeric(pois_lci)) %>%
  mutate(pois_lci = round(pois_lci, 3)) %>%
  mutate(pois_uci = as.numeric(pois_uci)) %>%
  mutate(pois_uci = round(pois_uci, 3)) %>%
  mutate(ord_coef = as.numeric(ord_coef)) %>%
  mutate(ord_coef = round(ord_coef, 3)) %>%
  mutate(ord_lci = as.numeric(ord_lci)) %>%
  mutate(ord_lci = round(ord_lci, 3)) %>%
  mutate(ord_uci = as.numeric(ord_uci)) %>%
  mutate(ord_uci = round(ord_uci, 3))

TableS3

write_csv(TableS3, "../Results/tableS3.csv", quote = FALSE)



########################################################################################
## Now run models of number of unique recipients

# First, run a separate model for each predictor model
age.numRec <- brm(numRecipients ~ age + (1 | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 984092)
summary(age.numRec)

sex.numRec <- brm(numRecipients ~ sex + (1 | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 97184)
summary(sex.numRec)

rel.numRec <- brm(numRecipients ~ rel + (1 | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 893579)
summary(rel.numRec)

adult.numRec <- brm(numRecipients ~ adult + (1 | camp), data = data, 
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 913480)
summary(adult.numRec)

# Now run a combined model which includes all predictors (as assume are relatively independent, results of full model should be similar to individual models - and they are)
full.numRec <- brm(numRecipients ~ age + sex + rel + adult + (1 | camp), data = data, 
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 431963)
summary(full.numRec)


# First check some model assumptions (of normality) - Residuals from the global model look relatively normal - And save these images
pdf("../Results/FigS6_Residuals_FullNumRecipModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(full.numRec)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-2, 33, "A", cex = 2.5)

qqnorm(residuals(full.numRec)[, "Estimate"], main = NULL)
qqline(residuals(full.numRec)[, "Estimate"])
text(-2.5, 2.5, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's a bit odd, but I don't think it's 'too' bad...(mainly looks weird because the values are only 0 to 5)
pdf("../Results/FigS7_Heteroskedasticity_FullNumRecipModel.pdf", width = 8, height = 6)

plot(residuals(full.numRec)[, "Estimate"] ~ fitted.values(full.numRec)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")

dev.off()



# Now plot the predicted values for age and number of respondents
data$fit_resp <- fitted(full.numRec)[, "Estimate"]

newdata_resp <- data.frame(age = 3:18, sex = mean(data$sex), rel = mean(data$rel), adult = mean(tableS1$adult))
newdata_resp$fit <- fitted(full.numRec, newdata = newdata_resp, re_formula = NA)[, "Estimate"]
newdata_resp$fit_lci <- fitted(full.numRec, newdata = newdata_resp, re_formula = NA)[, "Q2.5"]
newdata_resp$fit_uci <- fitted(full.numRec, newdata = newdata_resp, re_formula = NA)[, "Q97.5"]
head(newdata_resp)

(ageResp_fit <- ggplot(data = NULL) +
    geom_ribbon(data = newdata_resp, aes(x = age, ymin = fit_lci, ymax = fit_uci), fill = "gray90") +
    geom_point(data = data, aes(x = age, y = fit_resp)) +
    geom_line(data = newdata_resp, aes(x = age, y = fit), colour = "black", size = 1) +
    ylab("Predicted number of unique recipients") +
    xlab("Child age (years)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    scale_y_continuous(breaks = seq(0, 3, 1)) +
    scale_x_continuous(breaks = seq(3, 18, 3)))


# Arrange amount shared and number of recipients fitted value plots together and save as pdf file
pdf("../Results/FigS5_AgeCoop.pdf", width = 8, height = 12)

grid.arrange(age_fit, ageResp_fit, ncol = 1)

dev.off()



### Conducting sensitivity analysis to take variation in adult levels of cooperation into consideration.

## Our approach is as follows:
# 1.	Sample from a normal distribution using the mean and standard error of adult cooperation from each camp, and use this as the average adult level of cooperation for said camp and store these 1,000 datasets.
# 2.	Run the models (both univariable and multivariable) using the command 'brm_multiple' which runs the model in the 1,000 datasets and combines the results together.


## First, create 1,000 datasets with different adult cooperation values
set.seed(172486)
n <- 1000

df_list <- list() # Initialise an empty list to add dataframes to

# Loop over the number of lists to create, and vary adult levels of cooperation in each
for (i in 1:n) {
  data_temp <- data
  data_temp$adult_samp <- NA
  data_temp$adult_samp[data_temp$camp == "P1"] <- rnorm(n = 1, mean = 59.2, sd = 3.73)
  data_temp$adult_samp[data_temp$camp == "P2"] <- rnorm(n = 1, mean = 31.4, sd = 3.61)
  data_temp$adult_samp[data_temp$camp == "P3"] <- rnorm(n = 1, mean = 52.8, sd = 3.47)
  data_temp$adult_samp[data_temp$camp == "P4"] <- rnorm(n = 1, mean = 56.9, sd = 7.34)
  data_temp$adult_samp[data_temp$camp == "P5"] <- rnorm(n = 1, mean = 38.5, sd = 6.44)
  data_temp$adult_samp[data_temp$camp == "P6"] <- rnorm(n = 1, mean = 21.7, sd = 5.19)
  data_temp$adult_samp[data_temp$camp == "P7"] <- rnorm(n = 1, mean = 28.2, sd = 4.28)
  data_temp$adult_samp[data_temp$camp == "P8"] <- rnorm(n = 1, mean = 6.67, sd = 6.67)
  data_temp$adult_samp[data_temp$camp == "P9"] <- rnorm(n = 1, mean = 0, sd = 0)
  data_temp$adult_samp[data_temp$camp == "P10"] <- rnorm(n = 1, mean = 30.4, sd = 6.61)
  data_temp$adult_samp[data_temp$camp == "P11"] <- rnorm(n = 1, mean = 59, sd = 5.47)
  data_temp$adult_samp[data_temp$camp == "M1"] <- rnorm(n = 1, mean = 69.3, sd = 4.52)
  data_temp$adult_samp[data_temp$camp == "M2"] <- rnorm(n = 1, mean = 49.3, sd = 7.81)
  data_temp$adult_samp[data_temp$camp == "M3"] <- rnorm(n = 1, mean = 60.7, sd = 9.64)
  
  df_list[[i]] <- data_temp
}

#df_list

# Now run the combined brms model (lower the iterations and number of chains as well, to speed up processing) - This takes approx. 45 minutes to run.

# Univariable model first
adult.numRec_samp <- brm_multiple(numRecipients ~ adult + (1 | camp), data = df_list, 
                                  warmup = 1000, iter = 2000, chains = 1, init = "random", seed = 818414)
summary(adult.numRec_samp)

# Now for multivariable model
full.numRec_samp <- brm_multiple(numRecipients ~ age + sex + rel + adult + (1 | camp), data = df_list, 
                                 warmup = 1000, iter = 2000, chains = 1, init = "random", seed = 302509)
summary(full.numRec_samp)

# Extract and save results
table_varyAdult_numRec <- as.data.frame(cbind(c(rep(c("Adult coop"), 2)),
                                                   c("Univariable", "Multivariable"),
                                                   c(fixef(adult.numRec_samp)[2,1], fixef(full.numRec_samp)[5,1]),
                                                   c(fixef(adult.numRec_samp)[2,2], fixef(full.numRec_samp)[5,2]),
                                                   c(fixef(adult.numRec_samp)[2,3], fixef(full.numRec_samp)[5,3]),
                                                   c(fixef(adult.numRec_samp)[2,4], fixef(full.numRec_samp)[5,4])))
colnames(table_varyAdult_numRec) <- c("Variable", "Model", "Coefficient", "SE", "LCI", "UCI")

# Convert estimates to numeric and round
table_varyAdult_numRec <- table_varyAdult_numRec %>%
  mutate(Coefficient = as.numeric(Coefficient)) %>%
  mutate(Coefficient = round(Coefficient, 3)) %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(SE = round(SE, 3)) %>%
  mutate(LCI = as.numeric(LCI)) %>%
  mutate(LCI = round(LCI, 3)) %>%
  mutate(UCI = as.numeric(UCI)) %>%
  mutate(UCI = round(UCI, 3))

table_varyAdult_numRec

write_csv(table_varyAdult_numRec, "../Results/table_varyAdult_numRec.csv", quote = FALSE)


## As a further check, will also run the above analyses using poisson regression, which is used for count 
## data (which is what we have here)

# The mean and variances are roughly equivalent for number of recipients, meeting one of the assumptions of a
# poisson model (although there is an excess of '0's, which does violate model assumptions)
(mean(data$numRecipients)); (var(data$numRecipients))
barplot(table(data$numRecipients))

# Fit the individual and full models again
age.numRec.p <- brm(numRecipients ~ age + (1|camp), family = "poisson", data = data,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 916940)
summary(age.numRec.p)

sex.numRec.p <- brm(numRecipients ~ sex + (1|camp), family = "poisson", data = data,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 425745)
summary(sex.numRec.p)

rel.numRec.p <- brm(numRecipients ~ rel + (1|camp), family = "poisson", data = data,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 455104)
summary(rel.numRec.p)

adult.numRec.p <- brm(numRecipients ~ adult + (1|camp), family = "poisson", data = data,
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 581703)
summary(adult.numRec.p)

# Now run a combined model which includes all predictors
full.numRec.p <- brm(numRecipients ~ age + sex + rel + adult + (1|camp), family = "poisson", data = data,
                     warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 259190)
summary(full.numRec.p)


### Now for the ordinal regression model
# Construct ordinal variable of number of unique recipients - As above, will combine 4 and 5 recipients together, as low number of children gave to 5 others
data$numRec_ord <- data$numRecipients
data$numRec_ord[data$numRec_ord == 5] <- 4
data$numRec_ord <- factor(data$numRec_ord, ordered = T)
table(data$numRec_ord)

# Fit the individual and full models again
age.numRec.o <- brm(numRec_ord ~ age + (1|camp), data = data, family = "cumulative",
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 258918)
summary(age.numRec.o)

sex.numRec.o <- brm(numRec_ord ~ sex + (1|camp), data = data, family = "cumulative",
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 399534)
summary(sex.numRec.o)

rel.numRec.o <- brm(numRec_ord ~ rel + (1|camp), data = data, family = "cumulative",
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 958561)
summary(rel.numRec.o)

adult.numRec.o <- brm(numRec_ord ~ adult + (1|camp), data = data, family = "cumulative",
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 148804)
summary(adult.numRec.o)

# Now run a combined model which includes all predictors
full.numRec.o <- brm(numRec_ord ~ age + sex + rel + adult + (1|camp), data = data, family = "cumulative",
                     warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 263217)
summary(full.numRec.o)


## Make a table to save all these results to
model <- rep(c("Separate", "Full"), each = 4)
var <- rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)

TableS4 <- as.data.frame(cbind(model, var))
TableS4

# I'm sure there's a better way of combining results, but at least once this is set up I wont have to repeat it again if I re-run the models...
TableS4$linear_coef[TableS4$model == "Separate" & var == "Age"] <- round(fixef(age.numRec)[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Age"] <- round(fixef(age.numRec)[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Age"] <- round(fixef(age.numRec)[2, 3], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Age"] <- round(fixef(age.numRec)[2, 4], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Sex"] <- round(fixef(sex.numRec)[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Sex"] <- round(fixef(sex.numRec)[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Sex"] <- round(fixef(sex.numRec)[2, 3], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Sex"] <- round(fixef(sex.numRec)[2, 4], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.numRec)[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.numRec)[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.numRec)[2, 3], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(fixef(rel.numRec)[2, 4], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.numRec)[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.numRec)[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.numRec)[2, 3], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(fixef(adult.numRec)[2, 4], 3)

TableS4$linear_coef[TableS4$model == "Full"] <- round(fixef(full.numRec)[2:5, 1], 3)
TableS4$linear_se[TableS4$model == "Full"] <- round(fixef(full.numRec)[2:5, 2], 3)
TableS4$linear_lci[TableS4$model == "Full"] <- round(fixef(full.numRec)[2:5, 3], 3)
TableS4$linear_uci[TableS4$model == "Full"] <- round(fixef(full.numRec)[2:5, 4], 3)

TableS4


# Poisson results
TableS4$poisson_coef[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.p)[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.p)[2, 3]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.p)[2, 4]), 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.p)[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.p)[2, 3]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.p)[2, 4]), 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.p)[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.p)[2, 3]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.p)[2, 4]), 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.p)[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.p)[2, 3]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.p)[2, 4]), 3)

TableS4$poisson_coef[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.p)[2:5, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.p)[2:5, 3]), 3)
TableS4$poisson_uci[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.p)[2:5, 4]), 3)

TableS4


# Ordinal regression results
TableS4$ord_coef[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.o)[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.o)[5, 3]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Age"] <- round(exp(fixef(age.numRec.o)[5, 4]), 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.o)[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.o)[5, 3]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(fixef(sex.numRec.o)[5, 4]), 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.o)[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.o)[5, 3]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(fixef(rel.numRec.o)[5, 4]), 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.o)[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.o)[5, 3]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(fixef(adult.numRec.o)[5, 4]), 3)

TableS4$ord_coef[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.o)[5:8, 1]), 3)
TableS4$ord_lci[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.o)[5:8, 3]), 3)
TableS4$ord_uci[TableS4$model == "Full"] <- round(exp(fixef(full.numRec.o)[5:8, 4]), 3)

TableS4

# Save this table
write_csv(TableS4, file = "../Results/TableS4.csv", quote = FALSE)



# Next, explore the age association to test whether association is quadratic. Will explore various models, including:
# - Linear age term, camp-level random intercepts only
# - Quadratic age term, camp-level random intercepts only
# - Linear age term, camp-level random slopes and intercepts
# - Quadratic age term, camp-level random slopes and intercepts

data <- data %>%
  mutate(age2 = age^2)

## Linear age term, camp-level random intercepts only
age.noSlopes_numRec <- brm(numRecipients ~ age + (1 | camp), data = data, 
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 4029)
summary(age.noSlopes_numRec)

age.noSlopes_numRec <- add_criterion(age.noSlopes_numRec, "loo")


## Quadratic age term, camp-level random intercepts only
age2.noSlopes_numRec <- brm(numRecipients ~ age + age2 + (1 | camp), data = data, 
                     warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 541416)
summary(age2.noSlopes_numRec)

age2.noSlopes_numRec <- add_criterion(age2.noSlopes_numRec, "loo")


## Linear age term, camp-level random slopes and intercepts
age.slopes_numRec <- brm(numRecipients ~ age + (age | camp), data = data, 
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 970368)
summary(age.slopes_numRec)

age.slopes_numRec <- add_criterion(age.slopes_numRec, "loo")


## Quadratic age term, camp-level random slopes and intercepts
age2.slopes_numRec <- brm(numRecipients ~ age + age2 + (age + age2 | camp), data = data, 
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 33674)
summary(age2.slopes_numRec)

age2.slopes_numRec <- add_criterion(age2.slopes_numRec, "loo")


# Comparing the models, there is practically no difference in the loo model fit values, suggesting that random slopes and random intercepts is not a better fit than just random slopes (with age and age-squared RE model slightly worse), while age-squared term not improve model fit either. So the linear age term with camp-level random intercepts only appears to be the best fit to the data.
loo(age.noSlopes_numRec)
loo(age2.noSlopes_numRec)
loo(age.slopes_numRec)
loo(age2.slopes_numRec)

# Drop the age-squared variable
data <- data %>%
  select(-age2)





###############################################################################
## Kinship and camp relatedness, to see whether individuals preferentially gave
## to kin, relative to background camp-levels of relatedness

relate <- read_csv("AgtaChildCoop_KinshipAndCampRel.csv")

head(relate)
summary(relate)

# Make ID and camp factors
relate$id <- factor(relate$id)
relate$camp <- factor(relate$camp)


### Mixed effect model to see whether individuals preferentially shared with kin, relative to background levels of relatedness in camp. Need to control for ID as a random effect as some children are repeated multiple times in the dataset (if gave multiple gifts), plus potentially camp if is camp-level variation

## Will explore different random effects structures to identify appropriate model

# No random effects
null.noml <- brm(Rel ~ 1, data = relate,
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 375367)
summary(null.noml)

null.noml <- add_criterion(null.noml, "loo")

# Individual level random effects
null.indiv <- brm(Rel ~ 1 + (1|id), data = relate,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 588739)
summary(null.indiv)

null.indiv <- add_criterion(null.indiv, "loo")

# Camp level random effects
null.camp <- brm(Rel ~ 1 + (1|camp), data = relate,
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 881417)
summary(null.camp)

null.camp <- add_criterion(null.camp, "loo")

# Individual and camp level random effects
null.indiv_camp <- brm(Rel ~ 1 + (1|id) + (1|camp), data = relate,
                            warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 606000)
summary(null.indiv_camp)

null.indiv_camp <- add_criterion(null.indiv_camp, "loo")


## Compare these models - Individual ID random effects same fit as individual ID and camp random effects, so go with simpler model (individual ID random effects only)
loo(null.noml)
loo(null.indiv)
loo(null.camp)
loo(null.indiv_camp)


### Now run the actual model
camp.rel <- brm(Rel ~ Cond + (1|id), data = relate,
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 588739)
summary(camp.rel)


# Plot the raw data by condition - Make a quick table of mean values first
(rel.table <- relate %>%
    group_by(Cond) %>%
    summarise(n = length(Cond), mean = mean(Rel), SD = sd(Rel), 
              min = min(Rel), max = max(Rel), median = median(Rel)))

(histS8 <- ggplot(data = relate,
                  aes(x = Rel, colour = Cond)) +
    geom_histogram(binwidth = 0.01, fill = "white", position = "identity", alpha = 0.25, size = 2) +
    ylab("Number of observations") +
    xlab("Relatedness") +
    theme_bw() +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=seq(0,0.5,0.1)) +
    scale_y_continuous(limits = c(0, 170), expand = c(0, 0)) +
    theme(legend.position="top") +
    scale_colour_manual(values = c("black", "gray"), name = "Group", 
                          labels = c("Average camp rel.", "Rel. to recipients")) +
    geom_vline(data = rel.table, aes(xintercept=mean, colour = Cond), linetype="dashed", size=1))

# Save this plot
pdf("../Results/FigS8_CampRel_ByGroup.pdf", width = 8, height = 6)

histS8

dev.off()


# First check some model assumptions (of normality) - The residuals are really dodgy, and no transformation 
# would fix this (this is because the raw data are multimodal - Relatedness to recipients has a major mode 
# at 0.5, and small peaks at around 0, 0.125 and 0.25)
pdf("../Results/FigS9_Residuals_CampRelModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(camp.rel)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-0.3, 105, "A", cex = 2.5)

qqnorm(residuals(camp.rel)[, "Estimate"], main = NULL)
qqline(residuals(camp.rel)[, "Estimate"])
text(-2.5, 0.25, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's rather odd, but I don't think it's *too* bad...(looks
# weird because the raw data is very non-normal)
pdf("../Results/FigS10_Heteroskedasticity_CampRelModel.pdf", width = 8, height = 6)

plot(residuals(camp.rel)[, "Estimate"] ~ fitted.values(camp.rel)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")

dev.off()


### And a sanity check that including camp as an additional random effect doesn't alter results - Yup, results practically identical to above.
camp.rel2 <- brm(Rel ~ Cond + (1|id) + (1|camp), data = relate,
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 756473)
summary(camp.rel2)


#############################################################################################
##  Exploring the factors which predict who children gave to

recip <- read_csv("AgtaChildCoop_ChildRecipients.csv")

head(recip)
summary(recip)

# Convert ID variables to factors (although I won't use 'alter_id' as a random effect below, as can cause major convergence issues and nonsense results)
recip$id <- factor(recip$id)
recip$camp <- factor(recip$camp)
recip$alter_id <- factor(recip$alter_id)


## Find the optimal random effects structure in a null model where 'relatedness' is the outcome

# Full model (with all random effects terms) - Lots of convergence warnings and some r-hat values >= 1.01, so will see if removing terms improves with without affecting model fit
rel.ml <- brm(rel ~ 1 + (1 | id) + (1 | alter_id) + (1 | camp), data = recip,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 709080)
summary(rel.ml)

rel.ml <- add_criterion(rel.ml, "loo")

# Dropping alter ID - Better than full model, but still some issues
rel.ml_noAlter <- brm(rel ~ 1 + (1 | id) + (1 | camp), data = recip,
                           warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 986468)
summary(rel.ml_noAlter)

rel.ml_noAlter <- add_criterion(rel.ml_noAlter, "loo")

# Dropping camp - Better than full model, but still some issues
rel.ml_noCamp <- brm(rel ~ 1 + (1 | id) + (1 | alter_id), data = recip,
                          warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 550780)
summary(rel.ml_noCamp)

rel.ml_noCamp <- add_criterion(rel.ml_noCamp, "loo")

# Dropping camp and alter ID - Still a couple of warnings, but better than other models
rel.ml_noAlterCamp <- brm(rel ~ 1 + (1 | id), data = recip,
                               warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 916708)
summary(rel.ml_noAlterCamp)

rel.ml_noAlterCamp <- add_criterion(rel.ml_noAlterCamp, "loo")

# No multi-level structure
rel.noml <- brm(rel ~ 1, data = recip,
                          warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 192978)
summary(rel.noml)

rel.noml <- add_criterion(rel.noml, "loo")


## Comparing models - Best fit is most complex model, although SEs of LOOIC are very wide and and overlapping, meaning choice of best model not completely obvious. Addition of 'camp' doesn't make that much difference, though. Given warnings and issues of model fit, will use ego ID only as main model.
loo(rel.ml)
loo(rel.ml_noAlter)
loo(rel.ml_noCamp)
loo(rel.ml_noAlterCamp)
loo(rel.noml)


## Now look at whether age and sex are associated with the relatedness between giver and recipient 

# First with ego ID as random effect (and other models with 'camp' and alter ID as well, to check stability of results) - All give very similar results
rel.agesex <- brm(rel ~ egoAge + egoSex + (1 | id), data = recip,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 151088)
summary(rel.agesex)
#plot(rel.agesex, N = 3)
round(fixef(rel.agesex), 3)

rel.agesex_alter <- brm(rel ~ egoAge + egoSex + (1 | id) + (1 | alter_id), data = recip,
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 599992)
summary(rel.agesex_alter)

rel.agesex_alterCamp <- brm(rel ~ egoAge + egoSex + (1 | id) + (1 | alter_id) + (1 | camp), data = recip,
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 845179)
summary(rel.agesex_alterCamp)


# Now with interaction term - No evidence for interaction
rel.agebysex <- brm(rel ~ egoAge * egoSex + (1 | id), data = recip,
                         warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 780416)
summary(rel.agebysex)
round(fixef(rel.agebysex), 3)

rel.agebysex_alter <- brm(rel ~ egoAge * egoSex + (1 | id) + (1 | alter_id), data = recip,
                          warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 39681)
summary(rel.agebysex_alter)

rel.agebysex_alterCamp <- brm(rel ~ egoAge * egoSex + (1 | id) + (1 | alter_id) + (1 | camp), data = recip,
                          warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 983187)
summary(rel.agebysex_alterCamp)


## Strongest effect is that of ego age - As ego age increases, more likely to give to less related
## camp-mates. Although is a weak effect of sex as well (with boys more likely to share with close kin than girls)


# Check assumptions of main effects model

# Normality - The residuals are not quite normal, and I doubt that transformations will help given the
# odd distribution of relatedness
pdf("../Results/FigS11_Residuals_RelAgeSexModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(rel.agesex)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-0.3, 120, "A", cex = 2.5)

qqnorm(residuals(rel.agesex)[, "Estimate"], main = NULL)
qqline(residuals(rel.agesex)[, "Estimate"])
text(-2.5, 0.25, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It ain't too great...
pdf("../Results/FigS12_Heteroskedasticity_RelAgeSexModel.pdf", width = 8, height = 6)

plot(residuals(rel.agesex)[, "Estimate"] ~ fitted.values(rel.agesex)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")

dev.off()



## Make a nice plot of this age effect
rel_age <- recip %>%
  mutate(kin_cats = cut(rel, breaks=c(-Inf, 0.031249, 0.49, Inf), labels=c("NR","DK","PK"))) %>%
  mutate(age_cats = cut(egoAge, breaks=c(-Inf, 6.99, 10.99, Inf), labels=c("3-7","7-11","11+"))) %>%
  select(rel, kin_cats, egoAge, age_cats)

rel_age
summary(rel_age)

table(rel_age$kin_cats, rel_age$age_cats)

# Make summary stats
df <- rel_age %>%
  group_by(age_cats, kin_cats) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))

df

# Now make plot
(fig3 <- ggplot(data=df, aes(x=age_cats, y=freq, fill=kin_cats)) +
    geom_bar(stat="identity", position=position_dodge()) +
    geom_text(aes(label=n), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5) +
    xlab("Age categories") +
    ylab("Proportion of nominations") +
    theme_bw() +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
    scale_fill_manual(values = c("black", "gray40", "gray70"), name = "Kin categories",
                        labels = c("Non relatives", "Distant kin", "Primary kin")) +
    theme(legend.position=c(0.85, 0.85), legend.text = element_text(size = 14), 
          legend.title = element_text(size=16)))

# Save this plot
pdf("../Results/Fig3_RelByAge.pdf", width = 8, height = 6)

fig3

dev.off()



## As the outcome variable is very skewed, I'll run the same model but using a binary marker of 'shared with sibling or not' as the response variable
head(recip)

recip$sib <- ifelse(recip$rel == 0.5, 1, 0)
table(recip$sib)

rel.agesex.bin <- brm(sib ~ egoAge + egoSex + (1|id), family = "bernoulli", data = recip,
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 170005)
summary(rel.agesex.bin)
#plot(rel.agesex.bin, N = 3)

fixef(rel.agesex.bin)
exp(fixef(rel.agesex.bin))

### The results of this model should not be trusted!!! The parameter estimates are pretty ridiculous - For instance, the log odds for sex is 3.1, meaning the odds of sharing with siblings are 21 times higher if you're a boy!!

# The predicted probabilities are also highly improbable, as a boy aged 5 had a 97% probability of sharing with siblings, while for a girl aged 15 the probability of sharing with a sibling was 3%. This suggests estimation and convergence issues with this model.
newdata <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 2),
                            egoSex = rep(c(0, 1), each = 3)))
newdata
newdata$probs <- predict(rel.agesex.bin, newdata = newdata, type = "response", re_formula = NA)[, "Estimate"]
newdata


## Look at raw data by sex, to see whether this is plausible - Predicted values are in right direction, but seem too extreme given the raw data

# For boys
sib_age_boy <- recip %>%
  filter(egoSex == 1) %>%
  mutate(age_cats = cut(egoAge, breaks=c(-Inf, 6.99, 10.99, Inf), labels=c("3-7","7-11","11+"))) %>%
  select(sib, egoAge, age_cats)
summary(sib_age_boy)

df_boy <- sib_age_boy %>%
  group_by(age_cats, sib) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
df_boy

# For girls
sib_age_girl <- recip %>%
  filter(egoSex == 0) %>%
  mutate(age_cats = cut(egoAge, breaks=c(-Inf, 6.99, 10.99, Inf), labels=c("3-7","7-11","11+"))) %>%
  select(sib, egoAge, age_cats)
summary(sib_age_girl)

# Make summary stats
df_girl <- sib_age_girl %>%
  group_by(age_cats, sib) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
df_girl


# In the non-ML GLM the coefficients are much more sensible... Looking at the raw stats, there is a definite increase in boys giving to sibs, but the output from the ML model is just bananas! If I run the model without the mixed-effect then the estimates are much more sensible the OR for age is 0.87, while the OR for sex is 2.38 (but obviously this doesn't take the non-independence of data points into consideration).
rel.agesex.bin.noml <- brm(sib ~ egoAge + egoSex, family = "bernoulli", data = recip,
                           warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 979498)
summary(rel.agesex.bin.noml)

fixef(rel.agesex.bin.noml)
exp(fixef(rel.agesex.bin.noml))


## Try popping in some informative priors for these parameters (to specify priors on the intercept, have to use the "0 + Intercept", notation - see: https://www.rensvandeschoot.com/tutorials/brms-priors/).
get_prior(sib ~ 0 + Intercept + egoAge + egoSex + (1|id), data = recip)

# Set the priors:
# Age: As expect age association to be negative (given models above), will set age effect close to that of single-level sibling model (i.e., -0.1)
# Sex: As expect this to be positive (as males perhaps more likely to share with siblings), will set this prior to be positive, at a log-odds of 0.8 (similar to single-level model)
# Intercept: Expect baseline odds of sharing with kin to be quite high, so will set at a log-odds of 1 (odds ratio of 2.7; again, similar to the single-level model)
prior <- c(set_prior("normal(-0.1, 0.2)", class = "b", coef = "egoAge"),
           set_prior("normal(0.8, 0.5)", class = "b", coef = "egoSex"),
           set_prior("normal(1, 0.5)", class = "b", coef = "Intercept"))

rel.agesex.bin_prior <- brm(sib ~ 0 + Intercept + egoAge + egoSex + (1|id), 
                                 family = "bernoulli", data = recip,
                                 warmup = 1000, iter = 3000, chains = 4, init = "random", 
                                 prior = prior, seed = 548089)
summary(rel.agesex.bin_prior)
prior_summary(rel.agesex.bin_prior)
#plot(rel.agesex.bin_prior, N = 3)

## The parameter estimates are much less extreme and look more sensible now (e.g., odds ratio of boys giving to siblings is now 3.6, rather than 21)
fixef(rel.agesex.bin_prior)
exp(fixef(rel.agesex.bin_prior))

# Check if predicted values are more sensible - These do look more realistic as well, and although they show the same pattern of results as a original default prior model, they are less extreme.
newdata_prior <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 2),
                            egoSex = rep(c(0, 1), each = 3)))
newdata_prior
newdata_prior$probs <- predict(rel.agesex.bin_prior, newdata = newdata, type = "response", 
                               re_formula = NA)[, "Estimate"]
newdata_prior
newdata



## Will make a simple non-ML model using average relatedness to recipients, and see if same results hold (they do)
head(recip)

recip2 <- recip %>%
  group_by(id) %>%
  summarise(rel = mean(rel), egoAge = mean(egoAge), egoSex = mean(egoSex))

head(recip2)

hist(recip2$rel)

rel2.model <- brm(rel ~ egoAge + egoSex, data = recip2,
                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 355189)
summary(rel2.model)

round(fixef(rel2.model), 3)


# Tests of assumptions
pdf("../Results/FigS13_AssumptionPlots_SingleLevelRelAgeSexModel_.pdf", width = 12, height = 5)

par(mfrow = c(1,3))

hist(residuals(rel2.model)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-0.35, 33, "A", cex = 2.5)

qqnorm(residuals(rel2.model)[, "Estimate"], main = NULL)
qqline(residuals(rel2.model))[, "Estimate"]
text(-2.2, 0.28, "B", cex = 2.5)

plot(residuals(rel2.model)[, "Estimate"] ~ fitted.values(rel2.model)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")
text(0.4, 0.28, "C", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now run a single-level logistic model comparing giving only to sibs vs only giving to non-sibs
recip2$sib <- ifelse(recip2$rel == 0.5, 1, 0)
head(recip2)
table(recip2$sib)

rel2.bin <- brm(sib ~ egoAge + egoSex, data = recip2, family = "bernoulli",
                warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 325598)
summary(rel2.bin)

fixef(rel2.bin)
round(exp(fixef(rel2.bin)), 3)


## Make a table to save these results to
TableS5 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex"),
                            linear_coef = fixef(rel2.model)[,1],
                            linear_lci = fixef(rel2.model)[,3],
                            linear_uci = fixef(rel2.model)[,4],
                            logit_coef = exp(fixef(rel2.bin)[,1]),
                            logit_lci = exp(fixef(rel2.bin)[,3]),
                            logit_uci = exp(fixef(rel2.bin)[,4])
                            ))
TableS5

# Convert estimates to numeric and round
TableS5 <- TableS5 %>%
  mutate(linear_coef = as.numeric(linear_coef)) %>%
  mutate(linear_coef = round(linear_coef, 3)) %>%
  mutate(linear_lci = as.numeric(linear_lci)) %>%
  mutate(linear_lci = round(linear_lci, 3)) %>%
  mutate(linear_uci = as.numeric(linear_uci)) %>%
  mutate(linear_uci = round(linear_uci, 3)) %>%
  mutate(logit_coef = as.numeric(logit_coef)) %>%
  mutate(logit_coef = round(logit_coef, 3)) %>%
  mutate(logit_lci = as.numeric(logit_lci)) %>%
  mutate(logit_lci = round(logit_lci, 3)) %>%
  mutate(logit_uci = as.numeric(logit_uci)) %>%
  mutate(logit_uci = round(logit_uci, 3))

TableS5

write_csv(TableS5, "../Results/tableS5.csv", quote = FALSE)


# Look at predicted probabilities, as easier to interpret than odds ratios
newdata_noml <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 2),
                            egoSex = rep(c(0, 1), each = 3)))
newdata_noml
newdata_noml$probs <- predict(rel2.bin, newdata = newdata, type = "response")[, "Estimate"]
newdata_noml



################################################################################################
#### Next, explore how ego age, ego sex and relatedness (and their interactions) predict: i) recipient age; ii) the age difference between ego and recipient; iii) recipient sex; and iv) whether ego and recipient were of the same sex. 

### i) Factors predicting age of recipient

# Find the optimal random effects structure in a null model where 'recipient age' is the outcome

# Full model (with all random effects terms) - Lots of convergence warnings and high r-hat values (around 3 to 4)
recAge.ml <- brm(recAge ~ 1 + (1 | id) + (1 | alter_id) + (1 | camp), data = recip,
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 583653)
summary(recAge.ml)

recAge.ml <- add_criterion(recAge.ml, "loo")

# Dropping alter ID - Much better than full model, but still some issues
recAge.ml_noAlter <- brm(recAge ~ 1 + (1 | id) + (1 | camp), data = recip,
                              warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 646089)
summary(recAge.ml_noAlter)

recAge.ml_noAlter <- add_criterion(recAge.ml_noAlter, "loo")

# Dropping camp - Pretty much same as full model, as lots of warnings/issues and massive r-hat values
recAge.ml_noCamp <- brm(recAge ~ 1 + (1 | id) + (1 | alter_id), data = recip,
                             warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 178013)
summary(recAge.ml_noCamp)

recAge.ml_noCamp <- add_criterion(recAge.ml_noCamp, "loo")

# Dropping camp and alter ID - Still a couple of issues, but generally much better
recAge.ml_noAlterCamp <- brm(recAge ~ 1 + (1 | id), data = recip,
                                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 50583)
summary(recAge.ml_noAlterCamp)

recAge.ml_noAlterCamp <- add_criterion(recAge.ml_noAlterCamp, "loo")

# No random effects
recAge.noml <- brm(recAge ~ 1, data = recip,
                             warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 129754)
summary(recAge.noml)

recAge.noml <- add_criterion(recAge.noml, "loo")


## Comparing models - Best fit is apparently the most complex model, but the loo values are no way near each other suggesting they're not comparable at all... Comparing the models which fitted well, inclusion of camp not improve model fit, so just using ego ID
loo(recAge.ml)
loo(recAge.ml_noAlter)
loo(recAge.ml_noCamp)
loo(recAge.ml_noAlterCamp)
loo(recAge.noml)


## Now look at whether age, sex and relatedness are associated with recipient age. Here, we see that ego age is positively associated with recipient age, and relatedness is strongly negatively associated with recipient age
recAge.full <- brm(recAge ~ egoAge + egoSex + rel + (1 | id), data = recip,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 905316)
summary(recAge.full)

## Make a table to save these results to
TableS6 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = fixef(recAge.full)[,1],
                            se = fixef(recAge.full)[,2],
                            lci = fixef(recAge.full)[,3],
                            uci = fixef(recAge.full)[,4]
))
TableS6

# Convert estimates to numeric and round
TableS6 <- TableS6 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(se = as.numeric(se)) %>%
  mutate(se = round(se, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3))

TableS6

write_csv(TableS6, "../Results/tableS6.csv", quote = FALSE)


## Plot some predicted results to make this clearer
recip$fitted_age <- fitted(recAge.full)[, "Estimate"]
summary(recip$fitted_age)

df_age <- data.frame(egoAge = 3:18, egoSex = 0.5, rel = mean(recip$rel))
df_age$fit <- fitted(recAge.full, newdata = df_age, re_formula = NA)[, "Estimate"]
df_age$fit_uci <- fitted(recAge.full, newdata = df_age, re_formula = NA)[, "Q2.5"]
df_age$fit_lci <- fitted(recAge.full, newdata = df_age, re_formula = NA)[, "Q97.5"]
head(df_age)

(age_fit <- ggplot(data = NULL) +
    geom_ribbon(data = df_age, aes(ymin = fit_lci, ymax = fit_uci, x = egoAge), fill = "gray90") +
    geom_line(data = df_age, aes(x = egoAge, y = fit), colour = "black", size = 1) +
    geom_point(data = recip, aes(x = egoAge, y = fitted_age)) +
    ylab("Predicted age of recipient") +
    xlab("Child age (years)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    scale_y_continuous(breaks = seq(2, 13, 2), limits = c(1.5, 13.5)) +
    scale_x_continuous(breaks = seq(3, 18, 3)))

pdf("../Results/FigS14_EgoAndRecAge.pdf", width = 10, height = 8)
age_fit
dev.off()

df_rel <- data.frame(egoAge = mean(recip$egoAge), egoSex = 0.5, rel = seq(from = 0, to = 0.5, by = 0.05))
df_rel$fit <- fitted(recAge.full, newdata = df_rel, re_formula = NA)[, "Estimate"]
df_rel$fit_uci <- fitted(recAge.full, newdata = df_rel, re_formula = NA)[, "Q2.5"]
df_rel$fit_lci <- fitted(recAge.full, newdata = df_rel, re_formula = NA)[, "Q97.5"]
head(df_rel)

set.seed(295082)
(rel_fit <- ggplot(NULL) +
    geom_ribbon(data = df_rel, aes(ymin = fit_lci, ymax = fit_uci, x = rel), fill = "gray90") +
    geom_line(data = df_rel, aes(x = rel, y = fit), colour = "black", size = 1) +
    geom_point(data = recip, aes(x = rel, y = fitted_age), position = position_jitter(width = 0.005)) +
    ylab("Predicted age of recipient") +
    xlab("Relatedness") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    scale_y_continuous(breaks = seq(2, 13, 2), limits = c(1.5, 13.5)) +
    scale_x_continuous(breaks = seq(0, 0.5, 0.1)))

pdf("../Results/FigS15_EgoAgeAndRel.pdf", width = 10, height = 8)
rel_fit
dev.off()


## Interaction models

# Age by sex: No interaction
recAge.agebysex <- brm(recAge ~ egoAge * egoSex + rel + (1|id), data = recip,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 866750)
summary(recAge.agebysex)

# Age by relatedness: No interaction
recAge.agebyrel <- brm(recAge ~ egoAge * rel + egoSex + (1|id), data = recip,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 181861)
summary(recAge.agebyrel)

# sex by relatedness: Weak interaction, so will ignore here
recAge.sexbyrel <- brm(recAge ~ egoAge + egoSex * rel + (1|id), data = recip,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 59669)
summary(recAge.sexbyrel)


# what about model assumptions (using the main effects model)? Don't look too bad.
hist(residuals(recAge.full)[, "Estimate"], main = NULL, xlab = "Residuals")

qqnorm(residuals(recAge.full)[, "Estimate"], main = NULL)
qqline(residuals(recAge.full)[, "Estimate"])

plot(residuals(recAge.full)[, "Estimate"] ~ fitted.values(recAge.full)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")



### ii) Factors predicting age difference between ego and alter

# Find the optimal random effects structure in a null model where 'age difference' is the outcome

# Full model (with all random effects terms) - Lots of convergence warnings and high r-hat values (around 3 to 4)
ageDiff.ml <- brm(ageDiff ~ 1 + (1 | id) + (1 | alter_id) + (1 | camp), data = recip,
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 394107)
summary(ageDiff.ml)

ageDiff.ml <- add_criterion(ageDiff.ml, "loo")

# Dropping alter ID - Much better than full model, but still some issues
ageDiff.ml_noAlter <- brm(ageDiff ~ 1 + (1 | id) + (1 | camp), data = recip,
                               warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 294275)
summary(ageDiff.ml_noAlter)

ageDiff.ml_noAlter <- add_criterion(ageDiff.ml_noAlter, "loo")

# Dropping camp - Pretty much same as full model, as lots of warnings/issues and massive r-hat values
ageDiff.ml_noCamp <- brm(ageDiff ~ 1 + (1 | id) + (1 | alter_id), data = recip,
                              warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 947324)
summary(ageDiff.ml_noCamp)

ageDiff.ml_noCamp <- add_criterion(ageDiff.ml_noCamp, "loo")

# Dropping camp and alter ID - Still a couple of issues, but generally much better
ageDiff.ml_noAlterCamp <- brm(ageDiff ~ 1 + (1 | id), data = recip,
                                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 944599)
summary(ageDiff.ml_noAlterCamp)

ageDiff.ml_noAlterCamp <- add_criterion(ageDiff.ml_noAlterCamp, "loo")

# No random effects
ageDiff.noml <- brm(ageDiff ~ 1, data = recip,
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 375146)
summary(ageDiff.noml)

ageDiff.noml <- add_criterion(ageDiff.noml, "loo")


## Comparing models - Best fit is apparently the most complex model, but the loo values are no way near each other suggesting they're not comparable at all... Comparing the two models which fitted well, inclusion of camp slightly improves model fit, but not by much, so just using ego ID
loo(ageDiff.ml)
loo(ageDiff.ml_noAlter)
loo(ageDiff.ml_noCamp)
loo(ageDiff.ml_noAlterCamp)
loo(ageDiff.noml)


## Now look at whether age, sex and relatedness are associated with age difference. Here, we see that ego age is negatively associated with age difference (as ego age increased, so does the age difference), and relatedness is strongly negatively associated with age difference (meaning close kin recipients were likely to have a larger age gap/be younger than than non-kin recipients)
ageDiff.full <- brm(ageDiff ~ egoAge + egoSex + rel + (1|id), data = recip,
                              warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 537939)
summary(ageDiff.full)

## And if include camp RE? Same results
ageDiff.full_camp <- brm(ageDiff ~ egoAge + egoSex + rel + (1|id) + (1|camp), data = recip,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 229228)
summary(ageDiff.full_camp)

## Make a table to save these results to
TableS7 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = fixef(ageDiff.full)[,1],
                            se = fixef(ageDiff.full)[,2],
                            lci = fixef(ageDiff.full)[,3],
                            uci = fixef(ageDiff.full)[,4]
))
TableS7

# Convert estimates to numeric and round
TableS7 <- TableS7 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(se = as.numeric(se)) %>%
  mutate(se = round(se, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3))

TableS7

write_csv(TableS7, "../Results/tableS7.csv", quote = FALSE)



## Interaction models

# Age by sex: No interaction
ageDiff.agebysex <- brm(ageDiff ~ egoAge * egoSex + rel + (1|id), data = recip,
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 437615)
summary(ageDiff.agebysex)

# Age by relatedness: No interaction
ageDiff.agebyrel <- brm(ageDiff ~ egoAge * rel + egoSex + (1|id), data = recip,
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 35163)
summary(ageDiff.agebyrel)

# sex by relatedness: Weak interaction, so will ignore here
ageDiff.sexbyrel <- brm(ageDiff ~ egoAge + egoSex * rel + (1|id), data = recip,
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 338479)
summary(ageDiff.sexbyrel)


# what about model assumptions (using the main effects model)? Don't look too bad.
hist(residuals(ageDiff.full)[, "Estimate"], main = NULL, xlab = "Residuals")

qqnorm(residuals(ageDiff.full)[, "Estimate"], main = NULL)
qqline(residuals(ageDiff.full)[, "Estimate"])

plot(residuals(ageDiff.full)[, "Estimate"] ~ fitted.values(ageDiff.full)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")


## Put these assumption plots for alter age and age diff into one 3x2 plot
pdf("../Results/FigS16_AssumptionPlots_AlterAge_AgeDiff.pdf", width = 12, height = 8)

par(mfrow = c(2,3))

# Alter age
hist(residuals(recAge.full)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-6, 95, "1A", cex = 2)

qqnorm(residuals(recAge.full)[, "Estimate"], main = NULL)
qqline(residuals(recAge.full)[, "Estimate"])
text(-2.5, 9, "1B", cex = 2)

plot(residuals(recAge.full)[, "Estimate"] ~ fitted.values(recAge.full)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")
text(3.25, 9, "1C", cex = 2)

# Age diff
hist(residuals(ageDiff.full)[, "Estimate"], main = NULL, xlab = "Residuals")
text(-6, 95, "2A", cex = 2)

qqnorm(residuals(ageDiff.full)[, "Estimate"], main = NULL)
qqline(residuals(ageDiff.full)[, "Estimate"])
text(-2.5, 9, "2B", cex = 2)

plot(residuals(ageDiff.full)[, "Estimate"] ~ fitted.values(ageDiff.full)[, "Estimate"], 
     xlab = "Fitted values", ylab = "Residuals")
text(-11.5, 9, "2C", cex = 2)

dev.off()

par(mfrow = c(1, 1))



### iii) Factors predicting recipient sex

# Find the optimal random effects structure in a null model where 'recipient sex' is the outcome

# Full model (with all random effects terms) - Lots of convergence warnings and some higher r-hat values (1,02)
recSex.ml <- brm(recSex ~ 1 + (1 | id) + (1 | alter_id) + (1 | camp), data = recip, family = "bernoulli",
                      warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 579202)
summary(recSex.ml)

recSex.ml <- add_criterion(recSex.ml, "loo")

# Dropping alter ID - Much better than full model, but still some issues
recSex.ml_noAlter <- brm(recSex ~ 1 + (1 | id) + (1 | camp), data = recip, family = "bernoulli",
                              warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 546312)
summary(recSex.ml_noAlter)

recSex.ml_noAlter <- add_criterion(recSex.ml_noAlter, "loo")

# Dropping camp - Pretty much same as full model, as lots of warnings/issues
recSex.ml_noCamp <- brm(recSex ~ 1 + (1 | id) + (1 | alter_id), data = recip, family = "bernoulli",
                             warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 13079)
summary(recSex.ml_noCamp)

recSex.ml_noCamp <- add_criterion(recSex.ml_noCamp, "loo")

# Dropping camp and alter ID - Still a couple of issues, but generally much better
recSex.ml_noAlterCamp <- brm(recSex ~ 1 + (1 | id), data = recip, family = "bernoulli",
                                  warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 572006)
summary(recSex.ml_noAlterCamp)

recSex.ml_noAlterCamp <- add_criterion(recSex.ml_noAlterCamp, "loo")

# No random effects
recSex.noml <- brm(recSex ~ 1, data = recip, family = "bernoulli",
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 473144)
summary(recSex.noml)

recSex.noml <- add_criterion(recSex.noml, "loo")


## Comparing models - Best fit is apparently the most complex model, but the loo values are no way near each other suggesting they're not comparable at all...  Comparing the two models which fitted well, model fit is pretty much the same (meaning no/little benefit to inclusion of camp RE)
loo(recSex.ml)
loo(recSex.ml_noAlter)
loo(recSex.ml_noCamp)
loo(recSex.ml_noAlterCamp)
loo(recSex.noml)


## Now look at whether age, sex and relatedness are associated with sex of recipient. Only association is with ego sex, as participants much more likely to choose children of the same sex
sexRec.full <- brm(recSex ~ egoAge + egoSex + rel + (1 | id), data = recip, family = "bernoulli",
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 925748)
summary(sexRec.full)

## Make a table to save these results to
TableS8 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = exp(fixef(sexRec.full)[,1]),
                            lci = exp(fixef(sexRec.full)[,3]),
                            uci = exp(fixef(sexRec.full)[,4])
))
TableS8

# Convert estimates to numeric and round
TableS8 <- TableS8 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3))

TableS8

write_csv(TableS8, "../Results/tableS8.csv", quote = FALSE)


# Predicted probabilities, to help interpret odds ratios
newdata <- data.frame(cbind(egoAge = rep(mean(recip$egoAge), 2),
                            rel = rep(mean(recip$rel), 2),
                            egoSex = c(0, 1)))
newdata
newdata$probs <- fitted(sexRec.full, newdata = newdata, type = "response", re_formula = NA)[, "Estimate"]
newdata


## Interaction models

# Age by sex: No interaction
sexRec.agebysex <- brm(recSex ~ egoAge * egoSex + rel + (1|id), data = recip, family = "bernoulli",
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 727401)
summary(sexRec.agebysex)

exp(fixef(sexRec.agebysex))

# Age by relatedness: Is an association, with odds of nominating males increasing with age if r = 0, but decreasing with age if r = 0.5.
sexRec.agebyrel <- brm(recSex ~ egoAge * rel + egoSex + (1|id), data = recip, family = "bernoulli",
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 611302)
summary(sexRec.agebyrel)

exp(fixef(sexRec.agebyrel))

newdata <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 4),
                            rel = c(0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5),
                            egoSex = rep(c(0, 1), each = 6)))
newdata
newdata$probs <- fitted(sexRec.agebyrel, newdata = newdata, type = "response", re_formula = NA)[, "Estimate"]
newdata

# sex by relatedness: No interaction
sexRec.sexbyrel <- brm(recSex ~ egoAge + egoSex * rel + (1|id), data = recip, family = "bernoulli",
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 120120)
summary(sexRec.sexbyrel)

exp(fixef(sexRec.sexbyrel))



### iv) Factors predicting nominating someone of the same sex

# Find the optimal random effects structure in a null model where 'sex difference' is the outcome

# Full model (with all random effects terms) - Some convergence warnings, but not too bad fit
sexDiff.ml <- brm(sexDiff ~ 1 + (1 | id) + (1 | alter_id) + (1 | camp), data = recip, family = "bernoulli",
                       warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 643170)
summary(sexDiff.ml)

sexDiff.ml <- add_criterion(sexDiff.ml, "loo")

# Dropping alter ID - Better than full model, and no real issues
sexDiff.ml_noAlter <- brm(sexDiff ~ 1 + (1 | id) + (1 | camp), data = recip, family = "bernoulli",
                               warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 37367)
summary(sexDiff.ml_noAlter)

sexDiff.ml_noAlter <- add_criterion(sexDiff.ml_noAlter, "loo")

# Dropping camp - Similar to full model, but slightly fewer warnings
sexDiff.ml_noCamp <- brm(sexDiff ~ 1 + (1 | id) + (1 | alter_id), data = recip, family = "bernoulli",
                              warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 705101)
summary(sexDiff.ml_noCamp)

sexDiff.ml_noCamp <- add_criterion(sexDiff.ml_noCamp, "loo")

# Dropping camp and alter ID - Still a couple of issues, but generally much better
sexDiff.ml_noAlterCamp <- brm(sexDiff ~ 1 + (1 | id), data = recip, family = "bernoulli",
                                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 425972)
summary(sexDiff.ml_noAlterCamp)

sexDiff.ml_noAlterCamp <- add_criterion(sexDiff.ml_noAlterCamp, "loo")

# No random effects
sexDiff.noml <- brm(sexDiff ~ 1, data = recip, family = "bernoulli",
                   warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 398494)
summary(sexDiff.noml)

sexDiff.noml <- add_criterion(sexDiff.noml, "loo")


## Comparing models - Best fit is apparently the model with ego and recipient REs, although hard to know whether these are comparable, given issues above... Comparing the two models which fitted best, model fit is pretty much the same (meaning no/little benefit to inclusion of camp RE)
loo(sexDiff.ml)
loo(sexDiff.ml_noAlter)
loo(sexDiff.ml_noCamp)
loo(sexDiff.ml_noAlterCamp)
loo(sexDiff.noml)


## Now look at whether age, sex and relatedness are associated with being of same sex. No associations here.
sexDiff.full <- brm(sexDiff ~ egoAge + egoSex + rel + (1|id), family = "bernoulli", data = recip,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 720932)
summary(sexDiff.full)

# Check that inclusion of alter ID random effect not alter results - Get qualitatively similar results
sexDiff.full_alter <- brm(sexDiff ~ egoAge + egoSex + rel + (1|id) + (1|alter_id), family = "bernoulli", data = recip,
                    warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 93800)
summary(sexDiff.full_alter)

## Make a table to save these results to
TableS9 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = exp(fixef(sexDiff.full)[,1]),
                            lci = exp(fixef(sexDiff.full)[,3]),
                            uci = exp(fixef(sexDiff.full)[,4])
))
TableS9

# Convert estimates to numeric and round
TableS9 <- TableS9 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3))

TableS9

write_csv(TableS9, "../Results/tableS9.csv", quote = FALSE)


## Interaction models

# Age by sex: No interaction
sexDiff.agebysex <- brm(sexDiff ~ egoAge * egoSex + rel + (1|id), data = recip, family = "bernoulli",
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 988233)
summary(sexDiff.agebysex)

exp(fixef(sexDiff.agebysex))

# Age by relatedness: No interaction
sexDiff.agebyrel <- brm(sexDiff ~ egoAge * rel + egoSex + (1|id), data = recip, family = "bernoulli",
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 952432)
summary(sexDiff.agebyrel)

exp(fixef(sexDiff.agebyrel))

# sex by relatedness: No interaction
sexDiff.sexbyrel <- brm(sexDiff ~ egoAge + egoSex * rel + (1|id), data = recip, family = "bernoulli",
                        warmup = 1000, iter = 3000, chains = 4, init = "random", seed = 949846)
summary(sexDiff.sexbyrel)

exp(fixef(sexDiff.sexbyrel))

