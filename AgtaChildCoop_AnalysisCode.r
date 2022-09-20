## Analysis code for Daniel Major-Smith et al. "Cooperation and Partner Choice Among Agta Hunter-Gatherer Children: An Evolutionary Developmental Perspective"

## Author: Daniel Major-Smith
## R version 4.0.4


## Make sure workspace is clear
rm(list = ls())

# Install and load packages
#install.packages("tidyverse")
library(tidyverse)

#install.packages("lme4")
library(lme4)

#install.packages("lmerTest")
library(lmerTest)

#install.packages("ordinal")
library(ordinal)

#install.packages("gridExtra")
library(gridExtra)

#install.packages("dagitty")
library(dagitty)


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
  summarise(n = length(age), mean = round(mean(age), 1), sd = round(sd(age), 1), 
            min = round(min(age), 1), max = round(max(age), 1))

tableS1


# Also want to add a 'total' column to the bottom
total <- tribble(
  ~camp, ~n, ~mean, ~sd, ~min, ~max,
  "Total", length(data$age), round(mean(data$age), 1), round(sd(data$age), 1), 
  round(min(data$age), 1), round(max(data$age), 1)
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
summary(data$adult)
hist(data$adult)

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



### Testing for camp-level differences in cooperation, including camp as a random effect to account for variation at this level (using 'ranova' function from lmerTest to run a likelihood ratio test)
null.ml <- lmer(perGiven ~ (1|camp), REML=FALSE, data = data)
ranova(null.ml)

## Does addition of household RE improve model fit over null model? Marginally, but not really
null.ml_hh <- lmer(perGiven ~ (1|household), REML=FALSE, data = data)
ranova(null.ml_hh)

## And adding a 'household' RE to the null ML model with just camp as an RE does not seem to improve model fit at all, and causes the model to have singularity/convergence issues. So will just use camp-only REs
null.ml2 <- lmer(perGiven ~ (1|household) + (1|camp), REML=FALSE, data = data)
ranova(null.ml2)

data <- data %>%
  select(-household)


# The MLM is a much better fit to the data than the basic linear model - Camp level differences explain
# ~24% of the variance in child offers
summary(null.ml)
as.data.frame(lme4::VarCorr(null.ml))$vcov[1] / 
    (as.data.frame(lme4::VarCorr(null.ml))$vcov[1] + as.data.frame(lme4::VarCorr(null.ml))$vcov[2])

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
## Now run the cooperation model

# First, create an age squared variable and make sure age association is not quadratic (i.e., inclusion
# of age-squared term does not improve model fit relative to linear age only model) - Age squared not
# improve model fit, so not include in final models
data <- data %>%
  mutate(age2 = age^2)

age2.give <- lmer(perGiven ~ age + age2 + (1|camp), REML=FALSE, data = data)
summary(age2.give)

# Ideally would also explore whether random-slopes by age/camp also improves model fit, but are issues with model fit/convergence so cannot explore in depth (this could be due to small sample sizes, or due to little/no variation in the random slopes; if the former, then perhaps the model is mispecified and age slopes do differ by camp, but if the latter then the random-intercept only model may be appropriate). Will proceed just using random-intercepts with age as a linear predictor.
age.slopes <- lmer(perGiven ~ age + (age|camp), REML = FALSE, data = data)
summary(age.slopes)

data <- data %>%
  select(-age2)


# Next, run a separate model for each predictor
age.give <- lmer(perGiven ~ age + (1|camp), REML=FALSE, data = data)
summary(age.give)

sex.give <- lmer(perGiven ~ sex + (1|camp), REML=FALSE, data = data)
summary(sex.give)

rel.give <- lmer(perGiven ~ rel + (1|camp), REML=FALSE, data = data)
summary(rel.give)

adult.give <- lmer(perGiven ~ adult + (1|camp), REML=FALSE, data = data)
summary(adult.give)

# Now run a combined model which includes all predictors (based on our DAG, as we assume that these variables are relatively independent, the results of full model should be similar to individual models - and they are)
full.give <- lmer(perGiven ~ age + sex + rel + adult + (1|camp), REML=FALSE, data = data)
summary(full.give)

# Save these results in a table and export to CSV
table2 <- as.data.frame(cbind(c(rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)),
                         c(rep(c("Univariable", "Multivariable"), each = 4)),
                         c(rep(c("Individual", "Individual", "Individual", "Camp"), 2)),
                         c(coef(summary(age.give))[2,1], coef(summary(sex.give))[2,1],
                           coef(summary(rel.give))[2,1], coef(summary(adult.give))[2,1],
                           coef(summary(full.give))[2:5,1]),
                         c(coef(summary(age.give))[2,2], coef(summary(sex.give))[2,2],
                           coef(summary(rel.give))[2,2], coef(summary(adult.give))[2,2],
                           coef(summary(full.give))[2:5,2]),
                         c(confint(age.give)[4,1], confint(sex.give)[4,1], confint(rel.give)[4,1], 
                           confint(adult.give)[4,1], confint(full.give)[4:7,1]), 
                         c(confint(age.give)[4,2], confint(sex.give)[4,2], confint(rel.give)[4,2], 
                           confint(adult.give)[4,2], confint(full.give)[4:7,2]),
                         c(coef(summary(age.give))[2,5], coef(summary(sex.give))[2,5],
                           coef(summary(rel.give))[2,5], coef(summary(adult.give))[2,5],
                           coef(summary(full.give))[2:5,5])))
colnames(table2) <- c("Variable", "Model", "Level", "Coefficient", "SE", "LCI", "UCI", "p")

# Convert estimates to numeric and round
table2 <- table2 %>%
  mutate(Coefficient = as.numeric(Coefficient)) %>%
  mutate(Coefficient = round(Coefficient, 3)) %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(SE = round(SE, 3)) %>%
  mutate(LCI = as.numeric(LCI)) %>%
  mutate(LCI = round(LCI, 3)) %>%
  mutate(UCI = as.numeric(UCI)) %>%
  mutate(UCI = round(UCI, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

table2

write_csv(table2, "../Results/table2.csv", quote = FALSE)



## Check some model assumptions (of normality) - Residuals from the full model look relatively normal - And save these images
pdf("../Results/FigS3_Residuals_FullCoopModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(full.give), main = NULL, xlab = "Residuals")
text(-2.75, 27.5, "A", cex = 2.5)

qqnorm(residuals(full.give), main = NULL)
qqline(residuals(full.give))
text(-2.5, 3, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's a bit odd, but I don't think it's '*too* bad...(mainly looks weird because the range of values is only between 0 and 5)
pdf("../Results/FigS4_Heteroskedasticity_FullCoopModel.pdf", width = 8, height = 6)

plot(residuals(full.give) ~ fitted.values(full.give), xlab = "Fitted values", ylab = "Residuals")

dev.off()


### Want to plot the association between child and adult camp-level cooperation
campData <- data %>%
  add_count(camp) %>%
  rename(size = n) %>%
  select(camp, perGiven, adult, size) %>%
  group_by(camp) %>%
  summarise(childCoop = mean(perGiven), adultCoop = mean(adult), size = mean(size))

head(campData)

set.seed(1234)
(fig2 <- ggplot(data = NULL) +
    geom_smooth(data = campData, aes(x = adultCoop, y = childCoop),
                method = "lm", colour = "black", fill = "gray80") +
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
data$fit <- predict(full.give)
summary(data$fit)

(age_fit <- ggplot(data = data,
                     aes(x = age, y = fit)) +
    geom_smooth(method = "loess", colour = "black") +
    geom_point() +
    ylab("Predicted amount shared by children") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 18),
          axis.text.y = element_text(size = 14)) +
    scale_y_continuous(breaks = seq(0, 3, 1), limits = c(0, 3.1)) +
    scale_x_continuous(breaks = seq(3, 18, 3)))



### Suggested by a reviewer to re-run analyses removing high/low ages (3, or 15 to 18), as well as small camps (with <5 children in them). Personally disagree with this, as generally bad form to throw away data, but will run analyses and report in response to reviewers (but not final paper) that results do not materially change.
data_rerun <- data %>%
  filter(camp != "P1" & camp != "M3") %>%
  filter(age >= 4 & age < 15)

summary(data_rerun)

# Run a separate model for each predictor
age.give_rerun <- lmer(perGiven ~ age + (1|camp), REML=FALSE, data = data_rerun)
summary(age.give_rerun)

sex.give_rerun <- lmer(perGiven ~ sex + (1|camp), REML=FALSE, data = data_rerun)
summary(sex.give_rerun)

rel.give_rerun <- lmer(perGiven ~ rel + (1|camp), REML=FALSE, data = data_rerun)
summary(rel.give_rerun)

adult.give_rerun <- lmer(perGiven ~ adult + (1|camp), REML=FALSE, data = data_rerun)
summary(adult.give_rerun)

# Now run a combined model which includes all predictors (based on our DAG, as we assume that these variables are relatively independent, the results of full model should be similar to individual models - and they are)
full.give_rerun <- lmer(perGiven ~ age + sex + rel + adult + (1|camp), REML=FALSE, data = data_rerun)
summary(full.give_rerun)

# Save these results in a table and export to CSV
table2_rerun <- as.data.frame(cbind(c(rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)),
                              c(rep(c("Univariable", "Multivariable"), each = 4)),
                              c(rep(c("Individual", "Individual", "Individual", "Camp"), 2)),
                              c(coef(summary(age.give_rerun))[2,1], coef(summary(sex.give_rerun))[2,1],
                                coef(summary(rel.give_rerun))[2,1], coef(summary(adult.give_rerun))[2,1],
                                coef(summary(full.give_rerun))[2:5,1]),
                              c(coef(summary(age.give_rerun))[2,2], coef(summary(sex.give_rerun))[2,2],
                                coef(summary(rel.give_rerun))[2,2], coef(summary(adult.give_rerun))[2,2],
                                coef(summary(full.give_rerun))[2:5,2]),
                              c(confint(age.give_rerun)[4,1], confint(sex.give_rerun)[4,1], 
                                confint(rel.give_rerun)[4,1], confint(adult.give_rerun)[4,1], 
                                confint(full.give_rerun)[4:7,1]), 
                              c(confint(age.give_rerun)[4,2], confint(sex.give_rerun)[4,2], 
                                confint(rel.give_rerun)[4,2], confint(adult.give_rerun)[4,2], 
                                confint(full.give_rerun)[4:7,2]),
                              c(coef(summary(age.give_rerun))[2,5], coef(summary(sex.give_rerun))[2,5],
                                coef(summary(rel.give_rerun))[2,5], coef(summary(adult.give_rerun))[2,5],
                                coef(summary(full.give_rerun))[2:5,5])))
colnames(table2_rerun) <- c("Variable", "Model", "Level", "Coefficient", "SE", "LCI", "UCI", "p")

# Convert estimates to numeric and round
table2_rerun <- table2_rerun %>%
  mutate(Coefficient = as.numeric(Coefficient)) %>%
  mutate(Coefficient = round(Coefficient, 3)) %>%
  mutate(SE = as.numeric(SE)) %>%
  mutate(SE = round(SE, 3)) %>%
  mutate(LCI = as.numeric(LCI)) %>%
  mutate(LCI = round(LCI, 3)) %>%
  mutate(UCI = as.numeric(UCI)) %>%
  mutate(UCI = round(UCI, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

table2_rerun

write_csv(table2_rerun, "../Results/table2_rerun.csv", quote = FALSE)


## As a sensitivity analysis, will run the same models, but using poisson and ordinal regression/cumulative link models, as the number of gifts given (0 to 5) isn't really continuous - CLMs predict the odds (or log-odds) of being in a higher outcome category (amount shared), for a one-unit increase in the predictor variable. This is quite similar to poisson regression, but as poisson regressions require count data there's no (theoretical) upper value. But in our example there is an upper value (5) in which case an ordinal model may also be appropriate. Also, the distribution of our data is skewed, as there is an over-abundance of '0's for a poisson distribution. However, if all models give qualitatively similar results, this bolsters our confidence in our conclusions, even if some model assumptions are violated.

## Start with poisson distribution, because it (potentially) meets model assumptions better, which is used for count data (which is what we have here)

# The mean and variances are roughly equivalent for  number of gifts, meeting one of the assumptions of a  poisson model - Although there is an excess of '0's, which does violate poisson assumptions.
(mean(data$perGiven)); (var(data$perGiven))
barplot(table(data$perGiven))


# Fit the individual and full models again
age.give.p <- glmer(perGiven ~ age + (1|camp), family = "poisson", data = data)
summary(age.give.p)

sex.give.p <- glmer(perGiven ~ sex + (1|camp), family = "poisson", data = data)
summary(sex.give.p)

rel.give.p <- glmer(perGiven ~ rel + (1|camp), family = "poisson", data = data)
summary(rel.give.p)

adult.give.p <- glmer(perGiven ~ adult + (1|camp), family = "poisson", data = data)
summary(adult.give.p)

# Now run a combined model which includes all predictors
full.give.p <- glmer(perGiven ~ age + sex + rel + adult + (1|camp), family = "poisson", data = data)
summary(full.give.p)


### Now for the ordinal regression model
# Construct ordinal variable of number of resources given to others - Will also combine 4 and 5 gifts together, as low number of children gave 5
data$perGiven_ord <- data$perGiven
data$perGiven_ord[data$perGiven_ord == 5] <- 4
data$perGiven_ord <- factor(data$perGiven_ord, ordered = T)
table(data$perGiven_ord)

# Fit the individual and full models again
age.give.o <- clmm(perGiven_ord ~ age + (1|camp), data = data)
summary(age.give.o)

sex.give.o <- clmm(perGiven_ord ~ sex + (1|camp), data = data)
summary(sex.give.o)

rel.give.o <- clmm(perGiven_ord ~ rel + (1|camp), data = data)
summary(rel.give.o)

adult.give.o <- clmm(perGiven_ord ~ adult + (1|camp), data = data)
summary(adult.give.o)

# Now run a combined model which includes all predictors
full.give.o <- clmm(perGiven_ord ~ age + sex + rel + adult + (1|camp), data = data)
summary(full.give.o)


## Make a table to save all these results to
model <- rep(c("Separate", "Full"), each = 4)
var <- rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)

tableS2 <- as.data.frame(cbind(model, var))
tableS2

# I'm sure there's a better way of combining results, but at least it works, and once this is set up I wont have to repeat it again if I re-run the models.
tableS2$linear_coef[tableS2$model == "Separate" & var == "Age"] <- round(coef(summary(age.give))[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Age"] <- round(coef(summary(age.give))[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Age"] <- round(confint(age.give)[4, 1], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Age"] <- round(confint(age.give)[4, 2], 3)
tableS2$linear_p[tableS2$model == "Separate" & var == "Age"] <- round(coef(summary(age.give))[2, 5], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give))[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give))[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Sex"] <- round(confint(sex.give)[4, 1], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Sex"] <- round(confint(sex.give)[4, 2], 3)
tableS2$linear_p[tableS2$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give))[2, 5], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give))[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give))[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(confint(rel.give)[4, 1], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(confint(rel.give)[4, 2], 3)
tableS2$linear_p[tableS2$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give))[2, 5], 3)

tableS2$linear_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give))[2, 1], 3)
tableS2$linear_se[tableS2$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give))[2, 2], 3)
tableS2$linear_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(confint(adult.give)[4, 1], 3)
tableS2$linear_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(confint(adult.give)[4, 2], 3)
tableS2$linear_p[tableS2$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give))[2, 5], 3)

tableS2$linear_coef[tableS2$model == "Full"] <- round(coef(summary(full.give))[2:5, 1], 3)
tableS2$linear_se[tableS2$model == "Full"] <- round(coef(summary(full.give))[2:5, 2], 3)
tableS2$linear_lci[tableS2$model == "Full"] <- round(confint(full.give)[4:7, 1], 3)
tableS2$linear_uci[tableS2$model == "Full"] <- round(confint(full.give)[4:7, 2], 3)
tableS2$linear_p[tableS2$model == "Full"] <- round(coef(summary(full.give))[2:5, 5], 3)

tableS2


# Poisson results
tableS2$poisson_coef[tableS2$model == "Separate" & var == "Age"] <- round(exp(coef(summary(age.give.p))[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.p)[3, 1]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.p)[3, 2]), 3)
tableS2$poisson_p[tableS2$model == "Separate" & var == "Age"] <- round(coef(summary(age.give.p))[2, 4], 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Sex"] <- round(exp(coef(summary(sex.give.p))[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.p)[3, 1]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.p)[3, 2]), 3)
tableS2$poisson_p[tableS2$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give.p))[2, 4], 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(coef(summary(rel.give.p))[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.p)[3, 1]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.p)[3, 2]), 3)
tableS2$poisson_p[tableS2$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give.p))[2, 4], 3)

tableS2$poisson_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(coef(summary(adult.give.p))[2, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.p)[3, 1]), 3)
tableS2$poisson_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.p)[3, 2]), 3)
tableS2$poisson_p[tableS2$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give.p))[2, 4], 3)

tableS2$poisson_coef[tableS2$model == "Full"] <- round(exp(coef(summary(full.give.p))[2:5, 1]), 3)
tableS2$poisson_lci[tableS2$model == "Full"] <- round(exp(confint(full.give.p)[3:6, 1]), 3)
tableS2$poisson_uci[tableS2$model == "Full"] <- round(exp(confint(full.give.p)[3:6, 2]), 3)
tableS2$poisson_p[tableS2$model == "Full"] <- round(coef(summary(full.give.p))[2:5, 4], 3)

tableS2


# Ordinal regression results
tableS2$ord_coef[tableS2$model == "Separate" & var == "Age"] <- round(exp(coef(summary(age.give.o))[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.o)[5, 1]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.o)[5, 2]), 3)
tableS2$ord_p[tableS2$model == "Separate" & var == "Age"] <- round(coef(summary(age.give.o))[5, 4], 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Sex"] <- round(exp(coef(summary(sex.give.o))[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.o)[5, 1]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.o)[5, 2]), 3)
tableS2$ord_p[tableS2$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give.o))[5, 4], 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(coef(summary(rel.give.o))[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.o)[5, 1]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.o)[5, 2]), 3)
tableS2$ord_p[tableS2$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give.o))[5, 4], 3)

tableS2$ord_coef[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(coef(summary(adult.give.o))[5, 1]), 3)
tableS2$ord_lci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.o)[5, 1]), 3)
tableS2$ord_uci[tableS2$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.o)[5, 2]), 3)
tableS2$ord_p[tableS2$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give.o))[5, 4], 3)

tableS2$ord_coef[tableS2$model == "Full"] <- round(exp(coef(summary(full.give.o))[5:8, 1]), 3)
tableS2$ord_lci[tableS2$model == "Full"] <- round(exp(confint(full.give.o)[5:8, 1]), 3)
tableS2$ord_uci[tableS2$model == "Full"] <- round(exp(confint(full.give.o)[5:8, 2]), 3)
tableS2$ord_p[tableS2$model == "Full"] <- round(coef(summary(full.give.o))[5:8, 4], 3)

tableS2

# Save this table
write_csv(tableS2, file = "../Results/tableS2.csv", quote = FALSE)



### Reviewer also suggested running a logistic model with each keep vs share decision as the outcome. Have included these results in the response to reviewer document, but not in the main text (as the results are comparable, and there are already enough sensitivity analyses in the main text)

# Replicate data so is repeated 5 times
data_binary <- data %>%
  bind_rows(replicate(4, data, simplify = FALSE)) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(trial = row_number()) %>%
  ungroup(id)

# Create a new variable saying whether a resource was shared or not, based on the number in 'perGiven' (as we're not interested in the order in which gifts were shared, this should not matter)
data_binary <- data_binary %>%
  mutate(shared = ifelse(trial <= perGiven, 1, 0))

summary(data_binary)
head(data_binary, n = 20L)


## Now run a multi-level logistic model with both id and camp and random-effects

# Fit the individual and full models again
age.give.log <- glmer(shared ~ age + (1|id) + (1|camp), family = "binomial", data = data_binary)
summary(age.give.log)

sex.give.log <- glmer(shared ~ sex + (1|id) + (1|camp), family = "binomial", data = data_binary)
summary(sex.give.log)

rel.give.log <- glmer(shared ~ rel + (1|id) + (1|camp), family = "binomial", data = data_binary)
summary(rel.give.log)

adult.give.log <- glmer(shared ~ adult + (1|id) + (1|camp), family = "binomial", data = data_binary)
summary(adult.give.log)

# Now run a combined model which includes all predictors (some convergence warnings, but results look sensible and comparable to linear, ordinal and poisson results)
full.give.log <- glmer(shared ~ age + sex + rel + adult + (1|id) + (1|camp), family = "binomial", data = data_binary)
summary(full.give.log)


## Make a table to save all these results to
model <- rep(c("Separate", "Full"), each = 4)
var <- rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)

tableS2_log <- as.data.frame(cbind(model, var))
tableS2_log

# Combine results together
tableS2_log$log_coef[tableS2_log$model == "Separate" & var == "Age"] <- round(exp(coef(summary(age.give.log))[2, 1]), 3)
tableS2_log$log_lci[tableS2_log$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.log)[4, 1]), 3)
tableS2_log$log_uci[tableS2_log$model == "Separate" & var == "Age"] <- round(exp(confint(age.give.log)[4, 2]), 3)
tableS2_log$log_p[tableS2_log$model == "Separate" & var == "Age"] <- round(coef(summary(age.give.log))[2, 4], 3)

tableS2_log$log_coef[tableS2_log$model == "Separate" & var == "Sex"] <- round(exp(coef(summary(sex.give.log))[2, 1]), 3)
tableS2_log$log_lci[tableS2_log$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.log)[4, 1]), 3)
tableS2_log$log_uci[tableS2_log$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.give.log)[4, 2]), 3)
tableS2_log$log_p[tableS2_log$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.give.log))[2, 4], 3)

tableS2_log$log_coef[tableS2_log$model == "Separate" & var == "Relatedness"] <- round(exp(coef(summary(rel.give.log))[2, 1]), 3)
tableS2_log$log_lci[tableS2_log$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.log)[4, 1]), 3)
tableS2_log$log_uci[tableS2_log$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.give.log)[4, 2]), 3)
tableS2_log$log_p[tableS2_log$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.give.log))[2, 4], 3)

tableS2_log$log_coef[tableS2_log$model == "Separate" & var == "Adult coop"] <- round(exp(coef(summary(adult.give.log))[2, 1]), 3)
tableS2_log$log_lci[tableS2_log$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.log)[4, 1]), 3)
tableS2_log$log_uci[tableS2_log$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.give.log)[4, 2]), 3)
tableS2_log$log_p[tableS2_log$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.give.log))[2, 4], 3)

tableS2_log$log_coef[tableS2_log$model == "Full"] <- round(exp(coef(summary(full.give.log))[2:5, 1]), 3)
tableS2_log$log_lci[tableS2_log$model == "Full"] <- round(exp(confint(full.give.log)[4:7, 1]), 3)
tableS2_log$log_uci[tableS2_log$model == "Full"] <- round(exp(confint(full.give.log)[4:7, 2]), 3)
tableS2_log$log_p[tableS2_log$model == "Full"] <- round(coef(summary(full.give.log))[2:5, 4], 3)

tableS2_log

# Save this table
write_csv(tableS2_log, file = "../Results/tableS2_log.csv", quote = FALSE)



### Interaction test for adult coop by age (as can't use random-slopes/random-effects) - E.g., to see whether there's an association between age and cooperation in camps where adults were more cooperative, but not if adults were less cooperative.

# Linear model
full.give_int <- lmer(perGiven ~ age + sex + rel + adult + age:adult + (1|camp), REML=FALSE, data = data)
summary(full.give_int)

# Poisson model
full.give.p_int <- glmer(perGiven ~ age + sex + rel + adult + age:adult + (1|camp), family = "poisson", data = data)
summary(full.give.p_int)

# Ordinal model
full.give.o_int <- clmm(perGiven_ord ~ age + sex + rel + adult + age:adult + (1|camp), data = data)
summary(full.give.o_int)


## Some convergence issues for the poisson and ordinal models, so will convert age and adult cooperation to z-scores (which seems to resolve these problems)
data <- data %>%
  mutate(age_z = (age - mean(age)) / sd(age))

(data_camp <- data %>%
    group_by(camp) %>%
    summarise(mean_adult = mean(adult)))

data <- data %>%
  mutate(adult_z = (adult - mean(data_camp$mean_adult)) / sd(data_camp$mean_adult))

summary(data)

# Linear model
full.give_int_z <- lmer(perGiven ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), REML=FALSE, data = data)
summary(full.give_int_z)
confint(full.give_int_z)

# Poisson model
full.give.p_int_z <- glmer(perGiven ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), family = "poisson", data = data)
summary(full.give.p_int_z)
exp(coef(summary(full.give.p_int_z)))
exp(confint(full.give.p_int_z))

# Ordinal model
full.give.o_int_z <- clmm(perGiven_ord ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), data = data)
summary(full.give.o_int_z)
exp(coef(summary(full.give.o_int_z)))
exp(confint(full.give.o_int_z))


## Formal likelihood ratio tests of inclusion of interaction term
anova(full.give, full.give_int_z)
anova(full.give.p, full.give.p_int_z)
anova(full.give.o, full.give.o_int_z)


### Further suggestion by reviewer for taking variation in adult levels of cooperation into consideration.

## Our approach is as follows:
  # 1.	Sample from a normal distribution using the mean and standard error of adult cooperation from each camp, and use this as the average adult level of cooperation for said camp.
  # 2.	Run the models (both univariable and multivariable), and store the parameter estimates.
  # 3.	Iterate this process 1,000 times, sampling different adult levels of cooperation for each camp from the prior distribution each time.
  # 4.	Present the median and 95% credible intervals of these results.


## Set seed and embed script in a loop 1,000 times, run univariable and multivariable models for adult cooperation, and store estimates in a table.
set.seed(5678)
iter <- 1000

# Data frame to collect parameters of interest
res <- as.data.frame(array(dim = c(iter, 9)))
colnames(res) <- c("iteration", "uni_adult_coef", "uni_adult_se", "uni_adult_lci", "uni_adult_uci",
                   "multi_adult_coef", "multi_adult_se", "multi_adult_lci", "multi_adult_uci")
#head(res)

# Loop over each iteration (takes approx half an hour on a standard laptop for 1,000 iterations)
for (i in 1:iter) {
  
  # Print interation
  print(paste("Processing iteration", i))
  
  # Create the dataset and add adult cooperation value from prior distribution
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

  # Code any values < 0 as 0, and > 100 as 100 (as these values are impossible)
  data_temp$adult_samp[data_temp$adult_samp < 0] <- 0
  data_temp$adult_samp[data_temp$adult_samp > 100] <- 100
  
  # Run the univariable and multivariable models and store estimates
  adult.give_samp <- lmer(perGiven ~ adult_samp + (1|camp), REML=FALSE, data = data_temp)
  #summary(adult.give_samp)
  
  full.give_samp <- lmer(perGiven ~ age + sex + rel + adult_samp + (1|camp), REML=FALSE, data = data_temp)
  #summary(full.give_samp)
  
  # Store these results
  res[i, "iteration"] <- i
  res[i, "uni_adult_coef"] <- coef(summary(adult.give_samp))[2,1]
  res[i, "uni_adult_se"] <- coef(summary(adult.give_samp))[2,2]
  res[i, "uni_adult_lci"] <- confint(adult.give_samp)[4,1]
  res[i, "uni_adult_uci"] <- confint(adult.give_samp)[4,2] 
  res[i, "multi_adult_coef"] <- coef(summary(full.give_samp))[5,1]
  res[i, "multi_adult_se"] <- coef(summary(full.give_samp))[5,2]
  res[i, "multi_adult_lci"] <- confint(full.give_samp)[7,1] 
  res[i, "multi_adult_uci"] <- confint(full.give_samp)[7,2] 
  
}

res

# Save these results
write_csv(res, file = "../Results/amountShared_adultVariation.csv", quote = FALSE)


## Analyse iterations by taking median and 95% credible intervals as measures of effect (and make a nice histogram of these iterations)

## Univariable analysis

# Coefficient results
median(res$uni_adult_coef)
quantile(res$uni_adult_coef, c(0.025, 0.5, 0.975))

# Lower CI results
median(res$uni_adult_lci)
quantile(res$uni_adult_lci, c(0.025, 0.5, 0.975))

# Upper CI results
median(res$uni_adult_uci)
quantile(res$uni_adult_uci, c(0.025, 0.5, 0.975))


# Plot main coefficient results
med <- quantile(res$uni_adult_coef, 0.5)
lci <- quantile(res$uni_adult_coef, 0.025)
uci <- quantile(res$uni_adult_coef, 0.975)

hist(res$uni_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.04), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/amountShared_univarAdultCoop.pdf", height = 6, width = 8)
hist(res$uni_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.04), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


# Plot lower CI results
med <- quantile(res$uni_adult_lci, 0.5)
lci <- quantile(res$uni_adult_lci, 0.025)
uci <- quantile(res$uni_adult_lci, 0.975)

hist(res$uni_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.00, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/amountShared_univarAdultCoop_lci.pdf", height = 6, width = 8)
hist(res$uni_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.00, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


## Multivariable analysis

# Coefficient results
median(res$multi_adult_coef)
quantile(res$multi_adult_coef, c(0.025, 0.5, 0.975))

# Lower CI results
median(res$multi_adult_lci)
quantile(res$multi_adult_lci, c(0.025, 0.5, 0.975))

# Upper CI results
median(res$multi_adult_uci)
quantile(res$multi_adult_uci, c(0.025, 0.5, 0.975))


# Plot main coefficient results
med <- quantile(res$multi_adult_coef, 0.5)
lci <- quantile(res$multi_adult_coef, 0.025)
uci <- quantile(res$multi_adult_coef, 0.975)

hist(res$multi_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.018, 0.04), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/amountShared_multivarAdultCoop.pdf", height = 6, width = 8)
hist(res$multi_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.018, 0.04), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


# Plot lower CI results
med <- quantile(res$multi_adult_lci, 0.5)
lci <- quantile(res$multi_adult_lci, 0.025)
uci <- quantile(res$multi_adult_lci, 0.975)

hist(res$multi_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(-0.002, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/amountShared_multivarAdultCoop_lci.pdf", height = 6, width = 8)
hist(res$multi_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(-0.002, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()





############################################################################################
## Univariable models to explore whether parental cooperation predicts child-level cooperation. Just run simple univariate ML-models for this.

## First just mums data (n=155 - so lost 24 obs)
data_mum <- data %>%
  filter(!is.na(mother))

# Only very weak evidence that mums amount shared is associated with child levels of cooperation
mum_model <- lmer(perGiven ~ mother + (1|camp), REML=FALSE, data = data_mum)

summary(mum_model)
confint(mum_model)

# Check consistency of results if use poisson and ordinal models
mum_model.p <- glmer(perGiven ~ mother + (1|camp), family = "poisson", data = data_mum)
summary(mum_model.p)

mum_model.o <- clmm(perGiven_ord ~ mother + (1|camp), data = data_mum)
summary(mum_model.o)


## Now for just dads data (n=145 - so lost 34 obs)
data_dad <- data %>%
  filter(!is.na(father))

# No evidence that dads amount shared is associated with child levels of cooperation
dad_model <- lmer(perGiven ~ father + (1|camp), REML=FALSE, data = data_dad)

summary(dad_model)
confint(dad_model)

# Check consistency of results if use poisson and ordinal models
dad_model.p <- glmer(perGiven ~ father + (1|camp), family = "poisson", data = data_dad)
summary(dad_model.p)

dad_model.o <- clmm(perGiven_ord ~ father + (1|camp), data = data_dad)
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
parents_model <- lmer(perGiven ~ parents + (1|camp), REML=FALSE, data = data_mumdad)

summary(parents_model)
confint(parents_model)

# Check consistency of results if use poisson and ordinal models
parents_model.p <- glmer(perGiven ~ parents + (1|camp), family = "poisson", data = data_mumdad)
summary(parents_model.p)

parents_model.o <- clmm(perGiven_ord ~ parents + (1|camp), data = data_mumdad)
summary(parents_model.o)


## Make a table to save these results to
TableS3 <- data.frame(cbind(variable = c("Mother", "Father", "Parents"),
                            linear_coef = c(coef(summary(mum_model))[2,1], coef(summary(dad_model))[2,1],
                                            coef(summary(parents_model))[2,1]),
                            linear_se = c(coef(summary(mum_model))[2,2], coef(summary(dad_model))[2,2],
                                          coef(summary(parents_model))[2,2]),
                            linear_lci = c(confint(mum_model)[4,1], confint(dad_model)[4,1], 
                                           confint(parents_model)[4,1]),
                            linear_uci = c(confint(mum_model)[4,2], confint(dad_model)[4,2], 
                                           confint(parents_model)[4,2]),
                            linear_p = c(coef(summary(mum_model))[2,5], coef(summary(dad_model))[2,5],
                                            coef(summary(parents_model))[2,5]),
                            pois_coef = c(exp(coef(summary(mum_model.p))[2,1]), exp(coef(summary(dad_model.p))[2,1]),
                                       exp(coef(summary(parents_model.p))[2,1])),
                            pois_lci = c(exp(confint(mum_model.p)[3,1]), exp(confint(dad_model.p)[3,1]), 
                                         exp(confint(parents_model.p)[3,1])),
                            pois_uci = c(exp(confint(mum_model.p)[3,2]), exp(confint(dad_model.p)[3,2]), 
                                         exp(confint(parents_model.p)[3,2])),
                            pois_p = c(coef(summary(mum_model.p))[2,4], coef(summary(dad_model.p))[2,4],
                                          coef(summary(parents_model.p))[2,4]),
                            ord_coef = c(exp(coef(summary(mum_model.o))[5,1]), exp(coef(summary(dad_model.o))[5,1]), 
                                         exp(coef(summary(parents_model.o))[5,1])),
                            ord_lci = c(exp(confint(mum_model.o)[5,1]), exp(confint(dad_model.o)[5,1]), 
                                        exp(confint(parents_model.o)[5,1])),
                            ord_uci = c(exp(confint(mum_model.o)[5,2]), exp(confint(dad_model.o)[5,2]), 
                                        exp(confint(parents_model.o)[5,2])),
                            ord_p = c(coef(summary(mum_model.o))[5,4], coef(summary(dad_model.o))[5,4], 
                                         coef(summary(parents_model.o))[5,4])
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
  mutate(linear_p = as.numeric(linear_p)) %>%
  mutate(linear_p = round(linear_p, 3)) %>%
  mutate(pois_coef = as.numeric(pois_coef)) %>%
  mutate(pois_coef = round(pois_coef, 3)) %>%
  mutate(pois_lci = as.numeric(pois_lci)) %>%
  mutate(pois_lci = round(pois_lci, 3)) %>%
  mutate(pois_uci = as.numeric(pois_uci)) %>%
  mutate(pois_uci = round(pois_uci, 3)) %>%
  mutate(pois_p = as.numeric(pois_p)) %>%
  mutate(pois_p = round(pois_p, 3)) %>%
  mutate(ord_coef = as.numeric(ord_coef)) %>%
  mutate(ord_coef = round(ord_coef, 3)) %>%
  mutate(ord_lci = as.numeric(ord_lci)) %>%
  mutate(ord_lci = round(ord_lci, 3)) %>%
  mutate(ord_uci = as.numeric(ord_uci)) %>%
  mutate(ord_uci = round(ord_uci, 3)) %>%
  mutate(ord_p = as.numeric(ord_p)) %>%
  mutate(ord_p = round(ord_p, 3))

TableS3

write_csv(TableS3, "../Results/tableS3.csv", quote = FALSE)



########################################################################################
## Now run models of number of unique recipients

# As with overall levels of cooperation above, first create an age squared variable and make sure age association is not quadratic (i.e., inclusion of age-squared term does not improve model fit relative to linear age only model) - Age squared not improve model fit, so not include in final models
data <- data %>%
  mutate(age2 = age^2)

age2.numRec <- lmer(numRecipients ~ age + age2 + (1|camp), REML=FALSE, data = data)
summary(age2.numRec)

# Ideally would also explore whether random-slopes by age/camp also improves model fit, but are issues with model fit/convergence so cannot explore in depth (this could be due to small sample sizes, or due to little/no variation in the random slopes; if the former, then perhaps the model is mispecified and age slopes do differ by camp, but if the latter then the random-intercept only model may be appropriate). Will proceed just using random-intercepts with age as a linear predictor.
numRec.slopes <- lmer(numRecipients ~ age + (age|camp), REML = FALSE, data = data)
summary(numRec.slopes)

data <- data %>%
  select(-age2)


# Now, run a separate model for each predictor model
age.numRec <- lmer(numRecipients ~ age + (1|camp), REML=FALSE, data = data)
summary(age.numRec)

sex.numRec <- lmer(numRecipients ~ sex + (1|camp), REML=FALSE, data = data)
summary(sex.numRec)

rel.numRec <- lmer(numRecipients ~ rel + (1|camp), REML=FALSE, data = data)
summary(rel.numRec)

adult.numRec <- lmer(numRecipients ~ adult + (1|camp), REML=FALSE, data = data)
summary(adult.numRec)

# Now run a combined model which includes all predictors (as assume are relatively independent, results of full model should be similar to individual models - and they are)
full.numRec <- lmer(numRecipients ~ age + sex + rel + adult + (1|camp), REML=FALSE, data = data)
summary(full.numRec)

confint(full.numRec)


# First check some model assumptions (of normality) - Residuals from the global model look relatively normal - And save these images
pdf("../Results/FigS6_Residuals_FullNumRecipModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(full.numRec), main = NULL, xlab = "Residuals", ylim = c(0, 40))
text(-2.25, 35, "A", cex = 2.5)

qqnorm(residuals(full.numRec), main = NULL)
qqline(residuals(full.numRec))
text(-2.5, 2.5, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's a bit odd, but I don't think it's 'too' bad...(mainly looks weird because the values are only 0 to 5)
pdf("../Results/FigS7_Heteroskedasticity_FullNumRecipModel.pdf", width = 8, height = 6)

plot(residuals(full.numRec) ~ fitted.values(full.numRec), xlab = "Fitted values", ylab = "Residuals")

dev.off()



# Now plot the predicted values for age and number of respondents
data$fit_resp <- predict(full.numRec)

(ageResp_fit <- ggplot(data = data,
                    aes(x = age, y = fit_resp)) +
    geom_smooth(method = "loess", colour = "black") +
    geom_point() +
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

grid.arrange(age_fit,ageResp_fit, ncol = 1)

dev.off()


## As a further check, will also run the above analyses using poisson regression, which is used for count 
## data (which is what we have here)

# The mean and variances are roughly equivalent for number of recipients, meeting one of the assumptions of a
# poisson model (although there is an excess of '0's, which does violate model assumptions)
(mean(data$numRecipients)); (var(data$numRecipients))
barplot(table(data$numRecipients))

# Fit the individual and full models again
age.numRec.p <- glmer(numRecipients ~ age + (1|camp), family = "poisson", data = data)
summary(age.numRec.p)

sex.numRec.p <- glmer(numRecipients ~ sex + (1|camp), family = "poisson", data = data)
summary(sex.numRec.p)

rel.numRec.p <- glmer(numRecipients ~ rel + (1|camp), family = "poisson", data = data)
summary(rel.numRec.p)

adult.numRec.p <- glmer(numRecipients ~ adult + (1|camp), family = "poisson", data = data)
summary(adult.numRec.p)

# Now run a combined model which includes all predictors
full.numRec.p <- glmer(numRecipients ~ age + sex + rel + adult + (1|camp), family = "poisson", data = data)
summary(full.numRec.p)


### Now for the ordinal regression model
# Construct ordinal variable of number of unique recipients - As above, will combine 4 and 5 recipients together, as low number of children gave to 5 others
data$numRec_ord <- data$numRecipients
data$numRec_ord[data$numRec_ord == 5] <- 4
data$numRec_ord <- factor(data$numRec_ord, ordered = T)
table(data$numRec_ord)

# Fit the individual and full models again
age.numRec.o <- clmm(numRec_ord ~ age + (1|camp), data = data)
summary(age.numRec.o)

sex.numRec.o <- clmm(numRec_ord ~ sex + (1|camp), data = data)
summary(sex.numRec.o)

rel.numRec.o <- clmm(numRec_ord ~ rel + (1|camp), data = data)
summary(rel.numRec.o)

adult.numRec.o <- clmm(numRec_ord ~ adult + (1|camp), data = data)
summary(adult.numRec.o)

# Now run a combined model which includes all predictors
full.numRec.o <- clmm(numRec_ord ~ age + sex + rel + adult + (1|camp), data = data)
summary(full.numRec.o)


## Make a table to save all these results to
model <- rep(c("Separate", "Full"), each = 4)
var <- rep(c("Age", "Sex", "Relatedness", "Adult coop"), 2)

TableS4 <- as.data.frame(cbind(model, var))
TableS4

# I'm sure there's a better way of combining results, but at least once this is set up I wont have to repeat it again if I re-run the models...
TableS4$linear_coef[TableS4$model == "Separate" & var == "Age"] <- round(coef(summary(age.numRec))[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Age"] <- round(coef(summary(age.numRec))[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Age"] <- round(confint(age.numRec)[4, 1], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Age"] <- round(confint(age.numRec)[4, 2], 3)
TableS4$linear_p[TableS4$model == "Separate" & var == "Age"] <- round(coef(summary(age.numRec))[2, 5], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.numRec))[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.numRec))[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Sex"] <- round(confint(sex.numRec)[4, 1], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Sex"] <- round(confint(sex.numRec)[4, 2], 3)
TableS4$linear_p[TableS4$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.numRec))[2, 5], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.numRec))[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.numRec))[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(confint(rel.numRec)[4, 1], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(confint(rel.numRec)[4, 2], 3)
TableS4$linear_p[TableS4$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.numRec))[2, 5], 3)

TableS4$linear_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.numRec))[2, 1], 3)
TableS4$linear_se[TableS4$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.numRec))[2, 2], 3)
TableS4$linear_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(confint(adult.numRec)[4, 1], 3)
TableS4$linear_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(confint(adult.numRec)[4, 2], 3)
TableS4$linear_p[TableS4$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.numRec))[2, 5], 3)

TableS4$linear_coef[TableS4$model == "Full"] <- round(coef(summary(full.numRec))[2:5, 1], 3)
TableS4$linear_se[TableS4$model == "Full"] <- round(coef(summary(full.numRec))[2:5, 2], 3)
TableS4$linear_lci[TableS4$model == "Full"] <- round(confint(full.numRec)[4:7, 1], 3)
TableS4$linear_uci[TableS4$model == "Full"] <- round(confint(full.numRec)[4:7, 2], 3)
TableS4$linear_p[TableS4$model == "Full"] <- round(coef(summary(full.numRec))[2:5, 5], 3)

TableS4


# Poisson results
TableS4$poisson_coef[TableS4$model == "Separate" & var == "Age"] <- round(exp(coef(summary(age.numRec.p))[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Age"] <- round(exp(confint(age.numRec.p)[3, 1]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Age"] <- round(exp(confint(age.numRec.p)[3, 2]), 3)
TableS4$poisson_p[TableS4$model == "Separate" & var == "Age"] <- round(coef(summary(age.numRec.p))[2, 4], 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Sex"] <- round(exp(coef(summary(sex.numRec.p))[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.numRec.p)[3, 1]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.numRec.p)[3, 2]), 3)
TableS4$poisson_p[TableS4$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.numRec.p))[2, 4], 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(coef(summary(rel.numRec.p))[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.numRec.p)[3, 1]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.numRec.p)[3, 2]), 3)
TableS4$poisson_p[TableS4$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.numRec.p))[2, 4], 3)

TableS4$poisson_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(coef(summary(adult.numRec.p))[2, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.numRec.p)[3, 1]), 3)
TableS4$poisson_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.numRec.p)[3, 2]), 3)
TableS4$poisson_p[TableS4$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.numRec.p))[2, 4], 3)

TableS4$poisson_coef[TableS4$model == "Full"] <- round(exp(coef(summary(full.numRec.p))[2:5, 1]), 3)
TableS4$poisson_lci[TableS4$model == "Full"] <- round(exp(confint(full.numRec.p)[3:6, 1]), 3)
TableS4$poisson_uci[TableS4$model == "Full"] <- round(exp(confint(full.numRec.p)[3:6, 2]), 3)
TableS4$poisson_p[TableS4$model == "Full"] <- round(coef(summary(full.numRec.p))[2:5, 4], 3)

TableS4


# Ordinal regression results
TableS4$ord_coef[TableS4$model == "Separate" & var == "Age"] <- round(exp(coef(summary(age.numRec.o))[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Age"] <- round(exp(confint(age.numRec.o)[5, 1]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Age"] <- round(exp(confint(age.numRec.o)[5, 2]), 3)
TableS4$ord_p[TableS4$model == "Separate" & var == "Age"] <- round(coef(summary(age.numRec.o))[5, 4], 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Sex"] <- round(exp(coef(summary(sex.numRec.o))[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.numRec.o)[5, 1]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Sex"] <- round(exp(confint(sex.numRec.o)[5, 2]), 3)
TableS4$ord_p[TableS4$model == "Separate" & var == "Sex"] <- round(coef(summary(sex.numRec.o))[5, 4], 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(coef(summary(rel.numRec.o))[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.numRec.o)[5, 1]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Relatedness"] <- round(exp(confint(rel.numRec.o)[5, 2]), 3)
TableS4$ord_p[TableS4$model == "Separate" & var == "Relatedness"] <- round(coef(summary(rel.numRec.o))[5, 4], 3)

TableS4$ord_coef[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(coef(summary(adult.numRec.o))[5, 1]), 3)
TableS4$ord_lci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.numRec.o)[5, 1]), 3)
TableS4$ord_uci[TableS4$model == "Separate" & var == "Adult coop"] <- round(exp(confint(adult.numRec.o)[5, 2]), 3)
TableS4$ord_p[TableS4$model == "Separate" & var == "Adult coop"] <- round(coef(summary(adult.numRec.o))[5, 4], 3)

TableS4$ord_coef[TableS4$model == "Full"] <- round(exp(coef(summary(full.numRec.o))[5:8, 1]), 3)
TableS4$ord_lci[TableS4$model == "Full"] <- round(exp(confint(full.numRec.o)[5:8, 1]), 3)
TableS4$ord_uci[TableS4$model == "Full"] <- round(exp(confint(full.numRec.o)[5:8, 2]), 3)
TableS4$ord_p[TableS4$model == "Full"] <- round(coef(summary(full.numRec.o))[5:8, 4], 3)

TableS4

# Save this table
write_csv(TableS4, file = "../Results/tableS4.csv", quote = FALSE)



### Interaction test for adult coop by age (as can't use random-slopes/random-effects) - E.g., to see whether there's an association between age and number of unique recipient in camps where adults were more cooperative, but not if adults were less cooperative.

# Linear model
full.numRec_int <- lmer(numRecipients ~ age + sex + rel + adult + age:adult + (1|camp), REML=FALSE, data = data)
summary(full.numRec_int)

# Poisson model
full.numRec.p_int <- glmer(numRecipients ~ age + sex + rel + adult + age:adult + (1|camp), family = "poisson", data = data)
summary(full.numRec.p_int)

# Ordinal model
full.numRec.o_int <- clmm(numRec_ord ~ age + sex + rel + adult + age:adult + (1|camp), data = data)
summary(full.numRec.o_int)


## Some convergence issues for the poisson and ordinal models, so will use the age and adult cooperation z-scores (which seems to resolve these problems)

# Linear model
full.numRec_int_z <- lmer(numRecipients ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), REML=FALSE, data = data)
summary(full.numRec_int_z)
confint(full.numRec_int_z)

# Poisson model
full.numRec.p_int_z <- glmer(numRecipients ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), family = "poisson", data = data)
summary(full.numRec.p_int_z)
exp(coef(summary(full.numRec.p_int_z)))
exp(confint(full.numRec.p_int_z))

# Ordinal model
full.numRec.o_int_z <- clmm(numRec_ord ~ age_z + sex + rel + adult_z + age_z:adult_z + (1|camp), data = data)
summary(full.numRec.o_int_z)
exp(coef(summary(full.numRec.o_int_z)))
exp(confint(full.numRec.o_int_z))

## Formal likelihood ratio tests of inclusion of interaction term
anova(full.numRec, full.numRec_int_z)
anova(full.numRec.p, full.numRec.p_int_z)
anova(full.numRec.o, full.numRec.o_int_z)


### Further suggestion by reviewer for taking variation in adult levels of cooperation into consideration.

## Our approach is as follows:
# 1.	Sample from a normal distibution using the mean and standard error of adult cooperation from each camp, and use this as the average adult level of cooperation for said camp.
# 2.	Run the models (both univariable and multivariable), and store the parameter estimates.
# 3.	Iterate this process 1,000 times, sampling different adult levels of cooperation for each camp from the prior distribution each time.
# 4.	Present the median and 95% credible intervals of these results.


## Set seed and embed script in a loop 1,000 times, run univariable and multivariable models for adult cooperation, and store estimates in a table.
set.seed(45678)
iter <- 1000

# Data frame to collect parameters of interest
res_numRec <- as.data.frame(array(dim = c(iter, 9)))
colnames(res_numRec) <- c("iteration", "uni_adult_coef", "uni_adult_se", "uni_adult_lci", "uni_adult_uci",
                   "multi_adult_coef", "multi_adult_se", "multi_adult_lci", "multi_adult_uci")
#head(res_numRec)

# Loop over each iteration (takes approx half an hour on a standard laptop for 1,000 iterations)
for (i in 1:iter) {
  
  # Print interation
  print(paste("Processing iteration", i))
  
  # Create the dataset and add adult cooperation value from prior distribution
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
  
  # Code any values < 0 as 0, and > 100 as 100 (as these values are impossible)
  data_temp$adult_samp[data_temp$adult_samp < 0] <- 0
  data_temp$adult_samp[data_temp$adult_samp > 100] <- 100
  
  # Run the univariable and multivariable models and store estimates
  adult.numRec_samp <- lmer(numRecipients ~ adult_samp + (1|camp), REML=FALSE, data = data_temp)
  #summary(adult.numRec_samp)
  
  full.numRec_samp <- lmer(numRecipients ~ age + sex + rel + adult_samp + (1|camp), REML=FALSE, data = data_temp)
  #summary(full.numRec_samp)
  
  # Store these results
  res_numRec[i, "iteration"] <- i
  res_numRec[i, "uni_adult_coef"] <- coef(summary(adult.numRec_samp))[2,1]
  res_numRec[i, "uni_adult_se"] <- coef(summary(adult.numRec_samp))[2,2]
  res_numRec[i, "uni_adult_lci"] <- confint(adult.numRec_samp)[4,1]
  res_numRec[i, "uni_adult_uci"] <- confint(adult.numRec_samp)[4,2] 
  res_numRec[i, "multi_adult_coef"] <- coef(summary(full.numRec_samp))[5,1]
  res_numRec[i, "multi_adult_se"] <- coef(summary(full.numRec_samp))[5,2]
  res_numRec[i, "multi_adult_lci"] <- confint(full.numRec_samp)[7,1] 
  res_numRec[i, "multi_adult_uci"] <- confint(full.numRec_samp)[7,2] 
  
}

res_numRec

# Save these results
write_csv(res_numRec, file = "../Results/numberRecipients_adultVariation.csv", quote = FALSE)


## Analyse iterations by taking median and 95% credible intervals as measures of effect (and make a nice histogram of these iterations)

## Univariable analysis

# Coefficient results
median(res_numRec$uni_adult_coef)
quantile(res_numRec$uni_adult_coef, c(0.025, 0.5, 0.975))

# Lower CI results
median(res_numRec$uni_adult_lci)
quantile(res_numRec$uni_adult_lci, c(0.025, 0.5, 0.975))

# Upper CI results
median(res_numRec$uni_adult_uci)
quantile(res_numRec$uni_adult_uci, c(0.025, 0.5, 0.975))


# Plot main coefficient results
med <- quantile(res_numRec$uni_adult_coef, 0.5)
lci <- quantile(res_numRec$uni_adult_coef, 0.025)
uci <- quantile(res_numRec$uni_adult_coef, 0.975)

hist(res_numRec$uni_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.045), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/numberRecipients_univarAdultCoop.pdf", height = 6, width = 8)
hist(res_numRec$uni_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.045), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


# Plot lower CI results
med <- quantile(res_numRec$uni_adult_lci, 0.5)
lci <- quantile(res_numRec$uni_adult_lci, 0.025)
uci <- quantile(res_numRec$uni_adult_lci, 0.975)

hist(res_numRec$uni_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.003, 0.032), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/numberRecipients_univarAdultCoop_lci.pdf", height = 6, width = 8)
hist(res_numRec$uni_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.003, 0.032), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


## Multivariable analysis

# Coefficient results
median(res_numRec$multi_adult_coef)
quantile(res_numRec$multi_adult_coef, c(0.025, 0.5, 0.975))

# Lower CI results
median(res_numRec$multi_adult_lci)
quantile(res_numRec$multi_adult_lci, c(0.025, 0.5, 0.975))

# Upper CI results
median(res_numRec$multi_adult_uci)
quantile(res_numRec$multi_adult_uci, c(0.025, 0.5, 0.975))


# Plot main coefficient results
med <- quantile(res_numRec$multi_adult_coef, 0.5)
lci <- quantile(res_numRec$multi_adult_coef, 0.025)
uci <- quantile(res_numRec$multi_adult_coef, 0.975)

hist(res_numRec$multi_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.045), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/numberRecipients_multivarAdultCoop.pdf", height = 6, width = 8)
hist(res_numRec$multi_adult_coef, col = "lightblue", xlab = "Adult cooperation coefficient", xlim = c(0.02, 0.045), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()


# Plot lower CI results
med <- quantile(res_numRec$multi_adult_lci, 0.5)
lci <- quantile(res_numRec$multi_adult_lci, 0.025)
uci <- quantile(res_numRec$multi_adult_lci, 0.975)

hist(res_numRec$multi_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.00, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")

pdf("../Results/numberRecipients_multivarAdultCoop_lci.pdf", height = 6, width = 8)
hist(res_numRec$multi_adult_lci, col = "lightblue", xlab = "Adult cooperation lower CI", xlim = c(0.00, 0.03), 
     breaks = 20,  main = paste0("Median = ", round(med, 3), " (95% CI = ", round(lci, 3), " to ", round(uci, 3), ")"))
abline(v = med, lty = 3, lwd = 5, col = "red")
abline(v = lci, lty = 3, lwd = 3, col = "blue")
abline(v = uci, lty = 3, lwd = 3, col = "blue")
dev.off()




###############################################################################
## Kinship and camp relatedness, to see whether individuals preferentially gave
## to kin, relative to background camp-levels of relatedness

relate <- read_csv("AgtaChildCoop_KinshipAndCampRel.csv")

head(relate)
summary(relate)

# Make ID and camp factors
relate$id <- factor(relate$id)
relate$camp <- factor(relate$camp)

# Mixed effect model to see whether individuals preferentially shared with kin, relative to background levels of relatedness in camp. Need to control for ID as a random effect as some children are repeated multiple times in the dataset (if gave multiple gifts), plus potentially camp if is camp-level variation

## See if adding camp improves model fit - It doesn't
null.camp <- lmer(Rel ~ (1|id) + (1|camp), REML=FALSE, data = relate)
ranova(null.camp)

# Now run the models
camp.rel <- lmer(Rel ~ Cond + (1|id), REML=FALSE, data = relate)

summary(camp.rel)
confint(camp.rel)


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
# would fix this (this is because the raw data are bimodal - Relatedness to recipients has a major mode 
# at 0.5, and a small peak at around 0)
pdf("../Results/FigS9_Residuals_CampRelModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(camp.rel), main = NULL, xlab = "Residuals")
text(-0.3, 105, "A", cex = 2.5)

qqnorm(residuals(camp.rel), main = NULL)
qqline(residuals(camp.rel))
text(-2.5, 0.25, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It's rather odd, but I don't think it's *too* bad...(looks
# weird because the raw data is very non-normal)
pdf("../Results/FigS10_Heteroskedasticity_CampRelModel.pdf", width = 8, height = 6)

plot(residuals(camp.rel) ~ fitted.values(camp.rel), xlab = "Fitted values", ylab = "Residuals")

dev.off()



#############################################################################################
##  Exploring the factors which predict who children gave to

recip <- read_csv("AgtaChildCoop_ChildRecipients.csv")

head(recip)
summary(recip)

# Convert ID variables to factors (although I won't use 'alter_id' as a random effect below, as can cause major convergence issues and nonsense results)
recip$id <- factor(recip$id)
recip$camp <- factor(recip$camp)
recip$alter_id <- factor(recip$alter_id)


# Find the optimal random effects structure in a null model where 'relatedness' is the outcome - Individual ID random effect improves model fit, while camp-level random effect does not
rel.ml.null <- lmer(rel ~ (1|id) + (1|camp), REML = FALSE, data = recip)
ranova(rel.ml.null)

  
# Now look at whether age and sex are associated with the relatedness between giver and recipient
rel.agesex <- lmer(rel ~ egoAge + egoSex + (1|id), REML = FALSE, data = recip)
summary(rel.agesex)
confint(rel.agesex)

# Is there an interaction between age and sex? No, so will focus on main effects
rel.agebysex <- lmer(rel ~ egoAge * egoSex + (1|id), REML = FALSE, data = recip)
summary(rel.agebysex)
confint(rel.agebysex)

## Strongest effect is that of ego age - As ego age increases, more likely to give to less related
## camp-mates. Although is a weak effect of sex as well (with boys more likely to share with close kin than girls)


# Check assumptions of main effects model

# Normality - The residuals are really dodgy, and I doubt that transformations will help given the
# really weird distribution of relatedness
pdf("../Results/FigS11_Residuals_RelAgeSexModel.pdf", width = 12, height = 5)

par(mfrow = c(1,2))

hist(residuals(rel.agesex), main = NULL, xlab = "Residuals")
text(-0.2, 120, "A", cex = 2.5)

qqnorm(residuals(rel.agesex), main = NULL)
qqline(residuals(rel.agesex))
text(-2.5, 0.25, "B", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now test for heteroskedascity - It ain't too great...
pdf("../Results/FigS12_Heteroskedasticity_RelAgeSexModel.pdf", width = 8, height = 6)

plot(residuals(rel.agesex) ~ fitted.values(rel.agesex), xlab = "Fitted values", ylab = "Residuals")

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

rel.agesex.bin <- glmer(sib ~ egoAge + egoSex + (1|id), family = "binomial", data = recip)
summary(rel.agesex.bin)
exp(confint(rel.agesex.bin, method = "Wald"))

### THESE MIXED-EFFECTS LOGISTIC MODELS SHOULD NOT BE TRUSTED!!! The parameter estimates are pretty ridiculous - For instance, the log odds for sex is 3.5, meaning the odds of sharing with siblings are 33 times higher if you're a boy!!

# The predicted probabilities are also highly improbable, as a boy aged 5 had a 99% probability of sharing with siblings, while for a girl aged 15 the probability of sharing with a sibling was 1%. This suggests estimation and convergence issues with this model.
newdata <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 2),
                            egoSex = rep(c(0, 1), each = 3)))
newdata
newdata$probs <- predict(rel.agesex.bin, newdata = newdata, type = "response", re.form = NA)
newdata


# In the non-ML GLM the coefficients are much more sensible... Looking at the raw stats, there is a definite increase in boys giving to sibs, but the output from the ML model is just bananas! If I run the model without the mixed-effect then the estimates are much more sensible the OR for age is 0.87, while the OR for sex is 2.36 (but obviously this doesn't take the non-independence of data points into consideration).
table(recip$sib[recip$egoSex == 1])
table(recip$sib[recip$egoSex == 0])

rel.agesex.bin.noml <- glm(sib ~ egoAge + egoSex, family = "binomial", data = recip)
summary(rel.agesex.bin.noml)

exp(coefficients(rel.agesex.bin.noml))
exp(confint(rel.agesex.bin.noml))


## Will make a simple non-ML model using average relatedness to recipients, and see if same results hold. These results seem to be just fine, so not 100% sure why the glmer went all screwy...
head(recip)

recip2 <- recip %>%
  group_by(id) %>%
  summarise(rel = mean(rel), egoAge = mean(egoAge), egoSex = mean(egoSex))

head(recip2)

hist(recip2$rel)

rel2.model <- lm(rel ~ egoAge + egoSex, na.action = na.pass, data = recip2)
summary(rel2.model)


# Tests of assumptions
pdf("../Results/FigS13_AssumptionPlots_SingleLevelRelAgeSexModel_.pdf", width = 12, height = 5)

par(mfrow = c(1,3))

hist(residuals(rel2.model), main = NULL, xlab = "Residuals")
text(-0.35, 33, "A", cex = 2.5)

qqnorm(residuals(rel2.model), main = NULL)
qqline(residuals(rel2.model))
text(-2.2, 0.28, "B", cex = 2.5)

plot(residuals(rel2.model) ~ fitted.values(rel2.model), xlab = "Fitted values", ylab = "Residuals")
text(0.4, 0.28, "C", cex = 2.5)

dev.off()

par(mfrow = c(1, 1))


# Now run a single-level logistic model comparing giving only to sibs vs only giving to non-sibs
recip2$sib <- ifelse(recip2$rel == 0.5, 1, 0)
head(recip2)
table(recip2$sib)

rel2.bin <- glm(sib ~ egoAge + egoSex, family = "binomial", na.action = na.pass, data = recip2)
summary(rel2.bin)

exp(coef(rel2.bin))
exp(confint(rel2.bin))


## Make a table to save these results to
TableS5 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex"),
                            linear_coef = coef(summary(rel2.model))[,1],
                            linear_lci = confint(rel2.model)[,1],
                            linear_uci = confint(rel2.model)[,2],
                            linear_p = coef(summary(rel2.model))[,4],
                            logit_coef = exp(coef(summary(rel2.bin))[,1]),
                            logit_lci = exp(confint(rel2.bin)[,1]),
                            logit_uci = exp(confint(rel2.bin)[,2]),
                            logit_p = coef(summary(rel2.bin))[,4]
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
  mutate(linear_p = as.numeric(linear_p)) %>%
  mutate(linear_p = round(linear_p, 3)) %>%
  mutate(logit_coef = as.numeric(logit_coef)) %>%
  mutate(logit_coef = round(logit_coef, 3)) %>%
  mutate(logit_lci = as.numeric(logit_lci)) %>%
  mutate(logit_lci = round(logit_lci, 3)) %>%
  mutate(logit_uci = as.numeric(logit_uci)) %>%
  mutate(logit_uci = round(logit_uci, 3)) %>%
  mutate(logit_p = as.numeric(logit_p)) %>%
  mutate(logit_p = round(logit_p, 3))

TableS5

write_csv(TableS5, "../Results/tableS5.csv", quote = FALSE)


# Look at predicted probabilities, as easier to interpret than odds ratios
newdata <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 2),
                            egoSex = rep(c(0, 1), each = 3)))
newdata
newdata$probs <- predict(rel2.bin, newdata = newdata, type = "response")
newdata



################################################################################################
#### Next, explore how ego age, ego sex and relatedness (and their interactions) predict: i) recipient age; ii) the age difference between ego and recipient; iii) recipient sex; and iv) whether ego and recipient were of the same sex. 

### i) Factors predicting age of recipient

# Find the optimal random effects structure in a null model where 'recipient age' is the outcome - Ego ID improves model fit, while camp RE does not, so will just use ego ID as random effect
recAge.ml.null <- lmer(recAge ~ (1|id) + (1|camp), REML = FALSE, data = recip)
ranova(recAge.ml.null)

# Null model with ego ID as random effect
recAge.ml.null <- lmer(recAge ~ (1|id), REML = FALSE, data = recip)
summary(recAge.ml.null)


## Now look at whether age, sex and relatedness are associated with recipient age. Here, we see that ego age is positively associated with recipient age, and relatedness is strongly negatively associated with recipient age
recAge.full <- lmer(recAge ~ egoAge + egoSex + rel + (1|id), REML = FALSE, data = recip)
summary(recAge.full)

## Make a table to save these results to
TableS6 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = coef(summary(recAge.full))[,1],
                            se = coef(summary(recAge.full))[,2],
                            lci = confint(recAge.full)[3:6,1],
                            uci = confint(recAge.full)[3:6,2],
                            p = coef(summary(recAge.full))[,5]
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
  mutate(uci = round(uci, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

TableS6

write_csv(TableS6, "../Results/tableS6.csv", quote = FALSE)


## Plot some predicted results to make this clearer
recip$fitted_age <- fitted.values(recAge.full)
summary(recip$fitted_age)

(age_fit <- ggplot(data = recip,
                   aes(x = egoAge, y = fitted_age)) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point() +
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

set.seed(1234)
(rel_fit <- ggplot(data = recip,
                   aes(x = rel, y = fitted_age)) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point(position = position_jitter(width = 0.005)) +
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
recAge.agebysex <- lmer(recAge ~ egoAge * egoSex + rel + (1|id), REML = FALSE, data = recip)
summary(recAge.agebysex)

# Age by relatedness: No interaction
recAge.agebyrel <- lmer(recAge ~ egoAge * rel + egoSex + (1|id), REML = FALSE, data = recip)
summary(recAge.agebyrel)

# sex by relatedness: Weak interaction, so will ignore here
recAge.sexbyrel <- lmer(recAge ~ egoAge + egoSex * rel + (1|id), REML = FALSE, data = recip)
summary(recAge.sexbyrel)


# what about model assumptions (using the main effects model)? Don't look too bad.
hist(residuals(recAge.full), main = NULL, xlab = "Residuals")

qqnorm(residuals(recAge.full), main = NULL)
qqline(residuals(recAge.full))

plot(residuals(recAge.full) ~ fitted.values(recAge.full), xlab = "Fitted values", ylab = "Residuals")



### ii) Factors predicting age difference between ego and alter

# Find the optimal random effects structure in a null model where 'age difference' is the outcome - Both ego ID and camp REs improves model fit, so will just use both as random effects
ageDiff.ml.null <- lmer(ageDiff ~ (1|id) + (1|camp), REML = FALSE, data = recip)
ranova(ageDiff.ml.null)

summary(ageDiff.ml.null)


## Now look at whether age, sex and relatedness are associated with age difference. Here, we see that ego age is negatively associated with age difference (as ego age increased, so does the age difference), and relatedness is strongly negatively associated with age difference (meaning close kin recipients were likely to have a larger age gap/be younger than than non-kin recipients)
ageDiff.full <- lmer(ageDiff ~ egoAge + egoSex + rel + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(ageDiff.full)

## Make a table to save these results to
TableS7 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = coef(summary(ageDiff.full))[,1],
                            se = coef(summary(ageDiff.full))[,2],
                            lci = confint(ageDiff.full)[4:7,1],
                            uci = confint(ageDiff.full)[4:7,2],
                            p = coef(summary(ageDiff.full))[,5]
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
  mutate(uci = round(uci, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

TableS7

write_csv(TableS7, "../Results/tableS7.csv", quote = FALSE)



## Interaction models

# Age by sex: No interaction
ageDiff.agebysex <- lmer(ageDiff ~ egoAge * egoSex + rel + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(ageDiff.agebysex)

# Age by relatedness: No interaction
ageDiff.agebyrel <- lmer(ageDiff ~ egoAge * rel + egoSex + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(ageDiff.agebyrel)

# sex by relatedness: Weak interaction, so will ignore here
ageDiff.sexbyrel <- lmer(ageDiff ~ egoAge + egoSex * rel + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(ageDiff.sexbyrel)


# what about model assumptions (using the main effects model)? Don't look too bad.
hist(residuals(ageDiff.full), main = NULL, xlab = "Residuals")

qqnorm(residuals(ageDiff.full), main = NULL)
qqline(residuals(ageDiff.full))

plot(residuals(ageDiff.full) ~ fitted.values(ageDiff.full), xlab = "Fitted values", ylab = "Residuals")


## Put these assumption plots for alter age and age diff into one 3x2 plot
pdf("../Results/FigS16_AssumptionPlots_AlterAge_AgeDiff.pdf", width = 12, height = 8)

par(mfrow = c(2,3))

# Alter age
hist(residuals(recAge.full), main = NULL, xlab = "Residuals")
text(-6, 95, "1A", cex = 2)

qqnorm(residuals(recAge.full), main = NULL)
qqline(residuals(recAge.full))
text(-2.5, 9, "1B", cex = 2)

plot(residuals(recAge.full) ~ fitted.values(recAge.full), xlab = "Fitted values", ylab = "Residuals")
text(3.25, 9, "1C", cex = 2)

# Age diff
hist(residuals(ageDiff.full), main = NULL, xlab = "Residuals")
text(-6, 95, "2A", cex = 2)

qqnorm(residuals(ageDiff.full), main = NULL)
qqline(residuals(ageDiff.full))
text(-2.5, 9, "2B", cex = 2)

plot(residuals(ageDiff.full) ~ fitted.values(ageDiff.full), xlab = "Fitted values", ylab = "Residuals")
text(-11.5, 9, "2C", cex = 2)

dev.off()

par(mfrow = c(1, 1))



### iii) Factors predicting recipient sex

# Find the optimal random effects structure in a null model where 'recipient sex' is the outcome
recSex.ml.null <- glmer(recSex ~ (1|id) + (1|camp), family = "binomial", data = recip)
#ranova(recSex.ml.null)

# Have to use 'anova' here, as 'ranova' only works for linear multi-level models - Inclusion of camp RE does improve model fit, beyond that of just ego ID.
recSex.ml.null2 <- glmer(recSex ~ (1|id), family = "binomial", data = recip)
anova(recSex.ml.null, recSex.ml.null2)

summary(ageDiff.ml.null)


## Now look at whether age, sex and relatedness are associated with sex of recipient. Only association is with ego sex, as participants much more likely to choose children of the same sex
sexRec.full <- glmer(recSex ~ egoAge + egoSex + rel + (1|id) + (1|camp), family = "binomial", data = recip)
summary(sexRec.full)

exp(coef(summary(sexRec.full)))
exp(confint(sexRec.full, method = "Wald"))

## Make a table to save these results to
TableS8 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = exp(coef(summary(sexRec.full))[,1]),
                            lci = exp(confint(sexRec.full, method = "Wald")[3:6,1]),
                            uci = exp(confint(sexRec.full, method = "Wald")[3:6,2]),
                            p = coef(summary(sexRec.full))[,4]
))
TableS8

# Convert estimates to numeric and round
TableS8 <- TableS8 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

TableS8

write_csv(TableS8, "../Results/tableS8.csv", quote = FALSE)

# Predicted probabilities, to help interpret odds ratios
newdata <- data.frame(cbind(egoAge = rep(mean(recip$egoAge), 2),
                            rel = rep(mean(recip$rel), 2),
                            egoSex = c(0, 1)))
newdata
newdata$probs <- predict(sexRec.full, newdata = newdata, type = "response", re.form = NA)
newdata


## Interaction models

# Age by sex: No interaction
sexRec.agebysex <- lmer(recSex ~ egoAge * egoSex + rel + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(sexRec.agebysex)

# Age by relatedness: Is an association, with odds of nominating males increasing with age if r = 0, but decreasing with age if r = 0.5.
sexRec.agebyrel <- lmer(recSex ~ egoAge * rel + egoSex + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(sexRec.agebyrel)

exp(coef(summary(sexRec.agebyrel)))
exp(confint(sexRec.agebyrel))

newdata <- data.frame(cbind(egoAge = rep(c(5, 10, 15), 4),
                            rel = c(0, 0, 0, 0.5, 0.5, 0.5, 0, 0, 0, 0.5, 0.5, 0.5),
                            egoSex = rep(c(0, 1), each = 6)))
newdata
newdata$probs <- predict(sexRec.agebyrel, newdata = newdata, type = "response", re.form = NA)
newdata

# sex by relatedness: No interaction
sexRec.sexbyrel <- lmer(recSex ~ egoAge + egoSex * rel + (1|id) + (1|camp), REML = FALSE, data = recip)
summary(sexRec.sexbyrel)



### iv) Factors predicting nominating someone of the same sex

# Find the optimal random effects structure in a null model where 'sex difference' is the outcome - Get a convergence warning, most likely because 'camp' explains so little variation so cannot be estimated accurately. Will just use ego ID as a random effect, then.
sexDiff.ml.null <- glmer(sexDiff ~ (1|id) + (1|camp), family = "binomial", data = recip)
#ranova(recSex.ml.null)

summary(sexDiff.ml.null)

sexDiff.ml.null <- glmer(sexDiff ~ (1|id), family = "binomial", data = recip)
summary(sexDiff.ml.null)


## Now look at whether age, sex and relatedness are associated with being of same sex. No associations here.
sexDiff.full <- glmer(sexDiff ~ egoAge + egoSex + rel + (1|id), family = "binomial", data = recip)
summary(sexDiff.full)

exp(coef(summary(sexDiff.full)))
exp(confint(sexDiff.full, method = "Wald"))

## Make a table to save these results to
TableS9 <- data.frame(cbind(variable = c("Intercept", "Age", "Sex", "Relatedness"),
                            coef = exp(coef(summary(sexDiff.full))[,1]),
                            lci = exp(confint(sexDiff.full, method = "Wald")[2:5,1]),
                            uci = exp(confint(sexDiff.full, method = "Wald")[2:5,2]),
                            p = coef(summary(sexDiff.full))[,4]
))
TableS9

# Convert estimates to numeric and round
TableS9 <- TableS9 %>%
  mutate(coef = as.numeric(coef)) %>%
  mutate(coef = round(coef, 3)) %>%
  mutate(lci = as.numeric(lci)) %>%
  mutate(lci = round(lci, 3)) %>%
  mutate(uci = as.numeric(uci)) %>%
  mutate(uci = round(uci, 3)) %>%
  mutate(p = as.numeric(p)) %>%
  mutate(p = round(p, 3))

TableS9

write_csv(TableS9, "../Results/tableS9.csv", quote = FALSE)


## Interaction models

# Age by sex: No interaction
sexDiff.agebysex <- lmer(sexDiff ~ egoAge * egoSex + rel + (1|id), REML = FALSE, data = recip)
summary(sexDiff.agebysex)

# Age by relatedness: No interaction
sexDiff.agebyrel <- lmer(sexDiff ~ egoAge * rel + egoSex + (1|id), REML = FALSE, data = recip)
summary(sexDiff.agebyrel)

# sex by relatedness: No interaction
sexDiff.sexbyrel <- lmer(sexDiff ~ egoAge + egoSex * rel + (1|id), REML = FALSE, data = recip)
summary(sexDiff.sexbyrel)



