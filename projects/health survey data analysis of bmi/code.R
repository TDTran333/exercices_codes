# Load the NHANES and dplyr packages
library(dplyr)
library(NHANES)

# Load the NHANESraw data
data("NHANESraw")

# Take a glimpse at the contents
glimpse(NHANESraw)

# Load the ggplot2 package
library(ggplot2)

# Use mutate to create a 4-year weight variable and call it WTMEC4YR
NHANESraw <- NHANESraw %>% mutate(WTMEC4YR = WTMEC2YR/2)

# Calculate the sum of this weight variable
NHANESraw %>% summarize(sum(WTMEC4YR))

# Plot the sample weights using boxplots, with Race1 on the x-axis
ggplot(NHANESraw, aes(x = Race1, y = WTMEC4YR)) + geom_boxplot()

# Load the survey package
library(survey)

# Specify the survey design
nhanes_design <- svydesign(
  data = NHANESraw,
  strata = ~SDMVSTRA,
  id = ~SDMVPSU,
  nest = TRUE,
  weights = ~WTMEC4YR)

# Print a summary of this design
summary(nhanes_design)

# Select adults of Age >= 20 with subset
nhanes_adult <- subset(nhanes_design, Age >= 20)

# Print a summary of this subset
summary(nhanes_adult)

# Compare the number of observations in the full data to the adult data
nrow(nhanes_design)
nrow(nhanes_adult)

# Calculate the mean BMI in NHANESraw
bmi_mean_raw <- NHANESraw %>% 
  filter(Age >= 20) %>%
  summarize(mean(BMI, na.rm = TRUE))
bmi_mean_raw

# Calculate the survey-weighted mean BMI of US adults
bmi_mean <- svymean(~BMI, design = nhanes_adult, na.rm = TRUE)
bmi_mean

# Draw a weighted histogram of BMI in the US population
NHANESraw %>% 
  filter(Age >= 20) %>%
  ggplot(mapping = aes(x = BMI, weight = WTMEC4YR)) + 
  geom_histogram()+
  geom_vline(xintercept = coef(bmi_mean), color="red")

# Load the broom library
library(broom)

# Make a boxplot of BMI stratified by physically active status
NHANESraw %>% 
  filter(Age>=20) %>%
  ggplot(aes(PhysActive, BMI, weight = WTMEC4YR)) +
  geom_boxplot()

# Conduct a t-test comparing mean BMI between physically active status
survey_ttest <- svyttest(BMI ~PhysActive, design = nhanes_adult)

# Use broom to show the tidy results
tidy(survey_ttest)

# Estimate the proportion who are physically active by current smoking status
phys_by_smoke <- svyby(~PhysActive, by = ~SmokeNow, 
                       FUN = svymean, 
                       design = nhanes_adult, 
                       keep.names = FALSE)

# Print the table
phys_by_smoke

# Plot the proportions
ggplot(data = phys_by_smoke, 
       aes(x = SmokeNow, y = PhysActiveYes, fill = SmokeNow)) +
  geom_col() +
  ylab("Proportion Physically Active")

# Estimate mean BMI by current smoking status
BMI_by_smoke <- svyby(~BMI, by = ~SmokeNow, 
                      FUN = svymean,
                      design = nhanes_adult,
                      na.rm = TRUE)
BMI_by_smoke

# Plot the distribution of BMI by current smoking status
NHANESraw %>% 
  filter(Age>=20, !is.na(SmokeNow)) %>% 
  ggplot(aes(x = SmokeNow, y = BMI, weight = WTMEC4YR)) +
  geom_boxplot()

# Plot the distribution of BMI by smoking and physical activity status
NHANESraw %>% 
  filter(Age>=20) %>% 
  ggplot(aes(x = SmokeNow, y = BMI, weight = WTMEC4YR, color = PhysActive)) +
  geom_boxplot()

# Fit a multiple regression model
mod1 <- svyglm(BMI ~ SmokeNow*PhysActive, design = nhanes_adult)

# Tidy the model results
tidy_mod1 <- tidy(mod1)
tidy_mod1

# Calculate expected mean difference in BMI for activity within non-smokers
diff_non_smoke <- tidy_mod1 %>% 
  filter(term == "PhysActiveYes") %>% 
  select(estimate)
diff_non_smoke

# Calculate expected mean difference in BMI for activity within smokers
diff_smoke <- tidy_mod1 %>% 
  filter(term %in% c("PhysActiveYes", "SmokeNowYes:PhysActiveYes")) %>% 
  summarize(estimate = sum(estimate))
diff_smoke

# Adjust mod1 for other possible confounders
mod2 <- svyglm(BMI ~ PhysActive*SmokeNow + Race1 + Alcohol12PlusYr + Gender, 
               design = nhanes_adult)

# Tidy the output
tidy(mod2)
