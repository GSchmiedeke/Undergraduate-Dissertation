#Data set importing, cleaning, & merging [LOAD]

#Adds the pacman package to the library
library(pacman)
#Bulk loads the necessary packages
p_load(arm, binom, boot, car, caret, corrplot, dplyr, effects, effectsize, 
       effsize, forcats, GGally, gganimate, ggplot2, ggridges, ggspatial, ggthemes,  
       ggmap, gmodels, gplots, gridExtra, haven, janitor, labelled, lessR, lmtest, 
       magrittr, maps, maptools, MASS, mgcv, modeest, mosaic, nortest, patchwork,  
       performance, plyr, predtools, pROC, psych, purrr, pwr, ResourceSelection,   
       RColorBrewer, readr, readxl, sf, sjPlot, skimr, tidyr, tigris, tmap, vcd,   
       vcdExtra, viridis, visdat, WRS2)
#Imports the shootings data set and reads it into the global environment
school_shootings <- read_xlsx("/Volumes/G'S DRIVE/UoM/Courses/Third Year/Year-Round/CRIM30620/Data/School Shootings/CHDS School Shootings Dataset.xlsx", sheet = 2)
firearm_leg <- read_sav("/Volumes/G'S DRIVE/UoM/Courses/Third Year/Year-Round/CRIM30620/Data/Firearms Legislation/Firearm Legislation Documents/37363-0001-Data.sav")
state_boundary_data <- st_read("/Volumes/G'S DRIVE/UoM/Courses/Third Year/Year-Round/CRIM30620/Data/USA Geocodes/cb_2019_us_state_500k/cb_2019_us_state_500k.shp")
#Displays the names of the variables for the shootings data set
names(school_shootings)
#Cleans the variable names to all lowercase
school_shootings <- clean_names(school_shootings)
names(school_shootings)
names(firearm_leg)
firearm_leg <- clean_names(firearm_leg)
names(firearm_leg)
names(state_boundary_data)
state_boundary_data <- clean_names(state_boundary_data)
names(state_boundary_data)
#Creates a new variable in the shootings data set for 'year'
school_shootings <- school_shootings %>%
  mutate(year = substr(date, start = 1, stop = 4))
#Subsets the shootings data set to include only the years 1991-2019
ss_subset <- school_shootings %>%
  filter(year >= 1991 & year <= 2019)
#Creates a mapping table containing the abbreviated and full state names
state_table <- data.frame(state_full = c("Alabama", "Alaska", "Arizona", "Arkansas", 
                                         "California","Colorado", "Connecticut", 
                                         "Delaware", "Florida", "Georgia","Hawaii", 
                                         "Idaho", "Illinois", "Indiana", "Iowa", 
                                         "Kansas","Kentucky", "Louisiana", "Maine", 
                                         "Maryland", "Massachusetts","Michigan", 
                                         "Minnesota", "Mississippi", "Missouri", 
                                         "Montana", "Nebraska", "Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico", "New York", 
                                         "North Carolina", "North Dakota", "Ohio",
                                         "Oklahoma", "Oregon", "Pennsylvania", 
                                         "Rhode Island", "South Carolina", 
                                         "South Dakota", "Tennessee", "Texas",
                                         "Utah", "Vermont", "Virginia", "Washington",
                                         "West Virginia", "Wisconsin", "Wyoming"),
                          state_abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                                           "DE", "FL", "GA", "HI", "ID", "IL", "IN",
                                           "IA", "KS", "KY", "LA", 'ME', "MD", "MA",
                                           "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                                           "NH", "NJ", "NM", "NY","NC", "ND", "OH", "OK", "OR",
                                           "PA", "RI", "SC", "SD", "TN", "TX", "UT",
                                           "VT", "VA", "WA", "WV", "WI", "WY"))
#Merges the legislation data set to include a variable containing the abbreviated
#state names so it matches that of the shooting data set
firearm_leg <- merge(firearm_leg, state_table, by.x = "state", by.y = "state_full", 
                     all.x = "TRUE")
#Subsets the legislation data set to only include the following variables: 
#state, year, permit, registration, universal, and waiting
leg_subset <- firearm_leg %>%
  select(state_abbrev, year, permit, registration, universal, waiting)
#Creates a new data frame that computes the school shootings aggregates by year and state
ss_aggregates <- ss_subset %>%
  group_by(year, state) %>%
  dplyr::summarise(count = n())
#Creates a new object with the state names and abbreviations from the state table
all_states <- state_table
#Assigns the year values from the ss_aggregates data frame to a new object
all_years <- unique(ss_aggregates$year)
#Creates a template with combinations for all states and all years
template <- expand.grid(state = all_states$state_abbrev, year = all_years)
names(template)
names(ss_aggregates)
#Merges the template with the aggregated data
ss_template <- left_join(template, ss_aggregates, by = c("state" = "state", "year"))
#Replaces NA values with 0 for the incident count to account for all states and all years
ss_template$count[is.na(ss_template$count)] <- 0
#Checks if there are any missing values in the ss_template data
any(is.na(ss_template))
any(is.na(leg_subset))
#Checks if the classes of the state and state_abbrev variables match
class(ss_template$state) == class(leg_subset$state_abbrev)
class(ss_template$year) == class(leg_subset$year)
class(ss_template$year) 
class(leg_subset$year)
#Transforms the class of the year variable in the ss_template data set to numeric
ss_template$year <- as.numeric(ss_template$year)
class(ss_template$year)
class(ss_template$state) == class(leg_subset$state_abbrev)
names(ss_template)
names(leg_subset)
#Merges the two data sets by the state and year variables
ss_complete <- left_join(ss_template, leg_subset, by = c("state" = "state_abbrev", "year"), 
                         multiple = "all") 
any(is.na(ss_complete))
#Adds another variable to the data set that creates a binary measure out of the
#count variable (0 denoting no shootings and 1 denoting at least 1 shooting)
ss_complete <- ss_complete %>%
  mutate(binary_count = ifelse(count >= 1, 1, 0))
#Adds a sequential year variable to the data set by assigning a unique identifier
#to all observations within each specific year
ss_complete <- ss_complete %>%
  dplyr::mutate(year_seq = group_indices(., year)) 
names(ss_complete)
names(state_boundary_data)
ss_complete_spatial <- left_join(ss_complete, state_boundary_data, 
                                 by = c("state" = "stusps"))
#Defines a function to calculate the annual rates
calculate_annual_rates <- function(data, var_name) {
  var <- rlang::sym(var_name)
  expanded_data <- tidyr::expand(data, year, !!var) %>%
    left_join(data, by = c("year", var_name)) %>%
    dplyr::mutate(count = replace_na(count, 0)) %>%
    group_by(year, !!var) %>%
    dplyr::summarise(annual_shootings = sum(count, na.rm = TRUE),
                     num_states = sum(!is.na(state)),
                     annual_rate = ifelse(num_states == 0, 0, annual_shootings/num_states),
                     .groups = 'drop')}
#Creates a new object with the variables of interest
var_of_int <- c("permit", "registration", "universal", "waiting")
#Creates a list of data frames with the computed annual rates of school shootings for each 
#of the variables of interest
rates_dfs <- lapply(var_of_int, function(var) {
  calculate_annual_rates(ss_complete, var)})
#Names the list elements as the variable names for easier identification
names(rates_dfs) <- var_of_int
#Renames the variables in the data set
rates_dfs$permit <- rates_dfs$permit %>%
  dplyr::rename(annual_count_p = annual_shootings,
                num_states_p = num_states,
                annual_rate_p = annual_rate)
rates_dfs$registration <- rates_dfs$registration %>%
  dplyr::rename(annual_count_r = annual_shootings,
                num_states_r = num_states,
                annual_rate_r = annual_rate)
rates_dfs$universal <- rates_dfs$universal %>%
  dplyr::rename(annual_count_u = annual_shootings,
                num_states_u = num_states,
                annual_rate_u = annual_rate)
rates_dfs$waiting <- rates_dfs$waiting %>%
  dplyr::rename(annual_count_w = annual_shootings,
                num_states_w = num_states,
                annual_rate_w = annual_rate)
names(rates_dfs$permit)
names(rates_dfs$registration)
names(rates_dfs$universal)
names(rates_dfs$waiting)
#Joins the rates_dfs data frames together
rates_df <- reduce(rates_dfs, full_join, by = "year")
#Handles the multiple repeat observations by selecting the first and last
#observations for each year 
rates_final <- rates_df %>%
  group_by(year) %>%
  slice(c(1, n()))
#Removes unnecessary objects from the environment
rm(all_states, firearm_leg, leg_subset, school_shootings, ss_aggregates,
   ss_subset, ss_template, state_table, template, all_years, var_of_int,
   calculate_annual_rates, rates_dfs, rates_df, state_boundary_data)

#Descriptive Statistics [EDA]

#Ss_complete data set
#Displays the first few observations of the data set
head(ss_complete)
#Displays the last few observations of the data set
tail(ss_complete)
#Displays the dimensions of the data set
dim(ss_complete)
#Checks for any missing values in the data set
any(is.na(ss_complete))
class(ss_complete)
#Displays the structure of the data set
str(ss_complete)
#Displays the attributes of the data set
attributes(ss_complete)
#Provides a broad overview of each column's summary statistics 
skim(ss_complete)
#Computes the summary statistics of the data set
summary(ss_complete)
#State variable
str(ss_complete$state)
class(ss_complete$state)
#Year variable
str(ss_complete$year)
class(ss_complete$year)
#Year_seq variable
str(ss_complete$year_seq)
class(ss_complete$year_seq)
#Count variable
str(ss_complete$count)
class(ss_complete$count)
#Computes the minimum value for the count variable
min(ss_complete$count)
#Computes the maximum value for the count variable
max(ss_complete$count)
#Computes the median of the count variable
median(ss_complete$count)
#Computes the mean for the count variable
mean(ss_complete$count)
#Computes the mode for the count variable
mlv(ss_complete$count)
#Computes the quantiles for the count variable
quantile(ss_complete$count)
#Computes the range for the count variable
max(ss_complete$count) - min(ss_complete$count)
#Computes the interquartile range for the count variable
IQR(ss_complete$count)
#Computes the standard deviation of the count variable
sd(ss_complete$count)
#Computes the variance of the count variable
var(ss_complete$count)
#Computes the skewness of the count variable
skewness(ss_complete$count)
#Computes the coefficient of variation for the count variable
(sd(ss_complete$count)/mean(ss_complete$count))*100
summary(ss_complete$count)
#Computes the five number summary for the count variable
fivenum(ss_complete$count)
#Computes the favourite statistics for the count variable
favstats(ss_complete$count)
#Computes the descriptive statistics for the count variable
describeBy(ss_complete$count)
#Binary_count variable
str(ss_complete$binary_count)
class(ss_complete$binary_count)
mlv(ss_complete$binary_count)
IQR(ss_complete$binary_count)
var(ss_complete$binary_count)
(sd(ss_complete$binary_count)/mean(ss_complete$binary_count))*100
summary(ss_complete$binary_count)
fivenum(ss_complete$binary_count)
favstats(ss_complete$binary_count)
describeBy(ss_complete$binary_count)
#Permit variable
str(ss_complete$permit)
class(ss_complete$permit)
mlv(ss_complete$permit)
summary(ss_complete$permit)
fivenum(ss_complete$permit)
favstats(ss_complete$permit)
describeBy(ss_complete$permit)
#Calculates the descriptive statistics for counts and rates by permit status
describeBy(ss_complete$count, ss_complete$permit)
describeBy(ss_complete$binary_count, ss_complete$permit)
#Calculates the correlation coefficient between the permit and count variables
cor(x = ss_complete$permit, y = ss_complete$count)
cor(x = ss_complete$permit, y = ss_complete$binary_count)
#Registration variable
str(ss_complete$registration)
class(ss_complete$registration)
summary(ss_complete$registration)
fivenum(ss_complete$registration)
favstats(ss_complete$registration)
describeBy(ss_complete$registration)
describeBy(ss_complete$count, ss_complete$registration)
describeBy(ss_complete$binary_count, ss_complete$registration)
describeBy(ss_complete$binary_count, ss_complete$registration)
cor(x = ss_complete$registration, y = ss_complete$count)
cor(x = ss_complete$registration, y = ss_complete$binary_count)
#Universal variable
str(ss_complete$universal)
class(ss_complete$universal)
summary(ss_complete$universal)
fivenum(ss_complete$universal)
favstats(ss_complete$universal)
describeBy(ss_complete$universal)
describeBy(ss_complete$count, ss_complete$universal)
describeBy(ss_complete$binary_count, ss_complete$universal)
describeBy(ss_complete$binary_count, ss_complete$universal)
cor(x = ss_complete$universal, y = ss_complete$count)
cor(x = ss_complete$universal, y = ss_complete$binary_count)
#Waiting variable
str(ss_complete$waiting)
class(ss_complete$waiting)
summary(ss_complete$waiting)
fivenum(ss_complete$waiting)
favstats(ss_complete$waiting)
describeBy(ss_complete$waiting)
describeBy(ss_complete$count, ss_complete$waiting)
describeBy(ss_complete$binary_count, ss_complete$waiting)
describeBy(ss_complete$binary_count, ss_complete$waiting)
cor(x = ss_complete$waiting, y = ss_complete$count)
cor(x = ss_complete$waiting, y = ss_complete$binary_count)

#Inferential statistics [IFS]

#Runs a Levene's Test to test the homogeneity of variance of the permit and binary
#count variables
leveneTest(ss_complete$binary_count, ss_complete$permit, center = median)
leveneTest(ss_complete$binary_count, ss_complete$registration, center = median)
leveneTest(ss_complete$binary_count, ss_complete$universal, center = median)
leveneTest(ss_complete$binary_count, ss_complete$waiting, center = median)
#Runs a t-test for the permit and binary count variables
t.test(binary_count ~ permit, data = ss_complete)
t.test(binary_count ~ registration, data = ss_complete, var.equal = FALSE)
t.test(binary_count ~ universal, data = ss_complete, var.equal = FALSE)
t.test(binary_count ~ waiting, data = ss_complete, var.equal = FALSE)
#Runs a cohen's d test to calculate effect sizes of the permit and binary count
#variables 
cohen.d(binary_count ~ permit, data = ss_complete)
cohen.d(binary_count ~ registration, data = ss_complete)
cohen.d(binary_count ~ universal, data = ss_complete)
cohen.d(binary_count ~ waiting, data = ss_complete)
#Runs a power test for the results of the cohen's d test
pwr.t.test(n = 1450, d = -0.12, sig.level = 0.05, type = "two.sample",
           alternative = "two.sided")
pwr.t.test(n = 1450, d = -0.64, sig.level = 0.05, type = "two.sample",
           alternative = "two.sided")
pwr.t.test(n = 1450, d = 0.34, sig.level = 0.05, type = "two.sample",
           alternative = "two.sided")
pwr.t.test(n = 1450, d = 0.19, sig.level = 0.05, type = "two.sample",
           alternative = "two.sided")
#Calculates the correlation coefficient between the binary count and permit
#variables
cor(x = ss_complete$permit, y = ss_complete$binary_count)
cor(x = ss_complete$registration, y = ss_complete$binary_count)
cor(x = ss_complete$universal, y = ss_complete$binary_count)
cor(x = ss_complete$waiting, y = ss_complete$binary_count)
#Runs a logistic regression model on the binary count and variables of interest
logit_model <- glm(binary_count ~ permit + registration + universal + waiting +
                 year_seq, data = ss_complete, family = binomial(link = "logit"))
summary(logit_model)
#Displays a more parsimonious presentation of the results
arm::display(logit_model)
attributes(logit_model)
#Displays the predicted probabilities for the model
pred_prob <- predict(logit_model, type = "response") 
#Displays the first 10 values
pred_prob[1:10]
#Displays the standardised version of the model
arm::display(standardise(logit_model))
#Computes the confidence intervals for the model 
confint(logit_model)
#Computes the coefficients of model
summary(logit_model)$coefficients
#Exponentiates the coefficients of model
exp(coef(logit_model))
#Exponentiates the confidence intervals of model
exp(confint(logit_model))
# Extract the coefficients from the model
coefficients <- coef(logit_model)
# Convert the coefficients to odds ratios
odds_ratios <- exp(coefficients)
print(odds_ratios)
confint(odds_ratios)
exp(confint(odds_ratios))
#Extracts the confidence levels using standard errors
se <- sqrt(diag(vcov(logit_model)))
#Calculates the lower bound of the 95% confidence interval for the odds ratio
lower <- exp(coefficients - 1.96 * se)
#Calculates the upper bound of the 95% confidence interval for the odds ratio
upper <- exp(coefficients + 1.96 * se)
#Compiles the confidence intervals
approx_conf_int <- cbind(lower, upper)
print(approx_conf_int)
names(logit_model)
#Calculates the Model Chi-Square value
with(logit_model, null.deviance - deviance)
#Calculates the degrees of freedom difference
with(logit_model, df.null - df.residual)
#Calculates the p-value for the Chi-Square distribution
with(logit_model, pchisq(null.deviance - deviance, df.null - df.residual, 
                     lower.tail = FALSE))
#Calculates the Likelihood Ratio R2 of the model
with(logit_model, (null.deviance - deviance)/null.deviance)
#Defines the classes according to the cut-off
pred_class <- pred_prob > .5
class(pred_class)
#Converts the predictions to a factor with the same levels as the annual binary count
pred_class <- as_factor(pred_class)
levels(pred_class) <- c("0", "1")
class(pred_class)
levels(pred_class)
table(pred_class)
pred_class[1:10]
ss_complete$binary_count_f <- factor(ss_complete$binary_count, levels = c("0", "1"))
#Generates the confusion matrix
conf_matrix <- confusionMatrix(pred_class, ss_complete$binary_count_f)
#Prints the confusion matrix
print(conf_matrix)
#Displays the overall accuracy value
conf_matrix$overall['Accuracy']
#Displays the sensitivity value by class
conf_matrix$byClass['Sensitivity']
#Displays the specificity value by class
conf_matrix$byClass['Specificity']
#Returns the best sum of sensitivity and specificity for the cut-offs
coords(rocCurve, x = "best", best.method = "closest.topleft")
#Tests for outliers in the model
outlierTest(logit_model)
#Calculates the Variance Inflation Factor of the model
vif(logit_model)
#Calculates the Akaike Information Criterion for the model
AIC(logit_model)
#Calculates the Bayesian Information Criterion for the model
BIC(logit_model)
#Runs the Logit logistical regression to display the information above in one 
#simple display
Logit(binary_count ~ permit + registration + universal + waiting + year_seq,
      data = ss_complete, brief = TRUE)
rm(coefficients, lower, upper, odds_ratios, pred_class, resids, se, upper, 
   approx_conf_int)
summary(logit_model)
tab_model(logit_model, show.se = TRUE, show.aic = TRUE)

#Descriptive visualisations [DVIS]

ss_complete$permit_f <- as_factor(ss_complete$permit)
class(ss_complete$permit)
ss_complete$registration_f <- as_factor(ss_complete$registration)
class(ss_complete$registration)
ss_complete$universal_f <- as_factor(ss_complete$universal)
class(ss_complete$universal)
ss_complete$waiting_f <- as_factor(ss_complete$waiting)
class(ss_complete$waiting)
rates_final$permit_f <- as_factor(rates_final$permit)
class(rates_final$permit_f)
rates_final$registration_f <- as_factor(rates_final$registration)
class(rates_final$registration_f)
rates_final$universal_f <- as_factor(rates_final$universal)
class(rates_final$universal_f)
rates_final$waiting_f <- as_factor(rates_final$waiting)
class(rates_final$waiting_f)
#Histograms
#Creates a basic histogram for visualising the count variable
hist(ss_complete$count)
hist(ss_complete$binary_count)
#Creates a histogram with a density curve shaded area for the count variable
ggplot(ss_complete, aes(x = count)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, colour = 1, fill ="white") +
  labs(title = "Distribution and Density of the Count of School Shootings", 
       x = "Count",
       y = "Density") +
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25, adjust = 2.58) +
  geom_vline(xintercept = mean(ss_complete$count, na.rm = TRUE), linetype = "dashed", colour = "blue") +
  scale_y_continuous(breaks = seq(from = 0, to = 1.5, by = 0.1)) +
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1))
#Bar charts
#Creates a bar chart to depict the count by year
ggplot(ss_complete, aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#33CCCC") +
  labs(title = "Count of School Shootings by Year", x = "Year",
       y = "Count")
ggplot(ss_complete, aes(x = state, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#33CCCC") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of School Shootings by State", x = "State",
       y = "Count")
#Creates a bar plot to display the distribution of the binary_count variable
ggplot(ss_complete, aes(x = factor(binary_count), fill = factor(binary_count))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "#0077a3", "1" = "#60a7cf")) +
  labs(title = "Distribution of Binary Count",
       x = "Binary Count Category",
       y = "Frequency") +
  scale_y_continuous(breaks = seq(from = 0, to = 900, by = 100)) +
  geom_text(stat = "count", aes(label = ..count..),
            position = position_stack(vjust = 0.5),
            size = 3.5,
            colour = "white") +
  theme_minimal() +
  theme(legend.title = element_blank())
#Creates an object containing the bar graph information for the permit variable
bar_1 <- ggplot(ss_complete, aes(x = permit_f, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#8bbde2") +
  labs(title = "Count by Permit Requirement", x = "Permit Status", y = "Count")
bar_2 <- ggplot(ss_complete, aes(x = registration_f, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#60a7cf") +
  labs(title = "Count by Registration Requirement", x = "Registration Status", 
       y = "Count")
bar_3 <- ggplot(ss_complete, aes(x = universal_f, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#358fb9") +
  labs(title = "Count by Universal Background Check Requirement", 
       x = "Universal Status", y = "Count")
bar_4 <- ggplot(ss_complete, aes(x = waiting_f, y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "#0077a3") +
  labs(title = "Count by Waiting Period Requirement", x = "Waiting Status", 
       y = "Count")
#Sets the plot layout to two columns
plot_layout(ncol = 2)
#Combines the bar objects into one combined object
combined_bars_1 <- bar_1 + bar_2 + bar_3 + bar_4
#Displays the bar plots
combined_bars_1
rm(bar_1, bar_2, bar_3, bar_4)
#Line graphs
#Creates a line graph showing the count by year 
ss_complete %>%
  group_by(year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = annual_count)) +
  geom_line(colour = "#33CCCC") + 
  theme_minimal() +
  labs(title = "Annual Count of School Shootings by Year", x = "Year", y = 
         "Count of School Shootings")
#Creates a line graph depicting the annual count by state
ss_complete %>%
  group_by(state, year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = annual_count, group = state, color = state)) +
  geom_line() + 
  theme_minimal() +
  labs(title = "Annual Count of School Shootings by State", x = "Year", y = 
         "Count of School Shootings")
line_1 <- ss_complete %>%
  group_by(permit_f, year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = annual_count, group = permit_f, color = permit_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Permit", 
       x = "Year", 
       y = "Count") +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(from = 0, to = 120, by = 20)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_1
line_2 <- ss_complete %>%
  group_by(registration_f, year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = annual_count, group = registration_f, color = registration_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Registration", 
       x = "Year", 
       y = "Count") +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(from = 0, to = 120, by = 20)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_2
line_3 <- ss_complete %>%
  group_by(universal_f, year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = annual_count, group = universal_f, color = universal_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Universal", 
       x = "Year", 
       y = "Count") +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(from = 0, to = 120, by = 20)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_3
line_4 <- ss_complete %>%
  group_by(waiting_f, year) %>%
  dplyr::summarise(annual_count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = annual_count, group = waiting_f, color = waiting_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Waiting", 
       x = "Year", 
       y = "Count") +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_y_continuous(limits = c(0, 120), breaks = seq(from = 0, to = 120, by = 20)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_4
combined_lines_1 <- line_1 + line_2 + line_3 + line_4
combined_lines_1
line_a <- rates_final %>%
  group_by(permit_f, year) %>%
  ggplot(aes(x = year, y = annual_rate_p, group = permit_f, colour = permit_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Permit", x = "Year", y = "Rate") +
  theme(legend.title = element_blank(), legend.position = "top")  +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(from = 0, to = 4.5, by = 0.75)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_a
line_b <- rates_final %>%
  group_by(registration_f, year) %>%
  ggplot(aes(x = year, y = annual_rate_r, group = registration_f, colour = registration_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Registration", x = "Year", y = "Rate") +
  theme(legend.title = element_blank(), legend.position = "top")  +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(from = 0, to = 4.5, by = 0.75)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_b
line_c <- rates_final %>%
  group_by(universal_f, year) %>%
  ggplot(aes(x = year, y = annual_rate_u, group = universal_f, colour = universal_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Universal", x = "Year", y = "Rate") +
  theme(legend.title = element_blank(), legend.position = "top")  +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(from = 0, to = 4.5, by = 0.75)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_c
line_d <- rates_final %>%
  group_by(waiting_f, year) %>%
  ggplot(aes(x = year, y = annual_rate_w, group = waiting_f, colour = waiting_f)) +
  geom_line() +
  scale_colour_brewer(palette = "Paired") +
  theme_minimal() +
  labs(title = "Waiting", x = "Year", y = "Rate") +
  theme(legend.title = element_blank(), legend.position = "top")  +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(from = 0, to = 4.5, by = 0.75)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2020, by = 5))
line_d
combined_lines_2 <- line_a + line_b + line_c + line_d
combined_lines_2
rm(line_1, line_2, line_3, line_4, line_a, line_b, line_c, line_d)
#Boxplots
#Creates a boxplot to visualise the distribution of the count variable by year
boxplot(count ~ year, data = ss_complete, main = "Count by Year",
        xlab = "Year", ylab = "Count", las = 2)
boxplot(count ~ state, data = ss_complete, main = "Count by State",
        xlab = "State", ylab = "Count", las = 2)
ggplot(ss_complete, aes(x = factor(year), y = count)) +
  geom_boxplot() +
  labs(title = "Boxplot of School Shootings by Year",
       x = "Year", 
       y = "Count of School Shootings") +
  theme_minimal()
#Creates a box plot to display the distribution of the count variable
ggplot(ss_complete, aes(y = count)) + 
  geom_boxplot(fill = 4, alpha = 0.25, colour = 1, outlier.color = "blue") +
  stat_boxplot(geom = "errorbar", width = 0.25, colour = 1) +
  scale_y_continuous(breaks = seq(from = 0, to = 12, by = 2)) +
  ggtitle("Boxplot of Count of School Shootings")
#Spatial maps
class(ss_complete_spatial)
#Transforms the ss_complete_spatial data frame into a simple features object
ss_complete_spatial <- st_as_sf(ss_complete_spatial, crs = 4326)
class(ss_complete_spatial)
#Creates an object with the ss_complete_spatial filtered data for year 1991
data_1991 <- ss_complete_spatial %>%
  filter(year == 1991)
data_2019 <- ss_complete_spatial %>%
  filter(year == 2019)
class(data_1991)
data_1991 <- st_as_sf(data_1991, crs = 4326)
class(data_1991)
class(data_2019)
data_2019 <- st_as_sf(data_2019, crs = 4326)
class(data_2019)
#Defines the breaks for the count legend
breaks_1991 <- c(0, 1, 2, 3, 6, 9)
breaks_2019 <- c(0, 1, 2, 3, 4, 5, 6, 7, 9)
#Assigns a colour to each break
colours_1991 <- c("#DEEBF7", "#b6d2f5", "#8bbde2", "#60a7cf", "#00618c", "#00344a")
colours_2019 <- c("#DEEBF7", "#b6d2f5", "#8bbde2", "#60a7cf", "#358fb9", "#0077a3",
                  "#00618c", "#004b6b", "#00344a")
#Sets the tmap mode to plot
tmap_mode("plot")
#Sets the tmap style to colour blind
tmap_style("col_blind")
#Transforms the crs into an Albers Equal Area projection to maintain area
#proportionality
alaska_adjusted_data_1991 <- st_transform(data_1991, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 
                                          +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
alaska_adjusted_data_2019 <- st_transform(data_2019, crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 
                                          +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
#Creates a thematic map to display the count of school shootings by state for
#the year 1991
tm_shape(alaska_adjusted_data_1991) +
  tm_fill("count", 
          title = "Count",
          breaks = breaks_1991,
          palette = colours_1991,
          style = "cat") +
  tm_borders(alpha = 0.2) +
  tm_layout(main.title = "Count of School Shootings by State in 1991",
            main.title.size = 1,
            legend.title.size = 1.2,
            frame = FALSE, 
            legend.text.size = 1,
            legend.position = c("left", "centre"),
            outer.margins = 0,
            asp = NA) 
tm_shape(alaska_adjusted_data_2019) +
  tm_fill("count", 
          title = "Count",
          breaks = breaks_2019,
          palette = colours_2019,
          style = "cat") +
  tm_borders(alpha = 0.2) +
  tm_layout(main.title = "Count of School Shootings by State in 2019",
            main.title.size = 1,
            legend.title.size = 1.2,
            frame = FALSE, 
            legend.text.size = 1,
            legend.position = c("left", "centre"),
            outer.margins = 0,
            asp = NA) 
rm(breaks_1991, breaks_2019, colours_1991, colours_2019)

#Descriptive tables [DTAB]

#Count variable
class(ss_complete$count)
#Displays a frequency table for the total count by state
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE))
ss_complete %>%
  group_by(year) %>%
  dplyr::summarise(total_count = sum(count, na.rm = TRUE))
#Binary_count variable
ss_complete %>%
  dplyr::count(state, binary_count)
ss_complete %>%
  dplyr::count(year, binary_count)
#Permit variable
table(ss_complete$permit)
#Creates a proportion table for the permit variable
prop.table(table(ss_complete$permit))
#Summarizes data by each state's predominant permit status along with their respective
#total shootings, years, percentage make-up, state rates, and average annual rates
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(permit_status = ifelse(sum(permit == 1) > sum(permit == 0), 1, 0),
                   total_shootings = sum(count, na.rm = TRUE),
                   all_years = list(unique(year)),  
                   .groups = "drop") %>%
  group_by(permit_status) %>%
  dplyr::summarise(num_states = n(),
                   total_shootings = sum(total_shootings, na.rm = TRUE),
                   total_years = length(unique(unlist(all_years))),  
                   state_percentage = (num_states/50) * 100,
                   state_rate = total_shootings/num_states,
                   averaged_annual_rate = (total_shootings/num_states)/total_years,
                   .groups = "drop")
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(permit_status = ifelse(sum(permit == 1) > sum(permit == 0), 1, 0),
                   total_binary_count = sum(binary_count, na.rm = TRUE),
                   all_years = list(unique(year)),  
                   .groups = "drop") %>%
  group_by(permit_status) %>%
  dplyr::summarise(num_states = n(),
                   total_binary_count = sum(total_binary_count, na.rm = TRUE),
                   total_years = length(unique(unlist(all_years))),  
                   state_percentage = (num_states/50) * 100,
                   state_rate = total_binary_count/num_states,
                   averaged_annual_rate = (total_binary_count/num_states)/total_years,
                   .groups = "drop")
#Creates a proportion table of the permit and binary count variables
with(ss_complete, CrossTable(permit, binary_count, expected = TRUE, prop.c = FALSE, 
                             prop.t = FALSE, format = c("SPSS")))
#Registration variable
table(ss_complete$registration)
prop.table(table(ss_complete$registration))
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(reg_status = ifelse(sum(registration == 1) > sum(registration == 0), 1, 0),
                   total_shootings = sum(count, na.rm = TRUE),
                   all_years = list(unique(year)),  
                   .groups = "drop") %>%
  group_by(reg_status) %>%
  dplyr::summarise(num_states = n(),
                   total_shootings = sum(total_shootings, na.rm = TRUE),
                   total_years = length(unique(unlist(all_years))),  
                   state_percentage = (num_states/50) * 100,
                   state_rate = total_shootings/num_states,
                   averaged_annual_rate = (total_shootings/num_states)/total_years,
                   .groups = "drop")
with(ss_complete, CrossTable(registration, binary_count, expected = TRUE, prop.c = FALSE, 
                             prop.t = FALSE, format = c("SPSS")))
#Universal variable
table(ss_complete$universal)
prop.table(table(ss_complete$universal))
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(uni_status = ifelse(sum(universal == 1) > sum(universal == 0), 1, 0),
                   total_shootings = sum(count, na.rm = TRUE),
                   all_years = list(unique(year)),  
                   .groups = "drop") %>%
  group_by(uni_status) %>%
  dplyr::summarise(num_states = n(),
                   total_shootings = sum(total_shootings, na.rm = TRUE),
                   total_years = length(unique(unlist(all_years))),  
                   state_percentage = (num_states/50) * 100,
                   state_rate = total_shootings/num_states,
                   averaged_annual_rate = (total_shootings/num_states)/total_years,
                   .groups = "drop")
with(ss_complete, CrossTable(universal, binary_count, expected = TRUE, prop.c = FALSE, 
                             prop.t = FALSE, format = c("SPSS")))
#Waiting variable
table(ss_complete$waiting)
prop.table(table(ss_complete$waiting))
ss_complete %>%
  group_by(state) %>%
  dplyr::summarise(wait_status = ifelse(sum(waiting == 1) > sum(waiting == 0), 1, 0),
                   total_shootings = sum(count, na.rm = TRUE),
                   all_years = list(unique(year)),  
                   .groups = "drop") %>%
  group_by(wait_status) %>%
  dplyr::summarise(num_states = n(),
                   total_shootings = sum(total_shootings, na.rm = TRUE),
                   total_years = length(unique(unlist(all_years))),  
                   state_percentage = (num_states/50) * 100,
                   state_rate = total_shootings/num_states,
                   averaged_annual_rate = (total_shootings/num_states)/total_years,
                   .groups = "drop")
with(ss_complete, CrossTable(waiting, binary_count, expected = TRUE, prop.c = FALSE, 
                             prop.t = FALSE, format = c("SPSS")))

#Advanced tables [ATAB]

#Creates a table to display the model_1 results
sjPlot::tab_model(logit_model, show.se = TRUE, show.stat = TRUE, show.dev = TRUE,
                  show.aicc = TRUE, show.fstat = TRUE)
summary(logit_model)


#Advanced visualisations [AVIS]

#Displays the forest plot for the model
library(sjPlot)

# Assuming 'logit_model' is your logistic regression model
plot_model(logit_model, type = "est", 
           show.values = TRUE, 
           value.offset = .1, 
           ci.lvl = .95,
           dot.size = 3, 
           line.size = 1.5, 
           vline.color = "black",
           title = "Effects of Gun Law Provisions on School Shootings",
           axis.title = c("Odds Ratios"))
#Creates an object with the ROC curve 
rocCurve <- roc(response = ss_complete$binary_count,
                  predictor = pred_prob) 
#Calcualtes the Area Under Curve (AUC) value 
auc_value <- auc(rocCurve)
#Plots the ROC curve
plot(rocCurve, main = "ROC Curve", legacy.axes = TRUE)
#Adds the AUC value to the curve
text(x = 0.3, y = 0.2, labels = paste("AUC =", round(auc_value, 2)), cex = 1.1, 
     col = "black")
#Returns the best sum of sensitivity and specificity for the cut-offs
coords(rocCurve, x = "best", best.method = "closest.topleft")




