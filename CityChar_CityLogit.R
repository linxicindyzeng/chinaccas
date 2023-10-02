############################################################################################################################
# Name: Cindy Zeng
# Project: CCAS in China
# Task 1: Create and clean a panel data of >1m cities' characteristics from 2002-2021
#   Status: Done!
# Task 2: Use city characteristics to predict the switch year to a CCAS for high school education
#   Status: Paused. Results inconclusive due to small sample size.
############################################################################################################################

library(openxlsx)
library(latticeExtra)
library(Hmisc)
library(sjlabelled)
library(haven)
library(readxl)
library(readr)
library(Matrix)
library(glmnet) # for logit
library(ggplot2)
library(gridExtra) # for putting graphs together
library(png)
library(grid)
library(gridGraphics)
library(usethis)
library(devtools)
library(ggpubr)
library(patchwork)

library(dplyr)
library(broom)
library(ggplot2)
library(haven)
library(lfe)
library(lubridate)
library(stargazer)
library(tidyr)
library(estimatr)
library(censReg)
library(AER)
library(stringr)

# Setting the Working Directory
setwd("/Users/cindy/Dropbox/Mac/Desktop/College/Matters/Yale/CCAS/Play")

# Clean enviroment
rm(list=ls())

############################################################################################################################
############################################################################################################################
### Task 1: Create and clean a panel data of >1m cities' characteristics from 2002-2021
############################################################################################################################
############################################################################################################################

############################################################################################################################
### Merge each pYear data with rcode_city2.xlsx, according to city_c
############################################################################################################################

# List of years
years <- 2002:2021
rcode_city2 <- read_excel("rcode_city2.xlsx")
# Iterate over each year
for (year in years) {
  # Construct file names
  file_in <- paste0("pp", year, ".xlsx")
  file_out <- paste0("pp_", year, ".xlsx")
  # Read the input data set
  data <- read_excel(file_in)
  # Perform the merge with rcode_city2.xlsx
  merged_data <- left_join(data, rcode_city2, by = "city_c")
  # Replace column name
  colnames(merged_data) <- gsub("city_e\r\n", "city_e", colnames(merged_data))
  # Special treatment for 襄阳市 because it just would not work
  merged_data$region_code[merged_data$city_c == "襄阳市"] <- "420600"
  merged_data$city_e[merged_data$city_c == "襄阳市"] <- "Xiangyang"
  # Save the merged data set
  write.xlsx(merged_data, file_out, rowNames = FALSE)
}

############################################################################################################################
### Concatenate into one file
############################################################################################################################
# Define the file names
file_names <- paste0("pp_", years, ".xlsx")

# Iterate over the file names, standardize headers, now all pppYear.xlsx have the same width
for (file_name in file_names) {
  # Read the Excel file
  current_data <- read_excel(file_name, col_names = TRUE)
  # Define the expected column names
  expected_colnames <- c("region_code", "city_c", "city_e", "year", "popu", "grp", "grppc", "grpg", "eduexp", "numsec", "numpri", "stucol", "stuvoc", "stusec", "stupri")
  # Check if each expected column name exists in current_data
  missing_colnames <- setdiff(expected_colnames, colnames(current_data))
  # Generate new columns with missing colnames and empty entries
  for (colname in missing_colnames) {
    current_data <- current_data %>%
      mutate(!!colname := "")
  }
# Select specific columns in the desired order
current_data <- select(current_data, all_of(expected_colnames))
  
# Save the processed data to a new Excel file
file_out <- paste0("p", file_name)
write.xlsx(current_data, file_out, rowNames = FALSE)
}

# Create an empty data frame to store the merged data
md <- data.frame()
# Loop through each year
for (year in years) {
  # Read the file for the current year
  file_path <- paste0("ppp_", year, ".xlsx")
  current_data <- read_excel(file_path, col_names = TRUE)
  # Reorder columns based on a specific order
  current_data <- current_data[, c("region_code", "city_c", "city_e", "year", "popu", "grp", "grppc", "grpg", "eduexp", "numsec", "numpri", "stucol", "stuvoc", "stusec", "stupri")]
  # Append the current year's data to the merged data frame
  md <- rbind(md, current_data)
}
write.csv(md, "citycharfullpanel.csv")

############################################################################################################################
### Using H_Dates.xlsx as master data, iteratively left_join with pppYear.xlsx by city_c
############################################################################################################################

data0 <- read_excel("H_Dates.xlsx")
data2 <- data.frame()
for (year in years) {
  file_in <- paste0("ppp_", year, ".xlsx")
  data1 <- read_excel(file_in)
  data1 <- left_join(data0, data1, by = "city_c") # only retain entries with >1m popu
  data2 <- rbind(data2, data1)
}

# Okay, now, here is a problem. Of the 135 cities I'm interested in, some of them are not
# 地级市, so I ended up not having their characteristics. Now, I will find who they are.

# Identify cities without region_code

data2$grpg <- as.numeric(data2$grpg) # Convert grpg column to numeric type
data2[complete.cases(data2$city_c, data2$year) & data2$city_c == "襄阳市" & data2$year == 2021, "grpg"] <- -5.3

noregion <- data2[is.na(data2$region_code), ]
# noregion <- distinct(noregion, city_c) # delete duplicates in city_c, but this will only return a column of city_c
noregion <- noregion[order(noregion$year, decreasing = TRUE), ]
bad <- noregion[!duplicated(noregion$city_c), ]
bad <- distinct(bad, city_c) # bad is a list of city_c that needs to be specially treated

badx <- data2[data2$city_c %in% bad$city_c, ] # pull out all rows in data2 whose city_c is contained in bad
badx <- badx[order(badx$city_c),]

# delete city_c= 上饶市，兰州市，常德市，惠州市，抚顺市，枣庄市，泸州市，淮南市，
# 淮安市，潍坊市，菏泽市，
# many of them have city characteristic data strapping switch year but missing value 
# the second year following the switch year. I know why now, 错别字。Can't think
# of a good way to systematically address this problem. The only idea I came up with
# is to identify the city_c's sandwiching it, but then this hinges on both the one
# before it and after it are correct. Makes sense but not very reliable. I will
# identify them, and then go back to ppYear files and manually change them.

# 上饶 2014，义乌是县，余姚是县，兰州 2014，常德 2019 2020，惠州 2020，抚顺 2016，
# 昆山是县，晋江是县，枣庄 2019，泸州 2008，淮南 2008，淮安 2008，潍坊 2016，
# 菏泽 2019 2020，估计2009前后襄樊市改成了襄阳市我全部改成襄阳市，赤峰 2017，
# 长治 2008

# ouch, should have changed the pYear data.

# Now, badx is left with 义乌,余姚,昆山,晋江, whose data should be contained in 
# 县域统计 but the variables are not the same as the 地级市. 县级市 should have popu
# numsec, numpri, and that's it. In the meantime, drop these four.

df <- data2[!data2$city_c %in% bad$city_c, ]

# Label columns

data <- read_excel("p2019-demo.xlsx")
# Extract the labels from the first row
labels <- as.character(data[1, ])
labels <- c("", labels)
# Iterate over each column and assign labels
for (col in 1:ncol(df)) {
  label(df[[col]]) <- labels[col]
}
# special case: label(data$popu) <- "年末户籍人口（万人）Household Registered Population at Year-end (10 000 persons)" # using haven package
lapply(df, label) # see all labels

# Now we are all set for the baby logit model!
colnames(df)
# Generate indicators "swi" of whether the city has switched to CCAS in the year 
# of the characteristic (1 if year >= swy, 0 otherwise)
df$swi <- as.integer(df$year >= df$swy)

write.csv(df, "forlogit.csv")
write.xlsx(df, "forlogit.xlsx", rowNames = FALSE)

############################################################################################################################
############################################################################################################################
### Task 2: Baby logit models for forlogit.csv
############################################################################################################################
############################################################################################################################

###################################
### v1.0: using popu to predict swy
###################################

# Step 1: Install and load the necessary packages for logit
# install.packages("glmnet")
# library(glmnet)

# Step 2: Subset the data to include only the relevant variables
## 05311435 Okay, let's not think about training and testing just yet. Use all the 
## data to fit a baby logit model.

df <- read_excel("forlogit.xlsx") # hmm it seems like the header is automatically formed
# df$swi <- ifelse(df$swy <= df$year, 1, 0) # already done
str(df)
sapply(df, class)
df0 <- df[, c("region_code","swy","swi", "year", "popu", "grppc", "eduexp")]
# glm() too slow, convert all columns to integer type. Helps!
df0 <- as.data.frame(lapply(df0, as.integer))
str(df0)

## Dealing with missing 2018 grppc data: Impute using mean of 2017 and 2019 for that city
# Group the data frame by city_c
df0_grouped <- df0 %>%
  group_by(region_code)
# Calculate the mean of 'grppc' for each city in the years 2017 and 2019
mean_grppc <- df0_grouped %>%
  filter(year %in% c(2017, 2019)) %>%
  summarize(mean_grppc = mean(grppc, na.rm = TRUE), year = 2018)
# Join the mean_grppc values with the original data frame based on city_c and year
df0 <- df0 %>%
  left_join(mean_grppc, by = c("region_code", "year")) %>%
  mutate(grppc = ifelse(is.na(grppc), mean_grppc, grppc)) %>%
  select(-mean_grppc)

df0 <- as.data.frame(lapply(df0, as.integer))
write.xlsx(df0, "forlogit1.xlsx")

head(df0)
summary(df0)

### estimate a series of simple glm: swi ~ "swy"/"year"/"popu"/"grppc"/"eduexp"

# Remove rows with NA values
df0 <- na.omit(df0)
# List of individual predictor variables
predictor_vars <- c("swy", "popu", "grppc", "eduexp")
# Empty list to store regression models, results and plots

# popu
sglm <- as.formula(paste("swi ~ ","popu"))
sglmod <- glm(sglm, data = df0, family = binomial(link = "logit"))
sglmods[["popu"]] <- sglmod
sglmod_r[["popu"]] <- summary(sglmod)

# Create plot
plot_data <- data.frame(swi = df0$swi, predictor = df0[["popu"]])
# Predict the logit values using the fitted model
predicted <- predict(sglmod, type = "response")
plot_popu <- ggplot(plot_data, aes(x = predictor, y = swi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +  # Add OLS line
  geom_line(aes(y = predicted), color = "olivedrab") +  # Add predicted logit line
  labs(title = paste("Simple Logit of swi on", "popu"), x ="popu", y = "swi") +
  theme_minimal()

plot_popu

ggsave("plot_popu.png", plot = plot_popu, width = 10, height = 7, dpi = 300, bg = "white")

sglmplots[["popu"]] <- plot_popu

# grppc
sglm <- as.formula(paste("swi ~ ","grppc"))
sglmod <- glm(sglm, data = df0, family = binomial(link = "logit"))
sglmods[["grppc"]] <- sglmod
sglmod_r[["grppc"]] <- summary(sglmod)

# Create plot
plot_data <- data.frame(swi = df0$swi, predictor = df0[["grppc"]])
# Predict the logit values using the fitted model
predicted <- predict(sglmod, type = "response")
plot_grppc <- ggplot(plot_data, aes(x = predictor, y = swi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +  # Add OLS line
  geom_line(aes(y = predicted), color = "olivedrab") +  # Add predicted logit line
  labs(title = paste("Simple Logit of swi on", "grppc"), x ="grppc", y = "swi") +
  theme_minimal()

plot_grppc

ggsave("plot_grppc.png", plot = plot_grppc, width = 10, height = 7, dpi = 300, bg = "white")

sglmplots[["grppc"]] <- plot_grppc

# eduexp
sglm <- as.formula(paste("swi ~ ","eduexp"))
sglmod <- glm(sglm, data = df0, family = binomial(link = "logit"))
sglmods[["eduexp"]] <- sglmod
sglmod_r[["eduexp"]] <- summary(sglmod)

# Create plot
plot_data <- data.frame(swi = df0$swi, predictor = df0[["eduexp"]])
# Predict the logit values using the fitted model
predicted <- predict(sglmod, type = "response")
plot_eduexp <- ggplot(plot_data, aes(x = predictor, y = swi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +  # Add OLS line
  geom_line(aes(y = predicted), color = "olivedrab") +  # Add predicted logit line
  labs(title = paste("Simple Logit of swi on", "eduexp"), x ="eduexp", y = "swi") +
  theme_minimal()

plot_eduexp

ggsave("plot_eduexp.png", plot = plot_eduexp, width = 10, height = 7, dpi = 300, bg = "white")

sglmplots[["eduexp"]] <- plot_eduexp

# year
sglm <- as.formula(paste("swi ~ ","year"))
sglmod <- glm(sglm, data = df0, family = binomial(link = "logit"))
sglmods[["year"]] <- sglmod
sglmod_r[["year"]] <- summary(sglmod)

# Create plot
plot_data <- data.frame(swi = df0$swi, predictor = df0[["year"]])
# Predict the logit values using the fitted model
predicted <- predict(sglmod, type = "response")
plot_year <- ggplot(plot_data, aes(x = predictor, y = swi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +  # Add OLS line
  geom_line(aes(y = predicted), color = "olivedrab") +  # Add predicted logit line
  labs(title = paste("Simple Logit of swi on", "year"), x ="year", y = "swi") +
  theme_minimal()

plot_year

ggsave("plot_year.png", plot = plot_year, width = 10, height = 7, dpi = 300, bg = "white")

sglmplots[["year"]] <- plot_year

# Combine the plots using patchwork
combined_plots <- plot_popu + plot_grppc +
  plot_eduexp + plot_year +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Combined Simple Logit Plots",
                  subtitle = "Logit Switch Year Indicator (swi) on Predictors") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Display the combined plot
combined_plots
ggsave("comb_sglmplot.png", plot = combined_plots, width = 10, height = 7, dpi = 300, bg = "white")
# Still all messed up. Individual fine....

### Logit prediction: swi ~ "popu","grppc","eduexp"
glmod1 <- glm(swi ~ popu + grppc + eduexp, data = df0, family = binomial(link = "logit"))

### Logit prediction: swi ~ "popu","grppc","eduexp","year"
glmod2 <- glm(swi ~ popu + grppc + eduexp + year, data = df0, family = binomial(link = "logit"))

# Combine the models into a single list
all_models <- c(slmods, sglmods, list(glmod1,glmod2))

# Generate the regression table with stargazer using the regression models list
regression_table <- stargazer(list = c(slmods, sglmods, list(glmod1,glmod2)), 
            title = "Regression Results",
            align = TRUE,
            dep.var.labels = c("Switch Year Indicator", "Switch Year Indicator", "Switch Year Indicator"),
            covariate.labels = c("Switch Year", "Population", "GRP per Capita", "Education Expenditure","Characteristic Year"),
            omit.stat = c("LL", "ser", "f"),
            no.space = TRUE,
            out = "regression_table.html",
            type = "html")

# Save the table as HTML
writeLines(regression_table, "regression_table.html")

# df0_8000 <- subset(df0, popu > 8000) # 2015, a bunch of cities are all 9747
# df0_3000 <- subset(df0, popu > 3000) 
# 2004, 2005 eduexp seems weird too, lower than 2003. Okay 2004 prob fixed, copied wrong
# but 2005, most of the entries are 3-digit, but both 2004 and 2006 are 5-digit.
# Go back to original book. Okay fixed 2005's using basic_city_data_2.4 (2004).

#Logit: The coefficients in a logit model represent the change in the log-odds 
#(or equivalently, the odds ratio) of the binary outcome associated with a one-unit 
#change in the predictor, assuming all other predictors are held constant. 
#The coefficients can be more readily interpreted as the effect on the probability 
#of the outcome.

# Coefficients in the regression table are extremely small. I wonder why and if 
# there is anything we can do about it. [0602-15:37]

