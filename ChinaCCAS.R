###
# Name: Cindy Zeng
# Project: CCAS in China (Paper)
# Table: 
#       city_pop_swi: Sampled Cities, Population, and CCAS Establishment Years
# Graphs:
#       h_city_swi_bubble: CCAS Establishment Year (High School)
#       pm_city_swi_bubble: CCAS Establishment Year (Primary and Middle School)
#       Haccnum: Number of City-Level CCAS (High School) by Year
#       Hflow: The Number of New High School CCAS over Time
#       Hfrac: Fraction of City-Level CCAS (High School) by Year
#       PMaccnum: Number of City-Level CCAS (Primary and Middle School) by Year
#       PMflow: The Number of New Primary and Middle School CCAS over Time
#       PMfrac: Fraction of City-Level CCAS (Primary and Middle School) by Year
#       Hmechpie: The Distribution of Mechanisms among High School CCAS
#       --- Not in paper ---
#       Interactive Map: Generate a user-interactive graph showing the age of each city's high school CCAS in 2020
###

library(openxlsx)
library(latticeExtra)
library(Hmisc)
library(sjlabelled)
library(haven)
library(readxl)
library(readr)
library(Matrix)
library(glmnet)
library(ggplot2)
library(gridExtra)
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

library(kableExtra)
library(knitr)
library(ggrepel)
library(hchinamap)
library(rgdal)
library(plyr)
library(sf)
library(terra)
library(maptools)


# Setting the Working Directory
setwd("/Users/cindy/Dropbox/Mac/Desktop/College/Matters/Yale/CCAS/Play")

#### city_pop_swi ####

# Clean enviroment
rm(list=ls())

df <- read_excel("city_pop_swi_sheet.xlsx")

# Add commas to long numbers so that it's three digits, three digits
df$pop <- format(df$pop, big.mark = ",")

df %>%
  kbl(format = "latex",
      # col.names = c("Gender","Education","Count","Mean","Median","SD"),
      align = "l") %>%
  kable_minimal(full_width = F)
  # output of harcoded table is in the console
  # I later manually changed the NA's in the high school switch year on the overleaf
  # appendix table into ≤2022 for all 4 and ≤2023 for Changshu.

#### h_city_swi_bubble ####

# Clean enviroment
rm(list=ls())

df <- read_excel("h_city_swi_bubble_sheet.xlsx")
df$swy_h <- as.numeric(df$swy_h)
df$GRP_2021 <- as.numeric(df$GRP_2021)
df <- df[order(df$GRP_2021), ]

ggplot(df, aes(x = swy_h, y = GRP_2021, size = pop)) +
  geom_point(alpha = 0.4, color = "turquoise") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_text_repel(aes(label = pop), size = 2, box.padding = 0.5) +  
  scale_x_continuous(breaks = seq(2003, 2022, by = 1)) +
  labs(x = "CCAS Establishment Year", y = "Gross Regional Product per Capita (2021)") +
  theme_minimal() +
  scale_size_continuous(
    breaks = c(0, 1000000, 3000000, 5000000, 10000000, 15000000),
    labels = c("<1M", "1M to 3M", "3M to 5M", "5M to 10M", "10M to 15M", ">15M"),
    range = c(1, 30)
  ) +
  theme(legend.position = "none")  # Remove the size legend
ggsave("h_city_swi_bubble.jpg", width = 30, height = 20, units = c("cm"), dpi = 300)

#### pm_city_swi_bubble ####

# Clean enviroment
rm(list=ls())

df <- read_excel("pm_city_swi_bubble_sheet.xlsx")
df$swy_pm <- as.numeric(df$swy_pm)
df$GRP_2021 <- as.numeric(df$GRP_2021)
df <- df[order(df$GRP_2021), ]

ggplot(df, aes(x = swy_pm, y = GRP_2021, size = pop)) +
  geom_point(alpha = 0.4, color = "#c6e273") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_text_repel(aes(label = pop), size = 2, box.padding = 0.5) +  
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  labs(x = "CCAS Establishment Year", y = "Gross Regional Product per Capita (2021)") +
  theme_minimal() +
  scale_size_continuous(
    breaks = c(0, 1000000, 3000000, 5000000, 10000000, 15000000),
    labels = c("<1M", "1M to 3M", "3M to 5M", "5M to 10M", "10M to 15M", ">15M"),
    range = c(1, 30)
  ) +
  theme(legend.position = "none")  # Remove the size legend
ggsave("pm_city_swi_bubble.jpg", width = 30, height = 20, units = c("cm"), dpi = 300)

#### Haccnum: # of existing CCAS by year - line graph ####

file_in <- "H_Dates.xlsx"
Hswy0 <- read_excel(file_in) # 129 cities, I think I already deleted cities with no info

Hswy1 <- data.frame(matrix(ncol = 2, nrow = length(unique(Hswy0$swy))))
colnames(Hswy1) <- c("year", "CCASnum")

Hswy1$year <- unique(Hswy0$swy)
Hswy1 <- Hswy1[order(Hswy1$year), ]

# Count CCAS stock in a given year
Hswy1$CCASnum <- sapply(Hswy1$year, function(x) sum(Hswy0$swy <= x, na.rm = TRUE))

# using library(ggplot2)

# Create the plot using ggplot2
Haccnum <- ggplot(data = Hswy1, aes(x = year, y = CCASnum)) +
  geom_line(color = "dark turquoise", size = 1.5) +
  geom_point(color = "navy", size = 3) +
  scale_x_continuous(breaks = Hswy1$year, labels = Hswy1$year) +
  geom_text(aes(label = CCASnum), vjust = -1, color = "black") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = CCASnum),
               linetype = "dashed", color = "gray") +
  labs(# title = "Number of City-Level CCAS (High School) by Year",
       x = "Year",
       y = "Total Number of CCAS") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
ggsave("Haccnum.png", plot = Haccnum, width = 10, height = 7, dpi = 300, bg = "white")

#### Hflow: # of new CCAS by year ####

Hswy1 <- data.frame(matrix(ncol = 2, nrow = length(unique(Hswy0$swy))))
colnames(Hswy1) <- c("year", "CCASnum")

Hswy1$year <- unique(Hswy0$swy)
Hswy1 <- Hswy1[order(Hswy1$year), ]

# Count CCAS flow in a given year
Hswy1$CCASnum <- sapply(Hswy1$year, function(x) sum(Hswy0$swy == x, na.rm = TRUE))
# Create the plot using ggplot2
Hflow <- ggplot(data = Hswy1, aes(x = year, y = CCASnum)) +
  geom_line(color = "dark turquoise", size = 1.5) +
  geom_point(color = "navy", size = 3) +
  scale_x_continuous(breaks = Hswy1$year, labels = Hswy1$year) +
  geom_text(aes(label = CCASnum), vjust = -1, color = "black") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = CCASnum),
               linetype = "dashed", color = "gray") +
  labs(# title = "Number of City-Level CCAS (High School) by Year",
    x = "Year",
    y = "Number of New CCAS") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
ggsave("Hflow.png", plot = Hflow, width = 10, height = 7, dpi = 300, bg = "white")


#### Hfrac: percentage of existing CCAS by year ####

detach("package:plyr", unload=TRUE) 

# Calculate the counts and percentages
CCAStot <- sum(Hswy0$swy >= 1949 & Hswy0$swy <= 2023)
# Hswy2 <- cbind(Hswy1, CCASprop = Hswy1$CCASnum / CCAStot)
# Find the minimum and maximum values of swy
min_year <- min(Hswy0$swy)
max_year <- max(Hswy0$swy)
# Create a data frame with all combinations of city_c and year
expanded_data <- expand.grid(city_c = unique(Hswy0$city_c), year = min_year:max_year)
# Merge the expanded data frame with the original data frame
Hswy2 <- merge(expanded_data, Hswy0, by = "city_c", all.x = TRUE)
# Sort the data frame by year
Hswy2 <- Hswy2[order(Hswy2$year), ]
# Reset row names
rownames(Hswy2) <- NULL
# Creat the condition column for stack bar plot
Hswy2$cond <- ifelse(Hswy2$swy <= Hswy2$year, "CCAS", "No CCAS")
# num = number of cities with the same (cond) in that given year.
Hswy2 <- Hswy2 %>%
  group_by(year, cond) %>%
  mutate(num = n()) %>%
  ungroup()
# mutate so that in the stacked bar graph, no CCAS is on top of CCAS
Hswy2 <- Hswy2 %>%
  mutate(cond = factor(cond, levels = c("No CCAS", "CCAS")))
# create a new column "CCASnum": the number of "CCAS" in a given year
Hswy2$CCASnum <- ave(Hswy2$cond == "CCAS", Hswy2$year, FUN = sum)
Hswy2$CCAStot <- CCAStot

if (1==0){
  
  ## I know where the problem is now. In Hswy1, not all years from 2003 to 2022 are
  ## included. I need to fill that in. Um, problem still  unsolved. I just can't 
  ## label a stacked bar chart!!!
  
  # Find the missing years
  missing_years <- setdiff(min(Hswy1$year):max(Hswy1$year), Hswy1$year)
  # Create a new data frame for the missing years
  missing_data <- data.frame(year = missing_years)
  # Find the maximum year smaller than each missing year
  max_year_smaller <- sapply(missing_years, function(y) max(Hswy1$year[Hswy1$year < y]))
  # Fill in the values for the missing variables
  missing_columns <- setdiff(colnames(Hswy1), colnames(missing_data))
  for (col in missing_columns) {
    missing_data[[col]] <- Hswy1[[col]][match(max_year_smaller, Hswy1$year)]
  }
  # Bind the missing data to Hswy1
  Hswy1 <- rbind(Hswy1, missing_data)
  Hswy1 <- Hswy1[order(Hswy1$year),]
  Hswy1$CCAStot <- CCAStot
  Hswy1$num <- Hswy1$CCASnum
  rownames(Hswy1) <- NULL
}

Hfrac <- ggplot(Hswy2, aes(fill = cond, x = year, y = num)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("navy", "turquoise"), labels = c("No CCAS", "CCAS"), name = "CCAS status") +
  labs(# title = "Percentage of City-Level CCAS (High School) by Year",
       x = "Year", y = "Fraction of Cities") +
  theme_minimal() +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray")

# Adjust the plot margins to fit the text labels
g <- ggplotGrob(last_plot())
g$layout$clip[g$layout$name == "panel"] <- "off"
grid::grid.draw(g)

ggsave("Hfrac.png", plot = Hfrac, width = 10, height = 7, dpi = 300, bg = "white")

### PM

#### PMaccnum: # of existing CCAS by year - line graph ####

file_in <- "PM_Dates.xlsx"
PMswy0 <- read_excel(file_in) # 135 cities, I have not deleted cities with no info
PMswy0 <- PMswy0[!is.na(PMswy0$swy), , drop = FALSE] 
# 129 cities. No dropping dimensions, ensures that the result remains a data 
# frame, even if it has a single row or column

PMswy1 <- data.frame(matrix(ncol = 2, nrow = length(unique(PMswy0$swy))))
colnames(PMswy1) <- c("year", "CCASnum")

PMswy1$year <- unique(PMswy0$swy)
PMswy1 <- PMswy1[PMswy1$year != "Non", ] # delete those without CCAS
PMswy1$year <- as.numeric(PMswy1$year)
PMswy1 <- PMswy1[order(PMswy1$year), ] # 115 cities

# Count CCAS stock in a given year
PMswy1$CCASnum <- sapply(PMswy1$year, function(x) sum(PMswy0$swy <= x, na.rm = TRUE))

# using library(ggplot2)

# Create the plot using ggplot2
PMaccnum <- ggplot(data = PMswy1, aes(x = year, y = CCASnum)) +
  geom_line(color = "#c6e273", size = 1.5) +
  geom_point(color = "#568203", size = 3) +
  scale_x_continuous(breaks = PMswy1$year, labels = PMswy1$year) +
  geom_text(aes(label = CCASnum), vjust = -1, color = "black") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = CCASnum),
               linetype = "dashed", color = "gray") +
  labs(# title = "Number of City-Level CCAS (Primary and Middle School) by Year",
       x = "Year",
       y = "Total Number of CCAS") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
ggsave("PMaccnum.png", plot = PMaccnum, width = 10, height = 7, dpi = 300, bg = "white")

#### PMflow: # of new CCAS by year ####

# Count CCAS stock in a given year
PMswy1$CCASnum <- sapply(PMswy1$year, function(x) sum(PMswy0$swy == x, na.rm = TRUE))

# using library(ggplot2)

# Create the plot using ggplot2
PMflow <- ggplot(data = PMswy1, aes(x = year, y = CCASnum)) +
  geom_line(color = "#c6e273", size = 1.5) +
  geom_point(color = "#568203", size = 3) +
  scale_x_continuous(breaks = PMswy1$year, labels = PMswy1$year) +
  geom_text(aes(label = CCASnum), vjust = -1, color = "black") +
  geom_segment(aes(x = year, xend = year, y = 0, yend = CCASnum),
               linetype = "dashed", color = "gray") +
  labs(# title = "Number of City-Level CCAS (Primary and Middle School) by Year",
    x = "Year",
    y = "Number of New CCAS") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))
ggsave("PMflow.png", plot = PMflow, width = 10, height = 7, dpi = 300, bg = "white")

#### PMfrac: percentage of existing CCAS by year ####

PMswy0$swy <- as.numeric(PMswy0$swy)
PMswy0 <- PMswy0[!is.na(PMswy0$swy), , drop = FALSE] 
# Calculate the counts and percentages
CCAStot <- sum(PMswy0$swy >= 1949 & PMswy0$swy <= 2023)
# Hswy2 <- cbind(Hswy1, CCASprop = Hswy1$CCASnum / CCAStot)
# Find the minimum and maximum values of swy
PMswy0$swy <- as.numeric(PMswy0$swy)
min_year <- min(PMswy0$swy)
max_year <- max(PMswy0$swy)

# Create a data frame with all combinations of city_c and year
expanded_data <- expand.grid(city_c = unique(PMswy0$city_c), year = min_year:max_year)
# Merge the expanded data frame with the original data frame
PMswy2 <- merge(expanded_data, PMswy0, by = "city_c", all.x = TRUE)
# Sort the data frame by year
PMswy2 <- PMswy2[order(PMswy2$year), ]
# Reset row names
rownames(PMswy2) <- NULL
# Creat the condition column for stack bar plot
PMswy2$cond <- ifelse(PMswy2$swy <= PMswy2$year, "CCAS", "No CCAS")
# num = number of cities with the same (cond) in that given year.

# detach("package:plyr", unload=TRUE) 
# MUST UNLOAD plyr before the following operation or else mutate(num = n()) would report error.
PMswy2 <- PMswy2 %>%
  group_by(year, cond) %>%
  mutate(num = n()) %>%
  ungroup()
# mutate so that in the stacked bar graph, no CCAS is on top of CCAS
PMswy2 <- PMswy2 %>%
  mutate(cond = factor(cond, levels = c("No CCAS", "CCAS")))
# create a new column "CCASnum": the number of "CCAS" in a given year
PMswy2$CCASnum <- ave(PMswy2$cond == "CCAS", PMswy2$year, FUN = sum)
PMswy2$CCAStot <- CCAStot

PMfrac <- ggplot(PMswy2, aes(fill = cond, x = year, y = num)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("#568203", "#c6e273"), labels = c("No CCAS", "CCAS"), name = "CCAS status") +
  labs(# title = "Percentage of City-Level CCAS (Primary and Middle School) by Year",
       x = "Year", y = "Fraction of Cities") +
  theme_minimal() +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray")

# Adjust the plot margins to fit the text labels
g <- ggplotGrob(last_plot())
g$layout$clip[g$layout$name == "panel"] <- "off"
grid::grid.draw(g)

ggsave("PMfrac.png", plot = PMfrac, width = 10, height = 7, dpi = 300, bg = "white")

## put Haccnum.png, PMaccnum.png, Hfrac.png, PMfrac.png together in one big picture.

combined_plot <- Haccnum + PMaccnum + Hfrac + PMfrac 
combined_plot
ggsave("Comb1.png", plot = combined_plot, width = 20, height = 14, dpi = 300, bg = "white")


#### h_mech_pie ####

# Clean enviroment
rm(list=ls())

df <- read_excel("h_mech.xlsx")

# Define custom colors
colors <- c("#E6FFFF", "#B2FFFF", "#80FFFF", "#4DFFFF", "#00CCCC", "#008080")

# Calculate percentages and absolute numbers
mech_counts <- table(df$mech)
total_mechs <- sum(mech_counts)
percentages <- round(prop.table(mech_counts) * 100, 2)

# Create label_df dataframe
  # Use = instead of <- to assign values to columns in df
label_df <- data.frame(
  mech_n = c(1,2,3,4,5,6),
  mech_c = as.vector(mech_counts),
  mech_t = c("Boston", "SD+Boston", "SD/Chinese parallel", "SD+Adjustable", "Adjustable", "DA"),
  mech_p = as.vector(percentages)
)
# Reorder mech_t according to mech_n so that the pie slices are in the order of mech_n
# (from light turquoise to dark turquoise).
label_df$mech_t <- factor(label_df$mech_t, levels = label_df$mech_t[order(label_df$mech_n)])

# Create the pie chart with the desired order
pie_chart <- ggplot(label_df, aes(x = "", y = mech_c, fill = mech_t)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors, name = "Mechanisms", breaks = label_df$mech_t) +
  geom_text(aes(label = paste(mech_t, "\n", mech_c, " (", mech_p, "%)")),
            position = position_stack(vjust = 0.5),
            angle = 30, hjust = 0.5, size = 3.5) +
  theme_void() +
  theme(legend.position = "right")
print(pie_chart)
ggsave("Hmechpie.png", plot = pie_chart, width = 10, height = 7, dpi = 300, bg = "white")

#### Interactive Map: Generate a user-interactive graph showing the age of each city's high school CCAS in 2020  ####

# https://cran.r-project.org/web/packages/leafletCN/leafletCN.pdf
# https://rstudio.github.io/leaflet/showhide.html
# https://r-graph-gallery.com/325-background-map-from-geojson-format-in-r.html

library(htmltools)
library(magrittr)
library(sp)
library(jsonlite)
library(leaflet)
library(rgeos)
library(leafletCN)
library(shiny)
library(htmlwidgets)

demomap("city")

dat = data.frame(name = regionNames("china"),
                 value = runif(34))
geojsonMap(dat,"china")
dat$value2 = cut(dat$value, c(0, 0.25, 0.5, 1))
geojsonMap(dat,"china",
           namevar = ~name,
           valuevar = ~value2,
           palette="Reds",
           colorMethod="factor")
geojsonMap(dat,"china",
           namevar = ~name,
           valuevar = ~value2,
           palette = topo.colors(3),
           colorMethod="factor")

if(require(leaflet)){
  dat = data.frame(regionNames("china"),
                   runif(34))
  map = leafletGeo("china", dat)
  pal <- colorNumeric(
    palette = "Blues",
    domain = map$value)
  leaflet(map) %>% addTiles() %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 1,
                fillOpacity = 0.7,
                weight = 1,
                color = ~pal(value),
                popup = ~htmltools::htmlEscape(popup)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~value,
              title = "legendTitle",
              labFormat = leaflet::labelFormat(prefix = ""),
              opacity = 1)
}

Hswy3 <- Hswy2
Hswy3$CCASage <- pmax(0, Hswy3$year - Hswy3$swy)
Hswy_test <- Hswy3[Hswy3$year == 2020,]
Hswy_test$city_c <- gsub("市", "", Hswy_test$city_c)
Hswy_test$popup_content <- paste(Hswy_test$city_c, ": ", Hswy_test$CCASage, "years old in", Hswy_test$year)

print(class(Hswy_test)) # [1] "tbl_df"     "tbl"        "data.frame"  1st time
Hswy_test <- as.data.frame(Hswy_test) # [1] "data.frame"

Agein2020 <- geojsonMap(Hswy_test, "city", namevar = ~city_c, valuevar = ~CCASage,
                        palette = "YlOrRd", colorMethod = "numeric",
                        na.color = "#808080", popup = Hswy_test$popup_content, 
                        stroke = TRUE, smoothFactor = 1, weight = 1, fillOpacity = 0.7,
                        legendTitle = paste("CCAS Age in", Hswy_test[1,"year"]))
Agein2020
saveWidget(Agein2020, file = "Agein2020.html")
