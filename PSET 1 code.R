---
title: "PSET1"
author: "Gabriele Bortolotti, Natalie Brown, Grant Beard"
date: "1/29/2022"
output: html_document
---
  
  
rm(list = ls())
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("rio")
#install.packages("magrittr")
#install.packages("countrycode")
#install.packages("gridExtra")
library(tidyverse)
library(plotly)
library(rio)
library(magrittr)
library(countrycode)
library(gridExtra)
setwd("~/Documents/Personal/001 Graduate School/Chicago/MPP UChicago/AP II/PSET1")

fhouse <- import("Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx", 
                 sheet = "Country Ratings, Statuses ",
                 na = "-")


#############################
####### Question 2.a ########
#############################

### Cleaning the data by reconfiguring it such that each observation corresponds to one country-year.

# Filter the years 1995 to 2020
## Find the index of 1995
y1995 <- grep("1995-96", colnames(fhouse))-1

# Delete columns 2 till that index
fhouse95 <- fhouse[,-c(2:y1995)]

# Remove the PR column
fhouse95 %<>% select(1, starts_with("..."))

# Remove the "..." prefix
names(fhouse95) <- sub('^...', '', names(fhouse95)) 

# Filter only the status 
fhouse95 %<>% select(everything()[c(TRUE, FALSE)]) 

fhouse95 <- fhouse95[-c(1:2),]

# Rename with year
names(fhouse95) <- c("country", 1995:2020)

# pivot columns to rows
fh <- fhouse95 %>%
  pivot_longer(cols = 2:27, names_to = "year", values_to = "status") 

fh$year <- as.numeric(fh$year)

fh_share <- na.omit(fh) %>% 
  group_by(year, status) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

fh_share_f <- fh_share %>%
  filter(status == "F")

fh_share_nf <- fh_share %>%
  filter(status == "NF")

fh_share_pf <- fh_share %>%
  filter(status == "PF")

n  <- c(colMeans(matrix(fh_share_f$n, nrow=5)), 
        colMeans(matrix(fh_share_pf$n, nrow=5)),
        colMeans(matrix(fh_share_nf$n, nrow=5)))

year <- rep(c(1995, 2000, 2005, 2010, 2015, 2020), 3)
status <- c(rep("Free", 6), rep("Not Free", 6), rep("Partially Free", 6))

fh_2a <- data_frame(year, status, n)





### Line Plot
ggplotly(ggplot(fh_2a, aes(x = year, y = n)) + 
           geom_line(aes(color = factor(status, levels = c("Not Free", "Partially Free", "Free")), linetype = factor(status, levels = c("Not Free", "Partially Free", "Free"))), lwd=1) + 
           theme_bw() + 
           labs(y= "Number", x = "Year", title = "Share of Countries by Status (1995 - 2020)", caption = "Freedom House, Country and Territory Ratings and Statuses, 1973-2021") + 
           scale_linetype_manual("Status", values = c("solid","solid","solid")) + 
           scale_color_manual("Status", values = c("Partially Free" = "#a6cee3", "Not Free" = "#1f78b4", "Free" = "#b2df8a")))

# The data from Freedom House shows a positive trend in between 1995 and 2000, with the number of "Not Free" (NF) and "Partially Free" (PF) countries decreasing, and "Free" (F) countries increasing proportionally. From 2000-2005, while there is still an increase in the number of F countries, we see a slight downtick in PF countries as the number of NF countries increases towards 2005.  Since 2005, we see the wave of democratization reversing itself as there is simultaneously a decrease in the number of F countries and an increase in the number of PF countries while the number of NF countries stays roughly the same. 


#############################
####### Question 2.b ########
#############################

# CLean the data again, this time keeping the CF and PR. 
fh_2b <- fhouse

colnames(fh_2b) <- as.character(unlist(fh_2b[1,]))
fh_2b <- fh_2b[-1, ]

colnames(fh_2b) <- c("country", rep(c(1973:2020), each=3))

# Concatenate Column Name (Year) and the Type
colnames(fh_2b) <- paste(colnames(fh_2b), fh_2b[1, ])
fh_2b <- fh_2b[-1, ]

# Not that our columns are called "[year] PR" "[year] CL" "[year] Status" we can gather them accordingly  
names(fh_2b)[1] <- "country"
fh <- fh_2b %>%
  gather(key, value, 2:145) %>% 
  mutate(column = ifelse(substr(key, 6, 7) == "PR", "PR", # conditinal to 6th and 7th character, we will split them into PR, CL and Status
                       ifelse(substr(key, 6, 7) == "CL", "CL", 
                              ifelse(substr(key, 6, 7) == "St", "Status", NA))),
         year = rep(1973:2020, each = 3*205)) %>%
  select(country, year, key = column, value) 


# Now that we have a clean data set, we will create a new column that averages CF and PR for each country, each year

fh_index <- fh

fh_index %<>% spread(key = key, value = value)

# CL and PR are now characters, so turning them into integeres 
fh_index$CL <- as.numeric(fh_index$CL)
fh_index$PR <- as.numeric(fh_index$PR)

# Adding a column for the Freedom in the World Index (average of CL and PR) then normalize it to a score between 0 and 1 (where larger numbers correspond to more democratic countries)
fh_index %<>% mutate(FiW = (CL + PR)/2) %<>% 
  mutate(FiW_norm = (FiW-7)/(1-7))


### Plotting it visually

# Now we will lag the year to see which country has improved or gotten worse from previous year. Then mutate a variable that has 0 if the country has gotten worse, 1 if improved and NA if it stayed the same. 

fh_index %<>%
    mutate(FiW_norm = as.numeric(FiW_norm),
         year = as.numeric(year)) %>%
  group_by(country) %>%
  arrange(year) %>% 
  mutate(change = FiW_norm - lag(FiW_norm),
         improved = ifelse(change < 0, 0, ifelse(change > 0, 1, NA)))  %>%
  ungroup() %>% 
  select(country, year, CL, PR, Status, FiW_norm, improved) # dropping the columns we don't need

# Organizing the data for the plot
fh_change <- fh_index %>% 
  na.omit() %>%
  group_by(year, improved) %>% 
  count() 

# Renaming the 0s and 1s for clarity purposes
fh_change$improved <- ifelse(fh_change$improved == 0, "Worsen", "Improved")

p2 <- fh_change %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line(aes(color = factor(improved)), size = 1) + 
  scale_x_continuous(breaks = seq(1973, 2020, by = 5)) + 
  theme_bw() +
  labs(y= "Number of Countries", x = "Year", title = "Share of Countries That Became More or Less Democratic") + 
  scale_color_manual("Change", values = c("#1f78b4", "#b2df8a"))

ggplotly(p2)

## From mid-1970s to early 1980s, the number of countries improving their democracies and the number of countries worsening their democracies was relatively on par with one another. From the mid-1980s to late 2000s (with one exception spiking for worsen in 1993), the number of improved countries outpaces those that worsen. However, since the late 2000s, we see the number of countries improving their democracies decreasing fairly sharply  as the number of worsening countries increases. This may suggest that there may be some general degradation of support for democracy and that the processes of democratization and de-democratization aren't necessarily correlated as the actions of one country may not have as much influence over the change improvement / worsening of democracy of other countries.

#############################
####### Question 2.c ########
#############################

## Merging the data between UN and Freedom House, filtering 2005 and 2020

un_data <- read.csv("UNSD — Methodology.csv")

fh_un <- fh_index %>% 
  drop_na(FiW_norm) %>%
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c')) 

fh_un <- merge(fh_un, un_data, by.x = "iso3c", by.y = "ISO.alpha3.Code")

fh_un_2005_2020 <- fh_un %>% filter(year == 2005 | year == 2020)

## Compare Regional Shifts in Status between 2005 and 2020

fh_region <- fh_un_2005_2020 %>%
  group_by(Region.Name, year, Status) %>%
  count()

# Function to make a bar plot stacked for a specific region. 
reg_plot <- function(reg_g){
  fh_region %>% filter(Region.Name == reg_g) %>%
    ggplot(aes(x = factor(year), y = n)) + 
    geom_bar(aes(fill = factor(Status, levels = c("NF", "PF", "F"))), stat='identity', position = "fill", alpha = 0.8) + 
    theme_bw() +
    labs(fill = "Status", x = reg_g, y = "Share") +
    scale_fill_manual(values = c("PF" = "#a6cee3", "NF" = "#1f78b4", "F" = "#b2df8a"), labels = c("Not Free", "Partially Free", "Free"))
}

# Running the function with each region to get a separate graph. 
p_AS <- reg_plot("Asia")
p_AF <- reg_plot("Africa")
p_AM <- reg_plot("Americas")
p_EU <- reg_plot("Europe")
p_OC <- reg_plot("Oceania")

# Create a function to make a standalone legend that apply to all five graphs.
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(p_AS)

# Place the five graphs in the same grid
grid.arrange(p_AS + theme(legend.position = 'none'), p_AF + theme(legend.position = 'none'), p_AM + theme(legend.position = 'none'), p_EU + theme(legend.position = 'none'), p_OC + theme(legend.position = 'none'), legend, 
             ncol=3, nrow=2, widths=c(1/3, 1/3, 1/3))


## This graph paints a grim picture of the state of democracy in 2020. In 15 years, a big share of countries became less democratic, regardless of which region of the world they were in. Oceania was the only region that increased the share of Free countries between 2005 and 2020. Every other region experienced a decrease in the share of Free and / or Partially Free countries. Africa experienced a drop in share of both Free and Partially Free countries, as the share of Not Free countries grew, while similarly share of Asian countries shifted from Partially Free to Not Free.

#############################
####### Question 2.d ########
#############################


# Isolate the least developed countries (LDC)
ldc <- fh_un %>%
  filter(Least.Developed.Countries..LDC. == "x")

# Isolate the rest of the countries (NON_LDC)
non_ldc <- fh_un %>%
  filter(Least.Developed.Countries..LDC. == "")

# Create mean Freedom in the World (FiW) Index by year for LDC
ldc %<>% 
  group_by(year)  %>%
  drop_na(FiW_norm) %>%
  summarise_at(vars(FiW_norm), list(Mean_FiW = mean)) %>% ungroup()

# Create mean Freedom in the World (FiW) Index by year for NON_LDC
non_ldc %<>% 
  group_by(year) %>%
  drop_na(FiW_norm) %>%
  summarise_at(vars(FiW_norm), list(Mean_FiW = mean)) %>% ungroup()

# Merge the two data.sets so we can plot it together
final_data <- data.frame(year = ldc$year, 
                         "Least Developed" = ldc$Mean_FiW,
                         "Developed" = non_ldc$Mean_FiW)

final_data %<>% pivot_longer(cols = 2:3, names_to = "Development", values_to = "FiW_mean") 
final_data$year <- as.numeric(final_data$year)
x1995 <- which(final_data$year == 1994)[2]
final_data %<>% filter(row(final_data) > x1995) # alternative to filter to bypass error

# Plotting the graph and assigning it to a variable p4 
p4 <- final_data %>% 
  ggplot(aes(year, FiW_mean, colour = Development)) + 
  stat_smooth(geom = 'line', alpha = 0.5, se = F) + 
  geom_point(size = 2, alpha = 0.8) + 
  labs(y = "FiW Index", x = "Year")

# Running the graph through plotly so we can have an interactive graph  
ggplotly(p4)


## Overall, this graph shows for least-developed countries (LDCs) and developed countries (non-LDCs) somewhat similar trends in improvements to democracy (on average) over the last 25 years. While the two categories of countries experience similar trends in improvement to democracy over this time period, we see that on average richer countries are more democratic than poorer countries (as shown by the red non-LDC line being consistently above the blue LDC line in the graph). That general difference in average level of democracy between LDCs and non-LDCs, however, does not preclude LDCs and non-LDCs experiencing the same types of forces driving increases and decreases in average FiW index scores; this further supports the notion that democracy has changed roughly the same in LDC and non-LDC countries over the past 25 years.