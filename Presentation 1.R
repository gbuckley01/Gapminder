# Setting the library
library(dplyr)
library(ggplot2)
library(stringr)

###QUESTION 1

# Read the data from CSV files
lex <- read.csv("lex.csv")
gdp <- read.csv("gdp_pcap.csv")


# We want to analyze the relationship between GDP and life expectancy, 
#we'll select a recent year that likely has reliable data for both metrics. 
#Let's use the year 2019

# Process data for 2019
lex_2019 <- lex %>% 
  select(country, X2019) %>% 
  rename(life_expectancy = X2019)

gdp_2019 <- gdp %>% 
  select(country, X2019) %>% 
  rename(gdp_pcap = X2019) %>%
  mutate(gdp_pcap = as.numeric(str_remove(gdp_pcap, '[KkMm]')) * 
           ifelse(str_detect(gdp_pcap, 'M'), 1e6, ifelse(str_detect(gdp_pcap, 'k'), 1e3, 1)))

# Merge the datasets on country for the year 2019
data_2019 <- merge(lex_2019, gdp_2019, by = "country")

##DATA CLEANING AND PROCESSING

# Computing average values and standard deviations
mean_life_expectancy <- mean(data_2019$life_expectancy, na.rm = TRUE)
sd_life_expectancy <- sd(data_2019$life_expectancy, na.rm = TRUE)

mean_gdp_pcap <- mean(data_2019$gdp_pcap, na.rm = TRUE)
sd_gdp_pcap <- sd(data_2019$gdp_pcap, na.rm = TRUE)






# Removing outliers more than 2 standard deviations from the mean
data_2019 <- data_2019 %>%
  filter(between(life_expectancy, mean_life_expectancy - 2 * sd_life_expectancy, mean_life_expectancy + 2 * sd_life_expectancy) &
           between(gdp_pcap, mean_gdp_pcap - 2 * sd_gdp_pcap, mean_gdp_pcap + 2 * sd_gdp_pcap))

##VISUALIZING THE DATA
# Scatter plot to explore the relationship between life expectancy and GDP per capita
ggplot(data_2019, aes(x = gdp_pcap, y = life_expectancy)) +
  geom_point() +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Life Expectancy vs. GDP per Capita in 2019",
       x = "GDP per Capita",
       y = "Life Expectancy") +
  theme_minimal()


# Categorizing GDP per capita for further analysis
data_2019$gdp_category <- cut(data_2019$gdp_pcap,
                              breaks = quantile(data_2019$gdp_pcap, probs = c(0, 0.33, 0.66, 1)),
                              labels = c("Low", "Medium", "High"),
                              include.lowest = TRUE)

# Boxplot to compare life expectancy across different GDP per capita categories
ggplot(data_2019, aes(x = gdp_category, y = life_expectancy, fill = gdp_category)) +
  geom_boxplot() +
  labs(title = "Life Expectancy by GDP per Capita Category in 2019",
       x = "GDP per Capita Category",
       y = "Life Expectancy") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")

###QUESTION 2

# Reading and defining the tables we're using
co2 <- read.csv("co2_pcap_cons.csv")
lex <- read.csv("lex.csv")
continent <- read.csv("continents.csv")

# Pulling the columns we want to focus on from the tables above
lex_2019 <- lex[, c("country", "X2019")]
co2_2019 <- co2[, c("country", "X2019")]
continents <- continent[, c("Continent_Name", "Country_Name")]

continents$country <- str_split_i(continents$Country_Name, ",", 1)

# Step 1: Only keep 'Continent_Name' and 'country'
continents_small <- continents[, c("Continent_Name", "country")]

# Merging the 2 subsetted tables together
lex_co2 <- merge(lex_2019, co2_2019,
                 by = "country")  # Assuming "country" is present in both data frames
lex_co2_continents <- merge(lex_co2, continents_small,
                            by = "country")  # Assuming "country" is present in both data frames

# Redefining the column names in the table
colnames(lex_co2_continents) <- c("country", "lex", "co2", "Continent_Name")

# Plotting using ggplot
ggplot(lex_co2_continents, aes(x = co2, y = lex, col = Continent_Name)) + geom_point()


