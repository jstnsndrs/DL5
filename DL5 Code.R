rm(list = ls())


####Loading the world development dataset into R Studio.
df0 <- read.csv("/Users/justinsaunders/Documents/Kennesaw State University/IS 4550 Data MIning/World_Development_Indicators.csv", header = TRUE, sep = ",")
meta <- read.csv("/Users/justinsaunders/Documents/Kennesaw State University/IS 4550 Data MIning/WDI_Metadata.csv", header = TRUE, sep = ",")


colnames(df0)
library(ggplot2)

# Convert "Country.Code" to factors in df0
df0$Country.Code <- factor(df0$Country.Code)





# Select the relevant attributes for country name and income group categorizations
df1 <- df0 %>%
  select(Country.Code, Adjusted.net.national.income.per.capita..current.US....NY.ADJ.NNTY.PC.CD.)
# Rename the attributes for added clarity
colnames(df1) <- c("Country.Code", "Income.Group")
# Convert "Income.Group" to numeric
df1$Income.Group <- as.numeric(df1$Income.Group)
# Convert "Country.Code" to factors in df1
df1$Country.Code <- factor(df1$Country.Code)
# Histogram of 
ggplot(df1, aes(x = Income.Group)) +
  geom_histogram(fill = "blue", bins = 10) +
  labs(title = "Income Group Histogram", x = "Income Group") +
  theme_minimal()



# Load the necessary dplyr library
library(dplyr)
# Assuming df0$Income.Group contains numeric values representing GNI per capita
# Recategorize the Income.Group variable based on the U.N. recommendations
df1 <- df1 %>%
  mutate(Income.Group = case_when(
    Income.Group < 1036 ~ "Low-Income",
    between(Income.Group, 1036, 4085) ~ "Lower Middle-Income",
    between(Income.Group, 4086, 12615) ~ "Upper Middle-Income",
    Income.Group > 12615 ~ "High-Income",
    TRUE ~ "Uncategorized"  # For any values not falling into the specified ranges
  ))
# Summary of the new income group categories
table(df1$Income.Group)
unique(df1$Income.Group)



install.packages("countrycode")
library(countrycode)
# Map country codes to regions
df1$Region <- countrycode(
  sourcevar = df1$Country.Code,
  origin = "iso3c",
  destination = "region"
)
unique(df1$Region)
table(df1$Region)


# Merge df1 and df0 datasets based on the "Country.Code" column
WorldIndicatorsComplete <- bind_cols(df0, df1, by = "Country.Code")

colnames(WorldIndicatorsComplete)


# Reorder the columns to have "Country.Name," "Income.Group," and "Region" as the first three columns
WorldIndicatorsComplete <- WorldIndicatorsComplete %>%
  select(-Country.Code...53, Country.Code...1, Income.Group, Region, everything())

# Rename the dataset to "WorldIndicatorsComplete"
colnames(WorldIndicatorsComplete)[1] <- "Country.Name"

# Rearrange the columns to have "Country.Name," "Income.Group," and "Region" as the first three columns
WorldIndicatorsComplete <- WorldIndicatorsComplete %>%
  select(Country.Name, Income.Group, Region, everything())



# Export the merged dataset as a CSV file
write.csv(WorldIndicatorsComplete, file = "WorldIndicatorsComplete.csv", row.names = FALSE)

# View the first few rows of the merged dataset
head(WorldIndicatorsComplete)







# Merge df1 and df0 datasets based on the "Country.Code" column
WorldIndicatorsComplete <- cbind(df0, df1)
# Reorder the columns to have "Country.Name," "Income.Group," and "Region" as the first three columns
WorldIndicatorsComplete <- WorldIndicatorsComplete %>%
  select(Country.Code, Income.Group, Region, everything())
# Rename the dataset to "WorldIndicatorsComplete"
colnames(WorldIndicatorsComplete)[1] <- "Country.Name"



# Export the merged dataset as a CSV file
write.csv(WorldIndicatorsComplete, file = "WorldIndicatorsComplete.csv", row.names = FALSE)
# View the first few rows of the merged dataset
head(WorldIndicatorsComplete)





