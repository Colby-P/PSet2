# Problem Set 2

## 2. Correct code (8 changed made)

library(tidyverse)

# open my data
gspace = read_csv('greenspace_data_share.csv') # missing quotation marks (1)

# summarize average urban greenspace by region
tab1 = # Renamed "table" to "tabl" to prevent confusion with tidyverse command (2)
  gspace |>
    group_by(Major_Geo_Region) |> # added a pipe (3)
      summarise(
        obs = n(), # added a comma (4)
        avg = mean(annual_avg_2020), # added an underscore (5)
        weighted_avg = mean(annual_weight_avg_2020) # changed a space to an underscore (6)
      )

# output as table
knitr::kable(tab1, digits = 1) # added "knitr::" (7), changed "gspace" to "tab1" (8)

## 3. Total number of urban areas
dim(gspace)
#str(gspace)
#unique(gspace$City)

## 4. Summarize classification scores for 2021
summary(gspace$annual_avg_2021, digits = 2)
plot(annual_avg_2021 ~ ...1, gspace)
hist(gspace$annual_avg_2021)

## 5. a. Scored High or above for greenspace in 2015
gspace |>
  filter(
    indicator_2015 == 'High' | 
    indicator_2015 == 'Very High'
  ) |>
  count()

## 5. b. Scored Exceptionally Low at any point in the period of record
gspace |>
  filter(
    indicator_2010 == 'Exceptionally Low' | 
    indicator_2015 == 'Exceptionally Low'| 
    indicator_2020 == 'Exceptionally Low' |
    indicator_2021 == 'Exceptionally Low',
    ) |>
  count()

## 5. c. Urban areas in arid climates that became greener from 2010 to 2020 (use annual weighted avg)
gspace |>
  filter(
    Climate_region == 'Arid',
    annual_weight_avg_2020 > annual_weight_avg_2010
  ) |>
  count()

## 6. Urban areas became less green from 2010 to 2021 (use annual avg). Were the changes concentrated in geographical or climatic regions?
gspace |>
  filter(
    annual_avg_2010 > annual_avg_2021
  ) |>
  count()

tab2 =
  gspace |>
    filter(
      annual_avg_2010 > annual_avg_2021
    )
  
tab2 |>
  count(Major_Geo_Region, name = 'Freq')

tab2 |>
  count(Climate_region, name = 'Freq')

## 7. Histogram showing the change in greenspace from 2010 and 2021 (use annual avg)
tab3 =
  gspace |> 
    summarize(
      gspace_2010 = annual_avg_2010,
      gspace_2021 = annual_avg_2021,
      Diff = annual_avg_2021 - annual_avg_2010
    )

hist(tab3$Diff, main = 'Histogram (2010 - 2021)', xlab = 'Change in Greenspace')

## 8. Scatter plot of populated greenscape of 2021 vs. 2010
#plot(annual_weight_avg_2021 ~ annual_weight_avg_2010, gspace)
plot(gspace_2021 ~ gspace_2010, tab3, main = 'Population Weighted Greenspace', xlab = 'Avg 2010', ylab = 'Avg 2021')

## Bonus. Use color coding on cities with Diff > 0 and add 45 degree line to plot
plot(gspace_2021 ~ gspace_2010, tab3, col = ifelse(Diff > 0, 'darkgreen', 'black'), main = 'Population Weighted Greeenspace', xlab = 'Avg 2010', ylab = 'Avg 2021')
abline(0,1, col = 'red')

