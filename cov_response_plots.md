---
title: "Response to covid-19"
date: '2022-03-08'
output:
  html_document:
    theme: spacelab
    toc: no
    keep_md: yes 
---



## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
library(here)
```

```
## here() starts at C:/Users/Tiffany Zin/Desktop/Personal Repository/gov_responses_covid
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.6     v dplyr   1.0.8
## v tidyr   1.2.0     v stringr 1.4.0
## v readr   2.1.2     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(skimr)
library(naniar)
```

```
## 
## Attaching package: 'naniar'
```

```
## The following object is masked from 'package:skimr':
## 
##     n_complete
```

```r
library(purrr)
library(ggplot2)
library(tidyr)
library(visdat)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(skimr)
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
```

```
## The following object is masked from 'package:graphics':
## 
##     box
```

```r
library(ggVennDiagram)
library(RColorBrewer)
```


```r
options(scipen = 999)
```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


```r
covid <- readr::read_csv("Gov_Responses2Covid19_last.csv")
```

```
## Rows: 62700 Columns: 43
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## chr  (5): country, geoid, iso, d, continent
## dbl (38): cases, deaths, school, school_local, domestic, domestic_local, tra...
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
covid
```

```
## # A tibble: 62,700 x 43
##    country geoid iso   d         cases deaths school school_local domestic
##    <chr>   <chr> <chr> <chr>     <dbl>  <dbl>  <dbl>        <dbl>    <dbl>
##  1 Aruba   AW    ABW   1-Jan-20      0      0      0            0       NA
##  2 Aruba   AW    ABW   2-Jan-20      0      0      0            0       NA
##  3 Aruba   AW    ABW   3-Jan-20      0      0      0            0       NA
##  4 Aruba   AW    ABW   4-Jan-20      0      0      0            0       NA
##  5 Aruba   AW    ABW   5-Jan-20      0      0      0            0       NA
##  6 Aruba   AW    ABW   6-Jan-20      0      0      0            0       NA
##  7 Aruba   AW    ABW   7-Jan-20      0      0      0            0       NA
##  8 Aruba   AW    ABW   8-Jan-20      0      0      0            0       NA
##  9 Aruba   AW    ABW   9-Jan-20      0      0      0            0       NA
## 10 Aruba   AW    ABW   10-Jan-20     0      0      0            0       NA
## # ... with 62,690 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, ...
```


```r
summary(covid)
```

```
##    country             geoid               iso                 d            
##  Length:62700       Length:62700       Length:62700       Length:62700      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##      cases             deaths            school       school_local  
##  Min.   :    0.0   Min.   :   0.00   Min.   :0.000   Min.   :0.000  
##  1st Qu.:    0.0   1st Qu.:   0.00   1st Qu.:0.000   1st Qu.:0.000  
##  Median :    1.0   Median :   0.00   Median :1.000   Median :0.000  
##  Mean   :  590.9   Mean   :  17.66   Mean   :0.539   Mean   :0.132  
##  3rd Qu.:   83.0   3rd Qu.:   1.00   3rd Qu.:1.000   3rd Qu.:0.000  
##  Max.   :97894.0   Max.   :4928.00   Max.   :1.000   Max.   :1.000  
##  NA's   :4950      NA's   :4950      NA's   :16942   NA's   :16942  
##     domestic     domestic_local      travel     travel_partial    travel_dom   
##  Min.   :0.000   Min.   :0.000   Min.   :0.0    Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0    1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :1.0    Median :0.000   Median :0.000  
##  Mean   :0.196   Mean   :0.104   Mean   :0.6    Mean   :0.229   Mean   :0.213  
##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:1.0    3rd Qu.:0.000   3rd Qu.:0.000  
##  Max.   :1.000   Max.   :1.000   Max.   :1.0    Max.   :1.000   Max.   :1.000  
##  NA's   :7425    NA's   :7425    NA's   :7425   NA's   :7425    NA's   :7425   
##  travel_dom_partial      curf        curf_partial        mass      
##  Min.   :0.000      Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000      1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000      Median :0.000   Median :0.000   Median :1.000  
##  Mean   :0.121      Mean   :0.212   Mean   :0.054   Mean   :0.561  
##  3rd Qu.:0.000      3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:1.000  
##  Max.   :1.000      Max.   :1.000   Max.   :1.000   Max.   :1.000  
##  NA's   :7425       NA's   :7425    NA's   :7425    NA's   :7425   
##   mass_partial       elect       elect_partial       sport      
##  Min.   :0.000   Min.   :0.0     Min.   :0.00    Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:1.0     1st Qu.:0.00    1st Qu.:0.000  
##  Median :0.000   Median :1.0     Median :1.00    Median :1.000  
##  Mean   :0.124   Mean   :0.8     Mean   :0.56    Mean   :0.511  
##  3rd Qu.:0.000   3rd Qu.:1.0     3rd Qu.:1.00    3rd Qu.:1.000  
##  Max.   :1.000   Max.   :1.0     Max.   :1.00    Max.   :1.000  
##  NA's   :7425    NA's   :47866   NA's   :47866   NA's   :7425   
##  sport_partial        rest         rest_local      testing     testing_narrow 
##  Min.   :0.000   Min.   :0.000   Min.   :0.00   Min.   :0.00   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.00   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.00   Median :0.00   Median :0.000  
##  Mean   :0.158   Mean   :0.364   Mean   :0.18   Mean   :0.31   Mean   :0.219  
##  3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:0.00   3rd Qu.:1.00   3rd Qu.:0.000  
##  Max.   :1.000   Max.   :1.000   Max.   :1.00   Max.   :1.00   Max.   :1.000  
##  NA's   :7425    NA's   :7425    NA's   :7425   NA's   :7425   NA's   :7425   
##      masks       masks_partial    surveillance   surveillance_partial
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000       
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000       
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000       
##  Mean   :0.405   Mean   :0.134   Mean   :0.161   Mean   :0.084       
##  3rd Qu.:1.000   3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.000       
##  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :1.000       
##  NA's   :11241   NA's   :11241   NA's   :7425    NA's   :7425        
##      state      state_partial        cash            wage           credit     
##  Min.   :0.00   Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.00   Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.33   Mean   :0.027   Mean   :0.407   Mean   :0.417   Mean   :0.407  
##  3rd Qu.:1.00   3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:1.000  
##  Max.   :1.00   Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :1.000  
##  NA's   :7425   NA's   :7425    NA's   :8250    NA's   :8250    NA's   :8250   
##       taxc            taxd           export           rate      
##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.000  
##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000  
##  Median :0.000   Median :0.000   Median :0.000   Median :0.000  
##  Mean   :0.381   Mean   :0.415   Mean   :0.201   Mean   :0.468  
##  3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:0.000   3rd Qu.:1.000  
##  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :1.000  
##  NA's   :8250    NA's   :8250    NA's   :8250    NA's   :8250   
##  Rigidity_Public_Health Economic_Measures population_2019     
##  Min.   :0.000          Min.   :0.000     Min.   :       815  
##  1st Qu.:0.000          1st Qu.:0.000     1st Qu.:    889955  
##  Median :0.318          Median :0.429     Median :   6950000  
##  Mean   :0.301          Mean   :0.385     Mean   :  36560277  
##  3rd Qu.:0.500          3rd Qu.:0.714     3rd Qu.:  26000000  
##  Max.   :0.885          Max.   :1.000     Max.   :1400000000  
##  NA's   :7425           NA's   :8250      NA's   :4950        
##   continent        
##  Length:62700      
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
glimpse(covid)
```

```
## Rows: 62,700
## Columns: 43
## $ country                <chr> "Aruba", "Aruba", "Aruba", "Aruba", "Aruba", "A~
## $ geoid                  <chr> "AW", "AW", "AW", "AW", "AW", "AW", "AW", "AW",~
## $ iso                    <chr> "ABW", "ABW", "ABW", "ABW", "ABW", "ABW", "ABW"~
## $ d                      <chr> "1-Jan-20", "2-Jan-20", "3-Jan-20", "4-Jan-20",~
## $ cases                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ deaths                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ school                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ school_local           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ domestic               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ domestic_local         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ travel                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ travel_partial         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ travel_dom             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ travel_dom_partial     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ curf                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ curf_partial           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ mass                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ mass_partial           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ elect                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ elect_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ sport                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ sport_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ rest                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ rest_local             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ testing                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ testing_narrow         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ masks                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ masks_partial          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ surveillance           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ surveillance_partial   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ state                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ state_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ cash                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ wage                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ credit                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ taxc                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ taxd                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ export                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ rate                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ Rigidity_Public_Health <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
## $ Economic_Measures      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
## $ population_2019        <dbl> 106310, 106310, 106310, 106310, 106310, 106310,~
## $ continent              <chr> "America", "America", "America", "America", "Am~
```


```r
covid <- covid %>%
  rename(cov_date= d)
covid
```

```
## # A tibble: 62,700 x 43
##    country geoid iso   cov_date  cases deaths school school_local domestic
##    <chr>   <chr> <chr> <chr>     <dbl>  <dbl>  <dbl>        <dbl>    <dbl>
##  1 Aruba   AW    ABW   1-Jan-20      0      0      0            0       NA
##  2 Aruba   AW    ABW   2-Jan-20      0      0      0            0       NA
##  3 Aruba   AW    ABW   3-Jan-20      0      0      0            0       NA
##  4 Aruba   AW    ABW   4-Jan-20      0      0      0            0       NA
##  5 Aruba   AW    ABW   5-Jan-20      0      0      0            0       NA
##  6 Aruba   AW    ABW   6-Jan-20      0      0      0            0       NA
##  7 Aruba   AW    ABW   7-Jan-20      0      0      0            0       NA
##  8 Aruba   AW    ABW   8-Jan-20      0      0      0            0       NA
##  9 Aruba   AW    ABW   9-Jan-20      0      0      0            0       NA
## 10 Aruba   AW    ABW   10-Jan-20     0      0      0            0       NA
## # ... with 62,690 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, ...
```


```r
covid_clean <- covid %>%
  mutate(cov_date = dmy(cov_date))
covid_clean
```

```
## # A tibble: 62,700 x 43
##    country geoid iso   cov_date   cases deaths school school_local domestic
##    <chr>   <chr> <chr> <date>     <dbl>  <dbl>  <dbl>        <dbl>    <dbl>
##  1 Aruba   AW    ABW   2020-01-01     0      0      0            0       NA
##  2 Aruba   AW    ABW   2020-01-02     0      0      0            0       NA
##  3 Aruba   AW    ABW   2020-01-03     0      0      0            0       NA
##  4 Aruba   AW    ABW   2020-01-04     0      0      0            0       NA
##  5 Aruba   AW    ABW   2020-01-05     0      0      0            0       NA
##  6 Aruba   AW    ABW   2020-01-06     0      0      0            0       NA
##  7 Aruba   AW    ABW   2020-01-07     0      0      0            0       NA
##  8 Aruba   AW    ABW   2020-01-08     0      0      0            0       NA
##  9 Aruba   AW    ABW   2020-01-09     0      0      0            0       NA
## 10 Aruba   AW    ABW   2020-01-10     0      0      0            0       NA
## # ... with 62,690 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, ...
```

```r
covid_clean %>%
  group_by(country) %>%
  summarise(sum_cases = sum(cases),
            sum_deaths= sum(deaths)
            ) %>%
  ggplot(aes(x=sum_cases, y=sum_deaths))+
  geom_point()+
  geom_smooth(method = lm, se=F)+
  scale_x_log10()+
  scale_y_log10()+
  labs(title = "Number of Deaths vs. Cases for each Country",
       x= "Number of Cases (log10 scaled)",
       y= "Number of Deaths (log10 scaled)")
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## Warning: Transformation introduced infinite values in continuous x-axis
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 39 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 18 rows containing missing values (geom_point).
```

![](cov_response_plots_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


#Top 5 and Worst 5 Countries looking at cases & deaths

```r
covid_cases <- covid_clean %>%
  select(country, cases) %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))
covid_cases
```

```
## # A tibble: 228 x 2
##    country                  total_cases
##    <chr>                          <dbl>
##  1 United States of America     7233043
##  2 India                        6312584
##  3 Brazil                       4810935
##  4 Russia                       1176286
##  5 Colombia                      829679
##  6 Peru                          814829
##  7 Mexico                        789286
##  8 Spain                         780777
##  9 Argentina                     750988
## 10 South Africa                  674339
## # ... with 218 more rows
```


```r
covid_deaths <- covid_clean %>%
  select(country, deaths) %>%
  group_by(country) %>%
  summarise(total_deaths = sum(deaths)) %>%
  arrange(desc(total_deaths))
covid_deaths
```

```
## # A tibble: 228 x 2
##    country                  total_deaths
##    <chr>                           <dbl>
##  1 United States of America       206928
##  2 Brazil                         143952
##  3 India                           98678
##  4 Mexico                          77646
##  5 United Kingdom                  42143
##  6 Italy                           35956
##  7 Spain                           35813
##  8 Peru                            32463
##  9 France                          31956
## 10 Iran                            26169
## # ... with 218 more rows
```
## Most cases

```r
most_cases <- covid_cases %>%
  top_n(10, total_cases)
most_cases_vec <- most_cases %>%
  pull(country)
most_cases_vec
```

```
##  [1] "United States of America" "India"                   
##  [3] "Brazil"                   "Russia"                  
##  [5] "Colombia"                 "Peru"                    
##  [7] "Mexico"                   "Spain"                   
##  [9] "Argentina"                "South Africa"
```
## Most Deaths

```r
most_deaths <- covid_deaths %>%
   top_n(10, total_deaths)
most_deaths_vec <- most_deaths %>%
  pull(country)
most_deaths_vec
```

```
##  [1] "United States of America" "Brazil"                  
##  [3] "India"                    "Mexico"                  
##  [5] "United Kingdom"           "Italy"                   
##  [7] "Spain"                    "Peru"                    
##  [9] "France"                   "Iran"
```
## Least cases

```r
least_cases <- covid_cases %>%
  top_n(-10, total_cases)
least_cases_vec <- least_cases %>%
  pull(country)
least_cases_vec
```

```
##  [1] "New Caledonia"               "Saint Lucia"                
##  [3] "Grenada"                     "Laos"                       
##  [5] "Saint Kitts and Nevis"       "Greenland"                  
##  [7] "Falkland_Islands_(Malvinas)" "Holy See (the)"             
##  [9] "Anguilla"                    "Marshall Islands"           
## [11] "Solomon Islands"
```

## Least Deaths

```r
least_deaths <- covid_deaths %>%
  top_n(-10, total_deaths)
least_deaths_vec <- least_deaths %>%
  pull(country)
least_deaths_vec
```

```
##  [1] "Anguilla"                         "Bhutan"                          
##  [3] "Cambodia"                         "Dominica"                        
##  [5] "Eritrea"                          "Falkland_Islands_(Malvinas)"     
##  [7] "Faroe Islands"                    "Gibraltar"                       
##  [9] "Greenland"                        "Grenada"                         
## [11] "Holy See (the)"                   "Laos"                            
## [13] "Marshall Islands"                 "Mongolia"                        
## [15] "New Caledonia"                    "Saint Kitts and Nevis"           
## [17] "Saint Lucia"                      "Saint Vincent and the Grenadines"
## [19] "Seychelles"                       "Solomon Islands"                 
## [21] "Timor-Leste"
```

```r
most_list <- list(most_cases_vec, most_deaths_vec)
ggVennDiagram(most_list, 
              category.names= c("Most Cases", "Most Deaths"),
              label_color = "red3")+
  scale_fill_distiller(palette = "Blues")+
  labs(title= "Overlap between Countries with Highest Number of Cases and Deaths")
```

![](cov_response_plots_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
least_list <- list(least_cases_vec, least_deaths_vec)
ggVennDiagram(least_list, 
              category.names= c("Least Cases", "Least Deaths"),
              label_color = "red3")+
  scale_fill_distiller(palette = "Blues")+
  labs(title= "Overlap between Countries with Lowest Number of Cases and Deaths")
```

![](cov_response_plots_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
most_cases %>%
  top_n(5, total_cases)
```

```
## # A tibble: 5 x 2
##   country                  total_cases
##   <chr>                          <dbl>
## 1 United States of America     7233043
## 2 India                        6312584
## 3 Brazil                       4810935
## 4 Russia                       1176286
## 5 Colombia                      829679
```

```r
least_cases %>%
  top_n(-5, total_cases)
```

```
## # A tibble: 5 x 2
##   country                     total_cases
##   <chr>                             <dbl>
## 1 Falkland_Islands_(Malvinas)          13
## 2 Holy See (the)                       12
## 3 Anguilla                              3
## 4 Marshall Islands                      0
## 5 Solomon Islands                       0
```


```r
most_cases_5 <- covid_clean %>%
  filter(country== "United States of America"|country== "India"|country== "Brazil"|country== "Russia"|country== "Colombia")
most_cases_5
```

```
## # A tibble: 1,375 x 43
##    country geoid iso   cov_date   cases deaths school school_local domestic
##    <chr>   <chr> <chr> <date>     <dbl>  <dbl>  <dbl>        <dbl>    <dbl>
##  1 Brazil  BR    BRA   2020-01-01     0      0      0            0        0
##  2 Brazil  BR    BRA   2020-01-02     0      0      0            0        0
##  3 Brazil  BR    BRA   2020-01-03     0      0      0            0        0
##  4 Brazil  BR    BRA   2020-01-04     0      0      0            0        0
##  5 Brazil  BR    BRA   2020-01-05     0      0      0            0        0
##  6 Brazil  BR    BRA   2020-01-06     0      0      0            0        0
##  7 Brazil  BR    BRA   2020-01-07     0      0      0            0        0
##  8 Brazil  BR    BRA   2020-01-08     0      0      0            0        0
##  9 Brazil  BR    BRA   2020-01-09     0      0      0            0        0
## 10 Brazil  BR    BRA   2020-01-10     0      0      0            0        0
## # ... with 1,365 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, ...
```


```r
least_cases_5 <- covid_clean %>%
  filter(country== "Solomon Islands"|country== "Marshall Islands"|country== "Anguilla"|country== "Laos"|country== "Saint Kitts and Nevis")
least_cases_5
```

```
## # A tibble: 1,375 x 43
##    country  geoid iso   cov_date   cases deaths school school_local domestic
##    <chr>    <chr> <chr> <date>     <dbl>  <dbl>  <dbl>        <dbl>    <dbl>
##  1 Anguilla AI    AIA   2020-01-01     0      0      0            0        0
##  2 Anguilla AI    AIA   2020-01-02     0      0      0            0        0
##  3 Anguilla AI    AIA   2020-01-03     0      0      0            0        0
##  4 Anguilla AI    AIA   2020-01-04     0      0      0            0        0
##  5 Anguilla AI    AIA   2020-01-05     0      0      0            0        0
##  6 Anguilla AI    AIA   2020-01-06     0      0      0            0        0
##  7 Anguilla AI    AIA   2020-01-07     0      0      0            0        0
##  8 Anguilla AI    AIA   2020-01-08     0      0      0            0        0
##  9 Anguilla AI    AIA   2020-01-09     0      0      0            0        0
## 10 Anguilla AI    AIA   2020-01-10     0      0      0            0        0
## # ... with 1,365 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, ...
```

## 

```r
most_cases_5 %>%
  ggplot(aes(x=cov_date, 
             y=Rigidity_Public_Health,
             color= country))+
  geom_line() +
  facet_wrap(~country, ncol = 1)+
  labs(title = "Rigidity of Public Health for Top 5 Countries with Most Cases",
       x= "Date")
```

![](cov_response_plots_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

```r
least_cases_5 %>%
  ggplot(aes(x=cov_date, 
             y=Rigidity_Public_Health,
             color= country))+
  geom_line() +
  facet_wrap(~country, ncol = 1)+
  labs(title = "Rigidity of Public Health for Top 5 Countries with Least Cases",
       x= "Date")
```

![](cov_response_plots_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

