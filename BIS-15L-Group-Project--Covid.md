---
title: 'Group Project: COVID-19 Responses'
author: "Julie Trundle"
date: "2/15/2022"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(janitor)
library(here)
library(dplyr)
library(knitr)
library(naniar)
library(shiny)
library(shinydashboard)
library(lubridate)
library(ggVennDiagram)
```


```r
covid <- read_csv(here("Gov_Responses2Covid19_last (1).csv"))
```

```
## Rows: 62700 Columns: 43
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (5): country, geoid, iso, d, continent
## dbl (38): cases, deaths, school, school_local, domestic, domestic_local, tra...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
view(covid)
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
##  Min.   :0.000          Min.   :0.000     Min.   :8.150e+02  
##  1st Qu.:0.000          1st Qu.:0.000     1st Qu.:8.900e+05  
##  Median :0.318          Median :0.429     Median :6.950e+06  
##  Mean   :0.301          Mean   :0.385     Mean   :3.656e+07  
##  3rd Qu.:0.500          3rd Qu.:0.714     3rd Qu.:2.600e+07  
##  Max.   :0.885          Max.   :1.000     Max.   :1.400e+09  
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
## $ country                <chr> "Aruba", "Aruba", "Aruba", "Aruba", "Aruba", "A…
## $ geoid                  <chr> "AW", "AW", "AW", "AW", "AW", "AW", "AW", "AW",…
## $ iso                    <chr> "ABW", "ABW", "ABW", "ABW", "ABW", "ABW", "ABW"…
## $ d                      <chr> "1-Jan-20", "2-Jan-20", "3-Jan-20", "4-Jan-20",…
## $ cases                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ deaths                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ school                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ school_local           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ domestic               <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ domestic_local         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ travel                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ travel_partial         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ travel_dom             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ travel_dom_partial     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ curf                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ curf_partial           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ mass                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ mass_partial           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ elect                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ elect_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ sport                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ sport_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ rest                   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ rest_local             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ testing                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ testing_narrow         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ masks                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ masks_partial          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ surveillance           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ surveillance_partial   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ state                  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ state_partial          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ cash                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ wage                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ credit                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ taxc                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ taxd                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ export                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ rate                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ Rigidity_Public_Health <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ Economic_Measures      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
## $ population_2019        <dbl> 106310, 106310, 106310, 106310, 106310, 106310,…
## $ continent              <chr> "America", "America", "America", "America", "Am…
```


```r
naniar::miss_var_summary(covid)
```

```
## # A tibble: 43 × 3
##    variable      n_miss pct_miss
##    <chr>          <int>    <dbl>
##  1 elect          47866     76.3
##  2 elect_partial  47866     76.3
##  3 school         16942     27.0
##  4 school_local   16942     27.0
##  5 masks          11241     17.9
##  6 masks_partial  11241     17.9
##  7 cash            8250     13.2
##  8 wage            8250     13.2
##  9 credit          8250     13.2
## 10 taxc            8250     13.2
## # … with 33 more rows
```

```r
dim(covid)
```

```
## [1] 62700    43
```


```r
names(covid)
```

```
##  [1] "country"                "geoid"                  "iso"                   
##  [4] "d"                      "cases"                  "deaths"                
##  [7] "school"                 "school_local"           "domestic"              
## [10] "domestic_local"         "travel"                 "travel_partial"        
## [13] "travel_dom"             "travel_dom_partial"     "curf"                  
## [16] "curf_partial"           "mass"                   "mass_partial"          
## [19] "elect"                  "elect_partial"          "sport"                 
## [22] "sport_partial"          "rest"                   "rest_local"            
## [25] "testing"                "testing_narrow"         "masks"                 
## [28] "masks_partial"          "surveillance"           "surveillance_partial"  
## [31] "state"                  "state_partial"          "cash"                  
## [34] "wage"                   "credit"                 "taxc"                  
## [37] "taxd"                   "export"                 "rate"                  
## [40] "Rigidity_Public_Health" "Economic_Measures"      "population_2019"       
## [43] "continent"
```


```r
covid <- covid %>% 
  rename(cov_date = d)
names(covid)
```

```
##  [1] "country"                "geoid"                  "iso"                   
##  [4] "cov_date"               "cases"                  "deaths"                
##  [7] "school"                 "school_local"           "domestic"              
## [10] "domestic_local"         "travel"                 "travel_partial"        
## [13] "travel_dom"             "travel_dom_partial"     "curf"                  
## [16] "curf_partial"           "mass"                   "mass_partial"          
## [19] "elect"                  "elect_partial"          "sport"                 
## [22] "sport_partial"          "rest"                   "rest_local"            
## [25] "testing"                "testing_narrow"         "masks"                 
## [28] "masks_partial"          "surveillance"           "surveillance_partial"  
## [31] "state"                  "state_partial"          "cash"                  
## [34] "wage"                   "credit"                 "taxc"                  
## [37] "taxd"                   "export"                 "rate"                  
## [40] "Rigidity_Public_Health" "Economic_Measures"      "population_2019"       
## [43] "continent"
```


```r
covid_clean <- covid %>% 
  mutate(cov_date = dmy(cov_date)) %>% 
  clean_names()
covid_clean
```

```
## # A tibble: 62,700 × 43
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
## # … with 62,690 more rows, and 34 more variables: domestic_local <dbl>,
## #   travel <dbl>, travel_partial <dbl>, travel_dom <dbl>,
## #   travel_dom_partial <dbl>, curf <dbl>, curf_partial <dbl>, mass <dbl>,
## #   mass_partial <dbl>, elect <dbl>, elect_partial <dbl>, sport <dbl>,
## #   sport_partial <dbl>, rest <dbl>, rest_local <dbl>, testing <dbl>,
## #   testing_narrow <dbl>, masks <dbl>, masks_partial <dbl>, surveillance <dbl>,
## #   surveillance_partial <dbl>, state <dbl>, state_partial <dbl>, cash <dbl>, …
```

Total number of cases per country: 

```r
covid_clean %>% 
  select(country, cases) %>% 
  group_by(country) %>% 
  summarise(sum_cases=sum(cases)) %>% 
  arrange(desc(sum_cases))
```

```
## # A tibble: 228 × 2
##    country                  sum_cases
##    <chr>                        <dbl>
##  1 United States of America   7233043
##  2 India                      6312584
##  3 Brazil                     4810935
##  4 Russia                     1176286
##  5 Colombia                    829679
##  6 Peru                        814829
##  7 Mexico                      789286
##  8 Spain                       780777
##  9 Argentina                   750988
## 10 South Africa                674339
## # … with 218 more rows
```


```r
covid_clean %>% 
  group_by(country) %>% 
  summarise(sum_cases=sum(cases)) %>% 
  top_n(10, sum_cases) %>% 
  ggplot(aes(x=country, y=sum_cases, fill=country))+
  geom_col()+
  theme(axis.text.x = element_text(angle=30))
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Top 5 countries with the most total covid cases over 2020: United States, India, Brazil, Russia, Colombia. 

Total sum of deaths per country: 

```r
covid_clean %>%
  select(country, deaths) %>%
  group_by(country) %>%
  summarise(sum_deaths=sum(deaths)) %>%
  arrange(desc(sum_deaths))
```

```
## # A tibble: 228 × 2
##    country                  sum_deaths
##    <chr>                         <dbl>
##  1 United States of America     206928
##  2 Brazil                       143952
##  3 India                         98678
##  4 Mexico                        77646
##  5 United Kingdom                42143
##  6 Italy                         35956
##  7 Spain                         35813
##  8 Peru                          32463
##  9 France                        31956
## 10 Iran                          26169
## # … with 218 more rows
```

```r
covid_clean %>% 
  group_by(country) %>% 
  summarise(sum_deaths=sum(deaths)) %>% 
  top_n(10, sum_deaths) %>% 
  ggplot(aes(x=country, y=sum_deaths, fill=country))+
  geom_col()+
  theme(axis.text.x = element_text(angle=30))
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Top 5 countries with the most total deaths over 2020: United States of America, Brazil, India, Mexico, United Kingdom. 

Facet wrap with cases of the top 5 countries with most cases: 


```r
covid_clean %>% 
  filter(country=="United States of America" | country=="India" | country=="Brazil" | country=="Russia" | country=="Colombia") %>% 
  select(country, cov_date, cases) %>% 
  group_by(country) %>% 
  summarize(sum_cases = sum(cases))
```

```
## # A tibble: 5 × 2
##   country                  sum_cases
##   <chr>                        <dbl>
## 1 Brazil                     4810935
## 2 Colombia                    829679
## 3 India                      6312584
## 4 Russia                     1176286
## 5 United States of America   7233043
```



```r
covid_clean %>% 
  select(country, cov_date, cases) %>% 
  filter(country=="United States of America" | country=="India" | country=="Brazil" | country=="Russia" | country=="Colombia") %>%
  ggplot(aes(x=cov_date, y=cases, color=country))+
  scale_y_log10()+
  geom_point(size=0.2)+
  geom_line()+
  facet_wrap(~country)+
  theme(axis.text.x = element_text(size=0))+
  labs(title = "Top 5 Countries in Number of Cases",
       x = "Date",
       y = "Cases")
```

```
## Warning: Transformation introduced infinite values in continuous y-axis
## Transformation introduced infinite values in continuous y-axis
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


Economic measures taken by each country: 

```r
covid_clean %>% 
  select(country, economic_measures) %>% 
  filter(economic_measures!="NA") %>% 
  group_by(country) %>% 
  summarize(mean_economic_measures=mean(economic_measures)) %>% 
  arrange(desc(mean_economic_measures))
```

```
## # A tibble: 198 × 2
##    country                  mean_economic_measures
##    <chr>                                     <dbl>
##  1 Thailand                                  0.724
##  2 Spain                                     0.713
##  3 United States of America                  0.710
##  4 Trinidad and Tobago                       0.708
##  5 Australia                                 0.706
##  6 Portugal                                  0.696
##  7 Mongolia                                  0.696
##  8 Singapore                                 0.687
##  9 Austria                                   0.686
## 10 Canada                                    0.685
## # … with 188 more rows
```

```r
covid_clean %>% 
  select(country, economic_measures) %>% 
  filter(economic_measures!="NA") %>% 
  group_by(country) %>% 
  summarize(mean_economic_measures=mean(economic_measures)) %>% 
  arrange(mean_economic_measures)
```

```
## # A tibble: 198 × 2
##    country                          mean_economic_measures
##    <chr>                                             <dbl>
##  1 Djibouti                                              0
##  2 Eritrea                                               0
##  3 Kiribati                                              0
##  4 Libya                                                 0
##  5 Nauru                                                 0
##  6 Saint Vincent and the Grenadines                      0
##  7 Svalbard                                              0
##  8 Tuvalu                                                0
##  9 Wallis_and_Futuna                                     0
## 10 Yemen                                                 0
## # … with 188 more rows
```

Looking at difference in percent of cases between Thailand (most economic measures) and Eritrea (an example of no economic measures)

```r
covid_clean %>% 
  select(country, cov_date, economic_measures, cases, population_2019) %>% 
  filter(country=="Thailand" | country=="Eritrea") %>% 
  mutate(percent_cases = cases/population_2019) %>% 
  ggplot(aes(x=cov_date, y=percent_cases, color=country))+
  geom_point(size=0.8)+
  geom_line()+
  geom_smooth(method=lm, se=F)+
  theme(axis.text.x = element_blank())+
  labs(title = "Percent Cases Comparison: Thailand and Eritrea",
       x = "Date",
       y = "Percent of Cases")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
covid_clean %>% 
  select(country, rigidity_public_health) %>% 
  filter(rigidity_public_health!="NA") %>% 
  group_by(country) %>%
  summarize(mean_public_health_measures=mean(rigidity_public_health)) %>% 
  arrange(desc(mean_public_health_measures))
```

```
## # A tibble: 201 × 2
##    country      mean_public_health_measures
##    <chr>                              <dbl>
##  1 Morocco                            0.552
##  2 South Africa                       0.535
##  3 Philippines                        0.513
##  4 China                              0.508
##  5 Ecuador                            0.504
##  6 Paraguay                           0.500
##  7 Libya                              0.483
##  8 Panama                             0.480
##  9 Chile                              0.480
## 10 Iraq                               0.467
## # … with 191 more rows
```

```r
covid_clean %>% 
  select(country, rigidity_public_health) %>% 
  filter(rigidity_public_health!="NA") %>% 
  group_by(country) %>%
  summarize(mean_public_health_measures=mean(rigidity_public_health)) %>% 
  arrange((mean_public_health_measures))
```

```
## # A tibble: 201 × 2
##    country                               mean_public_health_measures
##    <chr>                                                       <dbl>
##  1 Wallis_and_Futuna                                         0      
##  2 Niue                                                      0.00331
##  3 Monaco                                                    0.0180 
##  4 Svalbard                                                  0.0183 
##  5 Democratic People's Republic of Korea                     0.0261 
##  6 Tokelau                                                   0.0263 
##  7 Burundi                                                   0.0398 
##  8 Saint Vincent and the Grenadines                          0.0438 
##  9 Anguilla                                                  0.0463 
## 10 Tanzania, United Republic of                              0.0726 
## # … with 191 more rows
```


```r
covid_clean %>% 
  select(country, cov_date, rigidity_public_health, cases, population_2019) %>% 
  filter(country=="Morocco" | country=="Monaco") %>% 
  mutate(percent_cases = cases/population_2019) %>% 
  ggplot(aes(x=cov_date, y=percent_cases, color=country))+
  geom_point(size=0.8)+
  geom_line()+
  geom_smooth(method=lm, se=F)+
  theme(axis.text.x = element_blank())+
  labs(title = "Percent Cases Comparison: Morocco and Monaco",
       x = "Date",
       y = "Percent of Cases")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-22-1.png)<!-- -->


```r
covid_clean %>% 
  select(country, cov_date, economic_measures) %>% 
  filter(country=="United States of America" | country=="India" | country=="Brazil" | country=="Russia" | country=="Colombia") %>%
  ggplot(aes(x=cov_date, y=economic_measures, color=country))+
  geom_smooth(size=0.6, se=F)+
  theme(axis.text.x = element_text(size=0))+
  labs(title = "Economic Measures of ")
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
Scatter plot showing economic measures vs public health measures:


```r
covid_clean %>% 
  select(country, economic_measures, rigidity_public_health) %>% 
  filter(economic_measures!="NA") %>% 
  filter(rigidity_public_health!="NA") %>% 
  group_by(country) %>% 
  summarize(mean_economic_measures=mean(economic_measures),
            mean_public_health_measures=mean(rigidity_public_health)) %>% 
  arrange(desc(mean_economic_measures))
```

```
## # A tibble: 193 × 3
##    country                  mean_economic_measures mean_public_health_measures
##    <chr>                                     <dbl>                       <dbl>
##  1 Thailand                                  0.724                       0.435
##  2 Spain                                     0.713                       0.378
##  3 United States of America                  0.710                       0.310
##  4 Trinidad and Tobago                       0.708                       0.320
##  5 Australia                                 0.706                       0.276
##  6 Portugal                                  0.696                       0.301
##  7 Mongolia                                  0.696                       0.371
##  8 Singapore                                 0.687                       0.372
##  9 Austria                                   0.686                       0.298
## 10 Canada                                    0.685                       0.286
## # … with 183 more rows
```


```r
covid_clean %>% 
  select(country, economic_measures, rigidity_public_health) %>% 
  filter(economic_measures!="NA") %>% 
  filter(rigidity_public_health!="NA") %>% 
  group_by(country) %>% 
  summarize(mean_economic_measures=mean(economic_measures),
            mean_public_health_measures=mean(rigidity_public_health)) %>% 
  ggplot(aes(x=mean_economic_measures, y=mean_public_health_measures))+
  geom_point(size=1)+
  geom_smooth(method=lm)+
  labs(title = "Economic Measures vs. Public Health Measures",
       x = "Mean Economic Measures",
       y = "Mean Public Health Measures")+
  theme(title = element_text(size=14),
        axis.title = element_text(size=11),
        axis.text = element_text(size=8))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

Venn Diagram showing countries with more than average economic measures vs public health measures: 


```r
summary(covid_clean)
```

```
##    country             geoid               iso               cov_date         
##  Length:62700       Length:62700       Length:62700       Min.   :2020-01-01  
##  Class :character   Class :character   Class :character   1st Qu.:2020-03-09  
##  Mode  :character   Mode  :character   Mode  :character   Median :2020-05-17  
##                                                           Mean   :2020-05-17  
##                                                           3rd Qu.:2020-07-25  
##                                                           Max.   :2020-10-01  
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
##  rigidity_public_health economic_measures population_2019    
##  Min.   :0.000          Min.   :0.000     Min.   :8.150e+02  
##  1st Qu.:0.000          1st Qu.:0.000     1st Qu.:8.900e+05  
##  Median :0.318          Median :0.429     Median :6.950e+06  
##  Mean   :0.301          Mean   :0.385     Mean   :3.656e+07  
##  3rd Qu.:0.500          3rd Qu.:0.714     3rd Qu.:2.600e+07  
##  Max.   :0.885          Max.   :1.000     Max.   :1.400e+09  
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
economic_measures_implemented <- covid_clean %>% 
  filter(economic_measures>="0.385") %>% 
  pull(country)

public_health_measures_implemented <- covid_clean %>% 
  filter(rigidity_public_health>="0.301") %>% 
  pull(country)
```


```r
high_health_measures <- list(economic_measures_implemented, public_health_measures_implemented)
```


```r
ggVennDiagram(high_health_measures, category.names = c("Economic Measures", "Health Measures"))
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-29-1.png)<!-- -->


```r
economic_measures_not_implemented <- covid %>% 
  filter(Economic_Measures<="0.385") %>% 
  pull(country)

public_health_measures_not_implemented <- covid %>% 
  filter(Rigidity_Public_Health<="0.301") %>% 
  pull(country)
```


```r
low_health_measures <- list(economic_measures_not_implemented, public_health_measures_not_implemented)
```


```r
ggVennDiagram(low_health_measures, category.names = c("Low Economic Measures", "Low Public Health Measures"))
```

![](BIS-15L-Group-Project--Covid_files/figure-html/unnamed-chunk-32-1.png)<!-- -->
