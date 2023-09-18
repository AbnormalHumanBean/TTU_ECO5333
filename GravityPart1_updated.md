Gravity Model Part 1
================
Ivanna Tindle
2023-09-18

## Coding assignment: Gravity Model

## **Data Retrieval:**

**Variables:**

$X_{ij}$ - exports between country i and country j for all i and j.

$GDP_i$ - real GDP in purchasing power equivalent \$PPE dollars for all
i.

$d_{ij}$ - a distance proxy between countries i and j for all i and j.

**Data:**

GDP: US \$, constant prices, constant PPPs, reference year 2015, for
2021

Link: <https://stats.oecd.org//Index.aspx?QueryId=126176>

Exports – Trade in value by partner countries (Edition 2022)

Link:
<https://www.oecd-ilibrary.org/trade/data/oecd-quarterly-international-trade-statistics/trade-in-value-by-partner-countries-edition-2022_3928be98-en>

Distance – Distance between country i’s and j’s capital using GeoDist
database.

Link: <http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6>

All files downloaded to same folder.

## Load Data

``` r
# Change working directory as needed.
setwd("~/R/Gravity")

#May need to change file names here due to saved file name

GDP <- read_csv("SNA_TABLE_GDP_2021.csv", 
    col_types = cols(Year = col_number(), 
        Value = col_number()))

export <- read_csv("QITS-2022-1-EN-20230503T012136.csv", show_col_types = FALSE)

dist_ij <- read_excel("dist_cepii.xls")
```

Data Cleaning

``` r
# Removing unnecessary variables

GDP <- GDP %>% select(LOCATION,Value) %>%
  #Selecting only vectors for country and gdp.
  rename(GDP = Value) 
  #Renaming Value to GDP for clearity

export <- export %>% 
  filter(FLOW=="EXP") %>%
  #removing all rows not for exports
  filter(TIME==2021) %>%
  #removing all rows not for the year 2021
  select(LOCATION, PARTNER, Value) %>% 
  #selecting only the vectors for each country pair, and the export value
  rename(Exports = Value)
  #renaming the value to specify which value

dist_ij <- dist_ij %>% 
  select(iso_o, iso_d, dist) %>%
  #Removing other types of distance
  rename(LOCATION = iso_o,
         PARTNER = iso_d)
  #renaming the pairs to match the other matrixes

# Creating a copy of GDP for easy merging later

GDP2 <- GDP %>% 
  rename(
  PARTNER=LOCATION,
  GDPj=GDP)

# Counting the list of countries for each

length(unique(GDP$LOCATION))
```

    ## [1] 56

``` r
length(unique(export$LOCATION))
```

    ## [1] 40

``` r
length(unique(dist_ij$LOCATION))
```

    ## [1] 224

``` r
# Displaying the dimensions of each

dim(GDP)
```

    ## [1] 56  2

``` r
dim(dist_ij)
```

    ## [1] 50176     3

``` r
dim(export)
```

    ## [1] 1910    3

## Creating Data Set

``` r
Data1 <- merge(dist_ij,export,by=c("LOCATION","PARTNER"))
  #Merging distance and exports, removing rows for those without information in both distance and exports. This step changes the dim the most. 
  Data <- Data1 %>% merge(GDP, by="LOCATION") %>% 
  #Adding GDP matched by location or country i
  rename(GDPi = GDP) %>% 
  #renaming those with location aka country i to GDPi
  merge(GDP2, by= "PARTNER") %>% 
  #adding in GDP again for partner or country j (GDPj was already renamed)
  relocate(Country_i = LOCATION) %>%
  #Moving the columns for niceness
  #Also renaming to country i
  relocate(GDPi, .after = Country_i) %>% 
  relocate(Country_j = PARTNER, .after = GDPi) %>% 
  #renaming so that the partner country is country j
  relocate(GDPj, .after = Country_j)

Data <- Data %>% arrange(Country_i)
#Sorting the Countries in alphabetical order. 

#Proving a quick double check to make sure looks like expected.
head(Data)
```

    ##   Country_i    GDPi Country_j      GDPj     dist    Exports
    ## 1       AUS 1417576       ARG  616052.8 11801.36  156251292
    ## 2       AUS 1417576       AUT  405145.8 15988.95   39474862
    ## 3       AUS 1417576       BEL  498161.4 16759.60  965550384
    ## 4       AUS 1417576       CAN 1690930.8 15586.66 1045200786
    ## 5       AUS 1417576       CHE  760153.3 16673.20 1420158612
    ## 6       AUS 1417576       CHL  274771.7 11353.72  404361427

``` r
length(unique(Data$Country_i))
```

    ## [1] 35

``` r
length(unique(Data$Country_j))
```

    ## [1] 42

``` r
#Can adjust the data to be square, but not needed yet. 

#Again making sure that the matrix is as expected. 

dim.data.frame(Data)
```

    ## [1] 1424    6

Data is listed as long. Each pair has its own row.

## Reshaping the whole Data Set

For it to be short, the row is country i and each column is for country
j for both GDP, exports, and distance.

``` r
# Now adjusting it to be wide instead of long. 

Data_wide <- Data %>%
 pivot_wider(
  names_from = Country_j,
  #this takes the names from country j (the partner country)
  values_from = c(GDPj,dist,Exports),
  #this takes the values from these three columns matched with the proper rows
  names_sep = "_"
  #so that the names are easy to read
)

head(Data_wide)
```

    ## # A tibble: 6 × 128
    ##   Country_i     GDPi GDPj_ARG GDPj_AUT GDPj_BEL GDPj_CAN GDPj_CHE GDPj_CHL
    ##   <chr>        <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 AUS       1417576.  616053.  405146.  498161. 1690931.  760153.  274772.
    ## 2 BEL        498161.  616053.  405146.      NA  1690931.  760153.  274772.
    ## 3 CAN       1690931.  616053.  405146.  498161.      NA   760153.  274772.
    ## 4 CHE        760153.  616053.  405146.  498161. 1690931.      NA   274772.
    ## 5 CHL        274772.  616053.  405146.  498161. 1690931.  760153.      NA 
    ## 6 COL        330935.  616053.  405146.  498161. 1690931.  760153.  274772.
    ## # ℹ 120 more variables: GDPj_COL <dbl>, GDPj_CRI <dbl>, GDPj_CZE <dbl>,
    ## #   GDPj_DEU <dbl>, GDPj_DNK <dbl>, GDPj_ESP <dbl>, GDPj_EST <dbl>,
    ## #   GDPj_FIN <dbl>, GDPj_FRA <dbl>, GDPj_GBR <dbl>, GDPj_GRC <dbl>,
    ## #   GDPj_HUN <dbl>, GDPj_IDN <dbl>, GDPj_IRL <dbl>, GDPj_ISL <dbl>,
    ## #   GDPj_ISR <dbl>, GDPj_ITA <dbl>, GDPj_JPN <dbl>, GDPj_KOR <dbl>,
    ## #   GDPj_LTU <dbl>, GDPj_LUX <dbl>, GDPj_LVA <dbl>, GDPj_MEX <dbl>,
    ## #   GDPj_NLD <dbl>, GDPj_NOR <dbl>, GDPj_NZL <dbl>, GDPj_POL <dbl>, …

``` r
#Double checking that the information did transfer correctly. 

length(unique(Data_wide$Country_i))
```

    ## [1] 35

``` r
dim.data.frame(Data_wide)
```

    ## [1]  35 128

## Reshaping to separate matrices

Creating separate matrices for each variable, of i by j.

**For Exports**

``` r
exports_paired = Data %>% select(Country_i,Country_j, Exports)
#Taking only exports out of the larger set.

W_exports_paired = exports_paired %>% pivot_wider(
  values_from = Exports,
  names_from = Country_j
)
#this makes it i by j
head(W_exports_paired)
```

    ## # A tibble: 6 × 43
    ##   Country_i    ARG     AUT     BEL     CAN     CHE     CHL     COL    CRI    CZE
    ##   <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
    ## 1 AUS       1.56e8 3.95e 7  9.66e8  1.05e9  1.42e9  4.04e8  4.14e7 3.97e6 9.63e7
    ## 2 BEL       5.10e8 3.38e 9 NA       3.73e9  5.03e9  5.36e8  4.31e8 7.20e7 3.49e9
    ## 3 CAN       2.83e8 2.74e 8  3.12e9 NA       2.42e9  9.75e8  8.01e8 1.47e8 1.81e8
    ## 4 CHE       6.85e8 1.02e10  5.06e9  4.13e9 NA       3.54e8  6.13e8 2.12e8 2.25e9
    ## 5 CHL       7.87e8 1.39e 8  4.73e8  1.17e9  9.14e8 NA       6.96e8 2.44e8 2.28e7
    ## 6 COL       2.95e8 3.57e 6  5.62e8  7.02e8  1.69e8  1.09e9 NA      2.61e8 5.45e6
    ## # ℹ 33 more variables: DEU <dbl>, DNK <dbl>, ESP <dbl>, EST <dbl>, FIN <dbl>,
    ## #   FRA <dbl>, GBR <dbl>, GRC <dbl>, HUN <dbl>, IDN <dbl>, IRL <dbl>,
    ## #   ISL <dbl>, ISR <dbl>, ITA <dbl>, JPN <dbl>, KOR <dbl>, LTU <dbl>,
    ## #   LUX <dbl>, LVA <dbl>, MEX <dbl>, NLD <dbl>, NOR <dbl>, NZL <dbl>,
    ## #   POL <dbl>, PRT <dbl>, SAU <dbl>, SVK <dbl>, SVN <dbl>, SWE <dbl>,
    ## #   TUR <dbl>, USA <dbl>, ZAF <dbl>, AUS <dbl>

``` r
#checking to make sure it is in fact as expected. 

dim(W_exports_paired) #Shows dimension of variable
```

    ## [1] 35 43

**For Distance**

``` r
# Preforming the same process as for exports. 
dist_paired = Data %>% select(Country_i, Country_j, dist) 

W_dist_paired = dist_paired %>% pivot_wider(
  values_from = dist,
  names_from = Country_j
)
head(W_dist_paired)
```

    ## # A tibble: 6 × 43
    ##   Country_i    ARG    AUT    BEL    CAN    CHE    CHL    COL    CRI    CZE
    ##   <chr>      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 AUS       11801. 15989. 16760. 15587. 16673. 11354. 14260. 13829. 16098.
    ## 2 BEL       11327.   914.    NA   6032.   487. 11905.  8874.  9046.   718.
    ## 3 CAN        8970.  6923.  6032.    NA   6441.  8632.  4363.  3774.  6688.
    ## 4 CHE       11215.   685.   487.  6441.    NA  11866.  9071.  9320.   623.
    ## 5 CHL        1128. 12522  11905.  8632. 11866.    NA   4269.  5043. 12487.
    ## 6 COL        4706.  9739.  8874.  4363.  9071.  4269.    NA   1158.  9586.
    ## # ℹ 33 more variables: DEU <dbl>, DNK <dbl>, ESP <dbl>, EST <dbl>, FIN <dbl>,
    ## #   FRA <dbl>, GBR <dbl>, GRC <dbl>, HUN <dbl>, IDN <dbl>, IRL <dbl>,
    ## #   ISL <dbl>, ISR <dbl>, ITA <dbl>, JPN <dbl>, KOR <dbl>, LTU <dbl>,
    ## #   LUX <dbl>, LVA <dbl>, MEX <dbl>, NLD <dbl>, NOR <dbl>, NZL <dbl>,
    ## #   POL <dbl>, PRT <dbl>, SAU <dbl>, SVK <dbl>, SVN <dbl>, SWE <dbl>,
    ## #   TUR <dbl>, USA <dbl>, ZAF <dbl>, AUS <dbl>

``` r
dim(W_dist_paired)
```

    ## [1] 35 43

**For GDP**

While GDP could be left to be one singular column. The difference
between the countries is more relevant for further analysis. Thus,
creating an i by j matrix using GDPi-GDPj.

``` r
gdp_paired = Data %>% select(Country_i, Country_j, GDPi,GDPj)
#selecting only gdpi and gpdj 
gdp_paired_S = gdp_paired %>% mutate(
  GDP_ij = GDPi-GDPj) %>%
  #creating a new column for the difference.
  select(GDP_ij,Country_i,Country_j)
#removing the old gdp's

W_gdp_paired = gdp_paired_S %>% pivot_wider(
  values_from = GDP_ij,
  names_from = Country_j
)
#Again pivoting the table so that it is i by j. 

head(W_gdp_paired)
```

    ## # A tibble: 6 × 43
    ##   Country_i      ARG      AUT      BEL       CAN      CHE     CHL     COL    CRI
    ##   <chr>        <dbl>    <dbl>    <dbl>     <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
    ## 1 AUS        801523. 1012430.  919414.  -273355.  657423.  1.14e6  1.09e6 1.35e6
    ## 2 BEL       -117891.   93016.      NA  -1192769. -261992.  2.23e5  1.67e5 4.32e5
    ## 3 CAN       1074878. 1285785. 1192769.       NA   930777.  1.42e6  1.36e6 1.62e6
    ## 4 CHE        144101.  355007.  261992.  -930777.      NA   4.85e5  4.29e5 6.94e5
    ## 5 CHL       -341281. -130374. -223390. -1416159. -485382. NA      -5.62e4 2.08e5
    ## 6 COL       -285118.  -74211. -167226. -1359996. -429218.  5.62e4 NA      2.65e5
    ## # ℹ 34 more variables: CZE <dbl>, DEU <dbl>, DNK <dbl>, ESP <dbl>, EST <dbl>,
    ## #   FIN <dbl>, FRA <dbl>, GBR <dbl>, GRC <dbl>, HUN <dbl>, IDN <dbl>,
    ## #   IRL <dbl>, ISL <dbl>, ISR <dbl>, ITA <dbl>, JPN <dbl>, KOR <dbl>,
    ## #   LTU <dbl>, LUX <dbl>, LVA <dbl>, MEX <dbl>, NLD <dbl>, NOR <dbl>,
    ## #   NZL <dbl>, POL <dbl>, PRT <dbl>, SAU <dbl>, SVK <dbl>, SVN <dbl>,
    ## #   SWE <dbl>, TUR <dbl>, USA <dbl>, ZAF <dbl>, AUS <dbl>

``` r
dim(W_gdp_paired)
```

    ## [1] 35 43

Now all the matrices are the same 35 by 43. The NA’s that are currently
in the matrix could be replaced with 0, as they are only for the i=j
pairs.
