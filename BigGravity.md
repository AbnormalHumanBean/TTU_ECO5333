Untitled
================
Ivanna Tindle
2023-11-15

## Coding assignment: Gravity Model

## Part 1

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

### Load Data

``` r
# Change working directory as needed.


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
  #Renaming Value to GDP for clarity

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
  #renaming the pairs to match the other matrices

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

``` r
save(GDP, file="bGDP.Rdata")
save(export, file="B_exports.Rdata")
```

### Creating Data Set

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
```

Data is listed as long. Each pair has its own row.

### Reshaping the whole Data Set

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

## Part 2

### Preparing the matrices

#### Making square

``` r
Country_i <- W_dist_paired %>% select(Country_i)
Country_i <- Country_i$Country_i
#pulling out the 35 to refer

gdpa <- GDP %>% filter(LOCATION %in% Country_i)
#Filtering for only the 35 countries
dist_1 <- W_dist_paired %>% select(all_of(Country_i))
#Again limiting
export_1 <- W_exports_paired %>% select(all_of(Country_i))

#Selecting the 35 countries, and making the pairs match that of dist and exports
gdp_1 <- gdp_paired %>% filter(Country_j %in% Country_i) %>% expand(Country_i,Country_j)

#Filling in the proper GDP for all the pairs
gdpi <-  gdpa$GDP[match(gdp_1$Country_i, Country_i)]
gdpj <-  gdpa$GDP[match(gdp_1$Country_j, Country_i)]
```

Transforming all into natural logs.

``` r
Ln_ex <- log(as.matrix(export_1))
Ln_d <- log(as.matrix(dist_1))
Ln_GDPi <- log(as.matrix(gdpi))
Ln_GDPj <- log(as.matrix(gdpj))
```

All are 35 by 35.

#### Making vector

Creating vector for each variable.

``` r
export_2 <- as.vector(Ln_ex)
d_2 <- as.vector(Ln_d)
Gi <- as.vector(Ln_GDPi)
Gj <- as.vector(Ln_GDPj)
```

Now each is 1225 by 1 or $N^2$ by 1.

Replacing any NA’s with 0. This is acceptable because the NA’s are the
i=j pairs. For example, the distance between Canada and Canada is 0.

``` r
D <- d_2
Ex <- export_2
D[is.na(D)] <- 0
Ex[is.na(Ex)] <- 0
Gi[is.na(Gi)] <- 0
Gj[is.na(Gj)] <- 0
```

### Find the Estimate

$\hat\alpha = (S'S)^{-1}S'X$

``` r
S <- cbind(rep(1,1225),D,Gi,Gj)
# The "X" matrix all independent variables
X <- Ex
# The "Y" matrix all the dependent variables
S.prime <- t(S)
#taking the transpose
S.primeS <- S.prime %*% S
S.primeX <- S.prime %*% X
#preforming the matrix multiplication
alpha_hat <- solve(S.primeS) %*%  S.primeX
#Solve finds the inverse of the matrix
```

#### 1. Alpha

``` r
print(alpha_hat)
```

    ##          [,1]
    ##    8.75636254
    ## D  1.27769386
    ## Gi 0.03518170
    ## Gj 0.03398416

#### 3. Fitted values

``` r
Xhat <- S %*% alpha_hat
```

#### 2. Residuals

``` r
e <- X-Xhat
```

#### 4. Standard Errors

``` r
#to obtain the variance
simga2 <- as.numeric((t(e) %*% e)/(1225-4))
V <-  (solve(t(S)%*%S)) * simga2
# square root of variance to obtain SE
se <- sqrt(diag(V))
```

#### 5. Confidence intervals

``` r
alpha <- .05
t <- qt(1-alpha/2, 1225-4)
CI_low <- alpha_hat-t*diag(V)
CI_high <- alpha_hat+t*diag(V)
CI <- cbind(CI_low, CI_high)
print(CI)
```

    ##          [,1]        [,2]
    ##    4.63419737 12.87852772
    ## D  1.27189221  1.28349551
    ## Gi 0.02515729  0.04520612
    ## Gj 0.02402139  0.04394693

## Part 3

### Structural to Reduced Form

$$
\log x_{ij}= \beta_{1i} \log y_i + \beta_{1j}\log y_j + \beta_{2ij}\log d_{ij} + \beta_{3ij} \delta_{ij}+ \gamma_i  +\gamma_j
$$

(I think)

### New Variables

``` r
dist_ij <- read_excel("dist_cepii.xls")
dist2 <- dist_ij %>% 
  select(iso_o, iso_d, contig) %>%
  #Removing other types of distance
  rename(Country_i = iso_o,
         Country_j = iso_d,
         ddist_ij= contig
         )
  #renaming the pairs to match the other matrices

Data2 <- merge(Data, dist2,by=c("Country_i","Country_j"))
#merging with the older data. 

Data3 <- Data2 %>% filter(Country_i %in% Country_i)
Data3 <- Data3 %>% filter(Country_j %in% Country_i)

length(unique(Data3$Country_j))
```

    ## [1] 35

``` r
length(unique(Data3$Country_i))
```

    ## [1] 35

``` r
#redoing the filtering so that all are 35x35
```

``` r
Data4 <- Data3 %>% complete(Country_i,Country_j, fill=as.list(gdpa),explicit = FALSE)
#filling in the missing combinations
Data5 <- Data4 %>% filter(!
  Country_i==Country_j)
#removing i=j
#At this point there was still missing data. 
nadata <- Data5[is.na(Data5$Exports),]
print(nadata)
```

    ## # A tibble: 7 × 7
    ##   Country_i Country_j  GDPi  GDPj  dist Exports ddist_ij
    ##   <chr>     <chr>     <dbl> <dbl> <dbl>   <dbl>    <dbl>
    ## 1 CHL       LUX          NA    NA    NA      NA       NA
    ## 2 CHL       SVK          NA    NA    NA      NA       NA
    ## 3 COL       LUX          NA    NA    NA      NA       NA
    ## 4 MEX       EST          NA    NA    NA      NA       NA
    ## 5 MEX       LTU          NA    NA    NA      NA       NA
    ## 6 NLD       GBR          NA    NA    NA      NA       NA
    ## 7 NZL       LUX          NA    NA    NA      NA       NA

``` r
#Filling in missing data with GDPi
Data5$GDPi[157] <- 274771.68
Data5$GDPi[165] <- 274771.68
Data5$GDPi[191] <- 330935.18
Data5$GDPi[793] <- 1206920.89
Data5$GDPi[803] <- 1206920.89
Data5$GDPi[829] <- 857602.88
Data5$GDPi[906] <- 214847.26

#missing data for GDPj 
Data5$GDPj[157] <- 68993.90
Data5$GDPj[165] <- 100845.35
Data5$GDPj[191] <- 68993.90
Data5$GDPj[793] <- 28890.86
Data5$GDPj[803] <- 51069.46
Data5$GDPj[829] <- 857602.88
Data5$GDPj[906] <- 68993.90

#Missing distance data pulled from other pair

Data5$dist[157] <- 11940.3000
Data5$dist[165] <- 12573.2300
Data5$dist[191] <-8991.1760
Data5$dist[793] <- 9899.34000
Data5$dist[803] <- 10255.6500
Data5$dist[829] <-360.31500
Data5$dist[906] <-18988.6200

#The following line is only because this is in progress, and so I could check that everything went right before moving onward. 

Data6 <- Data5
#Must take logs before 0's are added
Data7 <- Data6 %>% mutate(
  GDPi = log(GDPi),
  GDPj = log(GDPj),
  dist = log(dist),
  Exports = log(Exports)
)

Data7[is.na(Data7)] <- 0
#This will add zeros to exports, and the ddist_ij which is if they are sharing a border. I checked each pair to make sure they weren't. 
```

### Making Vectors

``` r
vgdpi <- as.vector(Data7$GDPi)
vgdpj <-as.vector(Data7$GDPj)
vdist <- as.vector(Data7$dist)
vexport <- as.vector(Data7$Exports)
```

### First redoing the part 2 with the data added properly

``` r
M <- cbind(rep(1,1190),vgdpi, vgdpj, vdist)
Ex2 <- as.vector(Data7$Exports)
MT<- t(M)

alpha_hat <- solve(MT %*% M) %*%  (MT %*% Ex2)

step1 <- MT %*% M
step2 <- MT %*% Ex2
#in processing its easier to break these down to figure out why something isn't working
```

``` r
print(alpha_hat)
```

    ##             [,1]
    ##        5.6146915
    ## vgdpi  0.8705028
    ## vgdpj  1.0198648
    ## vdist -1.2181729

``` r
Xhat <- M %*% alpha_hat
```

``` r
e <- Ex2-Xhat
```

``` r
#to obtain the variance
simga2 <- as.numeric((t(e) %*% e)/(1190-4))
V <-  (solve(t(M)%*%M)) * simga2
# square root of variance to obtain SE
se <- sqrt(diag(V))
```

#### Confidence intervals

``` r
alpha <- .05
t <- qt(1-alpha/2, 1190-4)
CI_low <- alpha_hat-t*diag(V)
CI_high <- alpha_hat+t*diag(V)
CI <- cbind(CI_low, CI_high)
print(CI)
```

    ##             [,1]       [,2]
    ##        4.7290757  6.5003073
    ## vgdpi  0.8680559  0.8729497
    ## vgdpj  1.0174132  1.0223165
    ## vdist -1.2218607 -1.2144850

New CI

### Dummy variables

``` r
#Creates a dummy for each country, with 1 for that country 0 for all others.
ndata <- Data7 %>% pivot_longer(cols=where(is.character),
                                names_to = "dummy_names",
                                values_to = "dummy_levels") %>% mutate(dummy_value=1) %>% 
  pivot_wider(names_from = c(dummy_names,dummy_levels), values_from = dummy_value,
              values_fill = 0
)

#Renaming dummy variables to something more insightful.  

ones <- grep("^Country_i", colnames(ndata))
             
ndata2 <- ndata %>% rename_with(~paste0(., "_i"), all_of(ones))
            
names(ndata2) <- sub("Country_i_", "", names(ndata2))

onesj <-  grep("^Country_j", colnames(ndata))

ndata2 <- ndata2 %>% rename_with(~paste0(., "_j"), onesj)
```

    ## Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    ## ℹ Please use `all_of()` or `any_of()` instead.
    ##   # Was:
    ##   data %>% select(onesj)
    ## 
    ##   # Now:
    ##   data %>% select(all_of(onesj))
    ## 
    ## See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
names(ndata2) <- sub("Country_j_", "", names(ndata2))

#Now relocating variables for easy of viewing

ndata3 <- ndata2 %>% select(order(colnames(.))) %>%
relocate(Exports, .before = everything()) %>% relocate(GDPi, .before = Exports) %>% relocate(GDPj, .after=GDPi) %>% relocate(dist, .after= GDPj) %>% relocate(ddist_ij, .after= dist)
#Setting USA as the reference country for the dummy variables
ndata4 <- ndata3 %>% select(-USA_i, -USA_j)
vexportsn <- as.vector(ndata4$Exports)
ndata4 <- ndata4 %>% select(-Exports)

sumgdp <- sum(GDP$GDP)
logsumgdp <- log(sumgdp)
```

### Regression New Variables

``` r
#Using the newly added variables, preforming regression again. #No constant term because of using a fixed effect model. 
X3 <- vexportsn
M2 <- data.matrix(ndata4)
M2T<- t(M2)

alpha_hat <- solve(M2T %*% M2) %*%  (M2T %*% X3)

step1 <- M2T %*% M2
step2 <- M2T %*% X3
print(alpha_hat)
```

    ##                  [,1]
    ## GDPi      -18.4821190
    ## GDPj       20.7960436
    ## dist       -1.3924613
    ## ddist_ij    0.2011203
    ## AUS_i     -51.7618679
    ## AUS_j      53.9940797
    ## BEL_i     -71.5453431
    ## BEL_j      74.1097636
    ## CAN_i     -48.5089677
    ## CAN_j      49.5112631
    ## CHE_i     -64.0743095
    ## CHE_j      64.8260696
    ## CHL_i     -83.9154670
    ## CHL_j      86.4251134
    ## COL_i     -81.0417359
    ## COL_j      81.6904278
    ## CZE_i     -88.1451388
    ## CZE_j      90.5689909
    ## DEU_i     -33.5663516
    ## DEU_j      34.5205582
    ## DNK_i     -79.4084445
    ## DNK_j      80.3393881
    ## ESP_i     -54.0114509
    ## ESP_j      55.6945662
    ## EST_i    -127.0752709
    ## EST_j     129.9634662
    ## FIN_i     -84.8316443
    ## FIN_j      86.5293654
    ## GBR_i     -37.7703487
    ## GBR_j      37.3864898
    ## GRC_i     -90.0704291
    ## GRC_j      91.4210605
    ## HUN_i     -94.7693762
    ## HUN_j      97.1031494
    ## IDN_i     -56.8906435
    ## IDN_j      58.0315774
    ## IRL_i     -73.8750648
    ## IRL_j      74.6871407
    ## ISR_i     -77.7513562
    ## ISR_j      78.8044151
    ## ITA_i     -46.1506473
    ## ITA_j      47.1839212
    ## JPN_i     -29.0945250
    ## JPN_j      30.3653484
    ## LTU_i    -115.7742272
    ## LTU_j     118.5768409
    ## LUX_i    -111.6426557
    ## LUX_j     110.1944073
    ## LVA_i    -125.6295257
    ## LVA_j     128.8352304
    ## MEX_i     -56.1168853
    ## MEX_j      56.5643609
    ## NLD_i     -60.8016975
    ## NLD_j      63.3401596
    ## NOR_i     -75.8867351
    ## NOR_j      76.2747909
    ## NZL_i     -88.3074717
    ## NZL_j      91.5231733
    ## POL_i     -68.1492575
    ## POL_j      69.9503398
    ## PRT_i     -88.0793765
    ## PRT_j      90.3382427
    ## SVK_i    -102.7113847
    ## SVK_j     104.1953992
    ## SVN_i    -115.7494861
    ## SVN_j     118.4367102
    ## SWE_i     -69.3887604
    ## SWE_j      70.6065435
    ## TUR_i     -56.4832497
    ## TUR_j      56.9305633
    ## ZAF_i     -77.8487741
    ## ZAF_j      81.5233937

``` r
Xhat <- M2 %*% alpha_hat
```

``` r
e <- X3-Xhat
```

``` r
#to obtain the variance
simga2 <- as.numeric((t(e) %*% e)/(1190-72))
V <-  (solve(t(M2)%*%M2)) * simga2
# square root of variance to obtain SE
se <- sqrt(diag(V))
```

#### Confidence intervals

``` r
alpha <- .05
t <- qt(1-alpha/2, 1190-72)
CI_low <- alpha_hat-t*diag(V)
CI_high <- alpha_hat+t*diag(V)
CI <- cbind(CI_low, CI_high)
print(CI)
```

    ##                  [,1]        [,2]
    ## GDPi      -20.8665445 -16.0976935
    ## GDPj       18.4046960  23.1873911
    ## dist       -1.4020133  -1.3829093
    ## ddist_ij    0.1056026   0.2966379
    ## AUS_i     -68.9638552 -34.5598806
    ## AUS_j      36.6581208  71.3300386
    ## BEL_i    -104.7365868 -38.3540995
    ## BEL_j      40.9041626 107.3153647
    ## CAN_i     -63.5510583 -33.4668772
    ## CAN_j      34.3965963  64.6259299
    ## CHE_i     -90.1962945 -37.9523246
    ## CHE_j      38.6981021  90.9540372
    ## CHL_i    -128.4258391 -39.4050949
    ## CHL_j      41.6745364 131.1756905
    ## COL_i    -121.8187551 -40.2647167
    ## COL_j      40.7169446 122.6639110
    ## CZE_i    -138.3274610 -37.9628166
    ## CZE_j      40.3422080 140.7957738
    ## DEU_i     -41.1163010 -26.0164022
    ## DEU_j      26.9784841  42.0626323
    ## DNK_i    -119.3394750 -39.4774140
    ## DNK_j      40.3871556 120.2916207
    ## ESP_i     -73.0113312 -35.0115706
    ## ESP_j      36.6708427  74.7182896
    ## EST_i    -230.0805609 -24.0699809
    ## EST_j      26.7867522 233.1401801
    ## FIN_i    -130.8222612 -38.8410275
    ## FIN_j      40.4795042 132.5792266
    ## GBR_i     -46.6829927 -28.8577048
    ## GBR_j      28.1350982  46.6378815
    ## GRC_i    -141.2741411 -38.8667171
    ## GRC_j      40.1203389 142.7217822
    ## HUN_i    -152.5475220 -36.9912305
    ## HUN_j      39.2612751 154.9450237
    ## IDN_i     -77.9219064 -35.8593807
    ## IDN_j      36.8673629  79.1957919
    ## IRL_i    -108.6415659 -39.1085637
    ## IRL_j      39.8773242 109.4969571
    ## ISR_i    -115.7800926 -39.7226197
    ## ISR_j      40.6801355 116.9286947
    ## ITA_i     -60.0625131 -32.2387816
    ## ITA_j      33.2700275  61.0978150
    ## JPN_i     -34.8372440 -23.3518059
    ## JPN_j      24.5751079  36.1555889
    ## LTU_i    -201.7151161 -29.8333383
    ## LTU_j      32.4981936 204.6554882
    ## LUX_i    -189.2105665 -34.0747449
    ## LUX_j      32.5359883 187.8528264
    ## LVA_i    -226.1418952 -25.1171563
    ## LVA_j      28.1553852 229.5150756
    ## MEX_i     -75.4333915 -36.8003792
    ## MEX_j      37.1325201  75.9962016
    ## NLD_i     -84.5047896 -37.0986055
    ## NLD_j      39.0624158  87.6179034
    ## NOR_i    -112.1288883 -39.6445818
    ## NOR_j      39.9941855 112.5553963
    ## NZL_i    -138.0107532 -38.6041903
    ## NZL_j      41.5202206 141.5261259
    ## POL_i     -98.1514576 -38.1470575
    ## POL_j      39.9247508  99.9759289
    ## PRT_i    -137.8107270 -38.3480261
    ## PRT_j      40.4955008 140.1809846
    ## SVK_i    -170.3078091 -35.1149604
    ## SVK_j      36.5154895 171.8753089
    ## SVN_i    -201.0916600 -30.4073122
    ## SVN_j      32.9731553 203.9002652
    ## SWE_i    -100.1996682 -38.5778526
    ## SWE_j      39.7734250 101.4396621
    ## TUR_i     -76.7210733 -36.2454260
    ## TUR_j      36.6670157  77.1941109
    ## ZAF_i    -117.3242565 -38.3732917
    ## ZAF_j      41.8463101 121.2004773

### Part 4
