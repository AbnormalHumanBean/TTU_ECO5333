Gravity Model Part 1
================
Ivanna Tindle
2023-09-16

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
Data <- merge(dist_ij,export,by=c("LOCATION","PARTNER")) %>%
  #Merging distance and exports, removing rows for those without information in both distance and exports. This step changes the dim the most. 
  merge(GDP, by="LOCATION") %>% 
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
#Again making sure that the matrix is as expected. 
```

Data is listed as long. Each pair has its own row.

## Reshaping Data Set

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

summary(Data_wide)
```

    ##   Country_i              GDPi             GDPj_ARG         GDPj_AUT     
    ##  Length:35          Min.   :   28891   Min.   :616053   Min.   :405146  
    ##  Class :character   1st Qu.:  212931   1st Qu.:616053   1st Qu.:405146  
    ##  Mode  :character   Median :  420794   Median :616053   Median :405146  
    ##                     Mean   : 1389652   Mean   :616053   Mean   :405146  
    ##                     3rd Qu.: 1168978   3rd Qu.:616053   3rd Qu.:405146  
    ##                     Max.   :20529463   Max.   :616053   Max.   :405146  
    ##                                                                         
    ##     GDPj_BEL         GDPj_CAN          GDPj_CHE         GDPj_CHL     
    ##  Min.   :498161   Min.   :1690931   Min.   :760153   Min.   :274772  
    ##  1st Qu.:498161   1st Qu.:1690931   1st Qu.:760153   1st Qu.:274772  
    ##  Median :498161   Median :1690931   Median :760153   Median :274772  
    ##  Mean   :498161   Mean   :1690931   Mean   :760153   Mean   :274772  
    ##  3rd Qu.:498161   3rd Qu.:1690931   3rd Qu.:760153   3rd Qu.:274772  
    ##  Max.   :498161   Max.   :1690931   Max.   :760153   Max.   :274772  
    ##  NA's   :1        NA's   :1         NA's   :1        NA's   :1       
    ##     GDPj_COL         GDPj_CRI        GDPj_CZE         GDPj_DEU      
    ##  Min.   :330935   Min.   :66427   Min.   :211014   Min.   :3554676  
    ##  1st Qu.:330935   1st Qu.:66427   1st Qu.:211014   1st Qu.:3554676  
    ##  Median :330935   Median :66427   Median :211014   Median :3554676  
    ##  Mean   :330935   Mean   :66427   Mean   :211014   Mean   :3554676  
    ##  3rd Qu.:330935   3rd Qu.:66427   3rd Qu.:211014   3rd Qu.:3554676  
    ##  Max.   :330935   Max.   :66427   Max.   :211014   Max.   :3554676  
    ##  NA's   :1                        NA's   :1        NA's   :1        
    ##     GDPj_DNK         GDPj_ESP          GDPj_EST        GDPj_FIN     
    ##  Min.   :346752   Min.   :1238778   Min.   :28891   Min.   :256629  
    ##  1st Qu.:346752   1st Qu.:1238778   1st Qu.:28891   1st Qu.:256629  
    ##  Median :346752   Median :1238778   Median :28891   Median :256629  
    ##  Mean   :346752   Mean   :1238778   Mean   :28891   Mean   :256629  
    ##  3rd Qu.:346752   3rd Qu.:1238778   3rd Qu.:28891   3rd Qu.:256629  
    ##  Max.   :346752   Max.   :1238778   Max.   :28891   Max.   :256629  
    ##  NA's   :1        NA's   :1         NA's   :2       NA's   :1       
    ##     GDPj_FRA          GDPj_GBR          GDPj_GRC         GDPj_HUN     
    ##  Min.   :2577596   Min.   :3038581   Min.   :201203   Min.   :150830  
    ##  1st Qu.:2577596   1st Qu.:3038581   1st Qu.:201203   1st Qu.:150830  
    ##  Median :2577596   Median :3038581   Median :201203   Median :150830  
    ##  Mean   :2577596   Mean   :3038581   Mean   :201203   Mean   :150830  
    ##  3rd Qu.:2577596   3rd Qu.:3038581   3rd Qu.:201203   3rd Qu.:150830  
    ##  Max.   :2577596   Max.   :3038581   Max.   :201203   Max.   :150830  
    ##                    NA's   :2         NA's   :1        NA's   :1       
    ##     GDPj_IDN          GDPj_IRL         GDPj_ISL        GDPj_ISR     
    ##  Min.   :1065711   Min.   :455950   Min.   :20051   Min.   :382100  
    ##  1st Qu.:1065711   1st Qu.:455950   1st Qu.:20051   1st Qu.:382100  
    ##  Median :1065711   Median :455950   Median :20051   Median :382100  
    ##  Mean   :1065711   Mean   :455950   Mean   :20051   Mean   :382100  
    ##  3rd Qu.:1065711   3rd Qu.:455950   3rd Qu.:20051   3rd Qu.:382100  
    ##  Max.   :1065711   Max.   :455950   Max.   :20051   Max.   :382100  
    ##  NA's   :1         NA's   :1        NA's   :3       NA's   :1       
    ##     GDPj_ITA          GDPj_JPN          GDPj_KOR          GDPj_LTU    
    ##  Min.   :1867907   Min.   :4462649   Min.   :1693951   Min.   :51069  
    ##  1st Qu.:1867907   1st Qu.:4462649   1st Qu.:1693951   1st Qu.:51069  
    ##  Median :1867907   Median :4462649   Median :1693951   Median :51069  
    ##  Mean   :1867907   Mean   :4462649   Mean   :1693951   Mean   :51069  
    ##  3rd Qu.:1867907   3rd Qu.:4462649   3rd Qu.:1693951   3rd Qu.:51069  
    ##  Max.   :1867907   Max.   :4462649   Max.   :1693951   Max.   :51069  
    ##  NA's   :1         NA's   :1                           NA's   :2      
    ##     GDPj_LUX        GDPj_LVA        GDPj_MEX          GDPj_NLD     
    ##  Min.   :68994   Min.   :31300   Min.   :1206921   Min.   :857603  
    ##  1st Qu.:68994   1st Qu.:31300   1st Qu.:1206921   1st Qu.:857603  
    ##  Median :68994   Median :31300   Median :1206921   Median :857603  
    ##  Mean   :68994   Mean   :31300   Mean   :1206921   Mean   :857603  
    ##  3rd Qu.:68994   3rd Qu.:31300   3rd Qu.:1206921   3rd Qu.:857603  
    ##  Max.   :68994   Max.   :31300   Max.   :1206921   Max.   :857603  
    ##  NA's   :4       NA's   :1       NA's   :1         NA's   :1       
    ##     GDPj_NOR         GDPj_NZL         GDPj_POL         GDPj_PRT     
    ##  Min.   :420794   Min.   :214847   Min.   :598792   Min.   :215126  
    ##  1st Qu.:420794   1st Qu.:214847   1st Qu.:598792   1st Qu.:215126  
    ##  Median :420794   Median :214847   Median :598792   Median :215126  
    ##  Mean   :420794   Mean   :214847   Mean   :598792   Mean   :215126  
    ##  3rd Qu.:420794   3rd Qu.:214847   3rd Qu.:598792   3rd Qu.:215126  
    ##  Max.   :420794   Max.   :214847   Max.   :598792   Max.   :215126  
    ##  NA's   :1        NA's   :1        NA's   :1        NA's   :1       
    ##     GDPj_SAU         GDPj_SVK         GDPj_SVN        GDPj_SWE     
    ##  Min.   :672127   Min.   :100845   Min.   :52164   Min.   :570953  
    ##  1st Qu.:672127   1st Qu.:100845   1st Qu.:52164   1st Qu.:570953  
    ##  Median :672127   Median :100845   Median :52164   Median :570953  
    ##  Mean   :672127   Mean   :100845   Mean   :52164   Mean   :570953  
    ##  3rd Qu.:672127   3rd Qu.:100845   3rd Qu.:52164   3rd Qu.:570953  
    ##  Max.   :672127   Max.   :100845   Max.   :52164   Max.   :570953  
    ##  NA's   :1        NA's   :2        NA's   :1       NA's   :1       
    ##     GDPj_TUR          GDPj_USA           GDPj_ZAF         GDPj_AUS      
    ##  Min.   :1131036   Min.   :20529463   Min.   :353716   Min.   :1417576  
    ##  1st Qu.:1131036   1st Qu.:20529463   1st Qu.:353716   1st Qu.:1417576  
    ##  Median :1131036   Median :20529463   Median :353716   Median :1417576  
    ##  Mean   :1131036   Mean   :20529463   Mean   :353716   Mean   :1417576  
    ##  3rd Qu.:1131036   3rd Qu.:20529463   3rd Qu.:353716   3rd Qu.:1417576  
    ##  Max.   :1131036   Max.   :20529463   Max.   :353716   Max.   :1417576  
    ##  NA's   :1         NA's   :1          NA's   :1        NA's   :1        
    ##     dist_ARG        dist_AUT           dist_BEL          dist_CAN      
    ##  Min.   : 1128   Min.   :   59.62   Min.   :  173.0   Min.   :  548.4  
    ##  1st Qu.:10539   1st Qu.:  817.97   1st Qu.:  930.4   1st Qu.: 6033.8  
    ##  Median :11581   Median : 1283.60   Median : 1462.7   Median : 6666.8  
    ##  Mean   :11052   Mean   : 3878.82   Mean   : 4006.6   Mean   : 7425.3  
    ##  3rd Qu.:12270   3rd Qu.: 6860.62   3rd Qu.: 5996.6   3rd Qu.: 7860.3  
    ##  Max.   :18372   Max.   :18322.31   Max.   :19011.8   Max.   :15815.4  
    ##                                     NA's   :1         NA's   :1        
    ##     dist_CHE          dist_CHL        dist_COL        dist_CRI    
    ##  Min.   :  309.9   Min.   : 4269   Min.   : 3089   Min.   : 1158  
    ##  1st Qu.:  781.4   1st Qu.:11386   1st Qu.: 8887   1st Qu.: 8887  
    ##  Median : 1567.1   Median :12193   Median : 9599   Median : 9764  
    ##  Mean   : 4040.5   Mean   :11738   Mean   : 9546   Mean   : 9347  
    ##  3rd Qu.: 6398.6   3rd Qu.:13079   3rd Qu.:10229   3rd Qu.:10242  
    ##  Max.   :19006.7   Max.   :17247   Max.   :19772   Max.   :18767  
    ##  NA's   :1         NA's   :1       NA's   :1                      
    ##     dist_CZE          dist_DEU          dist_DNK          dist_ESP      
    ##  Min.   :  293.1   Min.   :  173.5   Min.   :  485.3   Min.   :  500.9  
    ##  1st Qu.:  711.7   1st Qu.:  843.3   1st Qu.:  805.5   1st Qu.: 1511.2  
    ##  Median : 1263.8   Median : 1347.0   Median : 1158.7   Median : 2493.8  
    ##  Mean   : 3946.0   Mean   : 3960.4   Mean   : 3995.6   Mean   : 4597.0  
    ##  3rd Qu.: 6659.3   3rd Qu.: 6129.3   3rd Qu.: 6240.9   3rd Qu.: 5972.9  
    ##  Max.   :18419.0   Max.   :18824.8   Max.   :18247.0   Max.   :19586.2  
    ##  NA's   :1         NA's   :1         NA's   :1         NA's   :1        
    ##     dist_EST           dist_FIN           dist_FRA          dist_GBR      
    ##  Min.   :   80.98   Min.   :   80.98   Min.   :  262.4   Min.   :  323.8  
    ##  1st Qu.: 1345.68   1st Qu.: 1432.54   1st Qu.:  996.7   1st Qu.: 1230.8  
    ##  Median : 1794.21   Median : 1944.05   Median : 1545.8   Median : 1680.4  
    ##  Mean   : 4087.89   Mean   : 4295.32   Mean   : 3977.3   Mean   : 4240.1  
    ##  3rd Qu.: 6645.82   3rd Qu.: 6622.15   3rd Qu.: 5921.4   3rd Qu.: 5715.7  
    ##  Max.   :17411.42   Max.   :17362.61   Max.   :19263.9   Max.   :19147.1  
    ##  NA's   :2          NA's   :1                            NA's   :2        
    ##     dist_GRC          dist_HUN          dist_IDN        dist_IRL      
    ##  Min.   :  560.7   Min.   :  159.1   Min.   : 5512   Min.   :  460.4  
    ##  1st Qu.: 1712.2   1st Qu.:  986.0   1st Qu.:10148   1st Qu.: 1454.0  
    ##  Median : 2378.8   Median : 1415.7   Median :10765   Median : 1925.5  
    ##  Mean   : 4598.7   Mean   : 4053.2   Mean   :11239   Mean   : 4328.1  
    ##  3rd Qu.: 8000.8   3rd Qu.: 7101.2   3rd Qu.:11643   3rd Qu.: 5221.9  
    ##  Max.   :17523.3   Max.   :18128.4   Max.   :19772   Max.   :19023.8  
    ##  NA's   :1         NA's   :1         NA's   :1       NA's   :1        
    ##     dist_ISL        dist_ISR        dist_ITA          dist_JPN    
    ##  Min.   : 1499   Min.   : 1123   Min.   :  492.3   Min.   : 5792  
    ##  1st Qu.: 2287   1st Qu.: 2679   1st Qu.: 1205.6   1st Qu.: 8619  
    ##  Median : 2846   Median : 3261   Median : 1866.8   Median : 9358  
    ##  Mean   : 4457   Mean   : 5315   Mean   : 4235.1   Mean   : 9718  
    ##  3rd Qu.: 4174   3rd Qu.: 8417   3rd Qu.: 7040.7   3rd Qu.: 9822  
    ##  Max.   :17586   Max.   :16325   Max.   :18572.2   Max.   :17247  
    ##  NA's   :3       NA's   :1       NA's   :1         NA's   :1      
    ##     dist_KOR        dist_LTU          dist_LUX          dist_LVA      
    ##  Min.   : 1157   Min.   :  263.5   Min.   :  187.8   Min.   :  263.5  
    ##  1st Qu.: 7854   1st Qu.:  918.5   1st Qu.:  811.7   1st Qu.: 1088.8  
    ##  Median : 8544   Median : 1542.7   Median : 1325.9   Median : 1742.9  
    ##  Mean   : 8972   Mean   : 3975.0   Mean   : 3086.7   Mean   : 4165.2  
    ##  3rd Qu.: 9498   3rd Qu.: 6956.3   3rd Qu.: 2539.5   3rd Qu.: 6772.1  
    ##  Max.   :18375   Max.   :17568.3   Max.   :16686.9   Max.   :17559.4  
    ##                  NA's   :2         NA's   :4         NA's   :1        
    ##     dist_MEX        dist_NLD          dist_NOR          dist_NZL    
    ##  Min.   : 3089   Min.   :  173.0   Min.   :  417.6   Min.   : 2333  
    ##  1st Qu.: 9213   1st Qu.:  933.7   1st Qu.: 1068.5   1st Qu.:14991  
    ##  Median : 9880   Median : 1414.9   Median : 1522.4   Median :17826  
    ##  Mean   : 9686   Mean   : 3997.1   Mean   : 4167.9   Mean   :16130  
    ##  3rd Qu.:10345   3rd Qu.: 5957.7   3rd Qu.: 5935.4   3rd Qu.:18762  
    ##  Max.   :16863   Max.   :18867.0   Max.   :17991.7   Max.   :19586  
    ##  NA's   :1       NA's   :1         NA's   :1         NA's   :1      
    ##     dist_POL          dist_PRT          dist_SAU        dist_SVK      
    ##  Min.   :  393.8   Min.   :  500.9   Min.   : 2460   Min.   :  159.1  
    ##  1st Qu.:  853.6   1st Qu.: 1943.2   1st Qu.: 3983   1st Qu.:  891.6  
    ##  Median : 1352.9   Median : 2922.4   Median : 4567   Median : 1294.9  
    ##  Mean   : 4035.9   Mean   : 4859.3   Mean   : 6284   Mean   : 3743.1  
    ##  3rd Qu.: 6908.3   3rd Qu.: 5654.4   3rd Qu.: 7299   3rd Qu.: 6852.8  
    ##  Max.   :17914.2   Max.   :19335.4   Max.   :14890   Max.   :18263.9  
    ##  NA's   :1         NA's   :1         NA's   :1       NA's   :2        
    ##     dist_SVN          dist_SWE          dist_TUR          dist_USA      
    ##  Min.   :  308.9   Min.   :  377.9   Min.   :  560.7   Min.   :  548.4  
    ##  1st Qu.:  934.2   1st Qu.: 1121.4   1st Qu.: 1608.4   1st Qu.: 5898.0  
    ##  Median : 1528.4   Median : 1588.8   Median : 2176.0   Median : 6639.7  
    ##  Mean   : 4041.7   Mean   : 4153.1   Mean   : 4591.9   Mean   : 7350.7  
    ##  3rd Qu.: 6908.9   3rd Qu.: 6332.2   3rd Qu.: 8165.6   3rd Qu.: 7699.9  
    ##  Max.   :18478.3   Max.   :17738.6   Max.   :17234.5   Max.   :16180.3  
    ##  NA's   :1         NA's   :1         NA's   :1         NA's   :1        
    ##     dist_ZAF        dist_AUS      Exports_ARG         Exports_AUT       
    ##  Min.   : 7544   Min.   : 2333   Min.   :6.802e+06   Min.   :3.567e+06  
    ##  1st Qu.: 9065   1st Qu.:15023   1st Qu.:6.832e+07   1st Qu.:1.370e+08  
    ##  Median : 9638   Median :15702   Median :1.563e+08   Median :5.828e+08  
    ##  Mean   : 9958   Mean   :14690   Mean   :5.976e+08   Mean   :4.823e+09  
    ##  3rd Qu.:10454   3rd Qu.:16505   3rd Qu.:5.975e+08   3rd Qu.:3.655e+09  
    ##  Max.   :14746   Max.   :18191   Max.   :7.739e+09   Max.   :8.156e+10  
    ##  NA's   :1       NA's   :1                                              
    ##   Exports_BEL         Exports_CAN         Exports_CHE       
    ##  Min.   :1.343e+08   Min.   :4.079e+07   Min.   :6.683e+07  
    ##  1st Qu.:1.031e+09   1st Qu.:3.611e+08   1st Qu.:7.448e+08  
    ##  Median :2.743e+09   Median :1.014e+09   Median :1.424e+09  
    ##  Mean   :9.354e+09   Mean   :1.129e+10   Mean   :6.826e+09  
    ##  3rd Qu.:7.930e+09   3rd Qu.:3.401e+09   3rd Qu.:4.173e+09  
    ##  Max.   :7.439e+10   Max.   :3.069e+11   Max.   :7.383e+10  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_CHL         Exports_COL         Exports_CRI       
    ##  Min.   :7.396e+06   Min.   :1.291e+06   Min.   :1.343e+06  
    ##  1st Qu.:7.743e+07   1st Qu.:3.489e+07   1st Qu.:7.063e+06  
    ##  Median :2.372e+08   Median :1.308e+08   Median :3.655e+07  
    ##  Mean   :1.049e+09   Mean   :9.041e+08   Mean   :3.132e+08  
    ##  3rd Qu.:8.207e+08   3rd Qu.:6.748e+08   3rd Qu.:1.616e+08  
    ##  Max.   :1.734e+10   Max.   :1.645e+10   Max.   :7.312e+09  
    ##  NA's   :1           NA's   :1                              
    ##   Exports_CZE         Exports_DEU         Exports_DNK       
    ##  Min.   :5.453e+06   Min.   :5.690e+08   Min.   :7.321e+06  
    ##  1st Qu.:2.058e+08   1st Qu.:3.357e+09   1st Qu.:1.976e+08  
    ##  Median :6.798e+08   Median :1.345e+10   Median :8.163e+08  
    ##  Mean   :4.084e+09   Mean   :2.729e+10   Mean   :2.616e+09  
    ##  3rd Qu.:3.229e+09   3rd Qu.:3.817e+10   3rd Qu.:2.769e+09  
    ##  Max.   :5.467e+10   Max.   :1.571e+11   Max.   :2.447e+10  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_ESP         Exports_EST         Exports_FIN       
    ##  Min.   :1.038e+08   Min.   :3.062e+06   Min.   :9.944e+06  
    ##  1st Qu.:1.141e+09   1st Qu.:3.125e+07   1st Qu.:1.815e+08  
    ##  Median :2.372e+09   Median :2.101e+08   Median :5.121e+08  
    ##  Mean   :6.899e+09   Mean   :5.667e+08   Mean   :1.779e+09  
    ##  3rd Qu.:9.298e+09   3rd Qu.:4.239e+08   3rd Qu.:1.762e+09  
    ##  Max.   :5.194e+10   Max.   :2.813e+09   Max.   :1.419e+10  
    ##  NA's   :1           NA's   :2           NA's   :1          
    ##   Exports_FRA         Exports_GBR         Exports_GRC       
    ##  Min.   :1.903e+08   Min.   :4.115e+08   Min.   :1.918e+07  
    ##  1st Qu.:1.341e+09   1st Qu.:1.543e+09   1st Qu.:8.797e+07  
    ##  Median :5.905e+09   Median :4.424e+09   Median :3.727e+08  
    ##  Mean   :1.566e+10   Mean   :1.222e+10   Mean   :1.108e+09  
    ##  3rd Qu.:1.531e+10   3rd Qu.:1.406e+10   3rd Qu.:1.230e+09  
    ##  Max.   :1.211e+11   Max.   :7.753e+10   Max.   :8.414e+09  
    ##                      NA's   :2           NA's   :1          
    ##   Exports_HUN         Exports_IDN         Exports_IRL       
    ##  Min.   :2.130e+06   Min.   :6.905e+06   Min.   :1.361e+07  
    ##  1st Qu.:1.102e+08   1st Qu.:3.028e+07   1st Qu.:1.138e+08  
    ##  Median :4.346e+08   Median :1.720e+08   Median :7.007e+08  
    ##  Mean   :2.592e+09   Mean   :1.261e+09   Mean   :2.403e+09  
    ##  3rd Qu.:1.983e+09   3rd Qu.:8.688e+08   3rd Qu.:1.459e+09  
    ##  Max.   :3.395e+10   Max.   :1.331e+10   Max.   :2.910e+10  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_ISL         Exports_ISR         Exports_ITA       
    ##  Min.   :  1018557   Min.   :1.283e+07   Min.   :1.727e+08  
    ##  1st Qu.:  9804806   1st Qu.:1.788e+08   1st Qu.:9.931e+08  
    ##  Median : 60402831   Median :3.505e+08   Median :4.011e+09  
    ##  Mean   :179919533   Mean   :1.441e+09   Mean   :9.361e+09  
    ##  3rd Qu.:137194968   3rd Qu.:1.589e+09   3rd Qu.:1.078e+10  
    ##  Max.   :882606940   Max.   :1.282e+10   Max.   :8.887e+10  
    ##  NA's   :3           NA's   :1           NA's   :1          
    ##   Exports_JPN         Exports_KOR         Exports_LTU       
    ##  Min.   :9.029e+07   Min.   :6.256e+07   Min.   :3.029e+06  
    ##  1st Qu.:4.813e+08   1st Qu.:5.925e+08   1st Qu.:7.094e+07  
    ##  Median :2.027e+09   Median :1.395e+09   Median :2.966e+08  
    ##  Mean   :6.966e+09   Mean   :6.678e+09   Mean   :8.626e+08  
    ##  3rd Qu.:8.089e+09   3rd Qu.:4.692e+09   3rd Qu.:9.188e+08  
    ##  Max.   :7.496e+10   Max.   :6.577e+10   Max.   :5.514e+09  
    ##  NA's   :1                               NA's   :2          
    ##   Exports_LUX         Exports_LVA         Exports_MEX       
    ##  Min.   :2.060e+06   Min.   :1.716e+06   Min.   :8.669e+06  
    ##  1st Qu.:3.358e+07   1st Qu.:5.128e+07   1st Qu.:2.422e+08  
    ##  Median :9.675e+07   Median :1.955e+08   Median :8.049e+08  
    ##  Mean   :7.528e+08   Mean   :5.255e+08   Mean   :1.003e+10  
    ##  3rd Qu.:4.824e+08   3rd Qu.:5.281e+08   3rd Qu.:1.787e+09  
    ##  Max.   :7.774e+09   Max.   :3.801e+09   Max.   :2.765e+11  
    ##  NA's   :4           NA's   :1           NA's   :1          
    ##   Exports_NLD         Exports_NOR         Exports_NZL       
    ##  Min.   :5.560e+08   Min.   :3.429e+07   Min.   :1.092e+07  
    ##  1st Qu.:2.077e+09   1st Qu.:9.747e+07   1st Qu.:5.337e+07  
    ##  Median :4.654e+09   Median :8.278e+08   Median :1.198e+08  
    ##  Mean   :1.209e+10   Mean   :2.220e+09   Mean   :6.827e+08  
    ##  3rd Qu.:1.222e+10   3rd Qu.:2.274e+09   3rd Qu.:4.103e+08  
    ##  Max.   :1.089e+11   Max.   :2.023e+10   Max.   :8.156e+09  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_POL         Exports_PRT         Exports_SAU       
    ##  Min.   :7.451e+07   Min.   :1.762e+07   Min.   :3.236e+07  
    ##  1st Qu.:6.547e+08   1st Qu.:8.704e+07   1st Qu.:1.313e+08  
    ##  Median :2.795e+09   Median :3.223e+08   Median :4.516e+08  
    ##  Mean   :6.889e+09   Mean   :2.020e+09   Mean   :1.547e+09  
    ##  3rd Qu.:6.009e+09   3rd Qu.:1.441e+09   3rd Qu.:1.886e+09  
    ##  Max.   :9.239e+10   Max.   :2.938e+10   Max.   :1.114e+10  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_SVK         Exports_SVN         Exports_SWE       
    ##  Min.   :1.026e+06   Min.   :3.048e+06   Min.   :4.340e+07  
    ##  1st Qu.:4.627e+07   1st Qu.:5.698e+07   1st Qu.:2.488e+08  
    ##  Median :1.953e+08   Median :2.368e+08   Median :1.563e+09  
    ##  Mean   :2.030e+09   Mean   :1.007e+09   Mean   :4.007e+09  
    ##  3rd Qu.:1.133e+09   3rd Qu.:8.717e+08   3rd Qu.:5.839e+09  
    ##  Max.   :1.812e+10   Max.   :8.727e+09   Max.   :3.148e+10  
    ##  NA's   :2           NA's   :1           NA's   :1          
    ##   Exports_TUR         Exports_USA         Exports_ZAF       
    ##  Min.   :2.365e+07   Min.   :4.187e+08   Min.   :1.242e+07  
    ##  1st Qu.:6.412e+08   1st Qu.:4.255e+09   1st Qu.:1.571e+08  
    ##  Median :1.439e+09   Median :1.232e+10   Median :3.676e+08  
    ##  Mean   :3.159e+09   Mean   :4.537e+10   Mean   :1.074e+09  
    ##  3rd Qu.:3.042e+09   3rd Qu.:3.140e+10   3rd Qu.:9.399e+08  
    ##  Max.   :2.529e+10   Max.   :3.861e+11   Max.   :9.647e+09  
    ##  NA's   :1           NA's   :1           NA's   :1          
    ##   Exports_AUS       
    ##  Min.   :2.796e+07  
    ##  1st Qu.:3.040e+08  
    ##  Median :9.727e+08  
    ##  Mean   :2.873e+09  
    ##  3rd Qu.:2.410e+09  
    ##  Max.   :2.643e+10  
    ##  NA's   :1

``` r
# Double checking again that it makes sense. 

length(unique(Data_wide$Country_i))
```

    ## [1] 35

``` r
dim.data.frame(Data_wide)
```

    ## [1]  35 128
