---
title: "Reproducible research project 2"
author: "Sergio"
date: "2/9/2020"
output: 
  html_document:
    keep_md: true
---
# Impact of Severe Weather Events on Public Health and Economy in the United States

## Sinopsis 
In this report, I analyzed the impact of different weather events on public health and economy based on the storm database collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011. I will use the estimates of fatalities, injuries, property and crop damage toaddress the following questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

After analize these data, we found that excessive heat and tornado are the most harmful with respect to population health, while flood, drought, and hurricane/typhoon have the greatest economic consequences

## Data Processing

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

* Storm Data [47Mb](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)


* National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)




Load Libraries

```r
library(dplyr)
library(tidyr)
library(ggplot2)
```

First, Downloaded the file and unzip to obtain a csv file. Then, we read the generated csv file. If the data already exists in the working environment, we do not need to load it again. Otherwise, we read the csv file.

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!dir.exists("./data")){
    #print("Created dir")
    dir.create("./data")
}

if (!"stormData.csv.bz2" %in% dir("./data/")) {
    #print("Downdloadind Zip")
    download.file(fileUrl, destfile = "data/stormData.csv.bz2",method = "curl")
}
stormdata<-read.csv("stormData.csv.bz2")
```

There are 902297 rows and 37 columns in total.

```r
print(dim(stormdata))
head(stormdata, 5)
```

```
## [1] 902297     37
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE  EVTYPE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL TORNADO
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL TORNADO
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL TORNADO
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL TORNADO
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL TORNADO
##   BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN
## 1         0                                               0         NA
## 2         0                                               0         NA
## 3         0                                               0         NA
## 4         0                                               0         NA
## 5         0                                               0         NA
##   END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES INJURIES PROPDMG
## 1         0                      14.0   100 3   0          0       15    25.0
## 2         0                       2.0   150 2   0          0        0     2.5
## 3         0                       0.1   123 2   0          0        2    25.0
## 4         0                       0.0   100 2   0          0        2     2.5
## 5         0                       0.0   150 2   0          0        2     2.5
##   PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES LATITUDE LONGITUDE
## 1          K       0                                         3040      8812
## 2          K       0                                         3042      8755
## 3          K       0                                         3340      8742
## 4          K       0                                         3458      8626
## 5          K       0                                         3412      8642
##   LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1       3051       8806              1
## 2          0          0              2
## 3          0          0              3
## 4          0          0              4
## 5          0          0              5
```

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.


```r
stormdata$year<-format(strptime(stormdata$BGN_DATE,"%m/%d/%Y %H:%M:%S"),"%Y")
hist(as.numeric(stormdata$year), breaks = 30)
```

![](Reproducible_research_project2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



Based on the above histogram, we see that the number of events tracked starts to significantly increase around 1995.

## Examing column names

```r
colnames(stormdata)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"     "year"
```

## Data Subsetting
I subset the dataset on the parameters of interest. Basically, I removed the columns we don't need for clarity

```r
stormdata<-stormdata%>%
    filter(FATALITIES>0 | INJURIES>0| PROPDMG > 0 | CROPDMG > 0)%>%
    select(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)
stormdata
```

Changing Letters to uppercase to make the analysis

```r
stormdata$PROPDMGEXP<-sapply(stormdata$PROPDMGEXP, toupper)
stormdata$CROPDMGEXP<-sapply(stormdata$CROPDMGEXP, toupper)
```

Converting the property damage and crop damage data into comparable numerical forms according to the meaning of units described in the code book. Both PROPDMGEXP and CROPDMGEXP columns record a multiplier for each observation:

* 2 = 10^2
* 3 = 10^3
* 4 = 10^4
* 5 = 10^5
* 6 = 10^6
* 7 = 10^7
* Billion (B) = 10^9 
* Hundred (H) = 10^2
* Thousand (K) = 10^3
* Million (M) = 10^6



```r
stormdata$PROPDMGEXP<-recode(stormdata$PROPDMGEXP,"2"=10^2,
                             "3"=10^3,
                             "4"=10^4,
                             "5"=10^5,
                             "6"=10^6,
                             "7"=10^7,
                             "B"=10^9, 
                             "H"=10^2, 
                             "K"=10^3,
                             "M" = 10^6, .default = 0)
stormdata$CROPDMGEXP<-recode(stormdata$CROPDMGEXP,"B"=10^9, 
                             "H"=10^2,
                             "K"=10^3,
                             "M" = 10^6, .default = 0)
```

Creating Economic Cost Columns

```r
stormdata<-stormdata%>%mutate(propCost=PROPDMG*PROPDMGEXP,
                              cropCost=CROPDMG*CROPDMGEXP)
head(stormdata)
```

```
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP propCost
## 1 TORNADO          0       15    25.0       1000       0          0    25000
## 2 TORNADO          0        0     2.5       1000       0          0     2500
## 3 TORNADO          0        2    25.0       1000       0          0    25000
## 4 TORNADO          0        2     2.5       1000       0          0     2500
## 5 TORNADO          0        2     2.5       1000       0          0     2500
## 6 TORNADO          0        6     2.5       1000       0          0     2500
##   cropCost
## 1        0
## 2        0
## 3        0
## 4        0
## 5        0
## 6        0
```

Calculating Impact on Economy based on Property and Crop damage cost and selecting the top 15 events


```r
#top 15 Cost Damage
Top15_Damage<-stormdata%>%group_by(EVTYPE)%>%
    summarise(propCost=sum(propCost),
              cropCost=sum(cropCost),
              TotalCost=sum(propCost)+sum(cropCost))%>%
    arrange(desc(TotalCost))%>%head(15)
```

Calculating Impact on Public Health based on the number of  fatalities and injuries that are caused by the severe weather events and selecting the top 15


```r
Top15_Harm<-stormdata%>%group_by(EVTYPE)%>%
    summarise(FATALITIES=sum(FATALITIES),
              INJURIES=sum(INJURIES),
              Total=sum(FATALITIES)+sum(INJURIES))%>%
    arrange(desc(Total))%>%head(15)
```

## Results 

Events that are Most Harmful to Population Health, taking in account the amount of injuries and fatalities.

```r
Top15_Harm
```

```
## # A tibble: 15 x 4
##    EVTYPE            FATALITIES INJURIES Total
##    <chr>                  <dbl>    <dbl> <dbl>
##  1 TORNADO                 5633    91346 96979
##  2 EXCESSIVE HEAT          1903     6525  8428
##  3 TSTM WIND                504     6957  7461
##  4 FLOOD                    470     6789  7259
##  5 LIGHTNING                816     5230  6046
##  6 HEAT                     937     2100  3037
##  7 FLASH FLOOD              978     1777  2755
##  8 ICE STORM                 89     1975  2064
##  9 THUNDERSTORM WIND        133     1488  1621
## 10 WINTER STORM             206     1321  1527
## 11 HIGH WIND                248     1137  1385
## 12 HAIL                      15     1361  1376
## 13 HURRICANE/TYPHOON         64     1275  1339
## 14 HEAVY SNOW               127     1021  1148
## 15 WILDFIRE                  75      911   986
```

Reshaping data in order to plot

```r
Top15_Harm<-gather(Top15_Harm,key = "Type",value ="values", -EVTYPE)
```

Plotting Impact on Public Health results

```r
ggplot(Top15_Harm,aes(x=reorder(EVTYPE,-values),y=values,fill=Type))+
    geom_col(position = "dodge")+
    xlab("Severe Weather Type")+ ylab("Total number of health impact") +
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    ggtitle("Top 15 Fatalities and Injuries by \nSevere Weather Events in\n the U.S. from 1995 - 2011")+
    theme(plot.title = element_text(hjust = 0.5))
```

![](Reproducible_research_project2_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
#reshaping data to original form
#Top15_Harm<-Top15_Harm%>%spread(Type,values)%>%arrange(desc(Total))
```

Based on the above histograms, tornato causes most injuries and fatalities in the United States from 1995 to 2011.


Events that have the greatest economic consequences, taking in account Property and Crop damage cost

```r
Top15_Damage
```

```
## # A tibble: 15 x 4
##    EVTYPE                propCost    cropCost    TotalCost
##    <chr>                    <dbl>       <dbl>        <dbl>
##  1 FLOOD             144657709800  5661968450 150319678250
##  2 HURRICANE/TYPHOON  69305840000  2607872800  71913712800
##  3 TORNADO            56947380480   414953110  57362333590
##  4 STORM SURGE        43323536000        5000  43323541000
##  5 HAIL               15735267220  3025954450  18761221670
##  6 FLASH FLOOD        16822673510  1421317100  18243990610
##  7 DROUGHT             1046106000 13972566000  15018672000
##  8 HURRICANE          11868319010  2741910000  14610229010
##  9 RIVER FLOOD         5118945500  5029459000  10148404500
## 10 ICE STORM           3944927810  5022113500   8967041310
## 11 TROPICAL STORM      7703890550   678346000   8382236550
## 12 WINTER STORM        6688497250    26944000   6715441250
## 13 HIGH WIND           5270046260   638571300   5908617560
## 14 WILDFIRE            4765114000   295472800   5060586800
## 15 TSTM WIND           4484928440   554007350   5038935790
```

Reshaping data in order to plot

```r
Top15_Damage<-gather(Top15_Damage,key = "Type",value = "values",-EVTYPE)
```

Plotting Impact on Economy results

```r
ggplot(Top15_Damage,aes(x=reorder(EVTYPE,-values),y=(values/10^6),fill=Type))+
    geom_col(position = "dodge")+
    xlab("Severe Weather Type")+ ylab("Cost ( Million dollars)") +
    #scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
    ggtitle("Top 15 Crop and Property Damage by \nSevere Weather Events in\n the U.S. from 1995 - 2011")+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    theme(plot.title = element_text(hjust = 0.5))
```

![](Reproducible_research_project2_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
#reshaping data to original form
#Top15_Damage<-Top15_Damage%>%spread(Type,values)%>%arrange(desc(TotalCost))
```

Based on the above histograms, flood and hurricane/typhoon cause most property damage; drought and flood causes most crop damage in the United States from 1995 to 2011.However flood is the The most devastating weather event with the greatest economic cosequences (dollars) is a flood.
