# Analysis of Coverage
Bernhard Konrad  
December 22, 2015  

In this document we run a data quality analysis to check the observed receiver coverage. This is to control for outages, receivers not being in the water, or any other reason why a receiver did not detect any or as many fish as we would expect.

First, let's load the required packages and data.


```r
library(rgeos)
```

```
## rgeos version: 0.3-15, (SVN revision 515)
##  GEOS runtime version: 3.4.2-CAPI-1.8.2 r3921 
##  Linking to sp version: 1.1-1 
##  Polygon checking: TRUE
```

```r
library(maptools)
```

```
## Loading required package: sp
## Checking rgeos availability: TRUE
## 
## Attaching package: 'maptools'
## 
## The following object is masked from 'package:sp':
## 
##     nowrapSpatialLines
```

```r
library(klesdatr)
library(magrittr)
library(lubridate)
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following objects are masked from 'package:rgeos':
## 
##     intersect, setdiff, union
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

receiver <- klesdatr::receiver %>% 
  mutate(Section_n = as.integer(Section),
         Receiver_n = as.integer(Receiver))
glimpse(receiver)
```

```
## Observations: 32
## Variables:
## $ Receiver   (fctr) Gerrard Bridge, Gerrard Scafolding, Lardeau R. at ...
## $ Section    (fctr) Gerrard, Gerrard, Gerrard, Duncan Dam, Duncan Delt...
## $ ReceiverX  (int) 480380, 480626, 480847, 503432, 504927, 507121, 507...
## $ ReceiverY  (int) 5595360, 5595336, 5595246, 5566010, 5556704, 554540...
## $ Section_n  (int) 2, 2, 2, 5, 7, 9, 11, 11, 13, 13, 15, 17, 17, 17, 2...
## $ Receiver_n (int) 11, 12, 16, 8, 15, 10, 13, 14, 31, 30, 32, 4, 3, 2,...
```

```r
#fish_with_location <- klesdatr::calculate_location() %>%
#  mutate(Section_n = as.integer(Section))
#print(glimpse(fish_with_location))

detection <- klesdatr::detection %>% 
  mutate(Receiver_n = as.integer(Receiver))
glimpse(detection)
```

```
## Observations: 57110
## Variables:
## $ DetectionDate (date) 2008-05-17, 2008-05-18, 2008-05-20, 2008-05-20,...
## $ Fish          (fctr) 2, 2, 3, 3, 4, 2, 3, 3, 4, 4, 2, 2, 3, 3, 6, 6,...
## $ Receiver      (fctr) Woodbury Point, Woodbury Point, Pilot Point Mid...
## $ Hours         (int) 3, 1, 1, 1, 3, 1, 6, 6, 2, 1, 5, 3, 10, 7, 3, 3,...
## $ Receiver_n    (int) 32, 32, 23, 24, 23, 2, 20, 21, 18, 19, 23, 24, 1...
```

```r
section <- klesdatr::section %>%
  mutate(Section_n = as.integer(Section))
glimpse(section)
```

```
## Observations: 34
## Variables:
## $ Section   (fctr) Lardeau River, Gerrard, Trout Lake, Duncan Reservoi...
## $ System    (fctr) Lardeau River, Lardeau River, Lardeau River, Duncan...
## $ Area      (dbl) 3.2030167, 0.0309608, 28.5256199, 72.9419079, 0.1124...
## $ SectionX  (int) 494546, 480564, 469091, 502869, 503461, 503599, 5043...
## $ SectionY  (int) 5580270, 5595333, 5603774, 5585727, 5566212, 5561065...
## $ Section_n (int) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1...
```


We have 32 distinct receivers, but let's start by looking at aggregated statistics.


## Aggregate statistics

First we plot the total number of sightings by section. We copy code from `klesdatr::plot_section()` and aggregate the detection dates by section.


```r
section_with_detections <- section %>% 
  dplyr::right_join(detection %>%
                      join(receiver) %>% 
                      group_by(Section_n) %>% 
                      summarize(total_sightings = n()) %>% 
                      mutate(fraction_total_sightings = total_sightings/sum(total_sightings))
                    )
```

```
## Joining by: Receiver, Receiver_n
## Joining by: "Section_n"
```

```r
if (requireNamespace("rgeos", quietly = TRUE) & requireNamespace("maptools", quietly = TRUE)) {
    section_sp  <- klesdatr::section_sp %>% ggplot2::fortify(region = "Section")
    poly <- list(ggplot2::geom_polygon(data = dplyr::filter_(section_sp, ~!hole),
                                       ggplot2::aes_string(x = "long / 1000", y = "lat / 1000", group = "id"),
                                       alpha = 1/5,
                                       color = "grey50"),
                 ggplot2::geom_polygon(data = dplyr::filter_(section_sp, ~!hole),
                                       ggplot2::aes_string(x = "long / 1000", y = "lat / 1000", group = "id"),
                                       alpha = 1/5,
                                       color = "grey50"))
    } else {
    message("The packages 'maptools' and rgeos' are required for the full version of this spatial plot.")
    poly <- NULL
    }

ggplot2::ggplot(data = section_with_detections,
                ggplot2::aes_string(x = "SectionX / 1000", y = "SectionY / 1000")) +
    ggplot2::geom_point(aes(size = fraction_total_sightings)) +
    poly + 
    ggplot2::geom_text(aes(x = SectionX / 1000 + 2.5, label = Section_n), color = "blue", nudge_x = 2, size = 6) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(name = "Easting (km)") +
    ggplot2::scale_y_continuous(name = "Northing (km)")
```

![](coverage_analysis_files/figure-html/unnamed-chunk-2-1.png) 


Looking at the `DetectionDate` we see that the number of sightings is nowhere near constant...


```r
detection %>% 
  group_by(DetectionDate) %>% 
  summarize(total_sightings = n()) %>% 
  ggplot(aes(x = DetectionDate, y = total_sightings, label = total_sightings)) +
    geom_density(stat = "identity")
```

![](coverage_analysis_files/figure-html/unnamed-chunk-3-1.png) 

...but there don't seem to be obvious seasonal patterns.


```r
detection %>% 
  mutate(month = lubridate::month(DetectionDate)) %>% 
  group_by(month) %>% 
  summarize(total_detections = n()) %>% 
  ggplot(aes(x = month, y = total_detections, label = month.abb[month])) +
    geom_bar(stat = "identity") +
    geom_text(nudge_y = 80)
```

![](coverage_analysis_files/figure-html/unnamed-chunk-4-1.png) 

[Todo: Add in how many receivers were active]


## Individual receivers

Looking at the number of sightings per receiver we see huge differences. Indeed, some receivers rarely picked up any signals.


```r
detection %>% 
  group_by(Receiver_n) %>% 
  summarize(total_sightings = n()) %>% 
  ggplot(aes(x = Receiver_n, y = total_sightings, label = total_sightings)) +
    geom_bar(stat = "identity") +
    geom_text(color = "red", vjust = 0)
```

![](coverage_analysis_files/figure-html/unnamed-chunk-5-1.png) 

Here is a zoom at all the sightings for each receiver


```r
detection %>% 
#  filter(Receiver_n == 1) %>% 
  ggplot(aes(x = DetectionDate)) + 
    geom_point(aes(y = 0), shape = 124) +
    geom_density(kernel = "epanechnikov") +
    facet_wrap(~ Receiver, scales = "free_y")
```

![](coverage_analysis_files/figure-html/unnamed-chunk-6-1.png) 


[Todo: Highlight gaps in detection. Did other receivers in the same section get any signal during those gaps?]
