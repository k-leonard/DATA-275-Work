---
title: "homework_1_problem_3"
output: html_document
date: "2024-02-02"
---
# Impact Flux

Authors

Paul McSlarrow & Kendall Leonard

Part A:
In this problem, we are asked to look at asteroid impacts on the Moon and investigate the difference in number of strikes over time. We are given one CSV, called impacts, containing the following information about the asteroid impacts: date, time, shower, shower_speed, and Rmag. The date and time are in reference to when the strike occured. Shower is a boolean indicator for if the asteroid was apart of a asteroid shower or if it was a singular occurance. Shower speed is for if the asteroid was apart of the shower, how quickly the impacts occured. The Rmag stands for R magnitude, the R standing for the filter used, and magnitude for the magnitude of the strike.


```{r}
library(tidyverse)
impacts<- read_csv('impacts.csv')
head(impacts)
```

The second file, called obs_nights, has information on the nights that were observed for asteroid impacts. The CSV contains the following information: Avg_Area, Elapsed_Time, Num_Impacts, and Ignored. The average area and elapsed timeis in reference to  how much of the Moon was observed each night and for how long. Number of impacts is self-explatory, as is ignored.

```{r}
obs_nights<-read_csv('obs_nights.csv')
head(obs_nights)
```


To begin, we need to sort out the legitimate strikes. We can tell if a stike is legit by whether or not they have an Rmag listed.
```{r}
real_impacts<- impacts %>%
  filter(Rmag != 'NA')
```

Also, there are some nights in the data that were 'useless' and thus ignored, so we will also filter out nights that we deemed insignifanct.

```{r}

real_nights<- obs_nights%>%
  filter(Ignored == 'FALSE')

```

In order to calculate the overal fluctuation of impacts, we need total number of legitimate impacts divided by the average observation area divided by the total observation time. Lucky for us, we have all that!
```{r}
overall_impact_flux<- count(real_impacts)/(mean(real_nights$Avg_Area)/sum(real_nights$Elapsed_Time))
overall_impact_flux
```
The overall fluctuation of impacts is 3.133668! 

But, due to various quality of equipment, we need to have a cut off point of some kind to make sure our value is able to be compared against by other asteronomers. Space has a lot more small, dim objects than large, bright ones. If we were to categorize our observations based on the measured R magnitude, we would anticipate a greater number of faint impacts (higher magnitude) due to their alignment with an exponential trend. However, in practical terms, detecting dimmer objects becomes increasingly challenging. 

Consequently, our observational bias introduces an artificial suppression in the otherwise exponential rise of faint objects. To illustrate this, we examine the R magnitude distribution of impacts and pinpoint the threshold where the telescope system ceases to effectively capture a representative sample of all events. 

###PART B
```{r}
ggplot(real_impacts, aes(x=Rmag))+
  geom_density()
```

This deviation from the exponential pattern shows as a pronounced decline in the distribution, indicating that beyond this point, fewer impacts are observed than expected. We can see this decline begin at around a magnitude of 7.9.So in order to calculate and impact flux that others can use, we need to filter out the magnitudes that are higher than 7.9.


```{r}
real_impacts_light<- real_impacts %>%
  filter(Rmag <= 7.9)
```


With the magnitudes removed, we can now recalculate the impact flucuation and get a number that other's can use. 
```{r}
overall_impact_flux_light<- count(real_impacts_light)/(mean(real_nights$Avg_Area)/sum(real_nights$Elapsed_Time))
overall_impact_flux_light
```

The final impact fluctuation is 1.971179, a number others can use without the use of more sophisticated equipment!
