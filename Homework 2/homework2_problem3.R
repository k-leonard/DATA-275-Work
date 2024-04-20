---
title: "homework2_problem3"
output: html_document
date: "2024-02-13"
---

# HR Diagram


The goal of this problem is to build an HR diagram with data that consists of 1.3 million stars, all within 200 parsecs of Earth, and analyze the diagram to determine if limiting our data has an effect of our diagrma. We will start by loading in the tidyverse for ease of data manipulation as well as setting our working directory to access our data.
```{r}
library(tidyverse)
setwd("C:/Users/lazyp/Desktop/Willamette Univeristy/SPRING 2024/data275/homework-2-kendall-and-leila-main/data")
```

A good place to start is to read in our data and to examine it. From this examination, we are able to see that we are given three wavelengths: G, B and R; standing for Green, Blue, and Red respectively. These wavelegnths are given to us in terms of magnitude, making building an HR diagram that much easier. We are also given a column for the parallax, which we are told in the documentation is measured in milliarcseconds. 

```{r}
gaia<-read_csv("C:/Users/lazyp/Desktop/Willamette Univeristy/SPRING 2024/data275/homework-2-kendall-and-leila-main/data/gaia/gaia_200pc.csv")
gaia
```


Moving on to actually building the HR diagram, the first thing we need to do is to create a value for our x-axis. With the given information, it is much easier to build an observers diagram, which means we can simply subtract the Red magnitude from the Blue magnitude to get an accurate reading of the colors.
```{r}
gaia$horizontal_axis<-gaia$mb-gaia$mr
```


Now that we have our x-axis, we need to move on computing the absolute magnitude; the y-axis for our HR diagram. Unfortunately, it is not as simple as getting the x-axis. First, we need to transform the parallax column to be in arc seconds instead of milli-arcseconds. Thankfully, this is just simple division. 

Because a milliarcsecond is 10^-3 arcseconds, we just need to divide by 1000.
```{r}
gaia$parallax<- gaia$parallax/1000
```


Next, we need to utilize our parallax to get a distance. Because we have the parallax we can utilize the following equation: $d=1/p$, where d is the distance in parsecs, and p is the parallax measured in arc-seconds. This is just another simple division, 1 over our parallax, and we save it a column for later use in calculating the absolute magnitude.
```{r}
##getting distance
gaia$distance<- 1/gaia$parallax

```

Now we need the apparent magnitude of each of these stars, which we get by adding up the green, red, and blue magnitude columns, and it is saved to a new column for use in calculating the absolute magnitude.
```{r}
gaia$apparent_magnitude <- gaia$mg+gaia$mb+gaia$mr
```

Now we can use everything to get the absolute magnitude. We do this by altering the equation $m - M= 5* log10 (distance(pc)/10)$, where $M$ stands for absolute magnitude, while $m$ is for the apparent magnitude. We found it easiest to split up the process into two parts. One applies the log base 10 times 5 and the second part deals with the subtraction. From this we get the absolute magnitude of each star, which we save to a column to reference when making the actual HR diagram.

```{r}
##absolute magnitude
M<-5*log10(gaia$distance/10)
gaia$absolute_magnitude<- gaia$apparent_magnitude - M

head(gaia$absolute_magnitude)
```


Now that we have all of the pieces, next we make some diagrams! We ended up making a few different ones, but settled with one that not only showed the distribution of the stars, but also the frequency. 


```{r}

ggplot(gaia, aes(x=horizontal_axis, y=absolute_magnitude) ) +
  geom_bin2d(bins=1000, alpha = 0.75) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  scale_y_reverse()+
  xlab("Color (B-V)")+
  ylab("Absolute Magnitude")+
  ggtitle("HR Diagram of Stars within 200 parsecs")


```

Now that we have an actual HR diagram, we need to analyze it. Are all the parts of an HR diagram that we would normally expect visible? No, while we have a good chunk of the main sequence and the white dwarfs, we lose a lot of the lower half of the main sequence, as well as some of the upper half, and the giants. By only looking at stars within 200 parsecs, we lost some parts of the diagram. 

