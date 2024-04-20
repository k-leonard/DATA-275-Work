---
title: "HW2 Q2"
author: "Leila R Fischer"
date: '2024-02-13'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this problem we are tasked with determining the radial velocity of an unknown star. First, we read in the attached data.
```{r}
library(readr)
speedstar <- read_csv("~/Downloads/DS 275/DS 275 Homework/homework-2-kendall-and-leila-main/data/speedstar.csv")
speedstar
```
Then we loaded in ggplot and dplyr libraries for data manipulation and visualization.
```{r}
library(ggplot2)
library(dplyr)
```
First, we needed to visualize where distinct spectral peaks might be. We graphed the speedstar data up to 700 nm below, because there were no distinct peaks after 700 and this allowed us to see the consequential data more closely.
```{r}
spectrum = speedstar %>% filter(wavelength_nm<700) %>% ggplot(aes(x=wavelength_nm, y=flux)) + geom_line()
spectrum
```
The first spectral peak we focused on was at approximately 410 nm, known to be the characteristic peak of H(delta). Below we graphed a more magnified area around this peak and saved the data points in this range as a new data frame called h_delta. We also created a new data frame including just background data points, which we qualitatively derived from the h_delta graph.
```{r}
hd_range = speedstar %>% filter(wavelength_nm>405) %>% filter(wavelength_nm<414)
h_delta = hd_range %>%  ggplot(aes(x=wavelength_nm, y=flux)) + geom_line()
h_delta
hd_background = hd_range %>% filter(wavelength_nm<410 | wavelength_nm>412.5)
```
Next, we needed to fit a linear regression model to the background data in order to correct our spectral peak data. We did this by using the lm function so establish the linear relationship between flux and wavelength in the hd_background data frame.
```{r}
hd_model = lm(flux ~ wavelength_nm, data=hd_background)
hd_model
```
After establishing the model, we created a new data frame called hd_corrected in which we subtracted the predicted values based on the linear regression from the flux values in the hd_range data frame. We then added this as a column to the hd_range data frame.
```{r}
hd_corrected = hd_range$flux - predict(hd_model, newdata=hd_range)
hd_range$hd_corrected = data.frame(hd_corrected)
```
Finally, we can plot the corrected data as a scatter plot and gather the observed wavelength of 411.1498 nm.
```{r}
ggplot(hd_range, aes(x=wavelength_nm, y=hd_corrected$hd_corrected)) +geom_point()
```

The process of selecting a spectral peak, background correcting, and deriving the observed wavelength is completed four more times below.  

```{r}
na_range = speedstar %>% filter(wavelength_nm>585) %>% filter(wavelength_nm<593)
ggplot(na_range, aes(x=wavelength_nm, y=flux)) + geom_point()
```
```{r}
na_background = na_range %>% filter(wavelength_nm<586 | wavelength_nm>591)
na_model = lm(flux ~ wavelength_nm, data=na_background)
na_corrected = na_range$flux - predict(na_model, newdata=na_range)
na_range$na_corrected = data.frame(na_corrected)
ggplot(na_range, aes(x=wavelength_nm, y=na_corrected$na_corrected)) +geom_point()
```

```{r}
fe_range = speedstar %>% filter(wavelength_nm>437.5) %>% filter(wavelength_nm<443)
ggplot(fe_range, aes(x=wavelength_nm, y=flux)) + geom_point()
```
```{r}
fe_background = fe_range %>% filter(wavelength_nm<439 | wavelength_nm>441)
fe_model = lm(flux ~ wavelength_nm, data=fe_background)
fe_corrected = fe_range$flux - predict(fe_model, newdata=fe_range)
fe_range$fe_corrected = data.frame(fe_corrected)
ggplot(fe_range, aes(x=wavelength_nm, y=fe_corrected$fe_corrected)) +geom_point()
```

```{r}
hb_range = speedstar %>% filter(wavelength_nm>483) %>% filter(wavelength_nm<493)
ggplot(hb_range, aes(x=wavelength_nm, y=flux)) + geom_point()
```
```{r}
hb_background = hb_range %>% filter(wavelength_nm<485 | wavelength_nm>490)
hb_model = lm(flux ~ wavelength_nm, data=hb_background)
hb_corrected = hb_range$flux - predict(hb_model, newdata=hb_range)
hb_range$hb_corrected = data.frame(hb_corrected)
ggplot(hb_range, aes(x=wavelength_nm, y=hb_corrected$hb_corrected)) +geom_point()
```

```{r}
ha_range = speedstar %>% filter(wavelength_nm>653) %>% filter(wavelength_nm<663)
ggplot(ha_range, aes(x=wavelength_nm, y=flux)) + geom_point()
```
```{r}
ha_background = hb_range %>% filter(wavelength_nm<656 | wavelength_nm>660)
ha_model = lm(flux ~ wavelength_nm, data=ha_background)
ha_corrected = ha_range$flux - predict(ha_model, newdata=ha_range)
ha_range$ha_corrected = data.frame(ha_corrected)
ggplot(ha_range, aes(x=wavelength_nm, y=ha_corrected$ha_corrected)) +geom_point()
```

