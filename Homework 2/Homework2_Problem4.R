---
title: "Hwk2 Q4"
author: "Leila R Fischer"
date: '2024-02-13'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this problem we have been asked to determine which star cluster is older from two datasets. First, we loaded in the provided data and the dplyr/ggplot libraries.
```{r}
library(readr)
star_cluster1 <- read_csv("~/Downloads/DS 275/DS 275 Homework/homework-2-kendall-and-leila-main/data/star_cluster1.csv")
star_cluster1
star_cluster2 <- read_csv("~/Downloads/DS 275/DS 275 Homework/homework-2-kendall-and-leila-main/data/star_cluster2.csv")
star_cluster2
library(ggplot2)
library(dplyr)
```

```{r}
star_cluster1 = star_cluster1 %>% mutate(color = mb-mr)
```

```{r}
star_cluster1 = star_cluster1 %>% mutate(parsecs=1/parallax)
star_cluster1 = star_cluster1 %>% mutate(abs_mag = (-1*(5*log(parsecs/10))-mg))
```

```{r}
cluster1_HR = ggplot(star_cluster1, aes(x=color, y=abs_mag)) + geom_point() + scale_y_reverse()
cluster1_HR
```

```{r}
star_cluster2 = star_cluster2 %>% mutate(color = mb-mr)
star_cluster2 = star_cluster2 %>% mutate(parsecs=1/parallax)
star_cluster2 = star_cluster2 %>% mutate(abs_mag = (-1*(5*log(parsecs/10))-mg))
cluster2_HR = ggplot(star_cluster2, aes(x=color, y=abs_mag)) + geom_point() + scale_y_reverse()
cluster2_HR
```

