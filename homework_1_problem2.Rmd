---
title: "homework1_problem2"
output: html_document
date: "2024-02-01"
---
# Jupiter's Affect on Asteroids

Authors

Paul McSlarrow & Kendall Leonard


In this problem we have been asked to plot the distribution of asteroids as a function of their average distance from the Sun. To begin, we have been given a CSV which contains data of about 1 million asteroids in our solar system, including their semi-major axis and their eccentricity. 


```{r}
library(tidyverse)
setwd('C:/Users/lazyp/Desktop/Willamette Univeristy/SPRING 2024/data275/homework-1-team1-main/')
asteroids <- read_csv('asteroids.csv')
source('fitellipse.r')
head(asteroids)
```

When we first look at the data, we can immediately dismiss the eccentricty column, as it is not relevent to our goal. We are left with the name of the asteroid and their semi-major axis. 

When we plot the semi-major axis as a histogram to see the distribution and count, we can see some very distinct drops and increases in the distance.
```{r}
ggplot(asteroids, aes(x = a)) +
  geom_histogram(bins = 1000)+
  xlim(1.75, 3.78)+
  xlab("Semi-major Axis (Distance From Sun)")
```
We learn, from the information given in the assignment, that these major drops are due to  orbital resonance with the planet Jupiter. When an asteroid's distance from the Sun aligns its orbital period with a multiple of Jupiter's, it experiences an additional gravitational force, like to a child getting a boost on a swing. Over an extended period, Jupiter's influence gradually clears out asteroids from these specific gaps.

The second part of this question is to identify what resonences are to blame for the shaping of the histogram. We are given the 2:1, 5:2, 3:1, 7:3, and 4:1 resonances and in order to find the others, we need to follow the rule of the resonances. That is, there should be a gap for all irreducible rational numbers with numerator and denominator less than 10 and greater than or equal to 1.

We need to find these numbers, which we accomplish with the following code:

First, we define a gcd function to find assist in finding all irreducible fractions:
```{r}
gcd <- function(a, b) {
  if (b == 0) return(a)
  return(gcd(b, a %% b))
}

```

We also create a vector that we will later reference in the creation of the graphic.
```{r}
irreducible_fractions <- vector()
```

This is for loop will iterate through all fractions with with numerator and denominator less than 10 and greater than or equal to 1. It then checks to see if said fraction is irreducible, and if so, adds it to the previously made vector for further evaluation:
```{r}
for (numerator in 1:9) {
  for (denominator in 1:9) {
    fraction <- numerator / denominator
    # we then check the gcd to see if it is reducible
    checking <- gcd(numerator, denominator)
    if (checking == 1) { # if it isn't, aka gcd of 1, then apply kepler's 3rd law
      irreducible_fractions<-c(irreducible_fractions,fraction)}}}
```


It is important to note that these fractions are not the precise locations of the dips, but a variable needed in the calculations to locate the precise location of the dips. 

This calculation is Kepler's third law, which gives us back a semi-major axis distance from the sun, and the x-intercept we need to plot. For Kepler's third law, we need period of Jupiter, which is easily accessed through the power of Google.
```{r}
Jupiters_period<-11.86
```


Now that we have our list of irreducible fractions and Jupiter's period, we now need to apply Kepler's third Law. No one wants to do this by hand, so we create a function to do it for us:

```{r}
keplers_third_law<-function(a){
  (Jupiters_period/a)^(2/3)
}
```

And then a loop to apply it and a vector to keep it:

```{r}

keplers_fractions<-vector()

for (i in 1:length(irreducible_fractions)) {
  evaluated_fraction <- irreducible_fractions[i]
  evaluated_fraction<- keplers_third_law(evaluated_fraction)
  keplers_fractions<-c(keplers_fractions,evaluated_fraction)
}
```

Now we need to narrow down the values to fit within our given parameters,those being between 1.75 to 3.5 AU. We know our numbers are in astronomical units because we applied Kepler's third law to convert them. We will add these values to a list to be plotted.

```{r}
x_intercepts <- numeric()  # or c()

for (i in 1:length(keplers_fractions)) {
  fraction_being_checked <- keplers_fractions[i]
  if (fraction_being_checked >= 1.75 && fraction_being_checked <= 3.5) {
    x_intercepts <- c(x_intercepts, fraction_being_checked)
  }
}

     
```

Then, we need to create a graphic showing those x-intercepts on our histogram:
```{r}
# creating the graphic
ggplot(asteroids, aes(x = a)) +
  geom_histogram(bins = 1000) +
  xlim(1.75, 3.78) +
geom_vline(xintercept = x_intercepts, linetype = "dashed", color = "red", size = 1)
```

```{r}
odd_x_intercepts<-vector()
odd_x_intercepts<-c(odd_x_intercepts, x_intercepts[6])
odd_x_intercepts<-c(odd_x_intercepts, x_intercepts[9])

```

```{r}
ggplot(asteroids, aes(x = a)) +
  geom_histogram(bins = 1000) +
  xlim(1.75, 3.78) +
geom_vline(xintercept = x_intercepts, linetype = "dashed", color = "red", size = 1)+
  geom_vline(xintercept = odd_x_intercepts, linetype = "dashed", color = "green", size = 1)
```

For most of these lines, we can see that it is located at a significant dip in density. For those in green, there are extremely faint, if competely inpercepible dips in the histogram. We attempt to find the dips by adjusting the bins:
```{r}
ggplot(asteroids, aes(x = a)) +
  geom_histogram(bins = 10000) +
  xlim(1.75, 3.78) +
geom_vline(xintercept = x_intercepts, linetype = "dashed", color = "red", size = 1)+
  geom_vline(xintercept = odd_x_intercepts, linetype = "dashed", color = "green", size = 1)
```



And with an EXTREME amount of bins, we can see the very slight dips where those lines are located.
