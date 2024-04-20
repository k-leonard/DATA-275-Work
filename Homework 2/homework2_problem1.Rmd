---
title: "homework2_problem1"
output: html_document
date: "2024-02-11"
---
# Star Temperature


This problem revolves around the computation of stellar temperatures through spectral analysis. There are various methods that can be employed for this task, but we have chosen to employ Planck’s law due to the available information. Within the given data folder are the spectra of five distinct stars, denoted as star1.csv, star2.csv, ..., star5.csv. Our objective is to ascertain the surface temperature of each star, measured in Kelvin.

As previously stated, we chose to use Planck’s law because, within each CSV are the wavelengths of the light, as well as flux (which is brightness measured in $m^2$). 

Planck's law is the law used to determine the brightness emitted from a body when it is in thermal equilibrium. Planck's law is the following equation: $\left(\frac{8 \pi hc}{\lambda^5}\right) \left(\frac{1}{e^{\frac{hc}{\lambda kT}} - 1}\right)$. To explain the equation, it utilizes some constants and takes some estimated parameters for wavelength and temperature. The constants in the equation are $h$, which the Planck constant, and is equal to 6.6261×10−34, $c$, which represents the speed of light and is equal to 3×10^8. The constant $k_b$ stands for Boltzmann constant, which is equal to 1.381×10−23. Lambda and T are the estimated parameters for wavelength and temperature.

Since we have the information for wavelengths, we can build a model which can help us get a semi-accurate measurement of temperature.

But first, we must examine the data sets. In order to do so, we need to load in the tidyverse for ease of data manipulation, and set our working directory to access our data.
```{r}
setwd("C:/Users/lazyp/Desktop/Willamette Univeristy/SPRING 2024/data275/homework-2-kendall-and-leila-main/data")
library(tidyverse)
```

Since we need to repeat this process for 5 different stars, we will focus on one star at a time. 

## Star #1

First, we read in the data set, and examine the columns. 
```{r}
star_1<-read_csv("star1.csv")

```
After looking at the dataset, it becomes clear that the wavelengths are measured in nanometers, while we will need meters in order to implement Planck's law.
```{r}
star_1
```

Thus, we simply convert the nanometers to meters by dividing by 1E9:

```{r}
star_1$meters<-star_1$wavelength_nm / 1e9
```


After making sure we have the proper information needed for Planck's law, we can build a model.Planck's law is an equation  It is important to note that $A$ in this context still represents the wavelengths, and is a replacement for $\lambda$ in the equation, while $t$ still represents the temperature in kelvin. 

```{r}
model <- nls(
  flux ~ A * 2 * 6.626E-34 * 3E8^2 / meters^5 * 1 / exp(6.626E-34 * 3E8 / (meters * 1.38E-23 * t)) - 1,
  data = star_1,
  start = list(A = 100, t = 10000)
)
```

Now we should check the model to make sure it worked:
```{r}
model
```
In our model, we can see that we have now have values for wavelength and temperature. We can use these coefficients to model our fit against the data to determine how accurate our temperature is.

```{r}

star_1_plot <- ggplot(star_1, aes(meters, flux)) +
  geom_line(aes(color = "Data")) +
  geom_smooth(
    method = "nls",
    formula = y ~ A * 2 * 6.626E-34 * 3E8^2 / x^5 * 1 / exp(6.626E-34 * 3E8 / (x * 1.38E-23 * t)) - 1,
    method.args = list(start = coef(model)),
    se = FALSE,
    aes(color = "Model") 
  ) +
  labs(
    title = "Star 1",
    x = "Meters",
    y = "Flux"
  ) +
  scale_color_manual(values = c("Data" = "black", "Model" = "purple")) + 
  theme_minimal() 


```

We can see that our line (in purple) fits the data pretty well. We can tell this is a good fit because the line goes up with the peak in the data, and trails off with the data. 

```{r}
star_1_plot
```

Our good fit means we can extract the given temperature from the model and save it for our final table.

```{r}
temp1<-6.520e+03

temp1
```
For records sake, the temperature of Star #1 is  6,520 $^{\circ}$ K.  

Now, we need to repeat this process for the other data sets which were provided.
## Star 2

We start anew by importing the data set and analyzing its contents.

```{r}
star_2<- read_csv('star2.csv')
```
After looking at the data set, it once again becomes clear that the wavelengths are measured in nano-meters, while we will need meters in order to implement Planck's law.
```{r}
star_2
```

 Thus, we simply convert the nanometers to meters again:
```{r}
star_2$meters<-star_2$wavelength_nm / 1e9
```

With all the correct pieces, we are once again going a build the model, with the same notes from Star #1:

```{r}
model2 <- nls(
  flux ~ A * 2 * 6.626E-34 * 3E8^2 / meters^5 * 1 / exp(6.626E-34 * 3E8 / (meters * 1.38E-23 * t)) - 1,
  data = star_2,
  start = list(A = 10000, t = 10000)
)

```

We also need to check this model:
```{r}
model2
```
In our model, we can see that we have now have values for wavelength and temperature. We can use these coefficients to model our fit against the data to determine how accurate our temperature is.

```{r}

star_2_plot <- ggplot(star_2, aes(meters, flux)) +
  geom_line(aes(color = "Data")) +
  geom_smooth(
    method = "nls",
    formula = y ~ A * 2 * 6.626E-34 * 3E8^2 / x^5 * 1 / exp(6.626E-34 * 3E8 / (x * 1.38E-23 * t)) - 1,
    method.args = list(start = coef(model2)),
    se = FALSE,
    aes(color = "Model") 
  ) +
  labs(
    title = "Star 2",
    x = "Meters",
    y = "Flux"
  ) +
  scale_color_manual(values = c("Data" = "black", "Model" = "purple")) + 
  theme_minimal() 
```

We can see that our line (in purple) fits the data pretty well. We can tell this is a good fit because the line goes up with the peak in the data, and trails off with the data.
```{r}
star_2_plot
```

Our good fit means we can extract the given temperature from the model and save it for our final table.

```{r}
temp2<-1.004e+04
temp2
```

For the record, the temperature of Star #2 is  10,040 $^{\circ}$ K.  

## Star 3

We start again with reading in the data set and checking the contents:
```{r}
star_3<-read_csv('star3.csv')
```
Upon reviewing the dataset, it is evident once more that the wavelengths are expressed in nanometers. However, to apply Planck's law, we require the measurements in meters.
```{r}
star_3
```
Therefore, we proceed to convert nanometers to meters.

```{r}
star_3$meters<-star_3$wavelength_nm / 1e9
```


After making sure we have the proper information needed for Plack's law, we can build a model. It is important to note that $A$ in this context still represents the wavelengths, and is a replacement for $\lambda$ in the equation, while $t$ still represents the temperature in kelvin.

```{r}
model3 <- nls(
  flux ~ A * 2 * 6.626E-34 * 3E8^2 / meters^5 * 1 / exp(6.626E-34 * 3E8 / (meters * 1.38E-23 * t)) - 1,
  data = star_3,
  start = list(A = 10000, t = 10000)
)
```
We need to check the contents of the model as well:
```{r}
model3
```
Within our model, we observe that we now possess values for both wavelength and temperature. These coefficients can be employed to establish a fit against the data, allowing us to assess the accuracy of our temperature representation.

```{r}
star_3_plot <- ggplot(star_3, aes(meters, flux)) +
  geom_line(aes(color = "Data")) +
  geom_smooth(
    method = "nls",
    formula = y ~ A * 2 * 6.626E-34 * 3E8^2 / x^5 * 1 / exp(6.626E-34 * 3E8 / (x * 1.38E-23 * t)) - 1,
    method.args = list(start = coef(model3)),
    se = FALSE,
    aes(color = "Model") 
  ) +
  labs(
    title = "Star 3",
    x = "Meters",
    y = "Flux"
  ) +
  scale_color_manual(values = c("Data" = "black", "Model" = "purple")) + 
  theme_minimal() 
```
We can see that our line (in purple) fits the data pretty well. We can tell this is a good fit because the line goes up with the peak in the data, and trails off with the data.

```{r}
star_3_plot
```

With this affirmation, we will save the temperature to a variable for later use in creating our final table. 

```{r}
temp3<-4.799e+03
temp3
```

For the record, again, the temperature of Star #3 is 4,799 $^{\circ}$ K. 


And we do it all again:

## Star 4

We read in the CSV with the data and check it out:
```{r}
star_4<-read_csv('star4.csv')
```
Upon revisiting the dataset, it is apparent once again that the wavelengths are denominated in nanometers. Yet, for the application of Planck's law, measurements in meters are essential.
```{r}
star_4
```
 Consequently, we proceed with the conversion of nanometers to meters:

```{r}
star_4$meters<-star_4$wavelength_nm / 1e9
```

After making sure we have the proper information needed for Plack's law, we can build a model. It is important to note that $A$ in this context still represents the wavelengths, and is a replacement for $\lambda$ in the equation, while $t$ still represents the temperature in kelvin.

```{r}
model4 <- nls(
  flux ~ A * 2 * 6.626E-34 * 3E8^2 / meters^5 * 1 / exp(6.626E-34 * 3E8 / (meters * 1.38E-23 * t)) - 1,
  data = star_4,
  start = list(A = 10000, t = 10000)
)
```

Once again, checking the model for accuracy.

```{r}
model4
```
Within our model, we observe that we now possess values for both wavelength and temperature. These coefficients can be employed to establish a fit against the data, allowing us to assess the accuracy of our temperature representation.

```{r}
star_4_plot <- ggplot(star_4, aes(meters, flux)) +
  geom_line(aes(color = "Data")) +
  geom_smooth(
    method = "nls",
    formula = y ~ A * 2 * 6.626E-34 * 3E8^2 / x^5 * 1 / exp(6.626E-34 * 3E8 / (x * 1.38E-23 * t)) - 1,
    method.args = list(start = coef(model2)),
    se = FALSE,
    aes(color = "Model") 
  ) +
  labs(
    title = "Star 4",
    x = "Meters",
    y = "Flux"
  ) +
  scale_color_manual(values = c("Data" = "black", "Model" = "purple")) + 
  theme_minimal() 
```
We can see that our line (in purple) fits the data pretty well. We can tell this is a good fit because the line goes up with the peak in the data, and trails off with the data.

```{r}
star_4_plot
```

With this affirmation, we will save the temperature to a variable for later use in our final table.

```{r}
temp4<-5.841e+03
temp4
```

For record keeping: the temperature of Star #4 is 5,841$^{\circ}$ K. 

And we repeat for the final time:
## Star 5
We read in the corresponding CSV and examine the data 
```{r}
star_5<-read_csv('star5.csv')
```
Upon revisiting the dataset, it is apparent once again that the wavelengths are denominated in nanometers. Yet, for the application of Planck's law, measurements in meters are essential.
```{r}
star_5
```
Consequently, we proceed with the conversion of nanometers to meters:
```{r}
star_5$meters<-star_5$wavelength_nm / 1e9
```

And using this new column, we create the model.  It is important to once again note that $A$ in this context still represents the wavelengths, and is a replacement for $\lambda$ in the equation, while $t$ still represents the temperature in kelvin.
```{r}
model5 <- nls(
  flux ~ A * 2 * 6.626E-34 * 3E8^2 / meters^5 * 1 / exp(6.626E-34 * 3E8 / (meters * 1.38E-23 * t)) - 1,
  data = star_5,
  start = list(A = 1000, t = 10000)
)
```

We then check to make sure that the model worked.
```{r}
model5
```
Within our model, we observe that we now possess values for both wavelength and temperature. These coefficients can be employed to establish a fit against the data, allowing us to assess the accuracy of our temperature representation.

```{r}
star_5_plot <- ggplot(star_5, aes(meters, flux)) +
  geom_line(aes(color = "Data")) +
  geom_smooth(
    method = "nls",
    formula = y ~ A * 2 * 6.626E-34 * 3E8^2 / x^5 * 1 / exp(6.626E-34 * 3E8 / (x * 1.38E-23 * t)) - 1,
    method.args = list(start = coef(model2)),
    se = FALSE,
    aes(color = "Model") 
  ) +
  labs(
    title = "Star 5",
    x = "Meters",
    y = "Flux"
  ) +
  scale_color_manual(values = c("Data" = "black", "Model" = "purple")) + 
  theme_minimal() 
```
We can see that our line (in purple) fits the data pretty well. We can tell this is a good fit because the line goes up with the peak in the data, and trails off with the data.

```{r}
star_5_plot
```

With this affirmation, we will save the temperature to a variable for later use in our final table.

```{r}
temp5<-2.284e+03
temp5
```
For one final time: the temperature of Star #5 is 2,284$^{\circ}$ K. 

Now to fulfill the second part of the task, which is to present our findings in a table with proper labelling.
```{r}
method1<-"Planck's Law"
data= matrix(c(temp1,method1,temp2,method1, temp3,method1,temp4,method1, temp5, method1), ncol=2, byrow=TRUE)
 

rownames(data) = c('Star 1','Star 2','Star 3','Star 4', 'Star 5')
colnames(data) <- c('Temperature In Kelvin','Method')
 

final=as.table(data)
```
## The Final Table
```{r}
final
```

