# Homework 1 -- Team 1 (Paul McSlarrow and Kendall Leonard)
#setwd('Desktop/Data\ in\ the\ Cosmos/homework-1-team1/')
# ```{r}
# install.packages("rmarkdown")
# ```
# 


# Preprocessing

```{r echo=T, Ex1}
library(tidyverse)
source('fitellipse.r')
jupiter_data <- data.frame(read_csv('data/jupiter.csv'))
moon_data <- data.frame(read_csv('data/moon.csv'))

getwd()
head(jupiter_data, 4)
head(moon_data, 4)
```

```{r echo=T, Ex2}
format_RA_hms <- function(df) {
   return (
      df %>%
         mutate(RA = as.character(RA)) %>%
         separate(RA, into = c("RA_hours", "RA_minutes", "RA_seconds"), sep=" ") %>%
         mutate(
            RA_hours = as.numeric(RA_hours),
            RA_minutes = as.numeric(RA_minutes),
            RA_seconds = as.numeric(RA_seconds),
         ) %>%
         mutate(
            RA = paste(sprintf("%02d", RA_hours), sprintf("%02d", RA_minutes), sprintf("%05.2f", RA_seconds), sep = ":")
         )
   )
}

format_DEC_hms <- function(df) {
   return (
      df %>%
         mutate(DEC = as.character(DEC)) %>%
         separate(DEC, into = c("DEC_arc_hours", "DEC_arc_minutes", "DEC_arc_seconds"), sep=" ") %>%
         mutate(
            DEC_arc_hours = as.numeric(DEC_arc_hours),
            DEC_arc_minutes = as.numeric(DEC_arc_minutes),
            DEC_arc_seconds = as.numeric(DEC_arc_seconds),
         ) %>%
         mutate(
            DEC = paste(sprintf("%02d", DEC_arc_hours), sprintf("%02d", DEC_arc_minutes), sprintf("%05.2f", DEC_arc_seconds), sep = ":")
         )
   )
}
```


# Creating hour, minute, and second columns in numeric type
```{r echo=T, Ex3}
jupiter_data <- format_RA_hms(jupiter_data)
jupiter_data <- format_DEC_hms(jupiter_data)

moon_data <- format_RA_hms(moon_data)
moon_data <- format_DEC_hms(moon_data)
```


# Calculating the declination and right ascension based on the data-points (h m s)
```{r echo=T, Ex4}
jupiter_data <- jupiter_data %>%
   mutate(
      declination = (jupiter_data$DEC_arc_hours) + (jupiter_data$DEC_arc_minutes / 60) + (jupiter_data$DEC_arc_seconds / 3600),
      right_ascension = ((jupiter_data$RA_hours) + (jupiter_data$RA_minutes / 60) + (jupiter_data$RA_seconds / 3600)) * 15
   )

moon_data <- moon_data %>%
   mutate(
      declination = (moon_data$DEC_arc_hours) + (moon_data$DEC_arc_minutes / 60) + (moon_data$DEC_arc_seconds / 3600),
      right_ascension = ((moon_data$RA_hours) + (moon_data$RA_minutes / 60) + (moon_data$RA_seconds / 3600)) * 15
   )
```

# Selecting the necessary columns 
```{r echo=T, Ex5}
jupiter <- jupiter_data %>% select(Date, JD, Dist_AU, declination, right_ascension)
moon <- moon_data %>% select(Date, JD, declination, right_ascension)
head(jupiter, 4)
head(moon, 4)
```

# Joining Jupiter and Moon DF together
```{r echo=T, Ex6}
master_df <- cbind(jupiter, setNames(moon, paste0(names(moon), "_moon")))
master_df <- master_df %>%
   mutate(
      RA = right_ascension - right_ascension_moon,
      DEC = declination - declination_moon
   )
master_df<-master_df %>%
   filter(JD > 2459960)
```

# Plotting the orbit against JD to find peaks and troughs
```{r echo=T, Ex7}
ggplot(
   master_df,
   aes(x=JD, y=DEC)
) + geom_line()
```

# Calculating the Orbital Period
```{r echo=T, Ex8}
min_jd <- 2459973
max_jd <- 2459991
number_of_days <- max_jd - min_jd
years <- number_of_days / 365.25
```

# Calculating the semi major axis
```{r echo=T, Ex9}
fit <- fit_ellipse(x=master_df$RA, y=master_df$DEC)
semi_major <- fit$semimajor
```

# Calculating theta
```{r echo=T, Ex10}
master_df <- master_df %>%
   mutate(
      theta = sqrt(((right_ascension - right_ascension_moon) * cos(declination))^2 + (declination - declination_moon)^2 )
   )
```

# Calculating angular separation
```{r echo=T, Ex11}
average_distance <- mean(master_df$Dist_AU)

master_df <- master_df %>%
   mutate(
      angular_sep = ((2*pi*average_distance) * (semi_major / 360))
   )
```

# Selecting the necessary columns

```{r echo=T, Ex12}
master_df <- master_df %>%
   select(-declination, -right_ascension, -Date_moon, -JD_moon, -declination_moon, -right_ascension_moon)
```

# Calculating total mass
```{r echo=T, Ex13}
total_mass <- (mean(master_df$angular_sep)**3) / (years**2)
total_mass
```






