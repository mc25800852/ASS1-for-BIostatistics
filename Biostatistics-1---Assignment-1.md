Biostatistics 1 - Assignment 1
================
Group 3: William Fleury Dziuk, Galya Gadzhalova, Chen Ma,
Afroditi-Iliana Zotou
2025-11-19

## Introduction

Within this assignment we will analyse data on incident cases of colon
cancer in Sweden, across calendar year as well as by age and sex. The
data contain the number of colon cancer cases and demographics of the
population in Sweden (age, year and sex on July 1st). The purpose is to
describe the incidence of colon cancer in Sweden, especially the
incidence pattern across calendar year.

## Libraries

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.4.3

``` r
library(RColorBrewer)
library(scales)
```

    ## Warning: package 'scales' was built under R version 4.4.3

## 1

**Task:** Read in the file on number of colon cancer cases (the file
cases.tsv) and make sure that you understand the variables included.
Create a graph showing the number of cases by age group and sex.
Describe what you can conclude from the graph.

**Answer:**

``` r
#Read in Table
cases <- read.table("cases-1.tsv")

#Need to turn first row into headers
colnames(cases) <- cases[1, ]
cases <- cases[-1, ]
```

``` r
#Clean Up Data Frame Column Types

cases$agegroup <- factor(
  cases$agegroup,
  levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89"),
  ordered = TRUE
)

cases$sex <- factor(cases$sex)
cases$year <- as.numeric(cases$year)
cases$n <- as.numeric(cases$n)
```

``` r
 #Graph showing the number of cases by age group and sex.
ggplot(cases, aes(x = agegroup, y = n, fill = sex)) +
  geom_col() +
  facet_wrap(~ sex) +
  theme_minimal() +
  labs(
    title = "Number of Colon Cancer Cases by Age Group",
    fill = "Sex",
    x = "Age Group",
    y = "Number of Cases"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

![](Biostatistics-1---Assignment-1_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

In our data set we have the following variables:

## 2

**Task:** Obtain the total number of cases in each calendar year by
males and females. Create graphs showing the number of cases over
calendar years, separately for males and females. Describe what you can
conclude from the graphs.

**Answer:**

``` r
#Obtain the total number of cases in each calendar year by males and females
cases_by_year <- aggregate(n ~ year + sex, data = cases, sum)

#Create graphs showing the number of cases over calendar years,
#separately for males and females
ggplot(cases_by_year, aes(x = year, y = n, color = sex)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Cases over Calendar Years",
    color = "Sex",
    x = "Year",
    y = "Cases") +
  theme_minimal()
```

![](Biostatistics-1---Assignment-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 3

**Task:** Read in the file on number of persons at risk (the file
population.tsv). Make sure that you understand the variables included.
Create graphs that illustrate the population size over age groups and
calendar year si multaneously, separately by males and females.

**Answer:**

``` r
#Read in the table
population <- read.table("population-1.tsv")

#Need to turn first row into headers
colnames(population) <- population[1, ]
population <- population[-1, ]
```

``` r
#Clean up the Dataframe
population$agegroup <- factor(
  population$agegroup,
  levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
             "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
             "70-74", "75-79", "80-84", "85-89"),
  ordered = TRUE
)

population$sex <- factor(population$sex)
population$year <- as.numeric(population$year)
population$n_pop <- as.numeric(population$n_pop)
```

``` r
#Create graphs that illustrate the population size over age groups 
#and calendar year simultaneously, separately by males and females.

ggplot(population, aes(x = year, y = n_pop, color = agegroup)) +
  geom_line() +
  #geom_point() +
  facet_wrap(~ sex, ncol = 1) +
  scale_y_continuous(labels = label_number(scale = 1e-3,)) +
  labs(
    title = "Swedish Population by Year and Age Group",
    color = "Age Group",
    x = "Year",
    y = "Population (In Thousands)") +
  theme_minimal()
```

![](Biostatistics-1---Assignment-1_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## 4

**Task:** Merge the information on number of cases and the number of
persons at risk in each year, for each age group and sex. Does the
population file include the same age groups and calendar years as the
file including the number of cases? Also create a separate data frame
with the total number of cases and the total population size in each
calendar year by males and females.

**Answer:**

``` r
#Merged information on number of cases and the number of persons at risk in each year, 
#for each age group and sex. 
merged_df <- merge(cases, population, by = c("agegroup", "year", "sex"))

#Separate data frame with the total number of cases 
#and the total population size in each calendar year by males and females.

cases_sum <- aggregate(n ~ year + sex, data = merged_df, sum)
population_sum <- aggregate(n_pop ~ year + sex, data = merged_df, sum)
summary_df <- merge(cases_sum, population_sum, by = c("year", "sex"))
```

## 5

**Task:** Create a new variable for the incidence rate of colon cancer
by dividing the number of cases with the population size. Do this for
both the data including all the age groups and the data with the total
number of cases per year and sex. Describe shortly what an incidence
rate is, and your thoughts on if this is an appropriate way of
calculating an incidence rate.

**Answer:** We define Incidence rate (IR) as
$\text{Incidence Rate} = \frac{\text{Number of new colon cancer cases}}{\text{Population size}}$

We also calculated the rate per 1.000 person-years to easier interpret
the results.

$\text{Incidence Rate per }1.000=1.000 \times \frac{\text{Number of new colon cancer cases}}{\text{Population size}}$

``` r
# Create incidence rates

# Age incidence rates (agegroup × year × sex)
merged_df$Incidencerate <- merged_df$n / merged_df$n_pop
merged_df$Incidencerate_per1000 <- merged_df$Incidencerate * 1000

# Summary incidence rates (year × sex totals)
summary_df$Incidencerate <- summary_df$n / summary_df$n_pop
summary_df$Incidencerate_per1000 <- summary_df$Incidencerate * 1000

# Example rows 
cat("\n Example rows (summary_df) \n")
```

    ## 
    ##  Example rows (summary_df)

``` r
print(summary_df[sample(nrow(summary_df), 5),
                 c("year", "sex", "n", "n_pop", "Incidencerate",
                   "Incidencerate_per1000")])
```

    ##    year    sex    n   n_pop Incidencerate Incidencerate_per1000
    ## 29 1984 Female 1488 4222072  0.0003524336             0.3524336
    ## 23 1981 Female 1463 4204411  0.0003479679             0.3479679
    ## 47 1993 Female 1679 4424155  0.0003795075             0.3795075
    ## 53 1996 Female 1647 4474782  0.0003680626             0.3680626
    ## 73 2006 Female 1968 4589734  0.0004287830             0.4287830

``` r
cat("\n Example rows (merged_df) \n")
```

    ## 
    ##  Example rows (merged_df)

``` r
print(merged_df[sample(nrow(merged_df), 5),
                c("agegroup", "year", "sex", "n", "n_pop","Incidencerate",
                  "Incidencerate_per1000")])
```

    ##      agegroup year    sex   n  n_pop Incidencerate Incidencerate_per1000
    ## 1654    75-79 2001   Male 360 141848  0.0025379279             2.5379279
    ## 1376    60-64 2021   Male 245 285661  0.0008576600             0.8576600
    ## 1344    60-64 2005   Male 177 288355  0.0006138267             0.6138267
    ## 1873    85-89 2005 Female 304 155175  0.0019590785             1.9590785
    ## 1003      5-9 1994 Female   0 271531  0.0000000000             0.0000000

From the lecture notes “Introduction to Epidimiology” by Adina Feldman,
we define the Incidence rate (IR) as the number of new cases of the
outcome divided by the total person-time at risk, for a specific
follow-up period. It is a factor used to measure individuals who are
newly diagnosed with a disease during a specified period of time.

Theoretically, our definition used to calculate the Incidence rate
matches Feldman’s definition since both measure new cases relative to
the population at risk. However, our definition is a simplified version
of the formal one, since we assumed the population size to be an
approximation for person-time In our case, the exact person-time is not
available and we can also consider that the population does not
dramatically change over the year. Hence, we can conclude that this is
this is a valid way of calculating an incidence rate where detailed
follow‑up is not available.

## 6

**Task:** Plot the incidence rate of colon cancer over calendar time and
apply a smoother, separately by males and females (do this for the
incidence rate based on the total number of cases and the total
population size). Describe what you can conclude from the graphs

**Answer:**

``` r
# Plot the Incidence Rate 
ggplot(summary_df, aes(x = year, y = Incidencerate_per1000, color = sex)) +
  geom_point(alpha = 0.5) +                    # show actual data points
  geom_smooth(method = "loess", se = FALSE) +  # smoother curve
  facet_wrap(~ sex, ncol = 1) +                # separate for males/females
  theme_minimal() +
  labs(
    title = "Incidence Rate of Colon Cancer Over Time",
    x = "Calendar Year",
    y = "Incidence Rate (per 1.000 person-years)",
    color = "Sex"
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Biostatistics-1---Assignment-1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

From the above graph, we notice that the Incidence Rate is steadily
increasing over time for both males and females and it almost doubles in
the span of 50 years. This implies that the diagnosis of colon cancer
became more common and accessible over the decades.Additionally, the
Incidence rates are consistently higher for males meaning that for the
same age period, men have a higher chance of being diagnosed with colon
cancer than females. This means that the trends are similar for both
sexes exhibit but at different absolute levels. As for the LOESS
smoother, the curved line, it clearly shows a long-term upward trend by
ignoring minor fluctuations.

Confidence bands, as shown by the shaded areas, are relatively narrow,
indicating a statistically robust trend. In a more practical
perspective, we can interpret that there have been major improvements in
screening and diagnosing colon cancer over the past decades and
awareness has been raised encouraging testing especially for men.

## 7

**Task:** Since there is a lot of random variation of the incidence rate
from year to year, we can use a regression model to get smooth estimates
of the pattern of the incidence rate across calendar year. Fit a
suitable Poisson model with the total number of cases as dependent
variable, using the population size as an offset, and calendar year and
sex as independent variables.

**Answer:**

We will fit a Poisson Regression model for the dependent variable $Y$
that represents the number of new colon cancer cases. We assume:
$Y_i \sim \text{Poisson}(\lambda_i)$, where $\lambda_i$ is the expected
number of cases for observation $i$.

The model links predictors calendar year and sex to the expected count
via a log-linear model:

$$
\log(\lambda) = \log(\text{population}) + \beta_{0} + \beta_{1} \cdot \text{year} + \beta_{2} \cdot \text{sex}
$$

``` r
# Fit Poisson Regression Model
poisson_model <- glm(
  n ~ year + sex,                  # independent variables
  offset = log(n_pop),            # log of population size as offset
  family = poisson(link = "log"), # Poisson model
  data = summary_df
)

# View model summary
summary(poisson_model)
```

    ## 
    ## Call:
    ## glm(formula = n ~ year + sex, family = poisson(link = "log"), 
    ##     data = summary_df, offset = log(n_pop))
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.969e+01  3.071e-01  -96.68   <2e-16 ***
    ## year         1.094e-02  1.536e-04   71.25   <2e-16 ***
    ## sexMale     -5.592e-02  4.658e-03  -12.01   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 5505.07  on 105  degrees of freedom
    ## Residual deviance:  222.81  on 103  degrees of freedom
    ## AIC: 1211.4
    ## 
    ## Number of Fisher Scoring iterations: 3

*Parameters in the model that can be estimated:* $\beta_0$, $\beta_1$,
and $\beta_2$.

*Estimates that we get:* $\beta_0 = -29.69$, $\beta_1 = 0.01094$,
$\beta_2 = -0.05592$.

The Poisson regression model is defined as:

$$
\log(\lambda) = \log(\text{population}) -29.69 + 0.01094 \cdot \text{year} - 0.05592 \cdot \text{sex} 
$$

where $\lambda$ is the expected number of colon cancer cases and sex is
coded as 1 for males and 0 for females.

*Interpretation of estimates:*

The value $\beta_0 = -29.69$ represents the baseline log incidence rate
for females in year 0.

While year 0 is not meaningful directly, this intercept anchors the
model.

The value $\beta_1 = 0.01094$ means that for each additional calendar
year, the log incidence rate increases by 0.01094. The relative
exponential value $e^{0.01094} \approx 1.011$ means that the incidence
rate increases by approximately $1.1\%$ per year when considering sex
constant.

The value $\beta_2 = -0.05592$ suggests that males have a lower log
incidence rate than females. The relative exponential value
$e^{-0.05592} \approx 0.9456$ means that the incidence rate for males is
about 5.4% lower than for females.

As for the statistical significance, we see that all coefficients have
very small p-values ($< 2 \times 10^{-16}$), indicating strong
statistical significance. We can reject the null hypothesis that year
and sex have no effect on incidence rate.

The differnece between the Null and the Residual deviance suggests that
the model explains well the variation in the data. The value AIC is also
relatively low which is also an indicator for a good model fit.

The Poisson regression model with the total number of cases as dependent
variable, using the population size as an offset, and calendar year and
sex as independent variables fits the data well and provides a valid
estimate of incidence trends across calendar years. It shows that colon
cancer incidence rates in Sweden have increased steadily over time and
that females have slightly higher rates than males when adjusted for
population size.
