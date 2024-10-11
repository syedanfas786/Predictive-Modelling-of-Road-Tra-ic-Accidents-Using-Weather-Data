---

---

```{r }setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

## Including Plots



```{r}
  
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("mgcv")
install.packages("dplyr")
install.packages("RcppRoll")
library(tidyverse)
library(lubridate)
library(dplyr)
library(broom)
library(tidyr)
library(ggplot2)
library(mgcv)
```

```{r}
cav_data_link <- 'car_accidents_victoria.csv'
top_row <- read_csv(cav_data_link, col_names = FALSE, n_max = 1)
second_row <- read_csv(cav_data_link, n_max = 1)
column_names <- second_row %>%
  unlist(., use.names=FALSE) %>%
  make.unique(., sep = "__") # double underscore
column_names[2:5] <- str_c(column_names[2:5], '0', sep='__')
accidents <-
read_csv(cav_data_link, skip = 2, col_names = column_names)

WETH_DATA <- read.csv("NASA.csv")
WETH_DATA

```

```{r }
Gather_df <- accidents%>%
  gather(key = 'Regions',
         value = "Value",
         starts_with("new"),
         na.rm = TRUE,
         c(-DATE))
Gather_df


spread_df <- Gather_df %>%
  pivot_wider(names_from = Regions, values_from = Value)
spread_df <- spread_df %>%
  mutate(NewColumn = "Region_1")
spread_df



cleaned_data <- na.omit(accidents)
cleaned_data <- accidents[complete.cases(accidents), ]


print(cleaned_data)
```

```{r }
grouped_data <- cleaned_data %>%
  select(1:5) %>%
  group_by(DATE, FATAL__0, SERIOUS__0, NOINJURY__0, OTHER__0)



grouped_data


grouped_data$DATE <- dmy(grouped_data$DATE)
grouped_data <- transform(grouped_data, ndate = as.numeric(DATE),
                    nyear  = as.numeric(format(DATE, '%Y')),
                    nmonth = as.numeric(format(DATE, '%m')),
                    day    = as.numeric(format(DATE, '%d')))
str(grouped_data)
grouped_data

grouped_data <- grouped_data %>%
  mutate(Value = rowSums(select(., starts_with("FATAL__0"), starts_with("SERIOUS__0"), starts_with("NOINJURY__0"), starts_with("OTHER__0"))))

# Print the resulting data frame
print(grouped_data)
```


```{r }
linear_model <- lm(Value ~ DATE, data = grouped_data)
summary(linear_model)
```

```{r }
rsquared <- summary(linear_model)$r.squared
rsquared
```


```{r }
# Step 3: Plot the observed vs. predicted values
plot(grouped_data$day, grouped_data$Value, main = "Observed vs. Predicted Values")
abline(linear_model, col = "red")
```

```{r }
GEN_ADD_MODEL <- gam(Value ~ s(day), data = grouped_data)
summary(GEN_ADD_MODEL)

grouped_data$Weekday <- weekdays(grouped_data$DATE)
grouped_data

weekday_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
```

```{r }
# converting the weekday names to numeric values
grouped_data$WeekdayNumeric <- match(grouped_data$Weekday, weekday_names)
grouped_data
```

```{r }
#3.5

AIC(linear_model)
AIC(GEN_ADD_MODEL)
BIC(linear_model)
BIC(GEN_ADD_MODEL)
```

```{r }
class(grouped_data$Weekday)

```

```{r }
#4
WETH_DATA <- WETH_DATA %>%
  mutate(TE95 = quantile(MAX_TEM, probs = 0.95, na.rm = TRUE))
WETH_DATA

calculate_EHI <- function(Ti, TE95) {
  return((Ti - TE95))
}
```


```{r }
# Calculating EHI
WETH_DATA <- WETH_DATA %>%
  rowwise() %>%
  mutate(EHI_sig = calculate_EHI(TEM, TE95))
```

```{r }
# calculate EHIaccl (Acclimatization Index)
EHIaccl <- function(Ti, T30DAYS) {
  return((Ti - mean(T30DAYS)))
}

```



```{r }
# Create a new column 'T30DAYS' containing the 30-day rolling average of 'TEMP'
WETH_DATA$T30DAYS <- NA  

# Define a function to calculate the rolling average
rol_avg <- function(data, window_size) {
  result <- numeric(length(data))
  for (i in window_size:length(data)) {
    result[i] <- mean(data[(i - window_size + 1):i], na.rm = TRUE)
  }
  return(result)
}

# Calculate the rolling average and store it in 'T30DAYS'
WETH_DATA$T30DAYS <- rol_avg(WETH_DATA$TEM, window_size = 30)

# View the updated dataframe with 'T30DAYS' column
head(WETH_DATA)


# Calculate EHIaccl for each row
WETH_DATA <- WETH_DATA %>%
  rowwise() %>%
  mutate(EHI_accl = EHIaccl(TEM, T30DAYS))



# Calculate EHF as the product of EHI_sig and the maximum of 1 and EHI_accl
WETH_DATA <- WETH_DATA %>%
  mutate(EHF = EHI_sig * pmax(1, EHI_accl))

# View the updated dataframe with EHF column
head(WETH_DATA)
```

```{r }
WETH_DATA <- WETH_DATA[1:(nrow(WETH_DATA) - 4), ]

```

```{r }
library(ggplot2)

# DATE column
WETH_DATA$DATE <- as.Date(paste(WETH_DATA$YEAR, WETH_DATA$MO, WETH_DATA$DY, sep = "-"))

ggplot(WETH_DATA, aes(x = DATE, y = EHF)) +
  geom_line() +
  labs(x = "Date", y = "EHF Value", title = "EHF Values Over Time")


```


```{r }
ehf_gam <- gam(Value ~ s(day)+s(WETH_DATA$EHF),data=grouped_data,method="REML")
summary(ehf_gam)
glance(ehf_gam)
plot(ehf_gam,rug=TRUE,residuals = TRUE,pch=1,cex=1,shade=TRUE,shade.col="steelblue")
gam.check(ehf_gam)
```

```{r }
extra_gam<-gam(Value ~s(day)+s(WETH_DATA$EHF)+s(WETH_DATA$DEW),data=grouped_data,method="REML")
summary(extra_gam)
glance(extra_gam)
plot(extra_gam,rug=TRUE,residuals = TRUE,pch=1,cex=1,shade=TRUE,shade.col="steelblue")
gam.check(extra_gam)
```
