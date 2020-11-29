options(scipen=999)
library(dplyr)
library(ggplot2)
library(car)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(plotly)
## set wd
setwd('C:\\Users\\Vidusha\\Documents\\MSA\\SideProjectIdeas\\covid')

## read in data
covid = read.csv(file = 'jhcoviddata.csv', stringsAsFactors = FALSE)
by.state.stats = read.csv(file = 'NonCovidByStateData.csv', stringsAsFactors = FALSE)

## col names
covidcn = colnames(covid)

## get data by state
## filter out unwanted areas

bystate = covid %>%
  filter(location_type == 'county') %>%
  filter(state != 'District of Columbia') %>%
  filter(state != 'Puerto Rico') %>%
  group_by(state) %>%
  summarize(totalcases = sum(confirmed, na.rm = TRUE),
            totaldeaths = sum(deaths, na.rm = TRUE),
            statepop = sum(total_population, na.rm = TRUE),
            casesper100k = (totalcases/statepop) * 100000,
            deathsper100k = (totaldeaths/statepop) * 100000)

## merge covid stats with noncovid stats
bystate.combined = merge(by.state.stats, bystate, by = 'state')
bystate.combined$PopDensity = bystate.combined$statepop / bystate.combined$landarea
bystate.combined = bystate.combined[,-2]
############################
## breakdown by urbanization
############################

urbanlevel = covid %>%
  filter(location_type == 'county') %>%
  filter(state != 'District of Columbia') %>%
  filter(state != 'Puerto Rico') %>%
  group_by(NCHS_urbanization) %>%
  summarize(totalcases = sum(confirmed, na.rm = TRUE),
            totaldeaths = sum(deaths, na.rm = TRUE),
            pop = sum(total_population, na.rm = TRUE),
            casesper100k = (totalcases/pop) * 100000,
            deathsper100k = (totaldeaths/pop) * 100000)
## filter where urban not missing
covid.urban.plot = covid %>%
  filter(NCHS_urbanization != '')

## plot cases per urban type
ggplot(data = covid.urban.plot, aes(x = NCHS_urbanization, y = confirmed_per_100000, fill = NCHS_urbanization)) +
  geom_boxplot() +
  theme(legend.position = 'none') +
  stat_summary(fun = mean, shape = 15, color = 'black') +
  ggtitle('Cases per 100,000 for each Urbanization Level') +
  labs(x = 'Urbanization Level', y = 'Confirmed Cases per 100,000')

## CASED BASED ANOVA
cases.anova = aov(confirmed_per_100000 ~ NCHS_urbanization, data = covid.urban.plot)
summary(cases.anova)

## assume independence between counties

## check normality
plot(cases.anova, 2)
hist(cases.anova$residuals)
## errors have a slight right skew

##check group variances
plot(cases.anova, 1)
leveneTest(confirmed_per_100000 ~ NCHS_urbanization, data = covid.urban.plot)
## significant differences between variances

## use one way test
cases.oneway = oneway.test(confirmed_per_100000 ~ NCHS_urbanization, data = covid.urban.plot, var.equal = FALSE)
print(cases.oneway)
## significant differences between groups

## TUKEY
tukey.cases = TukeyHSD(cases.anova)
plot(tukey.cases, las=1)

## Significant Differences:
casedf = as.data.frame(tukey.cases$NCHS_urbanization)


## plot deaths per urban type
ggplot(data = covid.urban.plot, aes(x = NCHS_urbanization, y = deaths_per_100000, fill = NCHS_urbanization)) +
  geom_boxplot() +
  theme(legend.position = 'none') +
  stat_summary(fun = mean, shape = 15, color = 'black') +
  ggtitle('Deaths per 100,000 for each Urbanization Level') +
  labs(x = 'Urbanization Level', y = 'Confirmed Deaths per 100,000')

## DEATH BASED ANOVA
deaths.anova = aov(deaths_per_100000 ~ NCHS_urbanization, data = covid.urban.plot)
summary(cases.anova)

## assume independence between counties

## check normality
plot(deaths.anova, 2)
hist(deaths.anova$residuals)
## errors have a stronger right skew

##check group variances
plot(cases.anova, 1)
leveneTest(confirmed_per_100000 ~ NCHS_urbanization, data = covid.urban.plot)
## significant differences between variances

## use one way test
deaths.oneway = oneway.test(deaths_per_100000 ~ NCHS_urbanization, data = covid.urban.plot, var.equal = FALSE)
print(deaths.oneway)
## significant differences between groups but not as significant as cases

## TUKEY
tukey.deaths = TukeyHSD(deaths.anova)
plot(tukey.deaths, las=1)

## Significant Differences:
deathdf = as.data.frame(tukey.deaths$NCHS_urbanization)


#################################################
## Linear Regressions for by state variables
############################################

## for cases
## standardize parameters
bystate.norm = scale(bystate.combined[,-1], center=T, scale=T)
bystate.norm = as.data.frame(bystate.norm)
## create model with all parameters
full.model <- lm(casesper100k ~., data = bystate.combined[,-1])
step.model <- stepAIC(full.model, direction = "forward", 
                      trace = FALSE)
summary(step.model)

scatterplot(x = bystate.combined$ReligiousProp2016, y = bystate.combined$casesper100k)
scatterplot(x = bystate.norm$ReligiousProp2016, y = bystate.norm$casesper100k)

plot(step.model, 1)

#####

## colnames
bscn = colnames(bystate.combined)
bscn = bscn[2:8]

bystate.combined[,bscn[2]]

## check model for each variable to cases
coefdf = data.frame(matrix(ncol = 5, nrow = 0))
for (i in 1:length(bscn))
{
  full.model <- lm(casesper100k ~., data = bystate.combined[,c(bscn[i],'casesper100k')])
  print('-----')
  print(summary(full.model))
  print('-----')
  tempdf = as.data.frame(summary(full.model)$coefficients)
  coefdf = rbind(coefdf, tempdf)
  plot(x = bystate.combined[,bscn[i]], y = bystate.combined$casesper100k)
}
coefdf = coefdf[c(2,4,6,8,10,12,14),]

## religiousness is most significant predictor out of selected parameters
ggplot(data = bystate.combined, aes(x = ReligiousProp2016, y = casesper100k)) +
  geom_point() +
  ggtitle('Proportion of Religious People vs Cases per 100,000 for each State') +
  labs(x = 'Proportion of Religious People', y = 'Cases per 100,000') +
  geom_smooth(method = 'lm', formula = y~x, color = 'red')

## edurank higher edu was next
ggplot(data = bystate.combined, aes(x = EduRankHE, y = casesper100k)) +
  geom_point() +
  ggtitle('Higher Education Rank vs Cases per 100,000 for each State') +
  labs(x = 'Higher Education Rank', y = 'Cases per 100,000') +
  geom_smooth(method = 'lm', formula = y~x, color = 'red')

## med income was final significant variable
ggplot(data = bystate.combined, aes(x = MedIncome2018, y = casesper100k)) +
  geom_point() +
  ggtitle('2018 Median Income vs Cases per 100,000 for each State') +
  labs(x = '2018 Median Income', y = 'Cases per 100,000') +
  geom_smooth(method = 'lm', formula = y~x, color = 'red')


allvars.lm = lm(casesper100k ~., data = bystate.combined[,c(bscn[2:6],'casesper100k')])
summary(allvars.lm)
vif(allvars.lm)
cor(bystate.combined[,bscn[1:6]])

plot_ly(data = bystate.combined, x = ~MedIncome2018, y = ~ReligiousProp2016, z = ~casesper100k)