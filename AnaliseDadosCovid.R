setwd("C:/Users/hiago/OneDrive/Projetos/Analise-Dados-Covid/")


# %% [code] {"_execution_state":"idle","execution":{"iopub.status.busy":"2021-07-20T13:24:04.809088Z","iopub.execute_input":"2021-07-20T13:24:04.811440Z","iopub.status.idle":"2021-07-20T13:24:06.480134Z"}}
# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages

# %% [markdown]
# # Data anlysis with COVID-19 data from Brazil until May/2021
# ## Data obtained from website Brasil.io - https://brasil.io/dataset/covid19/files/
# ## Hiago W. Petris - 22/05/2021

# %% [code] {"execution":{"iopub.status.busy":"2021-07-20T13:27:37.450019Z","iopub.execute_input":"2021-07-20T13:27:37.452162Z","iopub.status.idle":"2021-07-20T13:27:37.482836Z"}}
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

# Download at: https://www.kaggle.com/hiagow/dados-covid-brasil-abril2021/

arq = "C:/Users/hiago/Downloads/caso.csv"

# %% [code] {"execution":{"iopub.status.busy":"2021-07-20T13:24:06.743948Z","iopub.execute_input":"2021-07-20T13:24:06.745498Z","iopub.status.idle":"2021-07-20T13:24:15.190573Z"}}
df = read_csv(arq)
# View(df)
head(df)
str(df)

# %% [markdown]
# # First part: Only data from state of Paraná

# %% [code] {"execution":{"iopub.status.busy":"2021-07-20T13:24:15.193773Z","iopub.execute_input":"2021-07-20T13:24:15.195698Z","iopub.status.idle":"2021-07-20T13:24:16.411668Z"}}
dfPR = df %>% 
  arrange(date) %>%
  filter(state=='PR', place_type=='state')
head(dfPR)
nrow(dfPR)
#View(dfPR)

# %% [markdown]
# ## Cumulative cases at Paraná

# %% [code] {"execution":{"iopub.status.busy":"2021-07-20T13:25:33.642768Z","iopub.execute_input":"2021-07-20T13:25:33.644797Z","iopub.status.idle":"2021-07-20T13:25:34.068274Z"}}
figCumulativeCases <- plot_ly(dfPR,x=~date,y=~confirmed,type='scatter',name='Cumulative cases',mode='lines')
figCumulativeCases <- figCumulativeCases %>% layout(xaxis=list(title='Date'),yaxis=list(title='Cumulative cases'))
figCumulativeCases <- figCumulativeCases %>% config(locale="pt-br")
figCumulativeCases

# %% [markdown]
# ## Cumulative cases and deaths

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:47:00.744954Z","iopub.execute_input":"2021-06-03T13:47:00.746417Z","iopub.status.idle":"2021-06-03T13:47:01.793765Z"}}
figCumulativeCasesAndDeaths <- figCumulativeCases %>% add_trace(y=~deaths,name='Cumulative deaths',mode='lines')
figCumulativeCasesAndDeaths

# %% [markdown]
# ## Death rate

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:47:18.792355Z","iopub.execute_input":"2021-06-03T13:47:18.79375Z","iopub.status.idle":"2021-06-03T13:47:20.136201Z"}}
figDeathRate <- plot_ly(dfPR,x=~date,y=~death_rate,type='scatter',name='Death rate',mode='lines')
figDeathRate <- figDeathRate %>% layout(xaxis=list(title='Date'),yaxis=list(title='Death rate'))
figDeathRate

# %% [markdown]
# ## New cases per day

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:48:07.428897Z","iopub.execute_input":"2021-06-03T13:48:07.430343Z","iopub.status.idle":"2021-06-03T13:48:07.467036Z"}}
# ?dplyr::lag
dfPRNewCases = dfPR

dfPRNewCases = dfPRNewCases %>%
  mutate(new_cases = confirmed-lag(confirmed), .after=confirmed)

dfPRNewCases$new_cases[1] = dfPRNewCases$confirmed[1]

head(dfPRNewCases)
#View(dfPRNewCases)

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:48:15.471288Z","iopub.execute_input":"2021-06-03T13:48:15.47291Z","iopub.status.idle":"2021-06-03T13:48:16.81236Z"}}
# New cases
figNewCases <- plot_ly(dfPRNewCases,x=~date,y=~new_cases,type='scatter',name='New Cases',mode='lines')
figNewCases <- figNewCases %>% layout(xaxis=list(title='Date'),yaxis=list(title='New Cases'))
figNewCases <- figNewCases %>% config(locale="pt-br")
figNewCases

# %% [markdown]
# ## Analysis of the impact of a restriction measure, getting 14 days before and 14 days after the measure

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:48:45.815265Z","iopub.execute_input":"2021-06-03T13:48:45.81675Z","iopub.status.idle":"2021-06-03T13:48:45.846095Z"}}
dfPRRestrictionMeasure = dfPRNewCases %>%
  filter(date>=as.Date('2021-03-31')-14, date<=as.Date('2021-04-05')+14) %>%
  select(date, new_cases, death_rate)

head(dfPRRestrictionMeasure)
# View(dfPRRestrictionMeasure)

# %% [markdown]
# ## Restriction measure on 31/03
# ### http://www.aen.pr.gov.br/modules/noticias/article.php?storyid=111589#:~:text=10%20de%20mar%C3%A7o.-,O%20decreto%207.230%2F21%2C%20assinado%20pelo%20governador%20Carlos%20Massa%20Ratinho,atender%20aos%20s%C3%A1bados%20nas%20modalidades
# ## Relax of the restriction measure on 05/04
# ### https://www.fecomerciopr.com.br/sala-de-imprensa/noticia/estado-revoga-decreto-de-restricoes-mais-rigidas-na-regiao-metropolitana-de-curitiba/

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:49:17.91721Z","iopub.execute_input":"2021-06-03T13:49:17.918717Z","iopub.status.idle":"2021-06-03T13:49:19.249157Z"}}
figRestrictionMeasure <- plot_ly(dfPRRestrictionMeasure,x=~date,y=~new_cases,type='scatter',name='New Cases',mode='lines')
figRestrictionMeasure <- figRestrictionMeasure %>% layout(xaxis=list(title='Date'),yaxis=list(title='New Cases'))
figRestrictionMeasure <- figRestrictionMeasure %>% config(locale="pt-br")

figRestrictionMeasure <- figRestrictionMeasure %>% add_segments(
  name="Restriction Measure", line=list(color="orange"), 
  x=as.Date('2021-03-31'), xend=as.Date('2021-03-31'),
  y=min(dfPRRestrictionMeasure$new_cases), yend=max(dfPRRestrictionMeasure$new_cases))

figRestrictionMeasure <- figRestrictionMeasure %>% layout(
  shapes=list(
    type="rect",fillcolor="orange", line=list(color='orange'), opacity=0.3, 
    x0=as.Date('2021-03-31'), x1=as.Date('2021-04-05'),
    y0=min(dfPRRestrictionMeasure$new_cases),y1=max(dfPRRestrictionMeasure$new_cases)
  )
)

figRestrictionMeasure <- figRestrictionMeasure %>% add_segments(
  name="Relax of restriction measure", line=list(color="green"), 
  x=as.Date('2021-04-05'), xend=as.Date('2021-04-05'),
  y=min(dfPRRestrictionMeasure$new_cases),yend=max(dfPRRestrictionMeasure$new_cases))

figRestrictionMeasure

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:49:28.921697Z","iopub.execute_input":"2021-06-03T13:49:28.923172Z","iopub.status.idle":"2021-06-03T13:49:30.315777Z"}}
# Same analysis with Death reate
figRestrictionMeasure2 <- plot_ly(dfPRRestrictionMeasure,x=~date,y=~death_rate,type='scatter',name='Death rate',mode='lines')
figRestrictionMeasure2 <- figRestrictionMeasure2 %>% layout(xaxis=list(title='Date'),yaxis=list(title='Death rate'))
figRestrictionMeasure2 <- figRestrictionMeasure2 %>% config(locale="pt-br")

figRestrictionMeasure2 <- figRestrictionMeasure2 %>% add_segments(
  name="Medida de restrição", line=list(color="orange"), 
  x=as.Date('2021-03-31'), xend=as.Date('2021-03-31'), 
  y=min(dfPRRestrictionMeasure$death_rate),yend=max(dfPRRestrictionMeasure$death_rate))

figRestrictionMeasure2 <- figRestrictionMeasure2 %>% layout(
  shapes=list(
    type="rect",fillcolor="orange", line=list(color='orange'), opacity=0.3, 
    x0=as.Date('2021-03-31'), x1=as.Date('2021-04-05'), 
    y0=min(dfPRRestrictionMeasure$death_rate),y1=max(dfPRRestrictionMeasure$death_rate)
  )
)

figRestrictionMeasure2 <- figRestrictionMeasure2 %>% add_segments(name="Relax of restriction measure", line=list(color="green"), x=as.Date('2021-04-05'), xend=as.Date('2021-04-05'),y=min(dfPRRestrictionMeasure$death_rate),yend=max(dfPRRestrictionMeasure$death_rate))
figRestrictionMeasure2

# %% [markdown]
# ## Contamination rate

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:49:57.751303Z","iopub.execute_input":"2021-06-03T13:49:57.752703Z","iopub.status.idle":"2021-06-03T13:49:57.785459Z"}}
# Calculate contamination rate  = new cases / new cases from a day before
dfPRContaminationRate = dfPRNewCases %>%
  mutate(contamination_rate = round(confirmed/lag(confirmed),2), .after=new_cases) %>%
  select(date,confirmed,contamination_rate)

dfPRContaminationRate$contamination_rate[1] = 0

head(dfPRContaminationRate)
# View(dfPRContaminationRate)

# %% [markdown]
# ## Death rate

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:50:15.750106Z","iopub.execute_input":"2021-06-03T13:50:15.751624Z","iopub.status.idle":"2021-06-03T13:50:16.988398Z"}}
# Change the scale of Y axos or apply some normalization could help in visualization, beacause it starts on 0
# and than has a peak at 2, but then it keeps approximately at 1
figDeathRate <- plot_ly(dfPRContaminationRate,x=~date,y=~contamination_rate,type='scatter',name='Contamination Rate',mode='lines')
figDeathRate <- figDeathRate %>% layout(xaxis=list(title='Date'),yaxis=list(title='Contamination Rate'))
figDeathRate <- figDeathRate %>% config(locale="pt-br")
figDeathRate

# %% [markdown]
# ## Deaths / New Cases

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:50:51.471334Z","iopub.execute_input":"2021-06-03T13:50:51.472892Z","iopub.status.idle":"2021-06-03T13:50:51.504502Z"}}
# Filter because it has and outlier
dfPRDeathsPerNewcases = dfPRNewCases %>%
  mutate(deaths_per_new_cases = ifelse(new_cases==0, deaths, round(deaths/new_cases,2)), .after=new_cases) %>%
  select(date,deaths,new_cases,deaths_per_new_cases) %>%
  filter(deaths_per_new_cases < 1000)

head(dfPRDeathsPerNewcases)

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:51:02.844588Z","iopub.execute_input":"2021-06-03T13:51:02.846002Z","iopub.status.idle":"2021-06-03T13:51:03.757349Z"}}
figDeathsNewCases <- plot_ly(dfPRDeathsPerNewcases,x=~date,y=~deaths_per_new_cases,type='scatter',name='Deaths/New Cases',mode='lines')
figDeathsNewCases <- figDeathsNewCases %>% layout(xaxis=list(title='Date'),yaxis=list(title='Deaths/New Cases'))
figDeathsNewCases <- figDeathsNewCases %>% config(locale="pt-br")
figDeathsNewCases

# %% [code] {"execution":{"iopub.status.busy":"2021-05-29T18:46:38.829551Z","iopub.execute_input":"2021-05-29T18:46:38.831026Z","iopub.status.idle":"2021-05-29T18:46:40.34225Z"}}
# TODO: LM new cases per death
#ggplot(dfPRNewCases,aes(x=estimated_population,y=contamination_rate)) +
# geom_point() +
#stat_smooth(method="lm", col="red")

# %% [markdown]
# # Data from Brasil Cities

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:54:43.783919Z","iopub.execute_input":"2021-06-03T13:54:43.785379Z","iopub.status.idle":"2021-06-03T13:54:52.606605Z"}}
# Creates dataframe with contamination rate and estimated population, only for cities
dfContaminationRatePopulation = df %>% 
  filter(place_type=='city') %>%
  select(date,city,state,estimated_population,confirmed) %>%
  arrange(city,state,date) %>%
  mutate(contamination_rate = confirmed/lag(confirmed), .before=confirmed)

# Removes column confirmed.
# dfContaminationRatePopulation$confirmed = NULL

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T13:55:19.879073Z","iopub.execute_input":"2021-06-03T13:55:19.880474Z","iopub.status.idle":"2021-06-03T13:55:20.101639Z"}}
# Removes NA values
dfContaminationRatePopulation = dfContaminationRatePopulation %>%
  filter(!is.na(contamination_rate) & !is.na(estimated_population))

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T14:08:23.351106Z","iopub.execute_input":"2021-06-03T14:08:23.352564Z","iopub.status.idle":"2021-06-03T14:08:23.608455Z"}}
# Calculates the mean of contamination rate for every city and the max value of estimated population
dfContaminationRatePopulation2 = dfContaminationRatePopulation %>%
  group_by(city,state) %>%
  summarise(meanContaminationRate = mean(contamination_rate), estimated_population = max(estimated_population)) %>%
  filter(!is.infinite(meanContaminationRate))

head(dfContaminationRatePopulation2)

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T14:08:26.634648Z","iopub.execute_input":"2021-06-03T14:08:26.636198Z","iopub.status.idle":"2021-06-03T14:08:27.093503Z"}}
ggplot(dfContaminationRatePopulation2, aes(x=meanContaminationRate,y=estimated_population)) + 
  geom_point()

# There are cities with low population and high contamination rate
# But no city with high population has contamination rate >  1.05

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T14:08:31.069066Z","iopub.execute_input":"2021-06-03T14:08:31.070481Z","iopub.status.idle":"2021-06-03T14:08:31.109377Z"}}
# Check for Cities with higher population (Sao Paulo and Rio) or higher contamination rate
dfContaminationRatePopulation2 %>% 
  filter(estimated_population > 4.0e+06 | meanContaminationRate > 1.5)

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T14:08:34.667192Z","iopub.execute_input":"2021-06-03T14:08:34.668629Z","iopub.status.idle":"2021-06-03T14:08:34.728402Z"}}
# Filter for "Santa Margarida do Sul" in the original dataframe
View(df %>% filter(city=='Santa Margarida do Sul') %>% arrange(date) %>% filter(confirmed>23000))

# Apparently it has an error on day 2021-04-04, with 23326 confirmed cases

# %% [markdown]
# ## Analyse distribution of the contamination rate means

# %% [code] {"execution":{"iopub.status.busy":"2021-06-03T14:08:37.961263Z","iopub.execute_input":"2021-06-03T14:08:37.962886Z","iopub.status.idle":"2021-06-03T14:08:38.292145Z"}}
# Filter by meanContaminationRete < 1.05 to facilitate visualization
dfContaminationRatePopulation3 = dfContaminationRatePopulation2 %>% 
  filter(meanContaminationRate < 1.05) %>% 
  arrange(meanContaminationRate)

mean(dfContaminationRatePopulation3$meanContaminationRate)
median(dfContaminationRatePopulation3$meanContaminationRate)
quantile(dfContaminationRatePopulation3$meanContaminationRate)

ggplot(dfContaminationRatePopulation3, aes(x=meanContaminationRate)) + geom_density()


# %% [code]
