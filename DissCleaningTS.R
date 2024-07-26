#Elias Chavarria-Mora
#Dissertation
#Empirical chapter 3
#This is code for cleaning up the dat and creating the time series for the VAR models


library (tidyverse) #includes dplyr, tidyr, ggplot2, stringr
library(rio) #for import
#library (tseries) #for time series analysis, including augmented dickey-fuller text
#library(vars) #for VAR
library(xts) #to transform into ts
library (ragg) #need for anti-aliasing: keeps graphics from looking pixelated. 
#library(stargazer) #fast table output for regressions, includes ascii, latex and html
options(max.print=1000000) 

setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data")
df <-read.csv('df_FullDiss_V6.csv', fileEncoding="UTF-8")

df<-df%>%
  dplyr::select(CountryElection, type_of_account, user_name, Analytic, Authenticity, created_at_seconds)

#Tambien,aca mas adelente, saque las cuentas que tiene que eliminar pq twittearon poco, como echandi, el
#codigo esta abajo

#all the positx thing is for later
#I want to prepare the dataset to use the TS data and create TS graphics
#class(df$created_at_seconds) #shows up as character
#head(df$created_at_seconds) #you can see the seconds data
#turn it into time data
#df$created_at_asTime<-as.POSIXct(df$created_at_seconds, format="%Y-%m-%d %H:%M:%OS")#format has to be identical to how the strings appear
#head(df$created_at_asTime) #It works fine
#tail(df$created_at_asTime)
df$created_at_seconds2<-as.Date(df$created_at_seconds)#I also need a version that is class Date
df$user_name<-as.factor(df$user_name) #make sure the user_name is a factor

#Separar en cada eleccion y por tipo de cuenta, con purrr
# Define a list of unique values for CountryElection and type_of_account
country_elections_vector <- unique(df$CountryElection) #son 12
account_types_vector <- unique(df$type_of_account) #son 2, so far so good, [1] es candidato
#print(country_elections_vector)
#ojo, los indices en country_elections_vector son
# [1] "Costa Rica 2010"  "Costa Rica 2014"  "Costa Rica 2018"  "Costa Rica 2022"  "El Salvador 2014"
#[6] "El Salvador 2019" "Guatemala 2011"   "Guatemala 2015"   "Guatemala 2019"   "Honduras 2013"   
#[11] "Honduras 2017"    "Honduras 2021" 
# Create a list of filtered dataframes for each combination of values
filtered_dfs <- map(country_elections_vector, function(countryelection) {
  map(account_types_vector, function(accounttype) {
    df %>%
      filter(CountryElection == countryelection) %>%
      filter(type_of_account == accounttype)
  })
})
#save each one individually
df_CR10_c <- filtered_dfs[[1]][[1]]
df_CR10_p <- filtered_dfs[[1]][[2]]
df_CR14_c <- filtered_dfs[[2]][[1]]
df_CR14_p <- filtered_dfs[[2]][[2]]
df_CR18_c <- filtered_dfs[[3]][[1]]
df_CR18_p <- filtered_dfs[[3]][[2]]
df_CR22_c <- filtered_dfs[[4]][[1]]
df_CR22_p <- filtered_dfs[[4]][[2]]
df_ES14_c <- filtered_dfs[[5]][[1]]
df_ES14_p <- filtered_dfs[[5]][[2]]
df_ES19_c <- filtered_dfs[[6]][[1]]
df_ES19_p <- filtered_dfs[[6]][[2]]
df_GU11_c <- filtered_dfs[[7]][[1]]
df_GU11_p <- filtered_dfs[[7]][[2]]
df_GU15_c <- filtered_dfs[[8]][[1]]
df_GU15_p <- filtered_dfs[[8]][[2]]
df_GU19_c <- filtered_dfs[[9]][[1]]
df_GU19_p <- filtered_dfs[[9]][[2]]
df_HO13_c <- filtered_dfs[[10]][[1]]
df_HO13_p <- filtered_dfs[[10]][[2]]
df_HO17_c <- filtered_dfs[[11]][[1]]
df_HO17_p <- filtered_dfs[[11]][[2]]
df_HO21_c <- filtered_dfs[[12]][[1]]
df_HO21_p <- filtered_dfs[[12]][[2]]

#Ojo, necesito saber la cantidad de parametros necesaria para estimar:
#list_dfs<-list(df_CR10_c, df_CR10_p, df_CR14_c, df_CR14_p, df_CR18_c, df_CR18_p, df_CR22_c, df_CR22_p, df_ES14_c, df_ES14_p, 
#               df_ES19_c, df_ES19_p, df_GU11_c, df_GU11_p, df_GU15_c, df_GU15_p, df_GU19_c, df_GU19_p, df_HO13_c, df_HO13_p,
#               df_HO17_c, df_HO17_p, df_HO21_c, df_HO21_p) 

# Apply the crosstab function to each dataframe in the list
#list_of_crosstabs <- map(list_dfs, ~ table(.$user_username))
# Print or manipulate the list of crosstabs as needed
#map(list_of_crosstabs, print)






#VAR
#following https://www.econometrics-with-r.org/16-1-vector-autoregressions.html
#Also, even more so, here https://towardsdatascience.com/a-deep-dive-on-vector-autoregression-in-r-58767ebb3f06
#best is https://www.idrisstsafack.com/post/vector-autoregressive-var-models-with-r-relationship-between-the-s-p500-bitcoin-chf-jpy

#Ok, first and foremost, I need to group the varible in time bins, ie, by minutes, hours or so
#try first with CR10_c, I should do it for each one, I suggest the following rules
#bins of 1 day, see which is the first hour that has all the relevant accounts, and which is the last

#CR10c
#primero, son 4, pero fishman sale dos veces, hay que eliminar a "Luis Fishman, dejar solo a "Luis Fishman Z"
#segund, periodos de inicon y funal de c/u
df_CR10_c<-df_CR10_c%>%
  filter(user_name!="Luis Fishman") #ok, esto hay que eliminarlo mas arriba, al puro incio con el df completo, con el CountryElectionUsername
#El tweet mas antiguo de Laura Chinchilla es 2009-10-04 3h (con ctreated_at_seconds)
#Trejos es 2009-10-19 21h
#fishman Z es 2009-10-25 3 h . Basado en esto, necesito todas las fechas deberian de ser mayores a 2009-10-25 3 h

#El tweet mas nuevo para lauchi es 2010-2-7 14h
#Fishman 2010-2-7 13h 
#trejos 2010-2-2 00h
class(df$created_at_seconds) #character
class(df$created_at_seconds2) #date 

df_CR10_c<-df_CR10_c%>%
  filter(created_at_seconds2>=as.Date("2009-10-25")&created_at_seconds2<=as.Date("2010-02-02")) 
#OK, ahora si tengo datos para todo el length. necesito sacar promedio de Spon y An por dia
df_CR10_c<-df_CR10_c%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

#Ok, I need to drop repetitions...
df_CR10_c2<-df_CR10_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

#Daily brackets can be a problem maybeee, En esta Eugenia trejos queda totalmente eliminado, solo quedan lauchi y fishman
#so, drop trejos...
df_CR10_c2<- df_CR10_c2%>%
  filter(user_name!="Eugenio Trejos")


#CR10p
#necesito una forma rapida de obtener max y min para crated at second2, pero por cada tipo de cuenta
min_vals_group<-df_CR10_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins, in this case, 2009-10-03

max_vals_group<-df_CR10_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2010-02-04

df_CR10_p<-df_CR10_p%>%
  filter(created_at_seconds2>=as.Date("2009-10-03")&created_at_seconds2<=as.Date("2010-02-04"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR10_p2<-df_CR10_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

#Just to see how many observations per account
df_CR10_p2_filtered <- df_CR10_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR10_p2_filtered) #three accounts (FA, PAC, PASE), all at least 67 obvs


#CR14c
min_vals_group<-df_CR14_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-11-06, excluyo a Otto Guevara pq no tuiteo hasta 2014

max_vals_group<-df_CR14_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2014-02-03

df_CR14_c<-df_CR14_c%>%
  filter(created_at_seconds2>=as.Date("2013-11-06")&created_at_seconds2<=as.Date("2014-02-03"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR14_c2<-df_CR14_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR14_c2_filtered <- df_CR14_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR14_c2_filtered)

#CR14p
min_vals_group<-df_CR14_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-10-18

max_vals_group<-df_CR14_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2014-03-26

df_CR14_p<-df_CR14_p%>%
  filter(created_at_seconds2>=as.Date("2013-10-18")&created_at_seconds2<=as.Date("2014-03-26"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR14_p2<-df_CR14_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR14_p2_filtered <- df_CR14_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR14_p2_filtered)
#PRC solo 16, get rid of it
df_CR14_p2<- df_CR14_p2%>%
  filter(user_name!="Renovación PRC")

#CR18c
min_vals_group<-df_CR18_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2017-11-10

max_vals_group<-df_CR18_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2018-02-02

df_CR18_c<-df_CR18_c%>%
  filter(created_at_seconds2>=as.Date("2017-11-10")&created_at_seconds2<=as.Date("2018-02-02"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR18_c2<-df_CR18_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR18_c2_filtered <- df_CR18_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR18_c2_filtered)
#Fabricio y sergio mena solo twittearon como 20 dias c/u pero deje asi ahora

#CR18p
min_vals_group<-df_CR18_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2017-10-11, quita a PIN que empezo super tarde

max_vals_group<-df_CR18_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2018-02-13

df_CR18_p<-df_CR18_p%>%
  filter(created_at_seconds2>=as.Date("2017-10-11")&created_at_seconds2<=as.Date("2018-02-13"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR18_p2<-df_CR18_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR18_p2_filtered <- df_CR18_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR18_p2_filtered)
#PRC solo 6, get rid of it
df_CR18_p2<- df_CR18_p2%>%
  filter(user_name!="Renovación PRC")

#CR22c
min_vals_group<-df_CR22_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2021-11-11

max_vals_group<-df_CR22_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2022-01-10

df_CR22_c<-df_CR22_c%>%
  filter(created_at_seconds2>=as.Date("2021-11-11")&created_at_seconds2<=as.Date("2022-01-10"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR22_c2<-df_CR22_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR22_c2_filtered <- df_CR22_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR22_c2_filtered)
#ugh a ton have less than 10, walter munoz, christian rivera, chavez, sergio mena
df_CR22_c2<- df_CR22_c2%>%
  filter(user_name!="Dr Walter Muñoz."|user_name!="Dr. Christian Rivera"
         |user_name!="Rodrigo Chaves"|user_name!="Sergio Mena Díaz")

#CR22p
min_vals_group<-df_CR22_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2021-10-11

max_vals_group<-df_CR22_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2022-02-01

df_CR22_p<-df_CR22_p%>%
  filter(created_at_seconds2>=as.Date("2021-10-11")&created_at_seconds2<=as.Date("2022-02-01"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_CR22_p2<-df_CR22_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_CR22_p2_filtered <- df_CR22_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_CR22_p2_filtered)

#ugh a ton have less than 10, walter munoz, christian rivera, chavez, sergio mena
df_CR22_p2<- df_CR22_p2%>%
  filter(user_name!="Partido Fuerza Nacional"|user_name!="Progreso Social Democrático"
         |user_name!="Pueblo Unido CR"|user_name!="Unidos Podemos Costa Rica")

#ES14c
min_vals_group<-df_ES14_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-09-23

max_vals_group<-df_ES14_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2014-03-09

df_ES14_c<-df_ES14_c%>%
  filter(created_at_seconds2>=as.Date("2013-09-23")&created_at_seconds2<=as.Date("2014-03-09"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_ES14_c2<-df_ES14_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_ES14_c2_filtered <- df_ES14_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_ES14_c2_filtered)



#ES14p
min_vals_group<-df_ES14_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-09-23

max_vals_group<-df_ES14_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2014-03-09

df_ES14_p<-df_ES14_p%>%
  filter(created_at_seconds2>=as.Date("2013-09-23")&created_at_seconds2<=as.Date("2014-03-09"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_ES14_p2<-df_ES14_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_ES14_p2_filtered <- df_ES14_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_ES14_p2_filtered)

df_ES14_p2<- df_ES14_p2%>%
  filter(user_name!="PSP")

#ES19c
min_vals_group<-df_ES19_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2018-10-03

max_vals_group<-df_ES19_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2019-02-03

df_ES19_c<-df_ES19_c%>%
  filter(created_at_seconds2>=as.Date("2018-10-03")&created_at_seconds2<=as.Date("2019-02-03"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_ES19_c2<-df_ES19_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_ES19_c2_filtered <- df_ES19_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_ES19_c2_filtered)

#ES19p
min_vals_group<-df_ES19_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2018-10-02

max_vals_group<-df_ES19_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2019-02-03

df_ES19_p<-df_ES19_p%>%
  filter(created_at_seconds2>=as.Date("2018-10-02")&created_at_seconds2<=as.Date("2019-02-03"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_ES19_p2<-df_ES19_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_ES19_p2_filtered <- df_ES19_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_ES19_p2_filtered)

#GU11c
min_vals_group<-df_GU11_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2011-05-08

max_vals_group<-df_GU11_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2011-09-03

df_GU11_c<-df_GU11_c%>%
  filter(created_at_seconds2>=as.Date("2011-05-08")&created_at_seconds2<=as.Date("2011-09-03"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU11_c2<-df_GU11_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU11_c2_filtered <- df_GU11_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU11_c2_filtered)

df_GU11_c2<- df_GU11_c2%>%
  filter(user_name!="Manuel Antonio Baldizón MAN")

#GU11p
min_vals_group<-df_GU11_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2011-05-03

max_vals_group<-df_GU11_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2011-09-26

df_GU11_p<-df_GU11_p%>%
  filter(created_at_seconds2>=as.Date("2011-05-03")&created_at_seconds2<=as.Date("2011-09-26"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU11_p2<-df_GU11_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU11_p2_filtered <- df_GU11_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU11_p2_filtered)

df_GU11_p2<- df_GU11_p2%>%
  filter(user_name!="CREO GUATEMALA"|user_name!="PANResponde")

#GU15c
min_vals_group<-df_GU15_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2015-05-22

max_vals_group<-df_GU15_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2015-09-02

df_GU15_c<-df_GU15_c%>%
  filter(created_at_seconds2>=as.Date("2015-05-22")&created_at_seconds2<=as.Date("2015-09-02"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU15_c2<-df_GU15_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU15_c2_filtered <- df_GU15_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU15_c2_filtered)

#GU15p

min_vals_group<-df_GU15_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2015-05-05

max_vals_group<-df_GU15_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2015-09-04

df_GU15_p<-df_GU15_p%>%
  filter(created_at_seconds2>=as.Date("2015-05-05")&created_at_seconds2<=as.Date("2015-09-04"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU15_p2<-df_GU15_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU15_p2_filtered <- df_GU15_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU15_p2_filtered)

#GU19c
min_vals_group<-df_GU19_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2019-01-18

max_vals_group<-df_GU19_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2019-08-07

df_GU19_c<-df_GU19_c%>%
  filter(created_at_seconds2>=as.Date("2019-01-18")&created_at_seconds2<=as.Date("2019-08-07"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU19_c2<-df_GU19_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU19_c2_filtered <- df_GU19_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU19_c2_filtered)

#GU19p
min_vals_group<-df_GU19_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2019-02-19

max_vals_group<-df_GU19_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2019-06-15

df_GU19_p<-df_GU19_p%>%
  filter(created_at_seconds2>=as.Date("2019-02-19")&created_at_seconds2<=as.Date("2019-06-15"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_GU19_p2<-df_GU19_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_GU19_p2_filtered <- df_GU19_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_GU19_p2_filtered)

df_GU19_p2<- df_GU19_p2%>%
  filter(user_name!="GUATEMALA PODEMOS")

#HO13c
min_vals_group<-df_HO13_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-05-23

max_vals_group<-df_HO13_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2013-11-24

df_HO13_c<-df_HO13_c%>%
  filter(created_at_seconds2>=as.Date("2013-05-23")&created_at_seconds2<=as.Date("2013-11-24"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO13_c2<-df_HO13_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO13_c2_filtered <- df_HO13_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO13_c2_filtered)

df_HO13_c2<- df_HO13_c2%>%
  filter(user_name!="Orle Solís")

#HO13p
min_vals_group<-df_HO13_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2013-05-24

max_vals_group<-df_HO13_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2013-11-24

df_HO13_p<-df_HO13_p%>%
  filter(created_at_seconds2>=as.Date("2013-05-24")&created_at_seconds2<=as.Date("2013-11-24"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO13_p2<-df_HO13_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO13_p2_filtered <- df_HO13_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO13_p2_filtered)

df_HO13_p2<- df_HO13_p2%>%
  filter(user_name!="PINU-SD HONDURAS")

#HO17c
min_vals_group<-df_HO17_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2017-05-25

max_vals_group<-df_HO17_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2017-11-25

df_HO17_c<-df_HO17_c%>%
  filter(created_at_seconds2>=as.Date("2017-05-25")&created_at_seconds2<=as.Date("2017-11-25"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO17_c2<-df_HO17_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO17_c2_filtered <- df_HO17_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO17_c2_filtered)

df_HO17_c2<- df_HO17_c2%>%
  filter(user_name!="Marlene Alvarenga")

#HO17p
min_vals_group<-df_HO17_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2017-05-25

max_vals_group<-df_HO17_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2017-11-14

df_HO17_p<-df_HO17_p%>%
  filter(created_at_seconds2>=as.Date("2017-05-25")&created_at_seconds2<=as.Date("2017-11-14"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO17_p2<-df_HO17_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO17_p2_filtered <- df_HO17_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO17_p2_filtered)

df_HO17_p2<- df_HO17_p2%>%
  filter(user_name!="Alianza Patriótica Hondureña"|user_name!="Diputados PAC")

#HO21c
min_vals_group<-df_HO21_c%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2021-05-27

max_vals_group<-df_HO21_c%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2021-11-21

df_HO21_c<-df_HO21_c%>%
  filter(created_at_seconds2>=as.Date("2021-05-27")&created_at_seconds2<=as.Date("2021-11-21"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO21_c2<-df_HO21_c%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO21_c2_filtered <- df_HO21_c2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO21_c2_filtered)


#HO21p
min_vals_group<-df_HO21_p%>%
  group_by(user_name)%>%
  summarize(min_val=min(created_at_seconds2))
min_vals_group #select the maximum of all the mins: 2021-06-22

max_vals_group<-df_HO21_p%>%
  group_by(user_name)%>%
  summarize(max_val=max(created_at_seconds2))
max_vals_group #select the minimum of all the maxs: 2021-09-10

df_HO21_p<-df_HO21_p%>%
  filter(created_at_seconds2>=as.Date("2021-06-22")&created_at_seconds2<=as.Date("2021-09-10"))%>%
  group_by(user_name,created_at_seconds2)%>%
  summarize(mean_Auth=mean(Authenticity), mean_An=mean(Analytic), across())

df_HO21_p2<-df_HO21_p%>%
  dplyr::select(user_name, created_at_seconds2, mean_Auth, mean_An, CountryElection)%>%
  distinct()

df_HO21_p2_filtered <- df_HO21_p2 %>%
  group_by(user_name) %>%  
  count(user_name)
print(df_HO21_p2_filtered)

df_HO21_p2<- df_HO21_p2%>%
  filter(user_name!="Alianza Patriótica Hondureña"|user_name!="LIDEHR"|user_name!="Partido Nueva Ruta de Honduras (PNRH)")


#Ok, siguiente es ver estacionariedad, ES y HO son mucho mejores que CR y GU, empezar con esos dos.

#no es solo esto, hay que tomar el df y transformarlo en ts; then check acf and pacf to see if they ts is stationary
#THIS SHOULD BE DONE FOR EACH INDIVIDUAL VARIABLE, SO EACH VARIABLE IS TREATED AS A TS FIRST, WITH ACF AND PACF

#Then, once you put togethern ina signle model and run the var, what you should report is:
#granger causality test (tells if a var granger causes the others)
#impulse repsonse fucntions.

#example of how to declare as ts
#lnIP <- ts(mp$lnIP, start = c(2003,1,1), frequency = 12) (but what aboit missings??)

#OK, first, ES
#ES14_c
print(df_ES14_c2_filtered)#Esto para sacar el total de cuentas, son 3, ahora, obtener un vector de c/u

# Create a complete sequence of dates from the minimum to the maximum date in the dataframe
#This is necessary to be able to identify and fill any missing value
ES14_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_ES14_c2$created_at_seconds2), max(df_ES14_c2$created_at_seconds2), 
                                                            by = "day")) #168 days

df_ES14_NormanQuijano<- df_ES14_c2%>% #No missing days
  filter(user_name=="Norman Quijano")
class(df_ES14_NormanQuijano$created_at_seconds2) #Class: Date
df_ES14_SalvadorSanchez<- df_ES14_c2%>%
  filter(user_name=="Salvador Sánchez Cerén")
df_ES14_TonySaca<- df_ES14_c2%>%
  filter(user_name=="Tony Saca")

#I need to make it so the accounts that have some missing days get the missing days added
df_ES14_SalvadorSanchez <- ES14_complete_dates %>%left_join(df_ES14_SalvadorSanchez, by = "created_at_seconds2")
df_ES14_TonySaca <- ES14_complete_dates %>%left_join(df_ES14_TonySaca, by = "created_at_seconds2")

#And then, fill in missing values with 0
df_ES14_SalvadorSanchez$mean_Auth[is.na(df_ES14_SalvadorSanchez$mean_Auth)]<-0
df_ES14_SalvadorSanchez$mean_An[is.na(df_ES14_SalvadorSanchez$mean_An)]<-0
df_ES14_TonySaca$mean_Auth[is.na(df_ES14_TonySaca$mean_Auth)]<-0
df_ES14_TonySaca$mean_An[is.na(df_ES14_TonySaca$mean_An)]<-0


#turn to ts
ES14_NormanQuijano_Spon<-ts(df_ES14_NormanQuijano$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_NormanQuijano_An<-ts(df_ES14_NormanQuijano$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_SalvadorSanchez_Spon<-ts(df_ES14_SalvadorSanchez$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_SalvadorSanchez_An<-ts(df_ES14_SalvadorSanchez$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_TonySaca_Spon<-ts(df_ES14_TonySaca$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_TonySaca_An<-ts(df_ES14_TonySaca$mean_An, start = c(2013, 9, 23), frequency=1) 

#merge again into a single df to be able to run the vAR
VARdf_ES14_c_Spon<-cbind(ES14_NormanQuijano_Spon, ES14_SalvadorSanchez_Spon, ES14_TonySaca_Spon)
VARdf_ES14_c_An<-cbind(ES14_NormanQuijano_An, ES14_SalvadorSanchez_An, ES14_TonySaca_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_ES14_c_Spon, 'VARdf_ES14_c_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_ES14_c_An, 'VARdf_ES14_c_An.csv', fileEncoding="UTF-8")

#ES14_p
print(df_ES14_p2_filtered)#5 accounts, all have missing days

# Create a complete sequence of dates
ES14p_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_ES14_p2$created_at_seconds2), max(df_ES14_p2$created_at_seconds2), 
                                                             by = "day")) #168 days

df_ES14_ARENA<- df_ES14_p2%>% 
  filter(user_name=="ARENA")
df_ES14_FMLN<- df_ES14_p2%>%
  filter(user_name=="FMLN")
df_ES14_FMLNOficial<- df_ES14_p2%>%
  filter(user_name=="FMLN Oficial")
df_ES14_PARTIDOGANA<- df_ES14_p2%>%
  filter(user_name=="PARTIDO GANA")
df_ES14_PCN<- df_ES14_p2%>%
  filter(user_name=="PCN")

#I need to make it so the accounts that have some missing days get the missing days added
df_ES14_ARENA <- ES14p_complete_dates %>%left_join(df_ES14_ARENA, by = "created_at_seconds2")
df_ES14_FMLN <- ES14p_complete_dates %>%left_join(df_ES14_FMLN, by = "created_at_seconds2")
df_ES14_FMLNOficial <- ES14p_complete_dates %>%left_join(df_ES14_FMLNOficial, by = "created_at_seconds2")
df_ES14_PARTIDOGANA <- ES14p_complete_dates %>%left_join(df_ES14_PARTIDOGANA, by = "created_at_seconds2")
df_ES14_PCN <- ES14p_complete_dates %>%left_join(df_ES14_PCN, by = "created_at_seconds2")


#And then, fill in missing values with 0
df_ES14_ARENA$mean_Auth[is.na(df_ES14_ARENA$mean_Auth)]<-0
df_ES14_ARENA$mean_An[is.na(df_ES14_ARENA$mean_An)]<-0
df_ES14_FMLN$mean_Auth[is.na(df_ES14_FMLN$mean_Auth)]<-0
df_ES14_FMLN$mean_An[is.na(df_ES14_FMLN$mean_An)]<-0
df_ES14_FMLNOficial$mean_Auth[is.na(df_ES14_FMLNOficial$mean_Auth)]<-0
df_ES14_FMLNOficial$mean_An[is.na(df_ES14_FMLNOficial$mean_An)]<-0
df_ES14_PARTIDOGANA$mean_Auth[is.na(df_ES14_PARTIDOGANA$mean_Auth)]<-0
df_ES14_PARTIDOGANA$mean_An[is.na(df_ES14_PARTIDOGANA$mean_An)]<-0
df_ES14_PCN$mean_Auth[is.na(df_ES14_PCN$mean_Auth)]<-0
df_ES14_PCN$mean_An[is.na(df_ES14_PCN$mean_An)]<-0

#turn to ts
ES14_ARENA_Spon<-ts(df_ES14_ARENA$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_ARENA_An<-ts(df_ES14_ARENA$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_FMLN_Spon<-ts(df_ES14_FMLN$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_FMLN_An<-ts(df_ES14_FMLN$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_FMLNOficial_Spon<-ts(df_ES14_FMLNOficial$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_FMLNOficial_An<-ts(df_ES14_FMLNOficial$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_PARTIDOGANA_Spon<-ts(df_ES14_PARTIDOGANA$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_PARTIDOGANA_An<-ts(df_ES14_PARTIDOGANA$mean_An, start = c(2013, 9, 23), frequency=1) 
ES14_PCN_Spon<-ts(df_ES14_PCN$mean_Auth, start = c(2013, 9, 23), frequency=1)
ES14_PCN_An<-ts(df_ES14_PCN$mean_An, start = c(2013, 9, 23), frequency=1) 

#merge again into a single df to be able to run the vAR
VARdf_ES14_p_Spon<-cbind(ES14_ARENA_Spon, ES14_FMLN_Spon, ES14_FMLNOficial_Spon, ES14_PARTIDOGANA_Spon, 
                         ES14_PCN_Spon)
VARdf_ES14_p_An<-cbind(ES14_ARENA_An, ES14_FMLN_An, ES14_FMLNOficial_An, ES14_PARTIDOGANA_An, 
                       ES14_PCN_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_ES14_p_Spon, 'VARdf_ES14_p_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_ES14_p_An, 'VARdf_ES14_p_An.csv', fileEncoding="UTF-8")


#ES19_c
print(df_ES19_c2_filtered)#4 accounts, Bukele only one with no missing dates

# Create a complete sequence of dates
ES19c_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_ES19_c2$created_at_seconds2), 
                                                             max(df_ES19_c2$created_at_seconds2), 
                                                             by = "day")) #124 days

df_ES19_CarlosCalleja<- df_ES19_c2%>% 
  filter(user_name=="Carlos Calleja")
df_ES19_HugoMartinez<- df_ES19_c2%>%
  filter(user_name=="Hugo Martínez")
df_ES19_JosueAlvarado<- df_ES19_c2%>%
  filter(user_name=="Josué Alvarado")
df_ES19_NayibBukele<- df_ES19_c2%>%
  filter(user_name=="Nayib Bukele")

#I need to make it so the accounts that have some missing days get the missing days added
df_ES19_CarlosCalleja <- ES19c_complete_dates %>%left_join(df_ES19_CarlosCalleja, by = "created_at_seconds2")
df_ES19_HugoMartinez <- ES19c_complete_dates %>%left_join(df_ES19_HugoMartinez, by = "created_at_seconds2")
df_ES19_JosueAlvarado <- ES19c_complete_dates %>%left_join(df_ES19_JosueAlvarado, by = "created_at_seconds2")



#And then, fill in missing values with 0
df_ES19_CarlosCalleja$mean_Auth[is.na(df_ES19_CarlosCalleja$mean_Auth)]<-0
df_ES19_CarlosCalleja$mean_An[is.na(df_ES19_CarlosCalleja$mean_An)]<-0
df_ES19_HugoMartinez$mean_Auth[is.na(df_ES19_HugoMartinez$mean_Auth)]<-0
df_ES19_HugoMartinez$mean_An[is.na(df_ES19_HugoMartinez$mean_An)]<-0
df_ES19_JosueAlvarado$mean_Auth[is.na(df_ES19_JosueAlvarado$mean_Auth)]<-0
df_ES19_JosueAlvarado$mean_An[is.na(df_ES19_JosueAlvarado$mean_An)]<-0

#turn to ts
ES19_CarlosCalleja_Spon<-ts(df_ES19_CarlosCalleja$mean_Auth, start = c(2018, 10, 03), frequency=1)
ES19_CarlosCalleja_An<-ts(df_ES19_CarlosCalleja$mean_An, start = c(2018, 10, 03), frequency=1) 
ES19_HugoMartinez_Spon<-ts(df_ES19_HugoMartinez$mean_Auth, start = c(2018, 10, 03), frequency=1)
ES19_HugoMartinez_An<-ts(df_ES19_HugoMartinez$mean_An, start = c(2018, 10, 03), frequency=1) 
ES19_JosueAlvarado_Spon<-ts(df_ES19_JosueAlvarado$mean_Auth, start = c(2018, 10, 03), frequency=1)
ES19_JosueAlvarado_An<-ts(df_ES19_JosueAlvarado$mean_An, start = c(2018, 10, 03), frequency=1) 
ES19_NayibBukele_Spon<-ts(df_ES19_NayibBukele$mean_Auth, start = c(2018, 10, 03), frequency=1)
ES19_NayibBukele_An<-ts(df_ES19_NayibBukele$mean_An, start = c(2018, 10, 03), frequency=1) 


#merge again into a single df to be able to run the vAR
VARdf_ES19_c_Spon<-cbind(ES19_CarlosCalleja_Spon, ES19_HugoMartinez_Spon, ES19_JosueAlvarado_Spon, ES19_NayibBukele_Spon)
VARdf_ES19_c_An<-cbind(ES19_CarlosCalleja_An, ES19_HugoMartinez_An, ES19_JosueAlvarado_An, ES19_NayibBukele_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_ES19_c_Spon, 'VARdf_ES19_c_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_ES19_c_An, 'VARdf_ES19_c_An.csv', fileEncoding="UTF-8")




#ES19_p
print(df_ES19_p2_filtered)#7 accounts, all missing at least one day

# Create a complete sequence of dates
ES19p_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_ES19_p2$created_at_seconds2), 
                                                             max(df_ES19_p2$created_at_seconds2), 
                                                             by = "day")) #125 days

df_ES19_ARENA<- df_ES19_p2%>% 
  filter(user_name=="ARENA")
df_ES19_DemocraciaSalvadorena<- df_ES19_p2%>%
  filter(user_name=="Democracia Salvadoreña")
df_ES19_FMLN<- df_ES19_p2%>%
  filter(user_name=="FMLN")
df_ES19_FMLNOficial<- df_ES19_p2%>%
  filter(user_name=="FMLN Oficial")
df_ES19_PARTIDOGANA<- df_ES19_p2%>%
  filter(user_name=="PARTIDO GANA")
df_ES19_PCN<- df_ES19_p2%>%
  filter(user_name=="PCN")
df_ES19_VAMOS<- df_ES19_p2%>%
  filter(user_name=="VAMOS")

#I need to make it so the accounts that have some missing days get the missing days added
df_ES19_ARENA <- ES19p_complete_dates %>%left_join(df_ES19_ARENA, by = "created_at_seconds2")
df_ES19_DemocraciaSalvadorena<- ES19p_complete_dates %>%left_join(df_ES19_DemocraciaSalvadorena, by = "created_at_seconds2")
df_ES19_FMLN <- ES19p_complete_dates %>%left_join(df_ES19_FMLN, by = "created_at_seconds2")
df_ES19_FMLNOficial <- ES19p_complete_dates %>%left_join(df_ES19_FMLNOficial, by = "created_at_seconds2")
df_ES19_PARTIDOGANA <- ES19p_complete_dates %>%left_join(df_ES19_PARTIDOGANA, by = "created_at_seconds2")
df_ES19_PCN <- ES19p_complete_dates %>%left_join(df_ES19_PCN, by = "created_at_seconds2")
df_ES19_VAMOS <- ES19p_complete_dates %>%left_join(df_ES19_VAMOS, by = "created_at_seconds2")

#And then, fill in missing values with 0
df_ES19_ARENA$mean_Auth[is.na(df_ES19_ARENA$mean_Auth)]<-0
df_ES19_ARENA$mean_An[is.na(df_ES19_ARENA$mean_An)]<-0
df_ES19_DemocraciaSalvadorena$mean_Auth[is.na(df_ES19_DemocraciaSalvadorena$mean_Auth)]<-0
df_ES19_DemocraciaSalvadorena$mean_An[is.na(df_ES19_DemocraciaSalvadorena$mean_An)]<-0
df_ES19_FMLN$mean_Auth[is.na(df_ES19_FMLN$mean_Auth)]<-0
df_ES19_FMLN$mean_An[is.na(df_ES19_FMLN$mean_An)]<-0
df_ES19_FMLNOficial$mean_Auth[is.na(df_ES19_FMLNOficial$mean_Auth)]<-0
df_ES19_FMLNOficial$mean_An[is.na(df_ES19_FMLNOficial$mean_An)]<-0
df_ES19_PARTIDOGANA$mean_Auth[is.na(df_ES19_PARTIDOGANA$mean_Auth)]<-0
df_ES19_PARTIDOGANA$mean_An[is.na(df_ES19_PARTIDOGANA$mean_An)]<-0
df_ES19_PCN$mean_Auth[is.na(df_ES19_PCN$mean_Auth)]<-0
df_ES19_PCN$mean_An[is.na(df_ES19_PCN$mean_An)]<-0
df_ES19_VAMOS$mean_Auth[is.na(df_ES19_VAMOS$mean_Auth)]<-0
df_ES19_VAMOS$mean_An[is.na(df_ES19_VAMOS$mean_An)]<-0

#turn to ts
ES19_ARENA_Spon<-ts(df_ES19_ARENA$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_ARENA_An<-ts(df_ES19_ARENA$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_DemocraciaSalvadorena_Spon<-ts(df_ES19_DemocraciaSalvadorena$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_DemocraciaSalvadorena_An<-ts(df_ES19_DemocraciaSalvadorena$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_FMLN_Spon<-ts(df_ES19_FMLN$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_FMLN_An<-ts(df_ES19_FMLN$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_FMLNOficial_Spon<-ts(df_ES19_FMLNOficial$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_FMLNOficial_An<-ts(df_ES19_FMLNOficial$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_PARTIDOGANA_Spon<-ts(df_ES19_PARTIDOGANA$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_PARTIDOGANA_An<-ts(df_ES19_PARTIDOGANA$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_PCN_Spon<-ts(df_ES19_PCN$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_PCN_An<-ts(df_ES19_PCN$mean_An, start = c(2018, 10, 02), frequency=1) 
ES19_VAMOS_Spon<-ts(df_ES19_VAMOS$mean_Auth, start = c(2018, 10, 02), frequency=1)
ES19_VAMOS_An<-ts(df_ES19_VAMOS$mean_An, start = c(2018, 10, 02), frequency=1) 


#merge again into a single df to be able to run the vAR
VARdf_ES19_p_Spon<-cbind(ES19_ARENA_Spon, ES19_DemocraciaSalvadorena_Spon, ES19_FMLN_Spon, ES19_FMLNOficial_Spon,
                         ES19_PARTIDOGANA_Spon, ES19_PCN_Spon, ES19_VAMOS_Spon)
VARdf_ES19_p_An<-cbind(ES19_ARENA_An, ES19_DemocraciaSalvadorena_An, ES19_FMLN_An, ES19_FMLNOficial_An,
                       ES19_PARTIDOGANA_An, ES19_PCN_An, ES19_VAMOS_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_ES19_p_Spon, 'VARdf_ES19_p_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_ES19_p_An, 'VARdf_ES19_p_An.csv', fileEncoding="UTF-8")


#HO13_c
print(df_HO13_c2_filtered)#3 accounts, all missing at least one day

# Create a complete sequence of dates
HO13c_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO13_c2$created_at_seconds2), 
                                                             max(df_HO13_c2$created_at_seconds2), 
                                                             by = "day")) #186 days

df_HO13_JOH<- df_HO13_c2%>% 
  filter(user_name=="Juan Orlando H.")
df_HO13_MauricioVilleda<- df_HO13_c2%>%
  filter(user_name=="Mauricio Villeda Bermúdez")
df_HO13_XiomaraCastro<- df_HO13_c2%>%
  filter(user_name=="Xiomara Castro de Zelaya")

#I need to make it so the accounts that have some missing days get the missing days added
df_HO13_JOH <- HO13c_complete_dates %>%left_join(df_HO13_JOH, by = "created_at_seconds2")
df_HO13_MauricioVilleda<- HO13c_complete_dates %>%left_join(df_HO13_MauricioVilleda, by = "created_at_seconds2")
df_HO13_XiomaraCastro <- HO13c_complete_dates %>%left_join(df_HO13_XiomaraCastro, by = "created_at_seconds2")


#And then, fill in missing values with 0
df_HO13_JOH$mean_Auth[is.na(df_HO13_JOH$mean_Auth)]<-0
df_HO13_JOH$mean_An[is.na(df_HO13_JOH$mean_An)]<-0
df_HO13_MauricioVilleda$mean_Auth[is.na(df_HO13_MauricioVilleda$mean_Auth)]<-0
df_HO13_MauricioVilleda$mean_An[is.na(df_HO13_MauricioVilleda$mean_An)]<-0
df_HO13_XiomaraCastro$mean_Auth[is.na(df_HO13_XiomaraCastro$mean_Auth)]<-0
df_HO13_XiomaraCastro$mean_An[is.na(df_HO13_XiomaraCastro$mean_An)]<-0

#turn to ts
HO13_JOH_Spon<-ts(df_HO13_JOH$mean_Auth, start = c(2013, 05, 23), frequency=1)
HO13_JOH_An<-ts(df_HO13_JOH$mean_An, start = c(2013, 05, 23), frequency=1) 
HO13_MauricioVilleda_Spon<-ts(df_HO13_MauricioVilleda$mean_Auth, start = c(2013, 05, 23), frequency=1)
HO13_MauricioVilleda_An<-ts(df_HO13_MauricioVilleda$mean_An, start = c(2013, 05, 23), frequency=1) 
HO13_XiomaraCastro_Spon<-ts(df_HO13_XiomaraCastro$mean_Auth, start = c(2013, 05, 23), frequency=1)
HO13_XiomaraCastro_An<-ts(df_HO13_XiomaraCastro$mean_An, start = c(2013, 05, 23), frequency=1) 



#merge again into a single df to be able to run the vAR
VARdf_HO13_c_Spon<-cbind(HO13_JOH_Spon, HO13_MauricioVilleda_Spon, HO13_XiomaraCastro_Spon)
VARdf_HO13_c_An<-cbind(HO13_JOH_An, HO13_MauricioVilleda_An, HO13_XiomaraCastro_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO13_c_Spon, 'VARdf_HO13_c_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO13_c_An, 'VARdf_HO13_c_An.csv', fileEncoding="UTF-8")

#HO13_p
print(df_HO13_p2_filtered)#2 accounts, all missing at least one day

# Create a complete sequence of dates
HO13p_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO13_p2$created_at_seconds2), 
                                                             max(df_HO13_p2$created_at_seconds2), 
                                                             by = "day")) #185 days

df_HO13_PartidoLiberal<- df_HO13_p2%>% 
  filter(user_name=="Partido Liberal de Honduras")
df_HO13_PartidoLibre<- df_HO13_p2%>%
  filter(user_name=="Partido Libre")


#I need to make it so the accounts that have some missing days get the missing days added
df_HO13_PartidoLiberal <- HO13p_complete_dates %>%left_join(df_HO13_PartidoLiberal, by = "created_at_seconds2")
df_HO13_PartidoLibre<- HO13p_complete_dates %>%left_join(df_HO13_PartidoLibre, by = "created_at_seconds2")



#And then, fill in missing values with 0
df_HO13_PartidoLiberal$mean_Auth[is.na(df_HO13_PartidoLiberal$mean_Auth)]<-0
df_HO13_PartidoLiberal$mean_An[is.na(df_HO13_PartidoLiberal$mean_An)]<-0
df_HO13_PartidoLibre$mean_Auth[is.na(df_HO13_PartidoLibre$mean_Auth)]<-0
df_HO13_PartidoLibre$mean_An[is.na(df_HO13_PartidoLibre$mean_An)]<-0



#turn to ts
HO13_PartidoLiberal_Spon<-ts(df_HO13_PartidoLiberal$mean_Auth, start = c(2013, 05, 24), frequency=1)
HO13_PartidoLiberal_An<-ts(df_HO13_PartidoLiberal$mean_An, start = c(2013, 05, 24), frequency=1) 
HO13_PartidoLibre_Spon<-ts(df_HO13_PartidoLibre$mean_Auth, start = c(2013, 05, 24), frequency=1)
HO13_PartidoLibre_An<-ts(df_HO13_PartidoLibre$mean_An, start = c(2013, 05, 24), frequency=1) 



#merge again into a single df to be able to run the vAR
VARdf_HO13_p_Spon<-cbind(HO13_PartidoLiberal_Spon, HO13_PartidoLibre_Spon)
VARdf_HO13_p_An<-cbind(HO13_PartidoLiberal_An, HO13_PartidoLibre_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO13_p_Spon, 'VARdf_HO13_p_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO13_p_An, 'VARdf_HO13_p_An.csv', fileEncoding="UTF-8")


#HO17_c
print(df_HO17_c2_filtered)#3 accounts, 2 missing at least one day

# Create a complete sequence of dates
HO17c_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO17_c2$created_at_seconds2), 
                                                             max(df_HO17_c2$created_at_seconds2), 
                                                             by = "day")) #185 days

df_HO17_LuisZelaya<- df_HO17_c2%>% 
  filter(user_name=="Luis Zelaya")
df_HO17_SalvadorNasralla<- df_HO17_c2%>%
  filter(user_name=="Salvador Nasralla")
df_HO17_JOH<- df_HO17_c2%>%
  filter(user_name=="Juan Orlando H.")

#I need to make it so the accounts that have some missing days get the missing days added
df_HO17_LuisZelaya <- HO17c_complete_dates %>%left_join(df_HO17_LuisZelaya, by = "created_at_seconds2")
df_HO17_SalvadorNasralla<- HO17c_complete_dates %>%left_join(df_HO17_SalvadorNasralla, by = "created_at_seconds2")



#And then, fill in missing values with 0
df_HO17_LuisZelaya$mean_Auth[is.na(df_HO17_LuisZelaya$mean_Auth)]<-0
df_HO17_LuisZelaya$mean_An[is.na(df_HO17_LuisZelaya$mean_An)]<-0
df_HO17_SalvadorNasralla$mean_Auth[is.na(df_HO17_SalvadorNasralla$mean_Auth)]<-0
df_HO17_SalvadorNasralla$mean_An[is.na(df_HO17_SalvadorNasralla$mean_An)]<-0



#turn to ts
HO17_LuisZelaya_Spon<-ts(df_HO17_LuisZelaya$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_LuisZelaya_An<-ts(df_HO17_LuisZelaya$mean_An, start = c(2017, 05, 25), frequency=1) 
HO17_SalvadorNasralla_Spon<-ts(df_HO17_SalvadorNasralla$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_SalvadorNasralla_An<-ts(df_HO17_SalvadorNasralla$mean_An, start = c(2017, 05, 25), frequency=1) 
HO17_JOH_Spon<-ts(df_HO17_JOH$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_JOH_An<-ts(df_HO17_JOH$mean_An, start = c(2017, 05, 25), frequency=1)



#merge again into a single df to be able to run the vAR
VARdf_HO17_c_Spon<-cbind(HO17_LuisZelaya_Spon, HO17_SalvadorNasralla_Spon, HO17_JOH_Spon)
VARdf_HO17_c_An<-cbind(HO17_LuisZelaya_An, HO17_SalvadorNasralla_An, HO17_JOH_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO17_c_Spon, 'VARdf_HO17_c_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO17_c_An, 'VARdf_HO17_c_An.csv', fileEncoding="UTF-8")


#HO17_p
print(df_HO17_p2_filtered)#3 accounts, all missing least one day

# Create a complete sequence of dates
HO17p_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO17_p2$created_at_seconds2), 
                                                             max(df_HO17_p2$created_at_seconds2), 
                                                             by = "day")) #174 days

df_HO17_PartidoLibre<- df_HO17_p2%>% 
  filter(user_name=="Partido Libre")
df_HO17_PartidoNacional<- df_HO17_p2%>%
  filter(user_name=="Partido Nacional")
df_HO17_PINUSD<- df_HO17_p2%>%
  filter(user_name=="PINU-SD HONDURAS")

#I need to make it so the accounts that have some missing days get the missing days added
df_HO17_PartidoLibre <- HO17p_complete_dates %>%left_join(df_HO17_PartidoLibre, by = "created_at_seconds2")
df_HO17_PartidoNacional<- HO17p_complete_dates %>%left_join(df_HO17_PartidoNacional, by = "created_at_seconds2")
df_HO17_PINUSD<- HO17p_complete_dates %>%left_join(df_HO17_PINUSD, by = "created_at_seconds2")


#And then, fill in missing values with 0
df_HO17_PartidoLibre$mean_Auth[is.na(df_HO17_PartidoLibre$mean_Auth)]<-0
df_HO17_PartidoLibre$mean_An[is.na(df_HO17_PartidoLibre$mean_An)]<-0
df_HO17_PartidoNacional$mean_Auth[is.na(df_HO17_PartidoNacional$mean_Auth)]<-0
df_HO17_PartidoNacional$mean_An[is.na(df_HO17_PartidoNacional$mean_An)]<-0
df_HO17_PINUSD$mean_Auth[is.na(df_HO17_PINUSD$mean_Auth)]<-0
df_HO17_PINUSD$mean_An[is.na(df_HO17_PINUSD$mean_An)]<-0


#turn to ts
HO17_PartidoLibre_Spon<-ts(df_HO17_PartidoLibre$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_PartidoLibre_An<-ts(df_HO17_PartidoLibre$mean_An, start = c(2017, 05, 25), frequency=1) 
HO17_PartidoNacional_Spon<-ts(df_HO17_PartidoNacional$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_PartidoNacional_An<-ts(df_HO17_PartidoNacional$mean_An, start = c(2017, 05, 25), frequency=1) 
HO17_PINUSD_Spon<-ts(df_HO17_PINUSD$mean_Auth, start = c(2017, 05, 25), frequency=1)
HO17_PINUSD_An<-ts(df_HO17_PINUSD$mean_An, start = c(2017, 05, 25), frequency=1)



#merge again into a single df to be able to run the vAR
VARdf_HO17_p_Spon<-cbind(HO17_PartidoLibre_Spon, HO17_PartidoNacional_Spon, HO17_PINUSD_Spon)
VARdf_HO17_p_An<-cbind(HO17_PartidoLibre_An, HO17_PartidoNacional_An, HO17_PINUSD_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO17_p_Spon, 'VARdf_HO17_p_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO17_p_An, 'VARdf_HO17_p_An.csv', fileEncoding="UTF-8")

#HO21_c
print(df_HO21_c2_filtered) #7 accounts, all missing least one day

# Create a complete sequence of dates
HO21c_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO21_c2$created_at_seconds2), 
                                                             max(df_HO21_c2$created_at_seconds2), 
                                                             by = "day")) #179 days

df_HO21_EsdrasAmadoLopez<- df_HO21_c2%>% 
  filter(user_name=="Esdras Amado Lopez")
df_HO21_MarlonEscotoTSH<- df_HO21_c2%>%
  filter(user_name=="Marlon Escoto TSH")
df_HO21_MarlonEscotoValerio<- df_HO21_c2%>%
  filter(user_name=="Marlon Escoto Valerio")
df_HO21_MiltonBenítez<- df_HO21_c2%>% 
  filter(user_name=="Milton Benítez")
df_HO21_SalvadorNasralla<- df_HO21_c2%>%
  filter(user_name=="Salvador Nasralla")
df_HO21_XiomaraCastrodeZelaya<- df_HO21_c2%>%
  filter(user_name=="Xiomara Castro de Zelaya")
df_HO21_YaniRosenthal<- df_HO21_c2%>%
  filter(user_name=="Yani Rosenthal")

#I need to make it so the accounts that have some missing days get the missing days added
df_HO21_EsdrasAmadoLopez <- HO21c_complete_dates %>%left_join(df_HO21_EsdrasAmadoLopez, by = "created_at_seconds2")
df_HO21_MarlonEscotoTSH<- HO21c_complete_dates %>%left_join(df_HO21_MarlonEscotoTSH, by = "created_at_seconds2")
df_HO21_MarlonEscotoValerio<- HO21c_complete_dates %>%left_join(df_HO21_MarlonEscotoValerio, by = "created_at_seconds2")
df_HO21_MiltonBenítez <- HO21c_complete_dates %>%left_join(df_HO21_MiltonBenítez, by = "created_at_seconds2")
df_HO21_SalvadorNasralla<- HO21c_complete_dates %>%left_join(df_HO21_SalvadorNasralla, by = "created_at_seconds2")
df_HO21_XiomaraCastrodeZelaya<- HO21c_complete_dates %>%left_join(df_HO21_XiomaraCastrodeZelaya, by = "created_at_seconds2")
df_HO21_YaniRosenthal <- HO21c_complete_dates %>%left_join(df_HO21_YaniRosenthal, by = "created_at_seconds2")


#And then, fill in missing values with 0
df_HO21_EsdrasAmadoLopez$mean_Auth[is.na(df_HO21_EsdrasAmadoLopez$mean_Auth)]<-0
df_HO21_EsdrasAmadoLopez$mean_An[is.na(df_HO21_EsdrasAmadoLopez$mean_An)]<-0
df_HO21_MarlonEscotoTSH$mean_Auth[is.na(df_HO21_MarlonEscotoTSH$mean_Auth)]<-0
df_HO21_MarlonEscotoTSH$mean_An[is.na(df_HO21_MarlonEscotoTSH$mean_An)]<-0
df_HO21_MarlonEscotoValerio$mean_Auth[is.na(df_HO21_MarlonEscotoValerio$mean_Auth)]<-0
df_HO21_MarlonEscotoValerio$mean_An[is.na(df_HO21_MarlonEscotoValerio$mean_An)]<-0
df_HO21_MiltonBenítez$mean_Auth[is.na(df_HO21_MiltonBenítez$mean_Auth)]<-0
df_HO21_MiltonBenítez$mean_An[is.na(df_HO21_MiltonBenítez$mean_An)]<-0
df_HO21_SalvadorNasralla$mean_Auth[is.na(df_HO21_SalvadorNasralla$mean_Auth)]<-0
df_HO21_SalvadorNasralla$mean_An[is.na(df_HO21_SalvadorNasralla$mean_An)]<-0
df_HO21_XiomaraCastrodeZelaya$mean_Auth[is.na(df_HO21_XiomaraCastrodeZelaya$mean_Auth)]<-0
df_HO21_XiomaraCastrodeZelaya$mean_An[is.na(df_HO21_XiomaraCastrodeZelaya$mean_An)]<-0
df_HO21_YaniRosenthal$mean_Auth[is.na(df_HO21_YaniRosenthal$mean_Auth)]<-0
df_HO21_YaniRosenthal$mean_An[is.na(df_HO21_YaniRosenthal$mean_An)]<-0




#turn to ts
HO21_EsdrasAmadoLopez_Spon<-ts(df_HO21_EsdrasAmadoLopez$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_EsdrasAmadoLopez_An<-ts(df_HO21_EsdrasAmadoLopez$mean_An, start = c(2021, 05, 27), frequency=1) 
HO21_MarlonEscotoTSH_Spon<-ts(df_HO21_MarlonEscotoTSH$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_MarlonEscotoTSH_An<-ts(df_HO21_MarlonEscotoTSH$mean_An, start = c(2021, 05, 27), frequency=1) 
HO21_MarlonEscotoValerio_Spon<-ts(df_HO21_MarlonEscotoValerio$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_MarlonEscotoValerio_An<-ts(df_HO21_MarlonEscotoValerio$mean_An, start = c(2021, 05, 27), frequency=1)
HO21_MiltonBenítez_Spon<-ts(df_HO21_MiltonBenítez$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_MiltonBenítez_An<-ts(df_HO21_MiltonBenítez$mean_An, start = c(2021, 05, 27), frequency=1) 
HO21_SalvadorNasralla_Spon<-ts(df_HO21_SalvadorNasralla$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_SalvadorNasralla_An<-ts(df_HO21_SalvadorNasralla$mean_An, start = c(2021, 05, 27), frequency=1) 
HO21_XiomaraCastrodeZelaya_Spon<-ts(df_HO21_XiomaraCastrodeZelaya$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_XiomaraCastrodeZelaya_An<-ts(df_HO21_XiomaraCastrodeZelaya$mean_An, start = c(2021, 05, 27), frequency=1)
HO21_YaniRosenthal_Spon<-ts(df_HO21_YaniRosenthal$mean_Auth, start = c(2021, 05, 27), frequency=1)
HO21_YaniRosenthal_An<-ts(df_HO21_YaniRosenthal$mean_An, start = c(2021, 05, 27), frequency=1) 


#merge again into a single df to be able to run the vAR
VARdf_HO21_c_Spon<-cbind(HO21_EsdrasAmadoLopez_Spon, HO21_MarlonEscotoTSH_Spon, HO21_MarlonEscotoValerio_Spon,
                         HO21_MiltonBenítez_Spon, HO21_SalvadorNasralla_Spon, HO21_XiomaraCastrodeZelaya_Spon,
                         HO21_YaniRosenthal_Spon)
VARdf_HO21_c_An<-cbind(HO21_EsdrasAmadoLopez_An, HO21_MarlonEscotoTSH_An, HO21_MarlonEscotoValerio_An,
                       HO21_MiltonBenítez_An, HO21_SalvadorNasralla_An, HO21_XiomaraCastrodeZelaya_An,
                       HO21_YaniRosenthal_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO21_c_Spon, 'VARdf_HO21_c_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO21_c_An, 'VARdf_HO21_c_An.csv', fileEncoding="UTF-8")

#HO21_p
print(df_HO21_p2_filtered) #3 accounts, all missing least one day

# Create a complete sequence of dates
HO21p_complete_dates <- data.frame(created_at_seconds2 = seq(min(df_HO21_p2$created_at_seconds2), 
                                                             max(df_HO21_p2$created_at_seconds2), 
                                                             by = "day")) #81 days

df_HO21_PartidoLiberal<- df_HO21_p2%>% 
  filter(user_name=="Partido Liberal de Honduras")
df_HO21_PartidoLibre<- df_HO21_p2%>%
  filter(user_name=="Partido Libre")
df_HO21_PartidoSalvador<- df_HO21_p2%>%
  filter(user_name=="Partido Salvador de Honduras")


#I need to make it so the accounts that have some missing days get the missing days added
df_HO21_PartidoLiberal <- HO21p_complete_dates %>%left_join(df_HO21_PartidoLiberal, by = "created_at_seconds2")
df_HO21_PartidoLibre<- HO21p_complete_dates %>%left_join(df_HO21_PartidoLibre, by = "created_at_seconds2")
df_HO21_PartidoSalvador<- HO21p_complete_dates %>%left_join(df_HO21_PartidoSalvador, by = "created_at_seconds2")


#And then, fill in missing values with 0
df_HO21_PartidoLiberal$mean_Auth[is.na(df_HO21_PartidoLiberal$mean_Auth)]<-0
df_HO21_PartidoLiberal$mean_An[is.na(df_HO21_PartidoLiberal$mean_An)]<-0
df_HO21_PartidoLibre$mean_Auth[is.na(df_HO21_PartidoLibre$mean_Auth)]<-0
df_HO21_PartidoLibre$mean_An[is.na(df_HO21_PartidoLibre$mean_An)]<-0
df_HO21_PartidoSalvador$mean_Auth[is.na(df_HO21_PartidoSalvador$mean_Auth)]<-0
df_HO21_PartidoSalvador$mean_An[is.na(df_HO21_PartidoSalvador$mean_An)]<-0



#turn to ts
HO21_PartidoLiberal_Spon<-ts(df_HO21_PartidoLiberal$mean_Auth, start = c(2021, 06, 22), frequency=1)
HO21_PartidoLiberal_An<-ts(df_HO21_PartidoLiberal$mean_An, start = c(2021, 06, 22), frequency=1) 
HO21_PartidoLibre_Spon<-ts(df_HO21_PartidoLibre$mean_Auth, start = c(2021, 06, 22), frequency=1)
HO21_PartidoLibre_An<-ts(df_HO21_PartidoLibre$mean_An, start = c(2021, 06, 22), frequency=1) 
HO21_PartidoSalvador_Spon<-ts(df_HO21_PartidoSalvador$mean_Auth, start = c(2021, 06, 22), frequency=1)
HO21_PartidoSalvador_An<-ts(df_HO21_PartidoSalvador$mean_An, start = c(2021, 06, 22), frequency=1)

#merge again into a single df to be able to run the vAR
VARdf_HO21_p_Spon<-cbind(HO21_PartidoLiberal_Spon, HO21_PartidoLibre_Spon, HO21_PartidoSalvador_Spon)
VARdf_HO21_p_An<-cbind(HO21_PartidoLiberal_An, HO21_PartidoLibre_An, HO21_PartidoSalvador_An)
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Data/Timeseries")
write.csv(VARdf_HO21_p_Spon, 'VARdf_HO21_p_Spon.csv', fileEncoding="UTF-8")
write.csv(VARdf_HO21_p_An, 'VARdf_HO21_p_An.csv', fileEncoding="UTF-8")



#visualization of ts data, preliminary, for all
#I will go ts by ts, getting the graphic and then doing the diagnostics for stationarity
setwd('C:/Elias/1 Serious/Academia/University of Pittsburgh/1 Dissertation/Images/TS')



#CR10
#Candidates, Spon
CR10_c_Spon_TS<-ggplot(data=df_CR10_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR10_c_Spon_TS
ggsave("CR10_c_Spon_TS.jpg", CR10_c_Spon_TS, device = agg_png, res = 300)
#quitar al segundo Fishman

#Candidates, An
CR10_c_An_TS<-ggplot(data=df_CR10_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR10_c_An_TS
ggsave("CR10_c_An_TS.jpg", CR10_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
CR10_p_Spon_TS<-ggplot(data=df_CR10_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR10_p_Spon_TS
ggsave("CR10_p_Spon_TS.jpg", CR10_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
CR10_p_An_TS<-ggplot(data=df_CR10_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR10_p_An_TS
ggsave("CR10_p_An_TS.jpg", CR10_p_An_TS, device = agg_png, res = 300)



#CR14
#Candidates, Spon
#smooth graphic
#smoot just adds a trend line, the standard is LOESS which is a local regression
#OJO, x tiene que ser created_at_seconds, pq es class Date, NO class "POSIXct" "POSIXt"
CR14_c_Spon_TS<-ggplot(data=df_CR14_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ #set se=F, so you don't get confidence intervals, useless here
  #loess is the standard, but can also do lm, glm, etc, but the others are not very useful so far
  #span allows to control roughness, ie, make the like less smooth, closer to the original time series
  #I cannot make it smaller than .7, I think the standard is .75
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ #angle rotates the text
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ #makes it so date in x axis appears as : month name- last two digits of year
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  #ggtitle("Analytic score in tweetsof candidates, Guatemala 2011")+
  theme(aspect.ratio = 1/4)
CR14_c_Spon_TS
ggsave("CR14_c_Spon_TS.jpg", CR14_c_Spon_TS, device = agg_png, res = 300)
#Table para ver cuantos tweets hizo c/u. Yo eliminaria todos los casos que sean muy pequeños, aca echandi
table(df_CR14_c$user_username)
df_CR14_c<-df_CR14_c%>%
  filter(user_username!="jmechandi") #SAQUE LOS TWEETS DE EHCANDI. SOLO HIZO 11 AL PURO INICIO DE LA CAMPAÑA
#OJOOOO!!!! otto guevara tambien puede valer la pena, solo twittea hasta el puro final

#Revisar estacionariedad de cada serie de tiempo, cada una TIENE que ser estacionaria
#PRimero, dickey-fuller test indica si es estacionaria
#Si no lo es, ver ACF (autocorrelaton function) y PACF (partial autocorrelation function)

#Johnny Araya


#Candidates, An
CR14_c_An_TS<-ggplot(data=df_CR14_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR14_c_An_TS
ggsave("CR14_c_An_TS.jpg", CR14_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
CR14_p_Spon_TS<-ggplot(data=df_CR14_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR14_p_Spon_TS
ggsave("CR14_p_Spon_TS.jpg", CR14_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
CR14_p_An_TS<-ggplot(data=df_CR14_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR14_p_An_TS
ggsave("CR14_p_An_TS.jpg", CR14_p_An_TS, device = agg_png, res = 300)



#CR18
#Candidates, Spon
CR18_c_Spon_TS<-ggplot(data=df_CR18_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR18_c_Spon_TS
ggsave("CR18_c_Spon_TS.jpg", CR18_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
CR18_c_An_TS<-ggplot(data=df_CR18_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR18_c_An_TS
ggsave("CR18_c_An_TS.jpg", CR18_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
CR18_p_Spon_TS<-ggplot(data=df_CR18_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR18_p_Spon_TS
ggsave("CR18_p_Spon_TS.jpg", CR18_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
CR18_p_An_TS<-ggplot(data=df_CR18_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR18_p_An_TS
ggsave("CR18_p_An_TS.jpg", CR18_p_An_TS, device = agg_png, res = 300)
#eliminar pin en este


#CR22 NECESITO MODIFICAR CON PALETA ORIGINAL, TIENE DEMASIADOS PARTIDOS Y CANDIDATOS
#Candidates, Spon
CR22_c_Spon_TS<-ggplot(data=df_CR22_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR22_c_Spon_TS
ggsave("CR22_c_Spon_TS.jpg", CR22_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
CR22_c_An_TS<-ggplot(data=df_CR22_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR22_c_An_TS
ggsave("CR22_c_An_TS.jpg", CR22_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
CR22_p_Spon_TS<-ggplot(data=df_CR22_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR22_p_Spon_TS
ggsave("CR22_p_Spon_TS.jpg", CR22_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
CR22_p_An_TS<-ggplot(data=df_CR22_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
CR22_p_An_TS
ggsave("CR22_p_An_TS.jpg", CR22_p_An_TS, device = agg_png, res = 300)

#ES14
#Candidates, Spon
ES14_c_Spon_TS<-ggplot(data=df_ES14_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES14_c_Spon_TS
ggsave("ES14_c_Spon_TS.jpg", ES14_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
ES14_c_An_TS<-ggplot(data=df_ES14_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES14_c_An_TS
ggsave("ES14_c_An_TS.jpg", ES14_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
ES14_p_Spon_TS<-ggplot(data=df_ES14_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES14_p_Spon_TS
ggsave("ES14_p_Spon_TS.jpg", ES14_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
ES14_p_An_TS<-ggplot(data=df_ES14_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES14_p_An_TS
ggsave("ES14_p_An_TS.jpg", ES14_p_An_TS, device = agg_png, res = 300)
#PSP es un despiche, deberia quitarlo



#ES19
#Candidates, Spon
ES19_c_Spon_TS<-ggplot(data=df_ES19_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES19_c_Spon_TS
ggsave("ES19_c_Spon_TS.jpg", ES19_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
ES19_c_An_TS<-ggplot(data=df_ES19_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES19_c_An_TS
ggsave("ES19_c_An_TS.jpg", ES19_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
ES19_p_Spon_TS<-ggplot(data=df_ES19_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES19_p_Spon_TS
ggsave("ES19_p_Spon_TS.jpg", ES19_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
ES19_p_An_TS<-ggplot(data=df_ES19_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
ES19_p_An_TS
ggsave("ES19_p_An_TS.jpg", ES19_p_An_TS, device = agg_png, res = 300)



#HO13
#Candidates, Spon
HO13_c_Spon_TS<-ggplot(data=df_HO13_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO13_c_Spon_TS
ggsave("HO13_c_Spon_TS.jpg", HO13_c_Spon_TS, device = agg_png, res = 300)
#orle sois talvez haya que quitarlo

#Candidates, An
HO13_c_An_TS<-ggplot(data=df_HO13_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO13_c_An_TS
ggsave("HO13_c_An_TS.jpg", HO13_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
HO13_p_Spon_TS<-ggplot(data=df_HO13_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO13_p_Spon_TS
ggsave("HO13_p_Spon_TS.jpg", HO13_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
HO13_p_An_TS<-ggplot(data=df_HO13_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO13_p_An_TS
ggsave("HO13_p_An_TS.jpg", HO13_p_An_TS, device = agg_png, res = 300)


#HO17
#Candidates, Spon
HO17_c_Spon_TS<-ggplot(data=df_HO17_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO17_c_Spon_TS
ggsave("HO17_c_Spon_TS.jpg", HO17_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
HO17_c_An_TS<-ggplot(data=df_HO17_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO17_c_An_TS
ggsave("HO17_c_An_TS.jpg", HO17_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
HO17_p_Spon_TS<-ggplot(data=df_HO17_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO17_p_Spon_TS
ggsave("HO17_p_Spon_TS.jpg", HO17_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
HO17_p_An_TS<-ggplot(data=df_HO17_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO17_p_An_TS
ggsave("HO17_p_An_TS.jpg", HO17_p_An_TS, device = agg_png, res = 300)

#HO21
#Candidates, Spon
HO21_c_Spon_TS<-ggplot(data=df_HO21_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO21_c_Spon_TS
ggsave("HO21_c_Spon_TS.jpg", HO21_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
HO21_c_An_TS<-ggplot(data=df_HO21_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO21_c_An_TS
ggsave("HO21_c_An_TS.jpg", HO21_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
HO21_p_Spon_TS<-ggplot(data=df_HO21_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO21_p_Spon_TS
ggsave("HO21_p_Spon_TS.jpg", HO21_p_Spon_TS, device = agg_png, res = 300)
#Uff aca fijo hay que quitar varias


#Parties, An
HO21_p_An_TS<-ggplot(data=df_HO21_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
HO21_p_An_TS
ggsave("HO21_p_An_TS.jpg", HO21_p_An_TS, device = agg_png, res = 300)

#GU11
#Candidates, Spon
GU11_c_Spon_TS<-ggplot(data=df_GU11_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU11_c_Spon_TS
ggsave("GU11_c_Spon_TS.jpg", GU11_c_Spon_TS, device = agg_png, res = 300)
#quitar a baldizon


#Candidates, An
GU11_c_An_TS<-ggplot(data=df_GU11_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU11_c_An_TS
ggsave("GU11_c_An_TS.jpg", GU11_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
GU11_p_Spon_TS<-ggplot(data=df_GU11_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU11_p_Spon_TS
ggsave("GU11_p_Spon_TS.jpg", GU11_p_Spon_TS, device = agg_png, res = 300)
#quitar a pan tal vez

#Parties, An
GU11_p_An_TS<-ggplot(data=df_GU11_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU11_p_An_TS
ggsave("GU11_p_An_TS.jpg", GU11_p_An_TS, device = agg_png, res = 300)


#GU15
#Candidates, Spon
GU15_c_Spon_TS<-ggplot(data=df_GU15_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU15_c_Spon_TS
ggsave("GU15_c_Spon_TS.jpg", GU15_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
GU15_c_An_TS<-ggplot(data=df_GU15_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU15_c_An_TS
ggsave("GU15_c_An_TS.jpg", GU15_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
GU15_p_Spon_TS<-ggplot(data=df_GU15_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU15_p_Spon_TS
ggsave("GU15_p_Spon_TS.jpg", GU15_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
GU15_p_An_TS<-ggplot(data=df_GU15_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU15_p_An_TS
ggsave("GU15_p_An_TS.jpg", GU15_p_An_TS, device = agg_png, res = 300)


#GU19 #necesito cambiar la paleta
#Candidates, Spon
GU19_c_Spon_TS<-ggplot(data=df_GU19_c,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU19_c_Spon_TS
ggsave("GU19_c_Spon_TS.jpg", GU19_c_Spon_TS, device = agg_png, res = 300)


#Candidates, An
GU19_c_An_TS<-ggplot(data=df_GU19_c,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU19_c_An_TS
ggsave("GU19_c_An_TS.jpg", GU19_c_An_TS, device = agg_png, res = 300)


#Parties, Spon
GU19_p_Spon_TS<-ggplot(data=df_GU19_p,(aes(x=created_at_seconds2, y=Authenticity, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Sponteneity")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU19_p_Spon_TS
ggsave("GU19_p_Spon_TS.jpg", GU19_p_Spon_TS, device = agg_png, res = 300)

#Parties, An
GU19_p_An_TS<-ggplot(data=df_GU19_p,(aes(x=created_at_seconds2, y=Analytic, group=user_name, colour=user_name)))+
  geom_smooth(linewidth=1, se=F, method="loess", span=.7)+ 
  theme_minimal()+
  theme(axis.text.x=
          element_text(angle=0, hjust=1, size=13))+ 
  theme(plot.title=
          element_text(hjust=0.5, size=18))+
  scale_x_date(date_labels = "%b-%y")+ 
  scale_color_brewer(palette = "Paired")+
  ylab("Analytic")+
  xlab("")+
  theme(aspect.ratio = 1/4)
GU19_p_An_TS
ggsave("GU19_p_An_TS.jpg", GU19_p_An_TS, device = agg_png, res = 300)

