setwd('G:/DSE flash/R scripts')
getwd()
library(ggplot2)
library(readr)
library(tidyverse)
library(tibble)
library(ggforce)

CaSus<-c('#7FFFD4','#E52B50','#884DA7','#293133',
         '#FF6600','#007FFF','#0000FF','#050402', 
         '#800000','#7FFF00','#00A86B','#FFFFFF',
         '#C0007F','#FFCC99','#93C572','#FF0000',
         '#FF6088','#704214','#228B22','#77DD77',
         '#F4C430','#A98307','#D8BFD8','#F0DC82',
         '#99CBFF','#0ED2EB','#000080','#FF7F50',
         '#FFFF00','#C6A664','#465945','#CCFF00',
         '#FF8C69','#F400A1','#964B00','#D1E231',
         '#00665C','#CCCCFF','#800080')

#EUROPEAN NATIONS PART

#Household level of internet access
Chli<-read_tsv('country_Households_level_of_internet_access.tsv', na = ":")
Chli <- Chli[-c(15:18),] #remove all aggregates but 'Euro Area'

#tidying

Chli$'2018'=gsub( "b", "", as.character(Chli$'2018'))
Chli$'2014'=gsub( "b", "", as.character(Chli$'2014'))
Chli$'2016'=gsub( "b", "", as.character(Chli$'2016'))
Chli$'2017'=gsub( "b", "", as.character(Chli$'2017'))
Chli$'2020'=gsub( "e", "", as.character(Chli$'2020'))
Chli$'2014'<-as.double(Chli$'2014')
Chli$`2016`<-as.double(Chli$`2016`)
Chli$`2017`<-as.double(Chli$'2017')
Chli$`2018`<-as.double(Chli$'2018')
Chli$`2020`<-as.double(Chli$'2020')


Chli<-Chli %>% 
  pivot_longer(c(`2011`:`2020`), names_to ='Year', values_to = 'Coefficient')

names(Chli)[1] <-'Country'

Chli$Year<-as.numeric(Chli$Year)

Chli$Country[1:10]<-'Albania'
Chli$Country[11:20]<-'Austria'
Chli$Country[21:30]<-'Bosnia and Herzegovina'
Chli$Country[31:40]<-'Belgium'
Chli$Country[41:50]<-'Bulgaria'
Chli$Country[51:60]<-'Switzerland'
Chli$Country[61:70]<-'Cyprus'
Chli$Country[71:80]<-'Czechia'
Chli$Country[81:90]<-'Germany'
Chli$Country[91:100]<-'Denmark'
Chli$Country[101:110]<-'Euro Area'
Chli$Country[111:120]<-'Estonia'
Chli$Country[121:130]<-'Greece'
Chli$Country[131:140]<-'Spain'
Chli$Country[141:150]<-'Finland'
Chli$Country[151:160]<-'France'
Chli$Country[161:170]<-'Croatia'
Chli$Country[171:180]<-'Hungary'
Chli$Country[181:190]<-'Ireland'
Chli$Country[191:200]<-'Iceland'
Chli$Country[201:210]<-'Italy'
Chli$Country[211:220]<-'Lithuania'
Chli$Country[221:230]<-'Luxembourg'
Chli$Country[231:240]<-'Latvia'
Chli$Country[241:250]<-'Montenegro'
Chli$Country[251:260]<-'North Macedonia'
Chli$Country[261:270]<-'Malta'
Chli$Country[271:280]<-'Netherlands'
Chli$Country[281:290]<-'Norway'
Chli$Country[291:300]<-'Poland'
Chli$Country[301:310]<-'Portugal'
Chli$Country[311:320]<-'Romania'
Chli$Country[321:330]<-'Serbia'
Chli$Country[331:340]<-'Sweden'
Chli$Country[341:350]<-'Slovenia'
Chli$Country[351:360]<-'Slovakia'
Chli$Country[361:370]<-'Turkey'
Chli$Country[371:380]<-'United Kingdom'
Chli$Country[381:390]<-'Kosovo'

Chli<-drop_na(Chli)
#plots (general -> European mean, countries level of coefficients through time)

ggplot(Chli, aes(x=Year, y=Coefficient, colour=Country)) + 
  geom_point() +
  scale_colour_manual(values=CaSus) +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(40,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2020, by=1), limits=c(2011,2021), expand=c(0,0)) +
  ggtitle("Level of Households with Internet Connection - Europe") +
  ylab("Coefficient") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()


ggplot(Chli, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col= Country)) +
  scale_color_manual(values=CaSus) +
  ylab("Coefficient") +
  xlab("Year") +
  ggtitle("Level of Households with Internet Connection \n - Europe") +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(40,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2020, by=1), limits=c(2011,2021), expand=c(0,0)) +
  theme_dark()

#facets 

ggplot(Chli, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Internet Connection") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(40,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35))
       


ggplot(Chli, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Internet Connection") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(40,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35))

#Boxplots


#boxplot with ggplot (general, increase IT levels in countries over time)

Chli$Year<-as.character(Chli$Year) #needed to plot boxplots

EU_IH_mean_develop<-ggplot(Chli, aes(Coefficient,Year))+
  geom_boxplot() +
  scale_y_discrete(breaks=seq(2011,2020, by=1))+
  scale_x_continuous(breaks = seq(10,100,by=2))+
  xlab("Level of Households with Internet Connection") +
  ggtitle('Distributions of connections in the Euro Area by year (quartiles)')+
  theme_bw()

EU_IH_mean_develop

#TO PLOT "Yearly coefficient" BOXPLOTS, USE DATASET WITH REMOVED NA
#remove Albania, Bosnia, Switzerland, Montenegro, Serbia and Kosovo
Chli <- Chli[-c(1,2,13:15,36:38,215:219,288:292,342:345), ] 
#reorder dataset to plot boxplot
Chli$country_sort <- with(Chli, reorder(Country, Coefficient, function(x) median(x, na.rm = T)))

#Developement of internet connections over time (focusing on most rapid and effective countries)
EU_IH_countries_sort<-ggplot(Chli, aes(Coefficient,country_sort))+
  geom_boxplot() +
  scale_x_continuous(breaks = seq(10,100, by=2 )) +
  theme_bw()
EU_IH_countries_sort + ggtitle('Developement of internet connections between 2011 and 2020 \n - Europe')+
  ylab('Country') +
  xlab('Level of Households with Internet Connection')

#--------------------------------------------------------------------------------

#BROADBAND CONNECTION
#Household level of broadband access
Chlb<-read_tsv('country_Households_with_broadband_access.tsv', na = c(': u', ':'))

Chlb <- Chlb[-c(15:17),-c(2,3)] #remove all aggregates but 'Euro Area' 
#and remove years 2009,2010


Chlb$'2018'=gsub( "b", "", as.character(Chlb$'2018'))
Chlb$'2014'=gsub( "b", "", as.character(Chlb$'2014'))
Chlb$'2016'=gsub( "b", "", as.character(Chlb$'2016'))
Chlb$'2017'=gsub( "b", "", as.character(Chlb$'2017'))
Chlb$'2019'=gsub( "b", "", as.character(Chlb$'2019'))
Chlb$'2020'=gsub( "b", "", as.character(Chlb$'2020'))
Chlb$'2020'=gsub( "e", "", as.character(Chlb$'2020'))
Chlb$'2014'<-as.double(Chlb$'2014')
Chlb$`2016`<-as.double(Chlb$`2016`)
Chlb$`2017`<-as.double(Chlb$'2017')
Chlb$`2018`<-as.double(Chlb$'2018')
Chlb$'2019'<-as.double(Chlb$'2019')
Chlb$`2020`<-as.double(Chlb$'2020')
Chlb<-Chlb %>% 
  pivot_longer(c(`2011`:`2020`), names_to ='Year', values_to = 'Coefficient')
names(Chlb)[1] <-'Country'
Chlb$Year<-as.numeric(Chlb$Year)

Chlb$Country[1:10]<-'Albania'
Chlb$Country[11:20]<-'Austria'
Chlb$Country[21:30]<-'Bosnia and Herzegovina'
Chlb$Country[31:40]<-'Belgium'
Chlb$Country[41:50]<-'Bulgaria'
Chlb$Country[51:60]<-'Switzerland'
Chlb$Country[61:70]<-'Cyprus'
Chlb$Country[71:80]<-'Czechia'
Chlb$Country[81:90]<-'Germany'
Chlb$Country[91:100]<-'Denmark'
Chlb$Country[101:110]<-'Euro Area'
Chlb$Country[111:120]<-'Estonia'
Chlb$Country[121:130]<-'Greece'
Chlb$Country[131:140]<-'Spain'
Chlb$Country[141:150]<-'Finland'
Chlb$Country[151:160]<-'France'
Chlb$Country[161:170]<-'Croatia'
Chlb$Country[171:180]<-'Hungary'
Chlb$Country[181:190]<-'Ireland'
Chlb$Country[191:200]<-'Iceland'
Chlb$Country[201:210]<-'Italy'
Chlb$Country[211:220]<-'Lithuania'
Chlb$Country[221:230]<-'Luxembourg'
Chlb$Country[231:240]<-'Latvia'
Chlb$Country[241:250]<-'Montenegro'
Chlb$Country[251:260]<-'North Macedonia'
Chlb$Country[261:270]<-'Malta'
Chlb$Country[271:280]<-'Netherlands'
Chlb$Country[281:290]<-'Norway'
Chlb$Country[291:300]<-'Poland'
Chlb$Country[301:310]<-'Portugal'
Chlb$Country[311:320]<-'Romania'
Chlb$Country[321:330]<-'Serbia'
Chlb$Country[331:340]<-'Sweden'
Chlb$Country[341:350]<-'Slovenia'
Chlb$Country[351:360]<-'Slovakia'
Chlb$Country[361:370]<-'Turkey'
Chlb$Country[371:380]<-'United Kingdom'
Chlb$Country[381:390]<-'Kosovo'
Chlb<-drop_na(Chlb)

#plots (general -> European mean, countries level of coefficients through time)

Chlb$Year<-as.numeric(Chlb$Year) #needed to plot geom_point

#lm 
ggplot(Chlb, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  scale_colour_manual(values = CaSus) +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(30,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2020, by=1), limits=c(2011,2021), expand=c(0,0)) +
  ylab("Level of Households with Broadband Connection") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()

#geom Lines
ggplot(Chlb, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col = Country)) +
  scale_colour_manual(values = CaSus) +
  ylab("Coefficient") +
  xlab("Year") +
  ggtitle('Level of Households with Broadband Connection \n - Europe') +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(30,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2020, by=1), limits=c(2011,2021), expand=c(0,0)) +
  theme_dark()
#facets
#lm 
ggplot(Chlb, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Broadband Connection") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(30,105), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))

#geom line
ggplot(Chlb, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Broadband Connection") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(30,105), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))

#boxplot with ggplot (general, increase IT levels in countries over time)

Chlb$Year<-as.character(Chlb$Year) #needed to plot boxplots

EU_BH_mean_develop<-ggplot(Chlb, aes(Coefficient,Year))+
  geom_boxplot() +
  scale_y_discrete(breaks=seq(2011,2020, by=1))+
  scale_x_continuous(breaks = seq(10,100,by=2))+
  xlab("Level of Households with Broadband Connection") +
  ggtitle('Distributions of  Broadband Connections in the Euro Area by Year (Quartiles)')+
  theme_bw()
EU_BH_mean_develop

#TO PLOT "Yearly coefficient" BOXPLOTS, USE DATASET WITH REMOVED NA
#remove Albania, Bosnia, Switzerland, Montenegro, Serbia and Kosovo
Chlb <- Chlb[-c(1,2,13:15,36:38,215:219,288:292,340:343), ] 

#reorder dataset to plot boxplot
Chlb$country_sort <- with(Chlb, reorder(Country, Coefficient, function(x) median(x, na.rm = T)))

#Development of internet connections over time (focusing on most rapid and effective countries)
EU_BH_countries_sort<-ggplot(Chlb, aes(Coefficient,country_sort))+
  geom_boxplot() +
  scale_x_continuous(breaks = seq(10,100, by=2 )) +
  theme_bw()
EU_BH_countries_sort + ggtitle('Developement of Broadband connections between 2011 and 2020 \n - Europe')+
  ylab('Country') +
  xlab('Level of Households with Broadband Connection') 

#--------------------------------------------------------------------------------
#NOT HAVING INTERNET AT HOME BECAUSE COST IS TOO HIGH (MISSING 2018 DATA)
#percentage of households 

Crnhi<-read_tsv('country_Reasons_not_having_internet_home.tsv', na=c(':',': u'))
Crnhi<-Crnhi[-c(15,16,17), -c(2,3,4)]

Crnhi$'2012'=gsub( 'u', '', as.character(Crnhi$'2012'))
Crnhi$'2013'=gsub( 'u', '', as.character(Crnhi$'2013'))
Crnhi$'2014'=gsub( "b", "", as.character(Crnhi$'2014'))
Crnhi$'2014'=gsub( 'u', '', as.character(Crnhi$'2014'))
Crnhi$'2015'=gsub( 'u', '', as.character(Crnhi$'2015'))
Crnhi$'2016'=gsub( 'u', '', as.character(Crnhi$'2016'))
Crnhi$'2016'=gsub( "b", "", as.character(Crnhi$'2016'))
Crnhi$'2017'=gsub( "b", "", as.character(Crnhi$'2017'))
Crnhi$'2017'=gsub( 'u', '', as.character(Crnhi$'2017'))
Crnhi$'2019'=gsub( "bu", "", as.character(Crnhi$'2019'))
Crnhi$'2019'=gsub( 'u', '', as.character(Crnhi$'2019'))
Crnhi$'2012'<-as.double(Crnhi$'2012')
Crnhi$'2013'<-as.double(Crnhi$'2013')
Crnhi$'2014'<-as.double(Crnhi$'2014')
Crnhi$'2015'<-as.double(Crnhi$'2015')
Crnhi$'2016'<-as.double(Crnhi$'2016')
Crnhi$'2017'<-as.double(Crnhi$'2017')
Crnhi$'2019'<-as.double(Crnhi$'2019')

Crnhi<-Crnhi %>% 
  pivot_longer(c(`2010`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(Crnhi)[1] <-'Country'
Crnhi$Year<-as.numeric(Crnhi$Year)

Crnhi$Country[1:9]<-'Albania'
Crnhi$Country[10:18]<-'Austria'
Crnhi$Country[19:27]<-'Bosnia and Herzegovina'
Crnhi$Country[28:36]<-'Belgium'
Crnhi$Country[37:45]<-'Bulgaria'
Crnhi$Country[46:54]<-'Switzerland'
Crnhi$Country[55:63]<-'Cyprus'
Crnhi$Country[64:72]<-'Czechia'
Crnhi$Country[73:81]<-'Germany'
Crnhi$Country[82:90]<-'Denmark'
Crnhi$Country[91:99]<-'Euro Area'
Crnhi$Country[100:108]<-'Estonia'
Crnhi$Country[109:117]<-'Greece'
Crnhi$Country[118:126]<-'Spain'
Crnhi$Country[127:135]<-'Finland'
Crnhi$Country[136:144]<-'France'
Crnhi$Country[145:153]<-'Croatia'
Crnhi$Country[154:162]<-'Hungary'
Crnhi$Country[163:171]<-'Ireland'
Crnhi$Country[172:180]<-'Iceland'
Crnhi$Country[181:189]<-'Italy'
Crnhi$Country[190:198]<-'Lithuania'
Crnhi$Country[199:207]<-'Luxembourg'
Crnhi$Country[208:216]<-'Latvia'
Crnhi$Country[217:225]<-'Montenegro'
Crnhi$Country[226:234]<-'North Macedonia'
Crnhi$Country[235:243]<-'Malta'
Crnhi$Country[244:252]<-'Netherlands'
Crnhi$Country[253:261]<-'Norway'
Crnhi$Country[262:270]<-'Poland'
Crnhi$Country[271:279]<-'Portugal'
Crnhi$Country[280:288]<-'Romania'
Crnhi$Country[289:297]<-'Serbia'
Crnhi$Country[298:306]<-'Sweden'
Crnhi$Country[307:315]<-'Slovenia'
Crnhi$Country[316:324]<-'Slovakia'
Crnhi$Country[325:333]<-'Turkey'
Crnhi$Country[334:342]<-'United Kingdom'
Crnhi$Country[343:351]<-'Kosovo'

Crnhi<-drop_na(Crnhi)

#plots
ggplot(Crnhi, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  scale_colour_manual(values=CaSus) +
  scale_y_continuous(breaks = seq(0,100, by=2 ),limits = c(0,70), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2010,2019, by=1), limits = c(2010,2020),expand=c(0,0)) +
  ylab("Percentage of Households not Having an Internet Connection because Cost is too High") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark() 

ggplot(Crnhi, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col=Country)) +
  scale_colour_manual(values=CaSus) +
  ylab("Percentage of Households not Having an Internet Connection because Cost is too High") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=2 ), limits=c(0,70), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2019, by = 1), limits = c(2010,2020), expand=c(0,0)) +
  theme_dark()

#facet wraps

ggplot(Crnhi, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Households not Having Internet Connection because \nAccess Cost is too High - Europe')+
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0,80),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 1)

ggplot(Crnhi, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Households not Having Internet Connection because \nAccess Cost is too High - Europe')+
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0,80),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 2)
  
ggplot(Crnhi, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Households not Having Internet Connection because \nAccess Cost is too High - Europe')+
  scale_y_continuous(breaks = seq(0, 80, 10), limits = c(0,80),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 3)

 
#geom line
ggplot(Crnhi, aes(y = Coefficient, x = Year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Percentage of Households not Having Internet Connections because Cost is too High") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0, 80, 20), limits = c(0,80),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))

#-------------------------------------------------------------------------------
#INTERNET USAGE
#Interaction with public authorities
#percentage of individuals using internet for interacting with public authorities 
#in the last 12 months

Cindia<-read_tsv('country_Individuals_internet_interacting_with_authorities.tsv',na=':')
Cindia<-Cindia[-c(15,16,17,18), ]

Cindia$'2014'=gsub( "b", "", as.character(Cindia$'2014'))
Cindia$'2016'=gsub( "b", "", as.character(Cindia$'2016'))
Cindia$'2017'=gsub( "b", "", as.character(Cindia$'2017'))
Cindia$'2018'=gsub( "b", "", as.character(Cindia$'2018'))
Cindia$'2019'=gsub( "b", "", as.character(Cindia$'2019'))
Cindia$'2020'=gsub( "u", "", as.character(Cindia$'2020'))
Cindia$'2020'=gsub( "e", "", as.character(Cindia$'2020'))
Cindia$'2014'<-as.double(Cindia$'2014')
Cindia$'2016'<-as.double(Cindia$'2016')
Cindia$'2017'<-as.double(Cindia$'2017')
Cindia$'2018'<-as.double(Cindia$'2018')
Cindia$'2019'<-as.double(Cindia$'2019')
Cindia$'2020'<-as.double(Cindia$'2020')

Cindia<-Cindia %>% 
  pivot_longer(c(`2011`:`2020`), names_to ='Year', values_to = 'Coefficient')
names(Cindia)[1] <-'Country'
Cindia$Year<-as.numeric(Cindia$Year)

Cindia$Country[1:10]<-'Albania'
Cindia$Country[11:20]<-'Austria'
Cindia$Country[21:30]<-'Bosnia and Herzegovina'
Cindia$Country[31:40]<-'Belgium'
Cindia$Country[41:50]<-'Bulgaria'
Cindia$Country[51:60]<-'Switzerland'
Cindia$Country[61:70]<-'Cyprus'
Cindia$Country[71:80]<-'Czechia'
Cindia$Country[81:90]<-'Germany'
Cindia$Country[91:100]<-'Denmark'
Cindia$Country[101:110]<-'Euro Area'
Cindia$Country[111:120]<-'Estonia'
Cindia$Country[121:130]<-'Greece'
Cindia$Country[131:140]<-'Spain'
Cindia$Country[141:150]<-'Finland'
Cindia$Country[151:160]<-'France'
Cindia$Country[161:170]<-'Croatia'
Cindia$Country[171:180]<-'Hungary'
Cindia$Country[181:190]<-'Ireland'
Cindia$Country[191:200]<-'Iceland'
Cindia$Country[201:210]<-'Italy'
Cindia$Country[211:220]<-'Lithuania'
Cindia$Country[221:230]<-'Luxembourg'
Cindia$Country[231:240]<-'Latvia'
Cindia$Country[241:250]<-'Montenegro'
Cindia$Country[251:260]<-'North Macedonia'
Cindia$Country[261:270]<-'Malta'
Cindia$Country[271:280]<-'Netherlands'
Cindia$Country[281:290]<-'Norway'
Cindia$Country[291:300]<-'Poland'
Cindia$Country[301:310]<-'Portugal'
Cindia$Country[311:320]<-'Romania'
Cindia$Country[321:330]<-'Serbia'
Cindia$Country[331:340]<-'Sweden'
Cindia$Country[341:350]<-'Slovenia'
Cindia$Country[351:360]<-'Slovakia'
Cindia$Country[361:370]<-'Turkey'
Cindia$Country[371:380]<-'United Kingdom'
Cindia$Country[381:390]<-'Kosovo'

Cindia<-drop_na(Cindia)

#plots
ggplot(Cindia, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  scale_colour_manual(values = CaSus)+
  scale_y_continuous(breaks = seq(0,100, by=2 ),limits = c(0,94), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2020, by=1), limits = c(2011,2021),expand=c(0,0)) +
  ylab("Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()

#geomline
ggplot(Cindia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col=Country)) +
  scale_colour_manual(values=CaSus) +
  ylab("Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=2 ), limits=c(0,94), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1), limits = c(2011,2021), expand=c(0,0)) +
  theme_dark()

#facets
ggplot(Cindia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet \nfor Interacting with PA in the Last 12 Months')+
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2),limits = c(2010,2021),expand = c(0,0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 1)

ggplot(Cindia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet \nfor Interacting with PA in the Last 12 Months')+
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2),limits = c(2010,2021),expand = c(0,0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 2)

ggplot(Cindia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  geom_line() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Coeffcient") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet \nfor Interacting with PA in the Last 12 Months')+
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2),limits = c(2010,2021),expand = c(0,0)) +
  theme_bw()+
  theme(axis.text.x = element_text(size=9,angle=35)) +
  facet_wrap_paginate(~ Country, ncol = 4,nrow = 4, page = 3)




#geom line
ggplot(Cindia, aes(y = Coefficient, x = Year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0,100),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2020, 2),limits = c(2011,2020)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35),
        axis.text.y = element_text(size=8,))

#-------------------------------------------------------------------------------
#REGIONS PART
#Level of internet access among regions of Italy
Rhia1<-read_tsv('region_households_with_internet_access.tsv', na=':')
Rhia1<-Rhia1[ ,-c(2,3)]
Chli2<-read_tsv('country_Households_level_of_internet_access.tsv', na = c(': u', ':'))
EUmeani<-Chli2[-c(1:10,12:43), -11]

EUmeani$`2014`<-as.double(EUmeani$'2014')
EUmeani$`2016`<-as.double(EUmeani$'2016')
EUmeani$`2017`<-as.double(EUmeani$'2017')
EUmeani$`2018`<-as.double(EUmeani$'2018')
EUmeani$`2019`<-as.double(EUmeani$'2019')

Rhia<-bind_rows(Rhia1,EUmeani)
Rhia<-Rhia[ ,-11]

Rhia<-Rhia %>% 
  pivot_longer(c(`2011`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(Rhia)[1] <-'Country'
Rhia$Year<-as.numeric(Rhia$Year)

Rhia$Country[1:9]<-'Piemonte'
Rhia$Country[10:18]<-"Valle d'Aosta"
Rhia$Country[19:27]<-'Liguria'
Rhia$Country[28:36]<-'Lombardia'
Rhia$Country[37:45]<-'Abruzzo'
Rhia$Country[46:54]<-'Molise'
Rhia$Country[55:63]<-'Campania'
Rhia$Country[64:72]<-'Puglia'
Rhia$Country[73:81]<-'Basilicata'
Rhia$Country[82:90]<-'Calabria'
Rhia$Country[91:99]<-'Sicilia'
Rhia$Country[100:108]<-'Sardegna'
Rhia$Country[109:117]<-'Aut. Prov. Bolzano'
Rhia$Country[118:126]<-'Aut. Prov. Trento'
Rhia$Country[127:135]<-'Veneto'
Rhia$Country[136:144]<-'Friuli-Venezia Giulia'
Rhia$Country[145:153]<-'Emilia-Romagna'
Rhia$Country[154:162]<-'Toscana'
Rhia$Country[163:171]<-'Umbria'
Rhia$Country[172:180]<-'Marche'
Rhia$Country[181:189]<-'Lazio'
Rhia$Country[190:198]<-'Euro Area'

Rhia<-drop_na(Rhia)

#regional plots
ggplot(Rhia, aes(x=Year, y=Coefficient, colour=Country)) + 
  geom_point() +
  scale_colour_manual(values=CaSus)+
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(20,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits=c(2011,2020), expand=c(0,0)) +
  ylab("Level of Households with Internet Connection - Italy") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()


ggplot(Rhia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col= Country)) +
  scale_color_manual(values = CaSus) +
  ylab("Level of Households with Internet Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(20,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits=c(2011,2020), expand=c(0,0)) +
  theme_dark()

#facets 

ggplot(Rhia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Internet Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(20,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2019, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))
  


ggplot(Rhia, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Internet Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(20,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2019, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))
       

#BOXPLOTS(REGION INTERNET LEVEL)

#First box plot plotted without EU mean, tidying
Rhia1<-Rhia1 %>% 
  pivot_longer(c(`2011`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(Rhia1)[1] <-'Country'

Rhia1$Country[1:9]<-'Piemonte'
Rhia1$Country[10:18]<-"Valle d'Aosta"
Rhia1$Country[19:27]<-'Liguria'
Rhia1$Country[28:36]<-'Lombardia'
Rhia1$Country[37:45]<-'Abruzzo'
Rhia1$Country[46:54]<-'Molise'
Rhia1$Country[55:63]<-'Campania'
Rhia1$Country[64:72]<-'Puglia'
Rhia1$Country[73:81]<-'Basilicata'
Rhia1$Country[82:90]<-'Calabria'
Rhia1$Country[91:99]<-'Sicilia'
Rhia1$Country[100:108]<-'Sardegna'
Rhia1$Country[109:117]<-'Aut. Prov. Bolzano'
Rhia1$Country[118:126]<-'Aut. Prov. Trento'
Rhia1$Country[127:135]<-'Veneto'
Rhia1$Country[136:144]<-'Friuli-Venezia Giulia'
Rhia1$Country[145:153]<-'Emilia-Romagna'
Rhia1$Country[154:162]<-'Toscana'
Rhia1$Country[163:171]<-'Umbria'
Rhia1$Country[172:180]<-'Marche'
Rhia1$Country[181:189]<-'Lazio'
Rhia1<-drop_na(Rhia1)

#boxplot with ggplot (general, increase IT levels in countries over time)

IT_IH_mean_develop<-ggplot(Rhia1, aes(Coefficient,Year),group=Country)+
  geom_boxplot() +
  scale_y_discrete(breaks=seq(2011,2019, by=1))+
  scale_x_continuous(breaks = seq(10,100,by=2))+
  xlab("Level of Households with Internet Connection - Italy") +
  ggtitle('Distributions of connections in Italy by year (quartiles)')+
  theme_bw()
IT_IH_mean_develop

#second boxplot, plotted with EU MEAN
#TO PLOT 'development' BOXPLOT, DATASET MUST HAVE NA VALUES REMOVED BEFORE PIVOTING
#here they were kept since missing values don't change much
Rhia$Year<-as.character(Rhia$Year) #needed to plot boxplots
#reorder dataset to plot boxplot
Rhia$country_sort <- with(Rhia, reorder(Country, Coefficient, function(x) median(x, na.rm = T)))

#Developement of internet connections over time (focusing on most rapid and effective countries)
IT_IH_countries_sort<-ggplot(Rhia, aes(Coefficient,country_sort))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(10,100, by=2 )) +
  theme_bw()
IT_IH_countries_sort + ggtitle('Developement of Internet Connections in Italy between 2009 and 2019')+
  ylab('Region') +
  xlab('Level of Households with Internet Connection - Italy')

#-------------------------------------------------------------------------------
#Broadband access among regions of italy
#household broadband access
Rhba1<-read_tsv('region_households_with_broadband_access.tsv', na=':')

Chlb2<-read_tsv('country_Households_with_broadband_access.tsv', na = c(': u', ':'))
EUmeanb<-Chlb2[-c(1:10,12:42), -13]

EUmeanb$`2014`<-as.double(EUmeanb$'2014')
EUmeanb$`2016`<-as.double(EUmeanb$'2016')
EUmeanb$`2017`<-as.double(EUmeanb$'2017')
EUmeanb$`2018`<-as.double(EUmeanb$'2018')
EUmeanb$`2019`<-as.double(EUmeanb$'2019')

Rhba<-bind_rows(Rhba1,EUmeanb)
Rhba<-Rhba[ ,-13]

Rhba<-Rhba %>% 
  pivot_longer(c(`2009`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(Rhba)[1] <-'Country'
Rhba$Year<-as.numeric(Rhba$Year)

Rhba$Country[1:11]<-'Piemonte'
Rhba$Country[12:22]<-"Valle d'Aosta"
Rhba$Country[23:33]<-'Liguria'
Rhba$Country[34:44]<-'Lombardia'
Rhba$Country[45:55]<-'Abruzzo'
Rhba$Country[56:66]<-'Molise'
Rhba$Country[67:77]<-'Campania'
Rhba$Country[78:88]<-'Puglia'
Rhba$Country[89:99]<-'Basilicata'
Rhba$Country[100:110]<-'Calabria'
Rhba$Country[111:121]<-'Sicilia'
Rhba$Country[122:132]<-'Sardegna'
Rhba$Country[133:143]<-'Aut. Prov. Bolzano'
Rhba$Country[144:154]<-'Aut. Prov. Trento'
Rhba$Country[155:165]<-'Veneto'
Rhba$Country[166:176]<-'Friuli-Venezia Giulia'
Rhba$Country[177:187]<-'Emilia-Romagna'
Rhba$Country[188:198]<-'Toscana'
Rhba$Country[199:209]<-'Umbria'
Rhba$Country[210:220]<-'Marche'
Rhba$Country[221:231]<-'Lazio'
Rhba$Country[232:242]<-'Euro Area'

Rhba<-drop_na(Rhba)

#regional plots
ggplot(Rhba, aes(x=Year, y=Coefficient, colour=Country)) + #need to set color palette
  geom_point() +
  scale_colour_manual(values=CaSus) +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(20,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2009,2019, by=1), limits=c(2009,2020), expand=c(0,0)) +
  ylab("Level of Households with Broadband Connection - Italy") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()


ggplot(Rhba, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col= Country)) +
  scale_colour_manual(values=CaSus) +
  ylab("Level of Households with Broadband Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(10,100, by=2), limits=c(20,100),expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2009,2019, by=1), limits=c(2009,2020), expand=c(0,0)) +
  theme_dark()

#facets 

ggplot(Rhba, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Broadband Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(20,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))
       

#need to adjust labels)
ggplot(Rhba, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Level of Households with Broadband Connection - Italy") +
  xlab("Year") +
  scale_y_continuous(breaks = seq(0,100, by=20 ), limits = c(20,110), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2009, 2019, by = 2)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35))
        
  

#BOXPLOTS(REGION BROADBAND)

#First box plot plotted without EU mean, tidying
Rhba1<-Rhba1 %>% 
  pivot_longer(c(`2009`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(Rhba1)[1] <-'Country'


Rhba1$Country[1:11]<-'Piemonte'
Rhba1$Country[12:22]<-"Valle d'Aosta"
Rhba1$Country[23:33]<-'Liguria'
Rhba1$Country[34:44]<-'Lombardia'
Rhba1$Country[45:55]<-'Abruzzo'
Rhba1$Country[56:66]<-'Molise'
Rhba1$Country[67:77]<-'Campania'
Rhba1$Country[78:88]<-'Puglia'
Rhba1$Country[89:99]<-'Basilicata'
Rhba1$Country[100:110]<-'Calabria'
Rhba1$Country[111:121]<-'Sicilia'
Rhba1$Country[122:132]<-'Sardegna'
Rhba1$Country[133:143]<-'Aut. Prov. Bolzano'
Rhba1$Country[144:154]<-'Aut. Prov. Trento'
Rhba1$Country[155:165]<-'Veneto'
Rhba1$Country[166:176]<-'Friuli-Venezia Giulia'
Rhba1$Country[177:187]<-'Emilia-Romagna'
Rhba1$Country[188:198]<-'Toscana'
Rhba1$Country[199:209]<-'Umbria'
Rhba1$Country[210:220]<-'Marche'
Rhba1$Country[221:231]<-'Lazio'

Rhba1<-drop_na(Rhba1)

#boxplot with ggplot (general, increase IT levels in countries over time)

IT_BH_mean_develop<-ggplot(Rhba1, aes(Coefficient,Year),group=Country)+
  geom_boxplot() +
  scale_y_discrete(breaks=seq(2009,2019, by=1))+
  scale_x_continuous(breaks = seq(10,100,by=2))+
  xlab("Level of Households with Broadband Connection - Italy") +
  ggtitle('Distributions of Broadband connections in Italy by year (quartiles)')+
  theme_bw()
IT_BH_mean_develop

#second boxplot, plotted with EU MEAN
#TO PLOT 'development' BOXPLOT, DATASET MUST HAVE NA VALUES REMOVED BEFORE PIVOTING
Rhba<-Rhba[-c(177:184,207:214), ]
Rhba$Year<-as.character(Rhba$Year) #needed to plot boxplots
#reorder dataset to plot boxplot
Rhba$country_sort <- with(Rhba, reorder(Country, Coefficient, function(x) median(x, na.rm = T)))

#Developement of internet connections over time (focusing on most rapid and effective countries)
IT_BH_countries_sort<-ggplot(Rhba, aes(Coefficient,country_sort))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(10,100, by=2 )) +
  theme_bw()
IT_BH_countries_sort + ggtitle('Developement of Broadband connections in Italy between 2009 and 2019')+
  ylab('Region') +
  xlab('Level of Households with Broadband Connection - Italy')

#-------------------------------------------------------------------------------
##INTERNET USAGE
#Interaction with public authorities
#percentage of individuals using internet for interacting with public authorities 
#in the last 12 months

RIiu1<-read_tsv('region_Individuals_internet_interacting.tsv', na=':')
Cindia2<-read_tsv('country_Individuals_internet_interacting_with_authorities.tsv', na = ':')
CindiaEUmean<-Cindia2[-c(1:10,12:43),]

CindiaEUmean$'2020'=gsub( "e", "", as.character(CindiaEUmean$'2020'))
CindiaEUmean$`2014`<-as.double(CindiaEUmean$'2014')
CindiaEUmean$`2016`<-as.double(CindiaEUmean$'2016')
CindiaEUmean$`2017`<-as.double(CindiaEUmean$'2017')
CindiaEUmean$`2018`<-as.double(CindiaEUmean$'2018')
CindiaEUmean$`2019`<-as.double(CindiaEUmean$'2019')
CindiaEUmean$`2020`<-as.double(CindiaEUmean$'2020')

RIiu<-bind_rows(RIiu1,CindiaEUmean)
RIiu<-RIiu[ ,-c(11,12)]

RIiu<-RIiu %>% 
  pivot_longer(c(`2011`:`2019`), names_to ='Year', values_to = 'Coefficient')
names(RIiu)[1] <-'Country'
RIiu$Year<-as.numeric(RIiu$Year)

RIiu$Country[1:9]<-'Italy'
RIiu$Country[10:18]<-'Nord Ovest'
RIiu$Country[19:27]<-'Piemonte'
RIiu$Country[28:36]<-"Valle d'Aosta"
RIiu$Country[37:45]<-'Liguria'
RIiu$Country[46:54]<-'Lombardia'
RIiu$Country[55:63]<-'Sud'
RIiu$Country[64:72]<-'Abruzzo'
RIiu$Country[73:81]<-'Molise'
RIiu$Country[82:90]<-'Campania'
RIiu$Country[91:99]<-'Puglia'
RIiu$Country[100:108]<-'Basilicata'
RIiu$Country[109:117]<-'Calabria'
RIiu$Country[118:126]<-'Isole'
RIiu$Country[127:135]<-'Sicilia'
RIiu$Country[136:144]<-'Sardegna'
RIiu$Country[145:153]<-'Nord Est'
RIiu$Country[154:162]<-'Aut. Prov. Bolzano'
RIiu$Country[163:171]<-'Aut. Prov. Trento'
RIiu$Country[172:180]<-'Veneto'
RIiu$Country[181:189]<-'Friuli Venezia-Giulia'
RIiu$Country[190:198]<-'Emilia-Romagna'
RIiu$Country[199:207]<-'Centro'
RIiu$Country[208:216]<-'Toscana'
RIiu$Country[217:225]<-'Umbria'
RIiu$Country[226:234]<-'Marche'
RIiu$Country[235:243]<-'Lazio'
RIiu$Country[244:252]<-'Euro Area'

RIiu<-drop_na(RIiu)

#plots
ggplot(RIiu, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  scale_colour_manual(values = CaSus) +
  scale_y_continuous(breaks = seq(0,100, by=2 ),limits = c(0,60), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months - Italy") +
  xlab("Year") +
  geom_smooth(method = 'lm', se=F) +
  theme_dark()
#geomline
ggplot(RIiu, aes(y = Coefficient, x = Year, group = Country)) +
  geom_line(aes(col = Country)) +
  geom_point(aes(col=Country)) +
  scale_colour_manual(values=CaSus) +
  labs(colour='Regions of Italy')+
  ylab("Percentage of Individuals") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months \n - Regions of Italy') +
  scale_y_continuous(breaks = seq(0,100, by=2 ), limits=c(0,60), expand=c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2019, by = 1), limits = c(2011,2020), expand=c(0,0)) +
  theme_dark() +
  theme(axis.title.y = element_text(margin=margin(t = 0, r = 0.5, b = 0, l = 0,'cm')))

#facets
ggplot(RIiu, aes(y = Coefficient, x = Year, group = Country)) +
  geom_smooth(method = 'lm', se=F) +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Percentage of Individuals") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months \n - Regions of Italy') +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0,60),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2019, 2),limits = c(2011,2019)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35),
        axis.title.y = 
          element_text(margin=margin(t = 0, r = 0.5, b = 0, l = 0,'cm')))
       

#geom line
ggplot(RIiu, aes(y = Coefficient, x = Year)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Country, ncol = 5) +
  ylab("Percentage of Individuals") +
  xlab("Year") +
  ggtitle('Percentage of Individuals who Used the Internet for Interacting with PA in the Last 12 Months 
          \n - Regions of Italy') +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0,60),expand = c(0,0)) +
  scale_x_continuous(breaks = seq(2011, 2019, 2),limits = c(2011,2019)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=9,angle=35),
        plot.margin = margin(0.5,0.5,0.5,0.5,'cm'),
        axis.title.y = element_text(margin=margin(t = 0, r = 0.5, b = 0, l = 0,'cm')))
  



#-------------------------------------------------------------------------------
#ITALY DETAILS
#RUN AFTER THE REST OF THE SCRIPTS

#broadband access (national)
ITbb<-Chlb[c(71:80,168:176),-4]
ITbb$Year<-as.numeric(ITbb$Year)

ggplot(ITbb, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  geom_line()+
  scale_color_manual(values = c('#00A86B','dark green')) +
  scale_y_continuous(breaks = seq(0,100, by=4 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Households with Broadband Access") +
  xlab("Year") +
  ggtitle('Comparison between European and Italian Trends: \nBroadband Access Level')+
  theme_bw()

lm(ITbb$Coefficient~ITbb$Year)


#geomline
ggplot(ITbb, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c('#00A86B','dark green')) +
  scale_y_continuous(breaks = seq(0,100, by=5 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Households") +
  xlab("Year") +
  ggtitle('Percentage of Households with Broadband Connection - Italy')+
  theme_bw()

#percentage of people not having connections due to cost too high
ITrnhi <-Crnhi[c(68:76,154:161), ]
ITrnhi$Year<-as.numeric(ITrnhi$Year)

ggplot(ITrnhi, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  geom_line()+
  scale_color_manual(values = c('#00A86B','dark green')) +
  scale_y_continuous(breaks = seq(0,100, by=4 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Individuals without Internet Access") +
  xlab("Year") +
  ggtitle('Comparison between European and Italian Trends: \nNot having Internet at Home Because Access Costs are too High')+
  theme_bw()

ggplot(ITrnhi, aes(x=Year, y=Coefficient)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,100, by=2 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2010,2019, by=1), limits = c(2010,2020),expand=c(0,0)) +
  ylab("Percentage of Households") +
  xlab("Year") +
  ggtitle('Percentage of Households with no Internet Connection because Cost is too high \n- Italy')+
  geom_line(colour='dark green') +
  theme_bw()

#percentage of interactions with pa
ITindia<-Cindia[c(79:88,176:184), ]
ITindia$Year<-as.numeric(ITindia$Year)

ggplot(ITindia, aes(x=Year, y=Coefficient, colour=Country)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  geom_line()+
  scale_color_manual(values = c('#00A86B','dark green')) +
  scale_y_continuous(breaks = seq(0,100, by=4 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Households with Broadband Access") +
  xlab("Year") +
  ggtitle('Comparison between European and Italian Trends: \nBroadband Access Level')+
  theme_bw()

ggplot(ITindia, aes(x=Year, y=Coefficient)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,100, by=2 ),limits = c(0,100), expand = c(0,0)) +
  scale_x_continuous(breaks=seq(2011,2019, by=1), limits = c(2011,2020),expand=c(0,0)) +
  ylab("Percentage of Individuals") +
  xlab("Year") +
  ggtitle('Percentage of Individuals Using Internet for Interaction with P.A.(Last 12 Months) \n - Italy')+
  geom_line(colour='dark green') +
  theme_bw()

