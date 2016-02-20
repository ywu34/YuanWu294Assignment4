#Econ 294
#Assignment 4

#0
print("First Name: Yuan")
print("Last Name: Wu")
print("Student ID: 1307193")

#1
library(foreign)
airports<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/airports.csv", stringsAsFactors = F)
flights<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/flights.csv", stringsAsFactors = F)
planes<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/planes.csv", stringsAsFactors = F)
weather<-read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/weather.csv", stringsAsFactors = F)

#2
weather$date<-as.Date(weather$date)
flights$date<-as.Date(flights$date)

#3
flights.2a<-subset(flights, dest=="OAK"|dest=="SFO")
print(paste("There are", nrow(flights.2a) ,"observations in flights.2a"))
flights.2b<-subset(flights, dep_delay>=1|arr_delay>=1)
print(paste("There are", nrow(flights.2b) ,"observations in flights.2a"))
flights.2c<-subset(flights, arr_delay>2*dep_delay&arr_delay>0&dep_delay>0)
print(paste("There are", nrow(flights.2c) ,"observations in flights.2a"))

#4
library(dplyr)
method1<-select(flights, arr_delay, dep_delay)
method2<-select(flights, contains("delay"))
method3<-select(flights, ends_with("delay"))

#5
dep_delay_desc<-arrange(flights, desc(dep_delay))
print(head(dep_delay_desc, n=5))
caught_up_desc<-arrange(flights, desc(arr_delay-dep_delay))
print(head(caught_up_desc, n=5))

#6
flights<- mutate(flights,
  speed = dist / (time / 60), 
  delta = arr_delay - dep_delay
  ) 
print(flights %>% arrange(desc(speed)) %>% select(plane, speed) %>% head(5))
print(flights %>% arrange(desc(delta)) %>% select(plane, delta) %>% head(5))
print(flights %>% arrange(delta) %>% select(plane, delta) %>% head(5))

#7
flights.7a<-flights %>%
  group_by(carrier) %>% 
  summarise(
    cancel = sum(cancelled),
    total = n(),
    percentage=cancel*100/total,
    deltamin = min(delta, na.rm = T),
    delta1stq = quantile(delta, 0.25, na.rm = T),
    deltamed = median(delta, na.rm = T),
    deltamean = mean(delta, na.rm = T),
    delta3rdq = quantile(delta, 0.75, na.rm = T),
    delta90q = quantile(delta, 0.9, na.rm = T),
    deltamax = max(delta, na.rm = T)
) %>% arrange(desc(percentage))
print(flights.7a)

day_delay<- dplyr::filter(
  summarise(
    group_by(
      dplyr::filter(
        flights,
        !is.na(dep_delay)
      ),
      date),
    delay=mean(dep_delay),
    n=n()
),
n>10
)
print("filter out rows that are not empty and count out the number of the rows that is left, grouped by dates. 
      Only the rows that are left with more than 10 rows will be displayed.
      also display the mean of departure delay in each date.")

day_delay.b<- flights %>% 
  dplyr::filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarise(
    delay=mean(dep_delay),
    n=n()
  ) %>%
  dplyr::filter(n>10) 

#8
day_delay$lag_delay<- dplyr::lag(day_delay$delay, n=1L)
day_delay$dif_delay<- day_delay$delay - day_delay$lag_delay
day_delay<-na.omit(day_delay)
day_delay<-dplyr::arrange(day_delay, desc(dif_delay))
print(head(day_delay,5))

#9
dest_delay<- flights %>%
  group_by(dest) %>%
  summarise(
    arr_delay=mean(arr_delay, na.rm=T),
    n=n()
  )

airports.9<- airports %>% 
  select(iata, airport, city, state, lat, long)
colnames(airports.9)[1]<-"dest"
colnames(airports.9)[2]<-"name"

df.9a<-dest_delay %>% 
  left_join(airports.9) %>%
  arrange(desc(arr_delay)) %>%
  select(city,state,arr_delay)
print(head(df.9a,5))

df.9b<-dest_delay %>%
  inner_join(airports.9)
#Not the same number of obs as in left_join. It is 2 observations shorter.

df.9c<-dest_delay %>%
  right_join(airports.9)
#There are 3376 observations. All mean and n are NA because join sequence where there is no match for the join. 

df.9d<-dest_delay %>%
  full_join(airports.9)
#There are 3378 observations and contains NA from all variables. 

#10
library(tidyr)
flightsdate<-flights %>%
  separate(date,c("date","midnight"),sep=" ") %>%
flightsdate$time<-as.character(paste(flightsdate$date,flightsdate$hour, sep="-"))

hourly_delay.10<-flightsdate%>%
  filter(!is.na(dep_delay))%>%
  group_by(time)%>%
  summarise(
    hourly_delay=mean(dep_delay,na.rm=T),
    n=n())

weather$time<-as.character(paste(weather$date,weather$hour, sep="-"))

df.10<-hourly_delay.10%>% 
  dplyr::left_join(weather, by = c("time") ) 

df.10.2<-df.10%>%
  arrange(desc(hourly_delay))%>% 
  select(hourly_delay,conditions)%>%  
  head(n=5)

print(df.10.2)

#11
df<-data.frame(treament=c("a","b"),subject1=c(3,4),subject2=c(5,6))
print(df)

df.11a<-df %>%
  gather(df, treatment, subject1, subject2, na.rm = T) %>%
  separate(df,c("discard","subject"),sep="t")%>%
  select(subject,treatment=treament,value=treatment) %>%
  arrange(subject)

print(df.11a)

df<- data.frame(
  subject=c(1,1,2,2),
  treatment=c("a","b","a","b"),
  value=c(3,4,5,6)
  
)
print(df)

df.11b<-df %>%
  spread(key=subject,value=value) %>%
  rename(subject1=`1`,subject2=`2`)

print(df.11b)

df<- data.frame(
  subject=c(1,2,3,4),
  demo=c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value=c(3,4,5,6)
)
print(df)

df.11c<-df %>%
  separate(demo,c("sex","age","state"),sep="_")
print(df.11c)

df<-data.frame(
  subject=c(1,2,3,4),
  sex=c("f","f","m",NA),
  age=c(11,55,65,NA),
  city=c("DC","NY","WA",NA),
  value=c(3,4,5,6)
)
print(df)

df.11d<- df%>%
  unite(demo,...=sex, age, city,sep=".")
df.11d[df.11d=="NA.NA.NA"]<-NA
print(df.11d)