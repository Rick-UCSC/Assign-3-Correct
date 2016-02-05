##Rick Shemanski
##Assign 3


RickShemanskiAssignment3 <- list(
  first = "Rick",
  last = "Shemanski",
  email = "rshemans@ucsc.edu",
  studentid = 1504018
  
)

print(RickShemanskiAssignment3)


#########################################################################################

library(foreign)

df.ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

class(df.ex)

#################################################################
######                                 2
library(dplyr)

df.ex.2a <- df.ex %>%
  dplyr::filter(
    year == 2013 & month ==12
  )

##number of obs = number of rows
print(nrow(df.ex.2a))

##summer split
## filter is like subset
df.ex.2b <- dplyr:: filter(df.ex, (month == 7 | month == 8 | month == 9) & year == 2013)

#count summer
print(nrow(df.ex.2b))


########################################################
###                  3


##Arrange
## ascending is defualt
df.ex.3a <- dplyr::arrange(df.ex,year, month )

###################################################
##########            4

df.ex.4a <- dplyr::select(df.ex, year:age)

df.ex.4b <- dplyr::select(df.ex, year, month, starts_with("i"))
#View(df.ex.4b)

#print unique values
print(distinct(select(df.ex,state)))

#print(unique(df.ex$state))


###########################################
#########              5

stndz <- function(x){
  (x- mean(x,na.rm=T)) / sd(x,na.rm = T)
  
}

#example 
stndz(df.ex$age)

#my min max function 
nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x,na.rm=T)-min(x, na.rm = T))
  
}

###mutate adding 
dplyr::mutate

df.ex.5a <- df.ex %>%mutate(
  rw.stndz = stndz(rw),
  rw_nrmlz = nrmlz(rw) 
)


###different way to add columns to data set
## cbind but i figured out mutate

df.ex.5b <- df.ex%>% 
  group_by(year, month)%>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count = n()
    
  )


###########################################################
##                      6

df.ex.6 <- df.ex %>% 
  group_by(year,month,state)%>%
  summarise(
    min_rw = min(rw, na.rm=T),
    mean_rw = mean(rw, na.rm = T),
    median_rw = median(rw, na.rm=T),
    max_rw = max(rw, na.rm = T),
    firstq_rw = quantile(rw,0.25, na.rm=T),
    thirdq_rw = quantile(rw,0.75, na.rm=T),
    count = n()
  )

df.ex.6b<-filter(df.ex.6, mean_rw==max(df.ex.6$mean_rw))

print(df.ex.6b)
print("In December 2013, Washington D.C had the highest mean real wage of 40.6258")


##################################################################
#############        7

#extra credit
df.ex.7a<- df.ex
df.ex.7a$state_char <- as.character(df.ex.7a$state)

df.ex.7a <- dplyr::arrange(df.ex.7a,year, month,desc(state_char) )
