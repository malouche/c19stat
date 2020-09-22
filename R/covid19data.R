
#' Extract COVID-19 data
#'
#' This function extract the COVID-19 from the JHU github account.
#' The dowloaded data contains 5 variables : data, iso3c code, confirmed, deaths and recovered
#' You can choose the set of coutries and the range of dates
#'
#'
#' @param  country  is the iso3c of the countries, byu default it's "all". This variable is a vector
#' @param  start is the starting date. By defaut it's equal to "2020-01-02"
#' @param  end is the ending data
#' @return A dataframe class with 5 variables
#' @author Dhafer Malouche
#' @example dt=covid19data(country = "TUN",start = "2020-04-01",end = "2020-06-25")
#' @import  dplyr
#' @export


covid19data<-function(country="all",start="2020-01-01",end="2020-09-22"){

  xs=as.Date(start)
  xe=as.Date(end)
  if(xe<xs) stop("Start date should be previous to End date")


  dcov_conf=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  dcov_deaths=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  dcov_recovered=read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


  dcov_conf=dcov_conf%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))

  dcov_conf=reshape2::melt(dcov_conf,id.vars=1)

  dcov_conf=dcov_conf%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c",warn = F))%>%na.omit()


  dcov_conf=dcov_conf%>%mutate(date=gsub(pattern = "X",replacement = "0",variable))%>%mutate(date=as.Date(gsub(pattern = "\\.",replacement = "-",date),format = "%m-%d-%y"))%>%dplyr::select(-variable)

  dcov_conf=dcov_conf%>%rename(confirmed=value)


  dcov_deaths=dcov_deaths%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))

  dcov_deaths=reshape2::melt(dcov_deaths,id.vars=1)

  dcov_deaths=dcov_deaths%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c",warn = F))%>%na.omit()


  dcov_deaths=dcov_deaths%>%mutate(date=gsub(pattern = "X",replacement = "0",variable))%>%mutate(date=as.Date(gsub(pattern = "\\.",replacement = "-",date),format = "%m-%d-%y"))%>%dplyr::select(-variable)

  dcov_deaths=dcov_deaths%>%rename(deaths=value)



  dcov_recovered=dcov_recovered%>%dplyr::select(-c(Province.State,Lat,Long))%>%group_by(Country.Region)%>%summarise_all(funs(sum))

  dcov_recovered=reshape2::melt(dcov_recovered,id.vars=1)

  dcov_recovered=dcov_recovered%>%mutate(iso3c=countrycode::countrycode(Country.Region,origin = "country.name",destination = "iso3c",warn = F))%>%na.omit()


  dcov_recovered=dcov_recovered%>%mutate(date=gsub(pattern = "X",replacement = "0",variable))%>%mutate(date=as.Date(gsub(pattern = "\\.",replacement = "-",date),format = "%m-%d-%y"))%>%dplyr::select(-variable)

  dcov_recovered=dcov_recovered%>%rename(recovered=value)


  dcov_conf=dcov_conf%>%dplyr::select(date,iso3c,confirmed)
  dcov_deaths=dcov_deaths%>%dplyr::select(date,iso3c,deaths)
  dcov_recovered=dcov_recovered%>%dplyr::select(date,iso3c,recovered)

  dcov_conf=dcov_conf%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)
  dcov_deaths=dcov_deaths%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)
  dcov_recovered=dcov_recovered%>%group_by(iso3c)%>%arrange(date, .by_group = TRUE)

  dcov1=dcov_conf%>%left_join(dcov_deaths,by=c("date","iso3c"))

  dcov1=dcov1%>%left_join(dcov_recovered,by=c("date","iso3c"))

  dcov1=dcov1%>%filter(confirmed>0)


  z=sapply(country,function(z) grep(z,dcov1$iso3c))
  z=unlist(lapply(z,length))

  if(sum(z==0)>=1) stop("Check your country codes")

  if(length(z))
  if('all'%in% country) dcov_final=dcov1
    else dcov_final=dcov1%>%filter(iso3c%in%country)

  dcov_final=dcov_final%>%filter(date>=xs & date<=xe)
      return(dcov_final)
}
