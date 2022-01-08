library(dplyr)
library(xml2)
library(httr)
library(sf)

# PTX api (copy from TDX website)
# https://github.com/ptxmotc/Sample-code/blob/master/R/get_ptx_data.R
get_ptx_data <- function (app_id, app_key, url){
  Sys.setlocale("LC_ALL","C")
  xdate <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%a, %d %b %Y %H:%M:%S GMT")
  sig <- hmac_sha1(app_key, paste("x-date:", xdate))

  authorization <- paste0(
    'hmac username="', app_id, '", ',
    'algorithm="hmac-sha1", ',
    'headers="x-date", ',
    'signature="', sig, '/"', sep = '')

  auth_header <- c(
    'Authorization'= authorization,
    'x-date'= as.character(xdate))

  dat <- GET(url,
             config = httr::config(ssl_verifypeer = 0L),
             add_headers(.headers = auth_header))

  print(http_status(dat)$message)
  Sys.setlocale(category = "LC_ALL", locale = "cht")
  return(content(dat))
}


Bus_StopOfRoute=function(app_id, app_key, county){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (county=="Intercity"){
    url="https://ptx.transportdata.tw/MOTC/v2/Bus/StopOfRoute/InterCity?&$format=XML"
  }else{
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Bus/StopOfRoute/City/", county, "?&$format=XML")
  }
  x=get_ptx_data(app_id, app_key, url)

  if (substr(xml_text(x), 1, 4)=="City"){
    print(paste0("City: '", county, "' is not accepted but Taipei, NewTaipei, Taoyuan, Taichung, Tainan, Kaohsiung, Keelung, Hsinchu, HsinchuCounty, MiaoliCounty, ChanghuaCounty, NantouCounty, YunlinCounty, ChiayiCounty, Chiayi, PingtungCounty, YilanCounty, HualienCounty, TaitungCounty, KinmenCounty, PenghuCounty, LienchiangCounty"))
  }else{
    bus_info=data.frame(RouteUID=xml_text(xml_find_all(x, xpath = ".//d1:RouteUID")),
                        RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName//d1:Zh_tw")),
                        SubRouteUID=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteUID")),
                        SubRouteName=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteName//d1:Zh_tw")),
                        Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")))

    num_of_route=xml_length(xml_find_all(x, xpath = ".//d1:BusStopOfRoute//d1:Stops"))

    print(paste0(length(num_of_route), " Routes"))

    if (county %in% c("Keelung","LienchiangCounty")){
      bus_stop_temp=data.frame(StopUID=xml_text(xml_find_all(x, xpath = ".//d1:StopUID")),
                               StopID=xml_text(xml_find_all(x, xpath = ".//d1:StopID")),
                               StopName=xml_text(xml_find_all(x, xpath = ".//d1:StopName//d1:Zh_tw")),
                               # StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                               StopSequence=xml_text(xml_find_all(x, xpath = ".//d1:StopSequence")),
                               PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                               PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")))
    }else{
      bus_stop_temp=data.frame(StopUID=xml_text(xml_find_all(x, xpath = ".//d1:StopUID")),
                               StopID=xml_text(xml_find_all(x, xpath = ".//d1:StopID")),
                               StopName=xml_text(xml_find_all(x, xpath = ".//d1:StopName//d1:Zh_tw")),
                               StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                               StopSequence=xml_text(xml_find_all(x, xpath = ".//d1:StopSequence")),
                               PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                               PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")))
    }

    bus_stop=data.frame()
    for (i in c(1:length(num_of_route))){
      sec_head=sum(num_of_route[0:(i-1)])+1
      sec_tail=sum(num_of_route[0:i])
      bus_stop=rbind(bus_stop, cbind(bus_info[i,], bus_stop_temp[c(sec_head:sec_tail),]))
      print(if((i %% 10==0 | i==length(num_of_route))){
        paste0(i, "/", length(num_of_route))
      }else{next})
    }
    rm(bus_info, bus_stop_temp, sec_head, sec_tail, num_of_route)

    print(paste0("#---", county, " Stop of Route Downloaded---#"))
    return(bus_stop)
  }
}


Bus_Shape=function(app_id, app_key, county){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (county=="Intercity"){
    url="https://ptx.transportdata.tw/MOTC/v2/Bus/Shape/InterCity?&$format=XML"
  }else{
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Bus/Shape/City/", county, "?&$format=XML")
  }
  x = get_ptx_data(app_id, app_key, url)

  bus_shape=data.frame(RouteUID=xml_text(xml_find_all(x, xpath = ".//d1:RouteUID")),
                       RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName")),
                       SubRouteUID=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteUID")),
                       SubRouteName=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteName")),
                       Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")),
                       Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))

  print(paste0("#---", county, " Bus Route Downloaded---#"))
  return(bus_shape)
}



