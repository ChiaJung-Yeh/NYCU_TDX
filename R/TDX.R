library(dplyr)
library(xml2)
library(httr)
library(sf)
library(urltools)

usethis::use_package("dplyr")
usethis::use_package("xml2")
usethis::use_package("httr")
usethis::use_package("sf")
usethis::use_package("urltools")

# TDX_County=read_xml("https://gist.motc.gov.tw/gist_api/V3/Map/Basic/City?$format=XML")
# TDX_County=data.frame(County=xml_text(xml_find_all(TDX_County, xpath = "//CityName")),
#                       Code=xml_text(xml_find_all(TDX_County, xpath = "//City")))
# TDX_County=rbind(TDX_County, cbind(County="公路客運", Code="Intercity"))
# usethis::use_data(TDX_County, overwrite=T)

# TDX_Railway=data.frame(Operator=c("臺鐵","高鐵","臺北捷運","高雄捷運","桃園捷運","新北捷運","臺中捷運","高雄輕軌"),
#                        Code=c("TRA","THSR","TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT"))
# usethis::use_data(TDX_Railway, overwrite=T)

# TDX_RoadClass=data.frame(RoadClassName=c("國道","省道快速公路","省道一般公路","以上全部"),
#                          RoadClass=c(0,1,3,"ALL"))
# usethis::use_data(TDX_Railway, overwrite=T)

# PTX api (copy from TDX website)
# https://github.com/ptxmotc/Sample-code/blob/master/R/get_ptx_data.R
.get_ptx_data <- function (app_id, app_key, url){
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



Bus_StopOfRoute=function(app_id, app_key, county, dtype="text", out=F){
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
  x=.get_ptx_data(app_id, app_key, url)

  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
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

    if (dtype=="text"){
      if (nchar(out)!=0 & out!=F){
        write.csv(bus_stop, out, row.names=F)
      }
      return(bus_stop)
    }else if (dtype=="sf"){
      bus_stop$Geometry=st_as_sfc(paste0("POINT(", bus_stop$PositionLon, " ", bus_stop$PositionLat, ")"))
      bus_stop=st_sf(bus_stop, crs=4326)

      if (grepl(".shp", out) & out!=F){
        write_sf(bus_stop, out, layer_options="ENCODING=UTF-8")
      }else if (!(grepl(".shp", out)) & out!=F){
        stop("The file name must contain '.shp'")
      }

      return(bus_stop)
    }else{
      stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
    }
  }
}



Bus_Shape=function(app_id, app_key, county, dtype="text", out=F){
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
  x=.get_ptx_data(app_id, app_key, url)

  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
  }else{
    bus_shape=data.frame(RouteUID=xml_text(xml_find_all(x, xpath = ".//d1:RouteUID")),
                         RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName//d1:Zh_tw")),
                         SubRouteUID=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteUID")),
                         SubRouteName=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteName//d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")),
                         Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))

    print(paste0("#---", county, " Bus Route Downloaded---#"))

    if (dtype=="text"){
      if (nchar(out)!=0 & out!=F){
        write.csv(bus_shape, out, row.names=F)
      }
      return(bus_shape)
    }else if (dtype=="sf"){
      bus_shape$Geometry=st_as_sfc(bus_shape$Geometry)
      bus_shape=st_sf(bus_shape, crs=4326)

      if (grepl(".shp", out) & out!=F){
        write_sf(bus_shape, out, layer_options="ENCODING=UTF-8")
      }else if (!(grepl(".shp", out)) & out!=F){
        stop("The file name must contain '.shp'")
      }

      return(bus_shape)
    }else{
      stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
    }
  }
}



Bus_Schedule=function(app_id, app_key, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (county=="Intercity"){
    url="https://ptx.transportdata.tw/MOTC/v2/Bus/Shape/InterCity?&$format=XML"
  }else{
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Bus/Schedule/City/", county, "?&$format=XML")
  }
  x=.get_ptx_data(app_id, app_key, url)

  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
  }else{
    bus_info=data.frame(RouteUID=xml_text(xml_find_all(x, xpath = ".//d1:RouteUID")),
                        RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName//d1:Zh_tw")),
                        SubRouteUID=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteUID")),
                        SubRouteName=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteName//d1:Zh_tw")),
                        Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")))

    # check whether the schedule is in timetable form or frequency form
    tt=as.character(xml_children(x))
    bus_info1=bus_info[grepl("Timetables", tt),]
    bus_info2=bus_info[grepl("Frequency", tt),]

    if (nrow(bus_info1)!=0){
      timetable=xml_length(xml_find_all(x, xpath = ".//d1:Timetables"))
      print(paste0("#---Timetables Downloading---#"))
      print(paste0(length(timetable), " Routes"))
      bus_schedule_1=data.frame(TripID=xml_text(xml_find_all(x, xpath = ".//d1:TripID")),
                                Sunday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Sunday")),
                                Monday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Monday")),
                                Tuesday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Tuesday")),
                                Wednesday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Wednesday")),
                                Thursday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Thursday")),
                                Friday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Friday")),
                                Saturday=xml_text(xml_find_all(x, xpath = ".//d1:Timetable//d1:ServiceDay//d1:Saturday")),
                                StopSequence=xml_text(xml_find_all(x, xpath = ".//d1:StopSequence")),
                                StopUID=xml_text(xml_find_all(x, xpath = ".//d1:StopTimes//d1:StopTime//d1:StopUID")),
                                StopName=xml_text(xml_find_all(x, xpath = ".//d1:StopTimes//d1:StopTime//d1:StopName//d1:Zh_tw")),
                                ArrivalTime=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalTime")),
                                DepartureTime=xml_text(xml_find_all(x, xpath = ".//d1:DepartureTime")))

      bus_schedule_timetable=data.frame()
      for (i in c(1:length(timetable))){
        sec_head=sum(timetable[0:(i-1)])+1
        sec_tail=sum(timetable[0:i])
        bus_schedule_timetable=rbind(bus_schedule_timetable, cbind(bus_info1[i,], bus_schedule_1[c(sec_head:sec_tail),]))
        print(if((i %% 10==0 | i==length(timetable))){
          paste0(i, "/", length(timetable))
        }else{next})
      }
    }

    if (nrow(bus_info2)!=0){
      freq=xml_length(xml_find_all(x, xpath = ".//d1:Frequencys"))
      print(paste0("#---Frequencys Downloading---#"))
      print(paste0(length(freq), " Routes"))
      bus_schedule_2=data.frame(StartTime=xml_text(xml_find_all(x, xpath = ".//d1:StartTime")),
                                EndTime=xml_text(xml_find_all(x, xpath = ".//d1:EndTime")),
                                MinHeadwayMins=xml_text(xml_find_all(x, xpath = ".//d1:MinHeadwayMins")),
                                MaxHeadwayMins=xml_text(xml_find_all(x, xpath = ".//d1:MaxHeadwayMins")),
                                Sunday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Sunday")),
                                Monday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Monday")),
                                Tuesday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Tuesday")),
                                Wednesday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Wednesday")),
                                Thursday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Thursday")),
                                Friday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Friday")),
                                Saturday=xml_text(xml_find_all(x, xpath = ".//d1:Frequency//d1:ServiceDay//d1:Saturday")))

      bus_schedule_frequency=data.frame()
      for (i in c(1:length(freq))){
        sec_head=sum(freq[0:(i-1)])+1
        sec_tail=sum(freq[0:i])
        bus_schedule_frequency=rbind(bus_schedule_frequency, cbind(bus_info2[i,], bus_schedule_2[c(sec_head:sec_tail),]))
        print(if((i %% 10==0 | i==length(freq))){
          paste0(i, "/", length(freq))
        }else{next})
      }
    }
  }

  print(paste0("#---", county, " Bus Schedule Downloaded---#"))
  if (nrow(bus_info1)!=0 & nrow(bus_info2)!=0){
    bus_schedule=bind_rows(bus_schedule_timetable, bus_schedule_frequency)
  }else if (nrow(bus_info1)!=0){
    bus_schedule=bus_schedule_timetable
  }else if (nrow(bus_info2)!=0){
    bus_schedule=bus_schedule_frequency
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(bus_schedule, out, row.names=F)
  }
  return(bus_schedule)
}



Rail_StationOfLine=function(app_id, app_key, operator, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (operator=="TRA"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/TRA/StationOfLine?&%24format=XML"
  }else if (operator=="THSR"){
    stop("Please use function 'Rail_Station' to retrieve the station of high speed rail (THSR).")
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/StationOfLine/", operator, "?&%24format=XML")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
  }

  x=.get_ptx_data(app_id, app_key, url)

  rail_line=data.frame(LineID=xml_text(xml_find_all(x, xpath = ".//d1:LineID")))

  num_of_station=xml_length(xml_find_all(x, xpath = ".//d1:Stations"))

  if (operator=="TRA"){
    rail_station_temp=data.frame(Sequence=xml_text(xml_find_all(x, xpath = ".//d1:Sequence")),
                                 StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                                 StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName")),
                                 TraveledDistance=xml_text(xml_find_all(x, xpath = ".//d1:TraveledDistance")))
  }else{
    rail_station_temp=data.frame(Sequence=xml_text(xml_find_all(x, xpath = ".//d1:Sequence")),
                                 StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                                 StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName")))
  }

  rail_station_line=data.frame()
  for (i in c(1:length(num_of_station))){
    sec_head=sum(num_of_station[0:(i-1)])+1
    sec_tail=sum(num_of_station[0:i])
    rail_station_line=rbind(rail_station_line, cbind(LineID=rail_line[i,], rail_station_temp[c(sec_head:sec_tail),]))
  }

  if (operator=="TRA"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/TRA/Line?&%24format=XML"
    x=.get_ptx_data(app_id, app_key, url)
    rail_line=data.frame(LineID=xml_text(xml_find_all(x, xpath = ".//d1:LineID")),
                         LineName=xml_text(xml_find_all(x, xpath = ".//d1:LineNameZh")),
                         LineSectionName=xml_text(xml_find_all(x, xpath = ".//d1:LineSectionNameZh")))
    rail_station_line=left_join(rail_station_line, rail_line)%>%
      select(LineID, LineName, LineSectionName, Sequence, StationID, StationName, TraveledDistance)
  }else{
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/Line/", operator,"?&%24format=XML")
    x=.get_ptx_data(app_id, app_key, url)
    rail_line=data.frame(LineID=xml_text(xml_find_all(x, xpath = ".//d1:LineID")),
                         LineName=xml_text(xml_find_all(x, xpath = ".//d1:LineName//d1:Zh_tw")))
    rail_station_line=left_join(rail_station_line, rail_line)%>%
      select(LineID, LineName, Sequence, StationID, StationName)
  }

  print(paste0("#---", operator, " Station of Line Downloaded---#"))

  if (nchar(out)!=0 & out!=F){
    write.csv(rail_station_line, out, row.names=F)
  }
  return(rail_station_line)
}



Rail_Station=function(app_id, app_key, operator, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (operator=="TRA"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/TRA/Station?&%24format=XML"
  }else if (operator=="THSR"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/Station?&%24format=XML"
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/Station/", operator, "?&%24format=XML")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above."))
  }

  x=.get_ptx_data(app_id, app_key, url)

  if (operator=="TRA"){
    rail_station=data.frame(StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName//d1:Zh_tw")),
                            StationUID=xml_text(xml_find_all(x, xpath = ".//d1:StationUID")),
                            StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                            LocationCity=xml_text(xml_find_all(x, xpath = ".//d1:LocationCity")),
                            LocationTown=xml_text(xml_find_all(x, xpath = ".//d1:LocationTown")),
                            LocationTownCode=xml_text(xml_find_all(x, xpath = ".//d1:LocationTownCode")),
                            PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")),
                            PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                            StationClass=xml_text(xml_find_all(x, xpath = ".//d1:StationClass")))
  }else{
    rail_station=data.frame(StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName//d1:Zh_tw")),
                            StationUID=xml_text(xml_find_all(x, xpath = ".//d1:StationUID")),
                            StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                            LocationCity=xml_text(xml_find_all(x, xpath = ".//d1:LocationCity")),
                            LocationTown=xml_text(xml_find_all(x, xpath = ".//d1:LocationTown")),
                            LocationTownCode=xml_text(xml_find_all(x, xpath = ".//d1:LocationTownCode")),
                            PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")),
                            PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")))
  }

  print(paste0("#---", operator, " Station Downloaded---#"))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_station, out, row.names=F)
    }
    return(rail_station)
  }else if (dtype=="sf"){
    rail_station$Geometry=st_as_sfc(paste0("POINT(", rail_station$PositionLon, " ", rail_station$PositionLat, ")"))
    rail_station=st_sf(rail_station, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(rail_station, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(rail_station)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Rail_Shape=function(app_id, app_key, operator, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")

  if (operator=="TRA"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/TRA/Shape?%24format=XML"
  }else if (operator=="THSR"){
    url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/Shape?%24format=XML"
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/Shape/", operator, "?&%24format=XML")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
  }

  x=.get_ptx_data(app_id, app_key, url)
  rail_shape=data.frame(LineID=xml_text(xml_find_all(x, xpath = ".//d1:LineID")),
                        LineName=xml_text(xml_find_all(x, xpath = ".//d1:LineName")),
                        Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))

  print(paste0("#---", operator, " Shape Downloaded---#"))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_shape, out, row.names=F)
    }
    return(rail_shape)
  }else if (dtype=="sf"){
    rail_shape$Geometry=st_as_sfc(rail_shape$Geometry)
    rail_shape=st_sf(rail_shape, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(rail_shape, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(rail_shape)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Bike_Station=function(app_id, app_key, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  url=paste0("https://ptx.transportdata.tw/MOTC/v2/Bike/Station/", county, "?&$format=XML")
  x=.get_ptx_data(app_id, app_key, url)
  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the parameter table above. And please ensure that '", county, "' has bike sharing system."))
  }
  bike_station=data.frame(StationUID=xml_text(xml_find_all(x, xpath = ".//d1:StationUID")),
                          StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName/d1:Zh_tw")),
                          PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")),
                          PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                          BikesCapacity=as.numeric(xml_text(xml_find_all(x, xpath = ".//d1:BikesCapacity"))),
                          ServiceType=xml_text(xml_find_all(x, xpath = ".//d1:ServiceType")))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bike_station, out, row.names=F)
    }
    return(bike_station)
  }else if (dtype=="sf"){
    bike_station$Geometry=paste0("POINT(", bike_station$PositionLon, " ", bike_station$PositionLat, ")")
    bike_station$Geometry=st_as_sfc(bike_station$Geometry)
    bike_station=st_sf(bike_station, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(bike_station, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(bike_station)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Geocoding=function(address, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(sf)) install.packages("sf")
  if (!require(urltools)) install.packages("urltools")

  temp_cou=0
  address_record=data.frame()
  for (i in c(1:length(address))){
    tryCatch({
      add_temp=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/GeoCode/Coordinate/Address/", url_encode(address[i]), "?&$format=xml"))

      if (length(xml_text(xml_find_all(add_temp, xpath=".//Address")))==0){
        print(paste0("CANNOT Geocode ", AddressOriginal=address[i]))
      }else{
        add_temp=data.frame(AddressOriginal=address[i],
                            AddressNew=xml_text(xml_find_all(add_temp, xpath=".//Address")),
                            Geometry=xml_text(xml_find_all(add_temp, xpath=".//Geometry")))
        address_record=rbind(address_record, add_temp)
      }

      if((i %% 10==0 | i==length(address))){
        print(paste0(i, "/", length(address)))
      }

      temp_cou=i
    }, error=function(err){
      print(paste("ERROR:", conditionMessage(err)))
      if (conditionMessage(err)=="Timeout was reached: [gist.motc.gov.tw] Connection timed out after 10005 milliseconds"){
        i=temp_cou-1
      }else{
        print(paste0("CANNOT Geocode ", AddressOriginal=address[i]))
      }
    })
  }

  address_record=distinct(address_record)

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(address_record, out, row.names=F)
    }
    return(address_record)
  }else if (dtype=="sf"){
    address_record$Geometry=st_as_sfc(address_record$Geometry)
    address_record=st_sf(address_record, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(address_record, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(address_record)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Road_Network=function(county, roadclass, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(sf)) install.packages("sf")

  if (county %in% c(TDX_County$Code[1:22], "ALL") & roadclass %in% c(0,1,3,"ALL")){
    if (county!="ALL" & roadclass=="ALL"){
      road=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/Road/Network/City/", county, "?&$format=xml"))
      road=data.frame(RoadClass=xml_text(xml_find_all(road, xpath=".//RoadClass")),
                      RoadClassName=xml_text(xml_find_all(road, xpath=".//RoadClassName")),
                      RoadID=xml_text(xml_find_all(road, xpath=".//RoadID")),
                      RoadName=xml_text(xml_find_all(road, xpath=".//RoadName")),
                      RoadNameID=xml_text(xml_find_all(road, xpath=".//RoadNameID")),
                      Geometry=xml_text(xml_find_all(road, xpath=".//Geometry")))
    }else if (county!="ALL" & roadclass %in% c(0,1,3)){
      road=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/Road/Network/City/", county, "?$filter=RoadClass%20eq%20'", roadclass, "'&$format=xml"))
      road=data.frame(RoadClass=xml_text(xml_find_all(road, xpath=".//RoadClass")),
                      RoadClassName=xml_text(xml_find_all(road, xpath=".//RoadClassName")),
                      RoadID=xml_text(xml_find_all(road, xpath=".//RoadID")),
                      RoadName=xml_text(xml_find_all(road, xpath=".//RoadName")),
                      RoadNameID=xml_text(xml_find_all(road, xpath=".//RoadNameID")),
                      Geometry=xml_text(xml_find_all(road, xpath=".//Geometry")))
    }else if (county=="ALL" & roadclass %in% c(0,1,3)){
      road=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/Road/Network/RoadClass/", roadclass, "?&$format=XML"))
      road=data.frame(RoadClass=xml_text(xml_find_all(road, xpath=".//RoadClass")),
                      RoadClassName=xml_text(xml_find_all(road, xpath=".//RoadClassName")),
                      RoadID=xml_text(xml_find_all(road, xpath=".//RoadID")),
                      RoadName=xml_text(xml_find_all(road, xpath=".//RoadName")),
                      RoadNameID=xml_text(xml_find_all(road, xpath=".//RoadNameID")),
                      Geometry=xml_text(xml_find_all(road, xpath=".//Geometry")))
    }else if (county=="ALL" & roadclass=="ALL"){
      road=data.frame()
      for (i in c(0,1,3)){
        temp=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/Road/Network/RoadClass/", i, "?&$format=XML"))
        temp=data.frame(RoadClass=xml_text(xml_find_all(temp, xpath=".//RoadClass")),
                        RoadClassName=xml_text(xml_find_all(temp, xpath=".//RoadClassName")),
                        RoadID=xml_text(xml_find_all(temp, xpath=".//RoadID")),
                        RoadName=xml_text(xml_find_all(temp, xpath=".//RoadName")),
                        RoadNameID=xml_text(xml_find_all(temp, xpath=".//RoadNameID")),
                        Geometry=xml_text(xml_find_all(temp, xpath=".//Geometry")))
        road=rbind(road, temp)
        print(paste0("Road Class ", i, " Downloaded"))
      }
    }
  }else if(!(county %in% c(TDX_County$Code[1:22], "ALL"))){
    print(c(TDX_County$Code[1:22], "ALL"))
    stop(paste0("'", county, "' is invalid county code. Please check out the parameter table above."))
  }else if(!(RoadClass %in% c(0,1,3,"ALL"))){
    print(TDX_RoadClass)
    stop(paste0("'", county, "' is invalid county code. Please check out the parameter table above."))
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(road, out, row.names=F)
    }
    return(road)
  }else if (dtype=="sf"){
    road$Geometry=st_as_sfc(road$Geometry)
    road=st_sf(road, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(road, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(road)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Rail_TimeTable=function(app_id, app_key, operator, record, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  Sys.setlocale(category = "LC_ALL", locale = "cht")

  if (record=="station"){
    if (operator=="TRA"){
      url="https://ptx.transportdata.tw/MOTC/v3/Rail/TRA/GeneralStationTimetable?&$format=xml"
    }else if (operator=="THSR"){
      stop("THSR does not provide 'station' time table up to now! Please use 'general' time table.")
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT")){
      url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/StationTimeTable/", operator, "?&%24format=XML")
    }else if (operator=="TMRT"){
      stop("TMRT does not provide 'station' time table up to now! Please check out other MRT system.")
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above."))
    }

    x=.get_ptx_data(app_id, app_key, url)
    if (operator=="TRA"){
      station=data.frame(StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                         StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName/d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")))

      num_of_table=xml_length(xml_find_all(x, xpath = ".//d1:Timetables"))

      rail_timetable_temp=data.frame(Sequence=xml_text(xml_find_all(x, xpath = ".//d1:Sequence")),
                                     TrainNo=xml_text(xml_find_all(x, xpath = ".//d1:TrainNo")),
                                     DestinationStationID=xml_text(xml_find_all(x, xpath = ".//d1:DestinationStationID")),
                                     DestinationStationName=xml_text(xml_find_all(x, xpath = ".//d1:DestinationStationName/d1:Zh_tw")),
                                     TrainTypeID=xml_text(xml_find_all(x, xpath = ".//d1:TrainTypeID")),
                                     TrainTypeCode=xml_text(xml_find_all(x, xpath = ".//d1:TrainTypeCode")),
                                     TrainTypeName=xml_text(xml_find_all(x, xpath = ".//d1:TrainTypeName/d1:Zh_tw")),
                                     ArrivalTime=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalTime")),
                                     DepartureTime=xml_text(xml_find_all(x, xpath = ".//d1:DepartureTime")))

      rail_timetable=data.frame()
      for (i in c(1:length(num_of_table))){
        sec_head=sum(num_of_table[0:(i-1)])+1
        sec_tail=sum(num_of_table[0:i])
        rail_timetable=rbind(rail_timetable, cbind(station[i,], rail_timetable_temp[c(sec_head:sec_tail),]))
      }
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT")){
      station=data.frame(RouteID=xml_text(xml_find_all(x, xpath = ".//d1:RouteID")),
                         LineID=xml_text(xml_find_all(x, xpath = ".//d1:LineID")),
                         StationID=xml_text(xml_find_all(x, xpath = ".//d1:StationID")),
                         StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName/d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath = ".//d1:Direction")),
                         DestinationStaionID=xml_text(xml_find_all(x, xpath = ".//d1:DestinationStaionID")),
                         DestinationStationName=xml_text(xml_find_all(x, xpath = ".//d1:DestinationStationName/d1:Zh_tw")))

      num_of_table=xml_length(xml_find_all(x, xpath = ".//d1:Timetables"))

      rail_timetable_temp=data.frame(Sequence=xml_text(xml_find_all(x, xpath = ".//d1:Sequence")),
                                     ArrivalTime=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalTime")),
                                     DepartureTime=xml_text(xml_find_all(x, xpath = ".//d1:DepartureTime")))

      rail_timetable=data.frame()
      for (i in c(1:length(num_of_table))){
        sec_head=sum(num_of_table[0:(i-1)])+1
        sec_tail=sum(num_of_table[0:i])
        rail_timetable=rbind(rail_timetable, cbind(station[i,], rail_timetable_temp[c(sec_head:sec_tail),]))
      }
    }
  }else if (record=="general"){
    if (operator=="TRA"){
      url="https://ptx.transportdata.tw/MOTC/v3/Rail/TRA/GeneralTrainTimetable?&$format=XML"
    }else if (operator=="THSR"){
      url="https://ptx.transportdata.tw/MOTC/v2/Rail/THSR/GeneralTimetable?&$format=XML"
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT","TMRT")){
      stop("MRT system does not provide 'general' time table up to now! Please use 'station' time table.")
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
    }

    x=.get_ptx_data(app_id, app_key, url)
    if (operator=="TRA"){
      tra_info=data.frame(TrainNo=xml_text(xml_find_all(x, xpath=".//d1:TrainNo")),
                          Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
                          TrainTypeID=xml_text(xml_find_all(x, xpath=".//d1:TrainTypeID")),
                          TrainTypeCode=xml_text(xml_find_all(x, xpath=".//d1:TrainTypeCode")),
                          TrainTypeName=xml_text(xml_find_all(x, xpath=".//d1:TrainTypeName//d1:Zh_tw")),
                          TripHeadSign=xml_text(xml_find_all(x, xpath=".//d1:TripHeadSign")),
                          StartingStationID=xml_text(xml_find_all(x, xpath=".//d1:StartingStationID")),
                          StartingStationName=xml_text(xml_find_all(x, xpath=".//d1:StartingStationName//d1:Zh_tw")),
                          EndingStationID=xml_text(xml_find_all(x, xpath=".//d1:EndingStationID")),
                          EndingStationName=xml_text(xml_find_all(x, xpath=".//d1:EndingStationName//d1:Zh_tw")),
                          TripLine=xml_text(xml_find_all(x, xpath=".//d1:TripLine")),
                          Monday=xml_text(xml_find_all(x, xpath=".//d1:Monday")),
                          Tuesday=xml_text(xml_find_all(x, xpath=".//d1:Tuesday")),
                          Wednesday=xml_text(xml_find_all(x, xpath=".//d1:Wednesday")),
                          Thursday=xml_text(xml_find_all(x, xpath=".//d1:Thursday")),
                          Friday=xml_text(xml_find_all(x, xpath=".//d1:Friday")),
                          Saturday=xml_text(xml_find_all(x, xpath=".//d1:Saturday")),
                          Sunday=xml_text(xml_find_all(x, xpath=".//d1:Sunday")),
                          NationalHolidays=xml_text(xml_find_all(x, xpath=".//d1:NationalHolidays")),
                          DayBeforeHoliday=xml_text(xml_find_all(x, xpath=".//d1:DayBeforeHoliday")),
                          DayAfterHoliday=xml_text(xml_find_all(x, xpath=".//d1:DayAfterHoliday")))

      num_of_station=xml_length(xml_find_all(x, xpath = ".//d1:StopTimes"))

      rail_timetable_temp=data.frame(StopSequence=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:StopSequence"))),
                                     StationID=xml_text(xml_find_all(x, xpath=".//d1:StationID")),
                                     StationName=xml_text(xml_find_all(x, xpath=".//d1:StationName//d1:Zh_tw")),
                                     ArrivalTime=xml_text(xml_find_all(x, xpath=".//d1:ArrivalTime")),
                                     DepartureTime=xml_text(xml_find_all(x, xpath=".//d1:DepartureTime")))

      rail_timetable=data.frame()
      for (i in c(1:length(num_of_station))){
        sec_head=sum(num_of_station[0:(i-1)])+1
        sec_tail=sum(num_of_station[0:i])
        rail_timetable=rbind(rail_timetable, cbind(tra_info[i, ], rail_timetable_temp[c(sec_head:sec_tail),]))
      }
    }else if (operator=="THSR"){
      tra_info=data.frame(TrainNo=xml_text(xml_find_all(x, xpath=".//d1:TrainNo")),
                          Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
                          StartingStationID=xml_text(xml_find_all(x, xpath=".//d1:StartingStationID")),
                          StartingStationName=xml_text(xml_find_all(x, xpath=".//d1:StartingStationName//d1:Zh_tw")),
                          EndingStationID=xml_text(xml_find_all(x, xpath=".//d1:EndingStationID")),
                          EndingStationName=xml_text(xml_find_all(x, xpath=".//d1:EndingStationName//d1:Zh_tw")),
                          Monday=xml_text(xml_find_all(x, xpath=".//d1:Monday")),
                          Tuesday=xml_text(xml_find_all(x, xpath=".//d1:Tuesday")),
                          Wednesday=xml_text(xml_find_all(x, xpath=".//d1:Wednesday")),
                          Thursday=xml_text(xml_find_all(x, xpath=".//d1:Thursday")),
                          Friday=xml_text(xml_find_all(x, xpath=".//d1:Friday")),
                          Saturday=xml_text(xml_find_all(x, xpath=".//d1:Saturday")),
                          Sunday=xml_text(xml_find_all(x, xpath=".//d1:Sunday")))

      num_of_station=xml_length(xml_find_all(x, xpath = ".//d1:StopTimes"))


      ArrivalTime=xml_text(xml_find_all(x, xpath=".//d1:ArrivalTime"))
      ArrivalTime=data.frame(id=which(grepl("ArrivalTime", xml_find_all(x, xpath=".//d1:StopTime"))), ArrivalTime)
      DepartureTime=xml_text(xml_find_all(x, xpath=".//d1:DepartureTime"))
      DepartureTime=data.frame(id=which(grepl("DepartureTime", xml_find_all(x, xpath=".//d1:StopTime"))), DepartureTime)
      temp=left_join(data.frame(id=c(1:length(xml_text(xml_find_all(x, xpath=".//d1:StopSequence"))))), ArrivalTime)%>%
        left_join(DepartureTime)%>%
        select(-id)

      rail_timetable_temp=data.frame(StopSequence=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:StopSequence"))),
                                     StationID=xml_text(xml_find_all(x, xpath=".//d1:StationID")),
                                     StationName=xml_text(xml_find_all(x, xpath=".//d1:StationName//d1:Zh_tw")),
                                     temp)

      rail_timetable=data.frame()
      for (i in c(1:length(num_of_station))){
        sec_head=sum(num_of_station[0:(i-1)])+1
        sec_tail=sum(num_of_station[0:i])
        rail_timetable=rbind(rail_timetable, cbind(tra_info[i, ], rail_timetable_temp[c(sec_head:sec_tail),]))
      }
    }
  }else{
    stop("'", record, "' is not valid format of timetable. Please use 'station' or 'general'.")
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(rail_timetable, out, row.names=F)
  }
  return(rail_timetable)
}



Bike_Shape=function(app_id, app_key, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  url=paste0("https://ptx.transportdata.tw/MOTC/v2/Cycling/Shape/", county, "?&$format=XML")
  x=.get_ptx_data(app_id, app_key, url)
  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the paramete table above.",
                "\nIf the county code is indeed in the parameter table, but the error still occurs, it means that the county has no cycling network now in TDX platform."))
  }

  RoadSectionStart=xml_text(xml_find_all(x, xpath = ".//d1:RoadSectionStart"))
  RoadSectionStart=data.frame(id=which(grepl("RoadSectionStart", xml_find_all(x, xpath=".//d1:BikeShape"))), RoadSectionStart)
  RoadSectionEnd=xml_text(xml_find_all(x, xpath = ".//d1:RoadSectionEnd"))
  RoadSectionEnd=data.frame(id=which(grepl("RoadSectionEnd", xml_find_all(x, xpath=".//d1:BikeShape"))), RoadSectionEnd)
  temp=left_join(data.frame(id=c(1:length(xml_text(xml_find_all(x, xpath=".//d1:City"))))), RoadSectionStart)%>%
    left_join(RoadSectionEnd)%>%
    select(-id)

  bike_shape=data.frame(RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName")),
                        City=xml_text(xml_find_all(x, xpath = ".//d1:City")),
                        temp,
                        CyclingLength=xml_text(xml_find_all(x, xpath = ".//d1:CyclingLength")),
                        Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bike_shape, out, row.names=F)
    }
    return(bike_shape)
  }else if (dtype=="sf"){
    bike_shape$Geometry=st_as_sfc(bike_shape$Geometry)
    bike_shape=st_sf(bike_shape, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(bike_shape, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(bike_shape)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}



Air_Schedule=function(app_id, app_key, domestic=T, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  Sys.setlocale(category = "LC_ALL", locale = "cht")
  if (domestic){
    url="https://ptx.transportdata.tw/MOTC/v2/Air/GeneralSchedule/Domestic?$format=xml"
    x=.get_ptx_data(app_id, app_key, url)
    air_schedule=data.frame(AirlineID=xml_text(xml_find_all(x, xpath = ".//d1:AirlineID")),
                            ScheduleStartDate=xml_text(xml_find_all(x, xpath = ".//d1:ScheduleStartDate")),
                            ScheduleEndDate=xml_text(xml_find_all(x, xpath = ".//d1:ScheduleEndDate")),
                            FlightNumber=xml_text(xml_find_all(x, xpath = ".//d1:FlightNumber")),
                            DepartureAirportID=xml_text(xml_find_all(x, xpath = ".//d1:DepartureAirportID")),
                            DepartureTime=xml_text(xml_find_all(x, xpath = ".//d1:DepartureTime")),
                            ArrivalAirportID=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalAirportID")),
                            ArrivalTime=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalTime")),
                            Monday=xml_text(xml_find_all(x, xpath = ".//d1:Monday")),
                            Tuesday=xml_text(xml_find_all(x, xpath = ".//d1:Tuesday")),
                            Wednesday=xml_text(xml_find_all(x, xpath = ".//d1:Wednesday")),
                            Thursday=xml_text(xml_find_all(x, xpath = ".//d1:Thursday")),
                            Friday=xml_text(xml_find_all(x, xpath = ".//d1:Friday")),
                            Saturday=xml_text(xml_find_all(x, xpath = ".//d1:Saturday")),
                            Sunday=xml_text(xml_find_all(x, xpath = ".//d1:Sunday")))
  }else{
    url="https://ptx.transportdata.tw/MOTC/v2/Air/GeneralSchedule/International?$format=xml"
    x=.get_ptx_data(app_id, app_key, url)

    Terminal=xml_text(xml_find_all(x, xpath = ".//d1:Terminal"))
    Terminal=data.frame(id=which(grepl("Terminal", xml_find_all(x, xpath=".//d1:GeneralFlightSchedule"))), Terminal)
    temp=left_join(data.frame(id=c(1:length(xml_text(xml_find_all(x, xpath=".//d1:GeneralFlightSchedule"))))), Terminal)%>%
      select(-id)

    air_schedule=data.frame(AirlineID=xml_text(xml_find_all(x, xpath = ".//d1:AirlineID"))[1:length(xml_find_all(x, xpath=".//d1:GeneralFlightSchedule"))],
                            FlightNumber=xml_text(xml_find_all(x, xpath = ".//d1:FlightNumber"))[1:length(xml_find_all(x, xpath=".//d1:GeneralFlightSchedule"))],
                            scheduleStartDate=xml_text(xml_find_all(x, xpath = ".//d1:ScheduleStartDate")),
                            ScheduleEndDate=xml_text(xml_find_all(x, xpath = ".//d1:ScheduleEndDate")),
                            DepartureAirportID=xml_text(xml_find_all(x, xpath = ".//d1:DepartureAirportID")),
                            DepartureTime=xml_text(xml_find_all(x, xpath = ".//d1:DepartureTime")),
                            ArrivalAirportID=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalAirportID")),
                            ArrivalTime=xml_text(xml_find_all(x, xpath = ".//d1:ArrivalTime")),
                            Monday=xml_text(xml_find_all(x, xpath = ".//d1:Monday")),
                            Tuesday=xml_text(xml_find_all(x, xpath = ".//d1:Tuesday")),
                            Wednesday=xml_text(xml_find_all(x, xpath = ".//d1:Wednesday")),
                            Thursday=xml_text(xml_find_all(x, xpath = ".//d1:Thursday")),
                            Friday=xml_text(xml_find_all(x, xpath = ".//d1:Friday")),
                            Saturday=xml_text(xml_find_all(x, xpath = ".//d1:Saturday")),
                            Sunday=xml_text(xml_find_all(x, xpath = ".//d1:Sunday")),
                            temp)
  }
  if (nchar(out)!=0 & out!=F){
    write.csv(air_schedule, out, row.names=F)
  }
  return(air_schedule)
}



ScenicSpot=function(app_id, app_key, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  Sys.setlocale(category = "LC_ALL", locale = "cht")


  if (county=="ALL"){
    url="https://ptx.transportdata.tw/MOTC/v2/Tourism/ScenicSpot?&$format=xml"
  }else{
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Tourism/ScenicSpot/", county, "?&$format=xml")
  }
  x=.get_ptx_data(app_id, app_key, url)

  if (substr(xml_text(x), 1, 4)=="City"){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
  }

  Address=xml_text(xml_find_all(x, xpath = ".//d1:Address"))
  Address=data.frame(id=which(grepl("Address", xml_find_all(x, xpath=".//d1:ScenicSpotTourismInfo"))), Address)
  temp=left_join(data.frame(id=c(1:length(xml_text(xml_find_all(x, xpath=".//d1:ScenicSpotTourismInfo"))))), Address)%>%
    select(-id)

  scenic_spot=data.frame(ScenicSpotID=xml_text(xml_find_all(x, xpath = ".//d1:ScenicSpotID")),
                         ScenicSpotName=xml_text(xml_find_all(x, xpath = ".//d1:ScenicSpotName")),
                         temp,
                         PositionLon=as.numeric(xml_text(xml_find_all(x, xpath = ".//d1:Position//d1:PositionLon"))),
                         PositionLat=as.numeric(xml_text(xml_find_all(x, xpath = ".//d1:Position//d1:PositionLat"))))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(scenic_spot, out, row.names=F)
    }
    return(scenic_spot)
  }else if (dtype=="sf"){
    scenic_spot$Geometry=st_as_sfc(paste0("POINT(", scenic_spot$PositionLon, " ", scenic_spot$PositionLat, ")"))
    scenic_spot=st_sf(scenic_spot, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(scenic_spot, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'")
    }

    return(scenic_spot)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'."))
  }
}





