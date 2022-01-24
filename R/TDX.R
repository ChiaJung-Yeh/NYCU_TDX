library(dplyr)
library(xml2)
library(httr)
library(sf)
library(urltools)

TDX_County=read_xml("https://gist.motc.gov.tw/gist_api/V3/Map/Basic/City?$format=XML")
TDX_County=data.frame(County=xml_text(xml_find_all(TDX_County, xpath = "//CityName")),
                      Code=xml_text(xml_find_all(TDX_County, xpath = "//City")))
TDX_County=rbind(TDX_County, cbind(County="公路客運", Code="Intercity"))
# usethis::use_data(TDX_County, overwrite=T)

TDX_Railway=data.frame(Operator=c("臺鐵","高鐵","臺北捷運","高雄捷運","桃園捷運","新北捷運","臺中捷運","高雄輕軌"),
                       Code=c("TRA","THSR","TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT"))
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
    warning(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
    print(TDX_County)
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
        warning("The file name must contain '.shp'")
      }

      return(bus_stop)
    }else{
      warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
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
    warning(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
    print(TDX_County)
  }else{
    bus_shape=data.frame(RouteUID=xml_text(xml_find_all(x, xpath = ".//d1:RouteUID")),
                         RouteName=xml_text(xml_find_all(x, xpath = ".//d1:RouteName")),
                         SubRouteUID=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteUID")),
                         SubRouteName=xml_text(xml_find_all(x, xpath = ".//d1:SubRouteName")),
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
        warning("The file name must contain '.shp'")
      }

      return(bus_shape)
    }else{
      warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
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
    warning(paste0("City: '", county, "' is not accepted. Please check out the parameter table above."))
    print(TDX_County)
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
    warning("Please use function 'Rail_Station' to retrieve the station of high speed rail (THSR).")
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://ptx.transportdata.tw/MOTC/v2/Rail/Metro/StationOfLine/", operator, "?&%24format=XML")
  }else{
    warning(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
    print(TDX_Railway)
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
    warning(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
    print(TDX_Railway)
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
      warning("The file name must contain '.shp'")
    }

    return(rail_station)
  }else{
    warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
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
    warning(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
    print(TDX_Railway)
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
      warning("The file name must contain '.shp'")
    }

    return(rail_shape)
  }else{
    warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
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
    warning(paste0("City: '", county, "' is not accepted. Please check out the parameter table above. And please ensure that '", county, "' has bike sharing system."))
    TDX_County
  }
  bike_station=data.frame(StationUID=xml_text(xml_find_all(x, xpath = ".//d1:StationUID")),
                          StationName=xml_text(xml_find_all(x, xpath = ".//d1:StationName/d1:Zh_tw")),
                          PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")),
                          PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                          BikesCapacity=xml_text(xml_find_all(x, xpath = ".//d1:BikesCapacity")),
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
      warning("The file name must contain '.shp'")
    }

    return(bike_station)
  }else{
    warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
  }
}



Geocoding=function(address, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(sf)) install.packages("sf")
  if (!require(sf)) install.packages("urltools")

  temp_cou=0
  address_record=data.frame()
  for (i in c(1:length(address))){
    tryCatch({
      if (i-temp_cou==2){
        i=i-1
      }
      add_temp=read_xml(paste0("https://gist.motc.gov.tw/gist_api/V3/Map/GeoCode/Coordinate/Address/", url_encode(address[i]), "?&$format=xml"))
      add_temp=data.frame(AddressOriginal=address[i],
                          AddressNew=xml_text(xml_find_all(add_temp, xpath=".//Address")),
                          Geometry=xml_text(xml_find_all(add_temp, xpath=".//Geometry")))
      address_record=rbind(address_record, add_temp)

      print(if((i %% 10==0 | i==length(address))){
        paste0(i, "/", length(address))
      }else{next})

      temp_cou=i
    }, error=function(e){})
  }

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
      warning("The file name must contain '.shp'")
    }

    return(address_record)
  }else{
    warning(paste0(dtype, " is not allowed format. Please use 'text' or 'sf'."))
  }
}


