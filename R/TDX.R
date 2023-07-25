library(dplyr)
library(jsonlite)
library(xml2)
library(httr)
library(sf)
library(urltools)
library(progress)
library(data.table)

usethis::use_package("dplyr")
usethis::use_package("jsonlite")
usethis::use_package("xml2")
usethis::use_package("httr")
usethis::use_package("sf")
usethis::use_package("urltools")
usethis::use_package("progress")
usethis::use_package("data.table")


# TDX_County=read.table("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_county.txt", encoding="UTF-8", sep=",", header=T)
# usethis::use_data(TDX_County, overwrite=T)
#
# TDX_Railway=read.table("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_railway.txt", encoding="UTF-8", sep=",", header=T)
# usethis::use_data(TDX_Railway, overwrite=T)
#
# TDX_RoadClass=read.table("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_roadclass.txt", encoding="UTF-8", sep=",", header=T)
# usethis::use_data(TDX_RoadClass, overwrite=T)


#---get the token---#
get_token=function(client_id, client_secret){
  if (!require(httr)) install.packages("httr")
  if (!require(jsonlite)) install.packages("jsonlite")

  if("TDX_token.txt" %in% dir(Sys.getenv("HOME"))){
    act=read.table(paste0(Sys.getenv("HOME"), "/TDX_token.txt"))$V1
    x=GET("https://tdx.transportdata.tw/api/basic/v2/Basic/County?%24format=JSON", add_headers(Accept="application/+json", Authorization=paste("Bearer", act)))

    if(grepl("invalid", x)){
      cat(paste0("The access token is expired or invalid. Get newer one!\n"))
    }else{
      cat(paste0("The access token is valid. Use it!\n"))
      return(act)
      break
    }
  }

  x=POST("https://tdx.transportdata.tw/auth/realms/TDXConnect/protocol/openid-connect/token",
         encode="form",
         body=list(
           grant_type="client_credentials",
           client_id=client_id,
           client_secret=client_secret
         ))
  act=content(x)$access_token
  if(is.null(act)){
    stop("Your 'client_id' or 'client_secret' is WRONG!!")
  }else{
    cat("Connect Successfully! This token will expire in 1 day. New token is stored.\n")
    write.table(act, paste0(Sys.getenv("HOME"), "/TDX_token.txt"), col.names=F, row.names=F)
    return(act)
  }
}



Bus_StopOfRoute=function(access_token, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/StopOfRoute/InterCity?%24format=JSON"
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/StopOfRoute/City/", county, "?%24format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bus_info=fromJSON(content(x, as="text"))
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })
  if("Message" %in% names(bus_info)){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  bus_info$RouteName=bus_info$RouteName$Zh_tw
  bus_info$SubRouteName=bus_info$SubRouteName$Zh_tw
  num_of_stop=mapply(function(x) nrow(bus_info$Stops[[x]]), c(1:nrow(bus_info)))
  cat(paste0(length(num_of_stop), " Routes\n"))
  bus_stop_temp=data.frame(StopUID=unlist(mapply(function(x) bus_info$Stops[[x]]$StopUID, c(1:nrow(bus_info)))),
                           StopID=unlist(mapply(function(x) bus_info$Stops[[x]]$StopID, c(1:nrow(bus_info)))),
                           StopName=unlist(mapply(function(x) bus_info$Stops[[x]]$StopName$Zh_tw, c(1:nrow(bus_info)))),
                           StopBoarding=unlist(mapply(function(x) bus_info$Stops[[x]]$StopBoarding, c(1:nrow(bus_info)))),
                           StopSequence=unlist(mapply(function(x) bus_info$Stops[[x]]$StopSequence, c(1:nrow(bus_info)))),
                           PositionLon=unlist(mapply(function(x) bus_info$Stops[[x]]$StopPosition$PositionLon, c(1:nrow(bus_info)))),
                           PositionLat=unlist(mapply(function(x) bus_info$Stops[[x]]$StopPosition$PositionLat, c(1:nrow(bus_info)))),
                           StationID=unlist(mapply(function(x) if(!is.null(bus_info$Stops[[x]]$StationID)) bus_info$Stops[[x]]$StationID else rep(NA, num_of_stop[x]), c(1:nrow(bus_info)))),
                           LocationCityCode=unlist(mapply(function(x) bus_info$Stops[[x]]$LocationCityCode, c(1:nrow(bus_info)))))

  bus_info=as.data.frame(lapply(bus_info[, c("RouteUID","RouteID","RouteName","SubRouteUID","SubRouteID","SubRouteName","Direction")], rep, num_of_stop))
  bus_stop=cbind(bus_info, bus_stop_temp)

  rm(bus_info, bus_stop_temp, num_of_stop)

  cat(paste0("#---", county, " Stop of Route Downloaded---#\n"))

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bus_stop, out, row.names=F)
    }
  }else if(dtype=="sf"){
    bus_stop$geometry=st_as_sfc(ifelse(!is.na(bus_stop$PositionLon), paste0("POINT(", bus_stop$PositionLon, " ", bus_stop$PositionLat, ")"), "GEOMETRYCOLLECTION EMPTY"))
    bus_stop=st_sf(bus_stop, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(bus_stop, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(bus_stop)
}



Bus_Route=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/Route/InterCity?%24format=JSON"
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/Route/City/", county, "?$format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bus_info=fromJSON(content(x, as="text"))
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })
  if("Message" %in% names(bus_info)){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  bus_info$RouteName=bus_info$RouteName$Zh_tw
  names(bus_info)[grepl("Zh", names(bus_info))]=gsub("Zh", "", names(bus_info)[grepl("Zh", names(bus_info))])
  num_of_subroute=mapply(function(x) nrow(bus_info$SubRoutes[[x]]), c(1:nrow(bus_info)))
  cat(paste0(length(num_of_route), " Routes\n"))

  bus_route_temp=data.frame(SubRouteUID=unlist(mapply(function(x) bus_info$SubRoutes[[x]]$SubRouteUID, c(1:nrow(bus_info)))),
                            SubRouteID=unlist(mapply(function(x) bus_info$SubRoutes[[x]]$SubRouteID, c(1:nrow(bus_info)))),
                            SubRouteName=unlist(mapply(function(x) bus_info$SubRoutes[[x]]$SubRouteName$Zh_tw, c(1:nrow(bus_info)))),
                            Direction=unlist(mapply(function(x) bus_info$SubRoutes[[x]]$Direction, c(1:nrow(bus_info)))),
                            FirstBusTime=unlist(mapply(function(x) if(!is.null(bus_info$SubRoutes[[x]]$FirstBusTime)) bus_info$SubRoutes[[x]]$FirstBusTime else rep(NA, num_of_subroute[x]), c(1:nrow(bus_info)))),
                            LastBusTime=unlist(mapply(function(x) if(!is.null(bus_info$SubRoutes[[x]]$LastBusTime)) bus_info$SubRoutes[[x]]$LastBusTime else rep(NA, num_of_subroute[x]), c(1:nrow(bus_info)))),
                            HolidayFirstBusTime=unlist(mapply(function(x) if(!is.null(bus_info$SubRoutes[[x]]$HolidayFirstBusTime)) bus_info$SubRoutes[[x]]$HolidayFirstBusTime else rep(NA, num_of_subroute[x]), c(1:nrow(bus_info)))),
                            HolidayLastBusTime=unlist(mapply(function(x) if(!is.null(bus_info$SubRoutes[[x]]$HolidayLastBusTime)) bus_info$SubRoutes[[x]]$HolidayLastBusTime else rep(NA, num_of_subroute[x]), c(1:nrow(bus_info)))))

  col_retain=c("RouteUID","RouteID","RouteName","BusRouteType","DepartureStopName","DestinationStopName","TicketPriceDescription","FareBufferZoneDescription")
  col_retain=col_retain[col_retain %in% names(bus_info)]
  bus_info=as.data.frame(lapply(bus_info[, col_retain], rep, num_of_subroute))
  bus_route=cbind(bus_info, bus_route_temp)

  col_seq=c("RouteUID","RouteID","RouteName", "SubRouteUID", "SubRouteID", "SubRouteName", "Direction", "BusRouteType","DepartureStopName","DestinationStopName","TicketPriceDescription","FareBufferZoneDescription", "FirstBusTime", "LastBusTime", "HolidayFirstBusTime", "HolidayLastBusTime")
  col_seq=col_seq[col_seq %in% names(bus_route)]

  if(nchar(out)!=0 & out!=F){
    write.csv(bus_route, out, row.names=F)
  }
  return(bus_route)
}



Bus_Shape=function(access_token, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/Shape/InterCity?&$format=JSON"
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/Shape/City/", county, "?&$format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bus_shape=fromJSON(content(x, as="text"))
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })
  if("Message" %in% names(bus_shape)){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  bus_shape$RouteName=bus_shape$RouteName$Zh_tw
  bus_shape$SubRouteName=bus_shape$SubRouteName$Zh_tw

  col_seq=c("RouteUID", "RouteID", "RouteName", "SubRouteUID", "SubRouteID", "SubRouteName", "Direction", "Geometry")
  col_seq=col_seq[col_seq %in% names(bus_shape)]
  bus_shape=bus_shape[, col_seq]%>%
    rename(geometry=Geometry)

  cat(paste0("#---", county, " Bus Shape Downloaded---#\n"))

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bus_shape, out, row.names=F)
    }
  }else if(dtype=="sf"){
    bus_shape$geometry=st_as_sfc(bus_shape$geometry)
    bus_shape=st_sf(bus_shape, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(bus_shape, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(bus_shape)
}



Bus_Schedule=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/Schedule/InterCity?&$format=JSON"
  }else if(county=="LienchiangCounty"){
    stop("Bus schedule data of LienchiangCounty is now unavaliable.")
  }
  else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/Schedule/City/", county, "?&$format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bus_info=fromJSON(content(x, as="text"))
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })
  if("Message" %in% names(bus_info)){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  bus_info$RouteName=bus_info$RouteName$Zh_tw
  bus_info$SubRouteName=bus_info$SubRouteName$Zh_tw
  bus_route=bus_info[, c("RouteUID","RouteID","RouteName","SubRouteUID","SubRouteID","SubRouteName","Direction")]

  num_of_freq=unlist(mapply(function(x) ifelse(is.null(nrow(bus_info$Frequencys[[x]])), 0, nrow(bus_info$Frequencys[[x]])), c(1:nrow(bus_info))))
  num_of_time=unlist(mapply(function(x) ifelse(is.null(nrow(bus_info$Timetables[[x]])), 0, nrow(bus_info$Timetables[[x]])), c(1:nrow(bus_info))))

  bus_freq=data.frame(StartTime=unlist(mapply(function(x) bus_info$Frequencys[[x]]$StartTime, c(1:nrow(bus_info)))),
                      EndTime=unlist(mapply(function(x) bus_info$Frequencys[[x]]$EndTime, c(1:nrow(bus_info)))),
                      MinHeadwayMins=unlist(mapply(function(x) bus_info$Frequencys[[x]]$MinHeadwayMins, c(1:nrow(bus_info)))),
                      MaxHeadwayMins=unlist(mapply(function(x) bus_info$Frequencys[[x]]$MaxHeadwayMins, c(1:nrow(bus_info)))),
                      rbindlist(mapply(function(x) bus_info$Frequencys[[x]]$ServiceDay, c(1:nrow(bus_info)))))
  bus_freq=cbind(bus_route[rep(c(1:nrow(bus_route)), times=num_of_freq),], bus_freq)

  retrieve_first=function(dat){
    temp=data.frame(t(mapply(function(x){
      first_row=t(unlist(dat[[x]][1,]))
      return(first_row)
    }, c(1:length(dat)))))
    return(temp)
  }
  bus_time=mapply(function(x) list(retrieve_first(bus_info$Timetables[[x]]$StopTimes)), which(num_of_time!=0))
  bus_time=do.call(bind_rows, bus_time)%>%
    data.frame()
  bus_time=select(bus_time, -X5)
  colnames(bus_time)=c("StopSequence","StopUID","StopID","StopName","ArrivalTime","DepartureTime")
  day_oper=mapply(function(x) list(bus_info$Timetables[[x]]$ServiceDay), c(1:nrow(bus_info))) %>% do.call(rbind, .)
  if(!is.null(day_oper)){
    bus_time=data.frame(TripID=unlist(mapply(function(x) ifelse(is.null(bus_info$Timetables[[x]]$TripID), list(rep(NA, num_of_time[x])), list(bus_info$Timetables[[x]]$TripID)), c(1:nrow(bus_info)))),
                        bus_time,
                        day_oper)
  }else{
    bus_time=data.frame(TripID=unlist(mapply(function(x) ifelse(is.null(bus_info$Timetables[[x]]$TripID), list(rep(NA, num_of_time[x])), list(bus_info$Timetables[[x]]$TripID)), c(1:nrow(bus_info)))),
                        bus_time)
  }
  bus_time=cbind(bus_route[rep(c(1:nrow(bus_route)), times=num_of_time),], bus_time)

  bus_schedule=bind_rows(bus_freq, bus_time)
  row.names(bus_schedule)=NULL

  cat(paste0("#---", county, " Bus Schedule Downloaded---#\n"))

  if(nchar(out)!=0 & out!=F){
    write.csv(bus_schedule, out, row.names=F)
  }
  return(bus_schedule)
}



Rail_StationOfLine=function(access_token, operator, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(operator=="TRA"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/StationOfLine?&%24format=JSON"
  }else if(operator=="THSR"){
    stop("Please use function 'Rail_Station()' to retrieve the station of high speed rail (THSR).")
  }else if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/StationOfLine/", operator, "?&%24format=JSON")
  }else if(operator=="AFR"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/StationOfLine?&%24format=JSON")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    if(operator=="AFR"){
      rail_info=fromJSON(content(x, as="text"))$StationOfLines
    }else{
      rail_info=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })

  num_of_station=unlist(mapply(function(x) nrow(rail_info$Stations[[x]]), c(1:nrow(rail_info))))

  if(operator=="TRA"){
    rail_station_temp=data.frame(Sequence=unlist(mapply(function(x) rail_info$Stations[[x]]$Sequence, c(1:nrow(rail_info)))),
                                 StationID=unlist(mapply(function(x) rail_info$Stations[[x]]$StationID, c(1:nrow(rail_info)))),
                                 StationName=unlist(mapply(function(x) rail_info$Stations[[x]]$StationName, c(1:nrow(rail_info)))),
                                 TraveledDistance=unlist(mapply(function(x) rail_info$Stations[[x]]$TraveledDistance, c(1:nrow(rail_info)))))
  }else if(operator=="AFR"){
    rail_station_temp=data.frame(Sequence=unlist(mapply(function(x) rail_info$Stations[[x]]$Sequence, c(1:nrow(rail_info)))),
                                 StationID=unlist(mapply(function(x) rail_info$Stations[[x]]$StationID, c(1:nrow(rail_info)))),
                                 StationName=unlist(mapply(function(x) rail_info$Stations[[x]]$StationName$Zh_tw, c(1:nrow(rail_info)))),
                                 CumulativeDistance=unlist(mapply(function(x) rail_info$Stations[[x]]$CumulativeDistance, c(1:nrow(rail_info)))))
  }else{
    rail_station_temp=data.frame(Sequence=unlist(mapply(function(x) rail_info$Stations[[x]]$Sequence, c(1:nrow(rail_info)))),
                                 StationID=unlist(mapply(function(x) rail_info$Stations[[x]]$StationID, c(1:nrow(rail_info)))),
                                 StationName=unlist(mapply(function(x) rail_info$Stations[[x]]$StationName$Zh_tw, c(1:nrow(rail_info)))))
  }

  rail_info=data.frame(LineID=rail_info$LineID[rep(c(1:nrow(rail_info)), num_of_station)])
  rail_station_line=cbind(rail_info, rail_station_temp)

  if (operator=="TRA"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/Line?&%24format=JSON"
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
    rail_line=fromJSON(content(x, as="text"))
    rail_line$LineName=rail_line$LineNameZh
    rail_line$LineSectionName=rail_line$LineSectionNameZh
    rail_station_line=left_join(rail_station_line, rail_line[, c("LineID","LineName","LineSectionName","IsBranch")], by=c("LineID"))%>%
      dplyr::select(LineID, LineName, LineSectionName, IsBranch, Sequence, StationID, StationName, TraveledDistance)
  }else if (operator=="AFR"){
    url="https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/Line?&%24format=JSON"
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
    rail_line=fromJSON(content(x, as="text"))$Lines
    rail_line$LineName=rail_line$LineName$Zh_tw
    rail_line$LineSectionName=rail_line$LineSectionName$Zh_tw
    rail_station_line=left_join(rail_station_line, rail_line)%>%
      dplyr::select(LineID, LineName, LineSectionName, IsBranch, Sequence, StationID, StationName, CumulativeDistance)
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/Line/", operator,"?&%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
    dat=content(x)
    rail_line=data.frame(LineID=unlist(mapply(function(x) dat[[x]]$LineID, c(1:length(dat)))),
                         LineName=unlist(mapply(function(x) dat[[x]]$LineName$Zh_tw, c(1:length(dat)))),
                         LineSectionName=unlist(mapply(function(x) ifelse(is.null(dat[[1]]$LineSectionName$Zh_tw), NA, dat[[1]]$LineSectionName$Zh_tw), c(1:length(dat)))),
                         IsBranch=unlist(mapply(function(x) dat[[x]]$IsBranch, c(1:length(dat)))))
    rail_station_line=left_join(rail_station_line, rail_line, by=c("LineID"))%>%
      dplyr::select(LineID, LineName, LineSectionName, IsBranch, Sequence, StationID, StationName)
  }

  cat(paste0("#---", operator, " Station of Line Downloaded---#\n"))

  if(nchar(out)!=0 & out!=F){
    write.csv(rail_station_line, out, row.names=F)
  }
  return(rail_station_line)
}



Rail_Station=function(access_token, operator, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(operator=="TRA"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/Station?&%24format=JSON"
  }else if(operator=="THSR"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/Station?&%24format=JSON"
  }else if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/Station/", operator, "?&%24format=JSON")
  }else if(operator=="AFR"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/Station?&%24format=JSON")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above."))
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    if(operator=="AFR"){
      rail_station=fromJSON(content(x, as="text"))$Stations
    }else{
      rail_station=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  rail_station$StationName=rail_station$StationName$Zh_tw
  rail_station$PositionLon=rail_station$StationPosition$PositionLon
  rail_station$PositionLat=rail_station$StationPosition$PositionLat

  if (operator=="TRA"){
    rail_station=rail_station[, c("StationUID","StationID","StationName","StationClass","StationAddress","StationPhone","LocationCity","LocationCityCode","LocationTown","LocationTownCode","PositionLon","PositionLat")]
  }else if(operator=="THSR"){
    rail_station=rail_station[, c("StationUID","StationID","StationCode","StationName","StationAddress","LocationCity","LocationCityCode","LocationTown","LocationTownCode","PositionLon","PositionLat")]
  }else if(operator=="AFR"){
    rail_station=rail_station[, c("StationUID","StationID","StationClass","ReservationCode","StationName","StationAddress","StationPhone","PositionLon","PositionLat")]
  }else{
    rail_station=rail_station[, c("StationUID","StationID","StationName","StationAddress","BikeAllowOnHoliday","LocationCity","LocationCityCode","LocationTown","LocationTownCode","PositionLon","PositionLat")]
  }

  cat(paste0("#---", operator, " Station Downloaded---#\n"))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_station, out, row.names=F)
    }
  }else if (dtype=="sf"){
    rail_station$geometry=st_as_sfc(ifelse(!is.na(rail_station$PositionLon), paste0("POINT(", rail_station$PositionLon, " ", rail_station$PositionLat, ")"), "GEOMETRYCOLLECTION EMPTY"))
    rail_station=st_sf(rail_station, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(rail_station, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(rail_station)
}



Rail_Shape=function(access_token, operator, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(operator=="TRA"){
    url="https://tdx.transportdata.tw/api/basic/v3/Rail/TRA/Shape?%24format=JSON"
  }else if(operator=="THSR"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/Shape?%24format=JSON"
  }else if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/Shape/", operator, "?&%24format=JSON")
  }else if(operator=="AFR"){
    stop("AFR does not provide route geometry data up to now! Please check out other rail system.")
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    if(operator %in% c("TRA","AFR")){
      rail_shape=fromJSON(content(x, as="text"))$Shapes
    }else{
      rail_shape=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  rail_shape$LineName=rail_shape$LineName$Zh_tw
  rail_shape=rename(rail_shape, geometry=Geometry)%>%
    dplyr::select(LineID, LineName, geometry)

  cat(paste0("#---", operator, " Shape Downloaded---#\n"))

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_shape, out, row.names=F)
    }
    return(rail_shape)
  }else if(dtype=="sf"){
    rail_shape$geometry=st_as_sfc(rail_shape$geometry)
    rail_shape=st_sf(rail_shape, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(rail_shape, out, layer_options="ENCODING=UTF-8")
    }
    return(rail_shape)
  }
}



Bike_Station=function(access_token, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bike/Station/City/", county, "?&$format=JSON")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bike_station=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })
  if("Message" %in% names(bike_station)){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' has no bike sharing system or data is not avaliable.\n", bike_station$Message))
    }
    else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }
  bike_station$StationName=bike_station$StationName$Zh_tw
  bike_station$StationAddress=bike_station$StationAddress$Zh_tw
  bike_station$PositionLon=bike_station$StationPosition$PositionLon
  bike_station$PositionLat=bike_station$StationPosition$PositionLat
  col_req=c("StationUID", "StationID", "StationName", "PositionLon", "PositionLat", "StationAddress", "BikesCapacity", "ServiceType")
  col_req=col_req[col_req %in% names(bike_station)]
  bike_station=dplyr::select(bike_station, col_req)

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bike_station, out, row.names=F)
    }
    return(bike_station)
  }else if (dtype=="sf"){
    bike_station$geometry=paste0("POINT(", bike_station$PositionLon, " ", bike_station$PositionLat, ")")
    bike_station$geometry=st_as_sfc(bike_station$geometry)
    bike_station=st_sf(bike_station, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(bike_station, out, layer_options="ENCODING=UTF-8")
    }

    return(bike_station)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



Geocoding=function(access_token, address, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(sf)) install.packages("sf")
  if (!require(urltools)) install.packages("urltools")
  if (!require(httr)) install.packages("httr")
  if (!require(progress)) install.packages("progress")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  pb=progress_bar$new(format="(:spin) [:bar] :percent  ", total=length(address), clear=F, width=80)
  address_record=data.frame()
  record_fail=c()

  for (i in c(1:length(address))){
    pb$tick()
    nexti=F
    while (!nexti){
      tryCatch({
        address_temp=gsub("?@", 1, address[i])%>%
          gsub("\\/", "", .) %>%
          url_encode()

        url=paste0("https://tdx.transportdata.tw/api/advanced/V3/Map/GeoCode/Coordinate/Address/", address_temp, "?&$format=JSON")
        add_temp=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))%>%
          content()

        if (length(add_temp)==0){
          cat(paste0("CANNOT Geocode ", AddressOriginal=address[i], "\n"))
          record_fail=c(record_fail, address[i])
        }else if (length(add_temp)==14){
          stop("Your access token is invalid!")
        }else{
          add_temp=data.frame(AddressOriginal=address[i],
                              AddressNew=add_temp[[1]]$Address,
                              geometry=add_temp[[1]]$Geometry)
          address_record=rbind(address_record, add_temp)
        }
        nexti=T

      }, error=function(err){
        # cat(paste0("ERROR:", conditionMessage(err), "\n"))
        if (grepl("externalptr", conditionMessage(err))){
          cat(paste0("Reconnect!\n"))
          nexti=F
        }else if(grepl("subscript out of bounds", conditionMessage(err))){
          cat(paste0("CANNOT Geocode ", AddressOriginal=address[i], "\n"))
          record_fail=c(record_fail, address[i])
          nexti=T
        }else{
          stop("Your access token is invalid!")
        }
      })
    }
  }

  datanum_ori=nrow(address_record)
  address_record=distinct(address_record)
  datanum_rev=nrow(address_record)

  cat("Geocoding Summary",
      paste0("Total:      ", length(address)),
      paste0("Success:    ", datanum_rev),
      paste0("Duplicated: ", datanum_ori-datanum_rev),
      paste0("Fail:       ", length(record_fail)),
      sep="\n")

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(address_record, out, row.names=F)
    }
    return(list(SUCCESS=address_record, FAIL=record_fail))
  }else if (dtype=="sf"){
    address_record$geometry=st_as_sfc(address_record$geometry)
    address_record=st_sf(address_record, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(address_record, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'\n")
    }

    return(list(SUCCESS=address_record, FAIL=record_fail))
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



Road_Network=function(access_token, county, roadclass, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(sf)) install.packages("sf")
  if (!require(httr)) install.packages("httr")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if (county %in% c(TDX_County$Code[1:22], "ALL") & roadclass %in% c(0,1,3,"ALL")){
    if (county!="ALL" & roadclass=="ALL"){
      # road of specific county and all road class
      url=paste0("https://tdx.transportdata.tw/api/basic/V3/Map/Road/Network/City/", county, "?&%24format=JSON")
      skip_flag=F
    }else if (county!="ALL" & roadclass %in% c(0,1,3)){
      # road of specific county and specific road class
      url=paste0("https://tdx.transportdata.tw/api/basic/V3/Map/Road/Network/City/", county, "?%24filter=RoadClass%20eq%20%27", roadclass, "%27&%24format=JSON")
      skip_flag=F
    }else if (county=="ALL" & roadclass %in% c(0,1,3)){
      # road of all county and specific road class
      url=paste0("https://tdx.transportdata.tw/api/basic/V3/Map/Road/Network/RoadClass/", roadclass, "?&$format=JSON")
      skip_flag=F
    }else if (county=="ALL" & roadclass=="ALL"){
      # road of all county and all road class
      road=data.frame()
      for (i in c(0,1,3)){
        url=paste0("https://tdx.transportdata.tw/api/basic/V3/Map/Road/Network/RoadClass/", i, "?&$format=JSON")
        x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

        tryCatch({
          temp=fromJSON(content(x, as="text"))
        }, error=function(err){
          stop(paste0("Your access token is invalid!"))
        })
        temp=temp[, c("RoadClass","RoadClassName","RoadID","RoadName","RoadNameID","Geometry")]%>%
          rename(geometry=Geometry)

        road=rbind(road, temp)
        cat(paste0("Road Class ", i, " Downloaded\n"))
        skip_flag=T
      }
    }

    if(skip_flag==F){
      x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

      tryCatch({
        road=fromJSON(content(x, as="text"))
      }, error=function(err){
        stop(paste0("Your access token is invalid!"))
      })

      if(class(road)=="data.frame"){
        road=road[, c("RoadClass","RoadClassName","RoadID","RoadName","RoadNameID","Geometry")]%>%
          rename(geometry=Geometry)
      }else{
        stop(paste0("'", county, " has no RoadClass ", roadclass))
      }
    }

  }else if(!(county %in% c(TDX_County$Code[1:22], "ALL"))){
    print(c(TDX_County$Code[1:22], "ALL"))
    stop(paste0("'", county, "' is invalid county code. Please check out the parameter table above."))
  }else if(!(RoadClass %in% c(0,1,3,"ALL"))){
    print(TDX_RoadClass)
    stop(paste0("'", RoadClass, "' is invalid RoadClass code. Please check out the parameter table above."))
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(road, out, row.names=F)
    }
    return(road)
  }else if (dtype=="sf"){
    road$geometry=st_as_sfc(road$geometry)
    road=st_sf(road, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(road, out, layer_options="ENCODING=UTF-8")
    }
    return(road)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



Rail_TimeTable=function(access_token, operator, record, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if (record=="station"){
    if (operator=="TRA"){
      url="https://tdx.transportdata.tw/api/basic/v3/Rail/TRA/GeneralStationTimetable?&$format=JSON"
    }else if (operator=="THSR"){
      stop("THSR does not provide 'station' time table up to now! Please use 'general' time table.")
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT")){
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/StationTimeTable/", operator, "?&%24format=JSON")
    }else if (operator %in% c("TMRT","AFR")){
      stop(paste0(operator, " does not provide 'station' time table up to now! Please check out other rail system."))
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above."))
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      if(operator=="TRA"){
        data_all=fromJSON(content(x, as="text"))$StationTimetables
      }else{
        data_all=fromJSON(content(x, as="text"))
      }
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })

    if (operator=="TRA"){
      data_all$StationName=data_all$StationName$Zh_tw
      station=data_all[, c("StationID","StationName","Direction")]%>%
        cbind(data_all$ServiceDay)

      rail_timetable_temp=data.frame(Sequence=unlist(mapply(function(x) data_all$Timetables[[x]]$Sequence, c(1:nrow(data_all)))),
                                     TrainNo=unlist(mapply(function(x) data_all$Timetables[[x]]$TrainNo, c(1:nrow(data_all)))),
                                     DestinationStationID=unlist(mapply(function(x) data_all$Timetables[[x]]$DestinationStationID, c(1:nrow(data_all)))),
                                     DestinationStationName=unlist(mapply(function(x) data_all$Timetables[[x]]$DestinationStationName$Zh_tw, c(1:nrow(data_all)))),
                                     TrainTypeID=unlist(mapply(function(x) data_all$Timetables[[x]]$TrainTypeID, c(1:nrow(data_all)))),
                                     TrainTypeCode=unlist(mapply(function(x) data_all$Timetables[[x]]$TrainTypeCode, c(1:nrow(data_all)))),
                                     TrainTypeName=unlist(mapply(function(x) data_all$Timetables[[x]]$TrainTypeName$Zh_tw, c(1:nrow(data_all)))),
                                     ArrivalTime=unlist(mapply(function(x) data_all$Timetables[[x]]$ArrivalTime, c(1:nrow(data_all)))),
                                     DepartureTime=unlist(mapply(function(x) data_all$Timetables[[x]]$DepartureTime, c(1:nrow(data_all)))))
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT")){
      data_all$StationName=data_all$StationName$Zh_tw
      data_all$DestinationStationName=data_all$DestinationStationName$Zh_tw
      station=data_all[, c("RouteID","LineID","StationID","StationName","Direction","DestinationStaionID","DestinationStationName")]%>%
        cbind(data_all$ServiceDay)

      rail_timetable_temp=data.frame(Sequence=unlist(mapply(function(x) data_all$Timetables[[x]]$Sequence, c(1:nrow(data_all)))),
                                     ArrivalTime=unlist(mapply(function(x) data_all$Timetables[[x]]$ArrivalTime, c(1:nrow(data_all)))),
                                     DepartureTime=unlist(mapply(function(x) data_all$Timetables[[x]]$DepartureTime, c(1:nrow(data_all)))))
    }
    num_of_table=mapply(function(x) nrow(data_all$Timetables[[x]]), c(1:nrow(data_all)))
    station=station[rep(c(1:nrow(station)), num_of_table), ]
    rail_timetable=cbind(station, rail_timetable_temp)
  }else if (record=="general"){
    if (operator=="TRA"){
      url="https://tdx.transportdata.tw/api/basic/v3/Rail/TRA/GeneralTrainTimetable?&$format=JSON"
    }else if (operator=="THSR"){
      url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/GeneralTimetable?&$format=JSON"
    }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","KLRT","TMRT")){
      stop("MRT system does not provide 'general' time table up to now! Please use 'station' time table.")
    }else if (operator =="AFR"){
      url="https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/GeneralTrainTimetable?&%24format=JSON"
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      if(operator %in% c("TRA","AFR")){
        data_all=fromJSON(content(x, as="text"))$TrainTimetables
      }else{
        data_all=fromJSON(content(x, as="text"))$GeneralTimetable
      }
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })

    if (operator %in% c("TRA","AFR")){
      tra_info=cbind(data_all$TrainInfo, data_all$ServiceDay)
      tra_info$TrainTypeName=tra_info$TrainTypeName$Zh_tw
      tra_info$StartingStationName=tra_info$StartingStationName$Zh_tw
      tra_info$EndingStationName=tra_info$EndingStationName$Zh_tw
      rail_timetable_temp=data.frame(StopSequence=unlist(mapply(function(x) data_all$StopTimes[[x]]$StopSequence, c(1:nrow(data_all)))),
                                     StationID=unlist(mapply(function(x) data_all$StopTimes[[x]]$StationID, c(1:nrow(data_all)))),
                                     StationName=unlist(mapply(function(x) data_all$StopTimes[[x]]$StationName$Zh_tw, c(1:nrow(data_all)))),
                                     ArrivalTime=unlist(mapply(function(x) data_all$StopTimes[[x]]$ArrivalTime, c(1:nrow(data_all)))),
                                     DepartureTime=unlist(mapply(function(x) data_all$StopTimes[[x]]$DepartureTime, c(1:nrow(data_all)))))

      num_of_station=mapply(function(x) nrow(data_all$StopTimes[[x]]), c(1:nrow(data_all)))
      tra_info=tra_info[rep(c(1:nrow(tra_info)), num_of_station), ]
      rail_timetable=cbind(tra_info, rail_timetable_temp)
    }else if (operator=="THSR"){
      temp=data.frame(TrainNo=data_all$GeneralTrainInfo$TrainNo,
                      Direction=data_all$GeneralTrainInfo$Direction,
                      StartingStationID=data_all$GeneralTrainInfo$StartingStationID,
                      EndingStationID=data_all$GeneralTrainInfo$EndingStationID)
      tra_info=cbind(temp, data_all$ServiceDay)

      rail_timetable_temp=data.frame(StopSequence=unlist(mapply(function(x) data_all$StopTimes[[x]]$StopSequence, c(1:nrow(data_all)))),
                                     StationID=unlist(mapply(function(x) data_all$StopTimes[[x]]$StationID, c(1:nrow(data_all)))),
                                     StationName=unlist(mapply(function(x) data_all$StopTimes[[x]]$StationName$Zh_tw, c(1:nrow(data_all)))),
                                     ArrivalTime=unlist(mapply(function(x) data_all$StopTimes[[x]]$ArrivalTime, c(1:nrow(data_all)))),
                                     DepartureTime=unlist(mapply(function(x) data_all$StopTimes[[x]]$DepartureTime, c(1:nrow(data_all)))))

      num_of_station=mapply(function(x) nrow(data_all$StopTimes[[x]]), c(1:nrow(data_all)))
      tra_info=tra_info[rep(c(1:nrow(tra_info)), num_of_station), ]
      rail_timetable=cbind(tra_info, rail_timetable_temp)
    }
  }else{
    stop("'", record, "' is not valid format of timetable. Please use 'station' or 'general'.")
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(rail_timetable, out, row.names=F)
  }
  return(rail_timetable)
}



Bike_Shape=function(access_token, county, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  url=paste0("https://tdx.transportdata.tw/api/basic/v2/Cycling/Shape/City/", county, "?&$format=JSON")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bike_shape=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })
  if("Message" %in% names(bike_shape) | length(bike_shape)==0){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' has no cycling path or data is not avaliable.\n", bike_station$Message))
    }
    else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bike_shape, out, row.names=F)
    }
  }else if (dtype=="sf"){
    bike_shape=rename(bike_shape, geometry=Geometry)
    bike_shape$geometry=st_as_sfc(bike_shape$geometry)
    bike_shape=st_sf(bike_shape, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(bike_shape, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(bike_shape)
}



Air_Schedule=function(access_token, domestic=T, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if (domestic){
    url="https://tdx.transportdata.tw/api/basic/v2/Air/GeneralSchedule/Domestic?$format=JSON"
  }else{
    url="https://tdx.transportdata.tw/api/basic/v2/Air/GeneralSchedule/International?$format=JSON"
  }

  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    air_schedule=fromJSON(content(x, as="text"))
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))
    stop(paste0("Your access token is invalid!"))
  })

  air_schedule$CodeShare_AirlineID=mapply(function(x) ifelse(is.null(air_schedule$CodeShare[[x]]$AirlineID), NA, paste(air_schedule$CodeShare[[x]]$AirlineID, collapse="|")), c(1:nrow(air_schedule)))
  air_schedule$CodeShare_FlightNumber=mapply(function(x) ifelse(is.null(air_schedule$CodeShare[[x]]$FlightNumber), NA, paste(air_schedule$CodeShare[[x]]$FlightNumber, collapse="|")), c(1:nrow(air_schedule)))
  air_schedule=dplyr::select(air_schedule, -CodeShare, -VersionID, -UpdateTime)

  if (nchar(out)!=0 & out!=F){
    write.csv(air_schedule, out, row.names=F)
  }
  return(air_schedule)
}



Tourism=function(access_token, county, poi, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if (!(county %in% c(TDX_County$Code[1:22], "ALL"))){
    print(TDX_County[1:22,])
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }else if (!(poi %in% c("ScenicSpot","Restaurant","Hotel"))){
    stop(paste0("City: '", poi, "' is not valid. Please use the 'ScenicSpot', 'Restaurant', or 'Hotel'."))
  }

  if (county=="ALL"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Tourism/", poi, "?&%24format=JSON")
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Tourism/", poi, "/", county, "?&$format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    poidf=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  if(length(poidf)==0){
    stop(paste0("'", poi, "' data of '", county, "' is not avaliable.\n"))
  }else{
    poidf=cbind(poidf, poidf$Position)
    if(poi=="ScenicSpot"){
      req_col=c("ScenicSpotID","ScenicSpotName","DescriptionDetail","Description","Address","ZipCode","Phone","WebsiteUrl","OpenTime","Level","Class1","Class2","Class3","City","PositionLon","PositionLat","TravelInfo")
      req_col=req_col[req_col %in% names(poidf)]
      poidf=poidf[, req_col]
    }else if(poi=="Restaurant"){
      req_col=c("RestaurantID","RestaurantName","Description","Address","Phone","OpenTime","Class","City","PositionLon","PositionLat")
      req_col=req_col[req_col %in% names(poidf)]
      poidf=poidf[, req_col]
    }else if(poi=="Hotel"){
      req_col=c("HotelID","HotelName","Description","Address","ZipCode","Phone","Fax","WebsiteUrl","OpenTime","Class","Spec","ServiceInfo","ParkingInfo","Grade","City","PositionLon","PositionLat")
      req_col=req_col[req_col %in% names(poidf)]
      poidf=poidf[, req_col]
    }
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(poidf, out, row.names=F)
    }
    return(poidf)
  }else if (dtype=="sf"){
    poidf$geometry=st_as_sfc(paste0("POINT(", poidf$PositionLon, " ", poidf$PositionLat, ")"))
    poidf=st_sf(poidf, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(poidf, out, layer_options="ENCODING=UTF-8")
    }
    return(poidf)
  }
}



Bus_TravelTime=function(access_token, county, routeid, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  cat(paste0("Total: ", length(routeid), " Routes\n"))

  traveltime_ALL=data.frame()
  for (route in routeid){
    if (county=="Intercity"){
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/S2STravelTime/InterCity/", route, "?&%24format=XML")
    }else{
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/S2STravelTime/City/", county, "/", route, "?&%24format=XML")
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))

      if (grepl("Unauthorized", conditionMessage(err))){
        stop(paste0("Your access token is invalid!"))
      }else{
        print(TDX_County)
        stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
      }
    })

    subroute_info=data.frame(RouteUID=xml_text(xml_find_all(x, xpath=".//d1:RouteUID")),
                             RouteID=xml_text(xml_find_all(x, xpath=".//d1:RouteID")),
                             SubRouteUID=xml_text(xml_find_all(x, xpath=".//d1:SubRouteUID")),
                             SubRouteID=xml_text(xml_find_all(x, xpath=".//d1:SubRouteID")),
                             Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")))

    subroute_info=as.data.frame(lapply(subroute_info, rep, xml_length(xml_find_all(x, xpath = ".//d1:TravelTimes"))))

    week_info=data.frame(Weekday=xml_text(xml_find_all(x, xpath = ".//d1:Weekday")),
                         StartHour=xml_text(xml_find_all(x, xpath = ".//d1:StartHour")),
                         EndHour=xml_text(xml_find_all(x, xpath = ".//d1:EndHour")))
    week_info=cbind(subroute_info, week_info)

    num_of_od=xml_length(xml_find_all(x, xpath = ".//d1:S2STimes"))

    traveltime_temp=data.frame(FromStopID=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:FromStopID"))),
                               ToStopID=xml_text(xml_find_all(x, xpath=".//d1:ToStopID")),
                               FromStationID=xml_text(xml_find_all(x, xpath=".//d1:FromStationID")),
                               ToStationID=xml_text(xml_find_all(x, xpath=".//d1:ToStationID")),
                               RunTime=xml_text(xml_find_all(x, xpath=".//d1:RunTime")))

    week_info=as.data.frame(lapply(week_info, rep, num_of_od))
    traveltime=cbind(week_info, traveltime_temp)

    traveltime_ALL=rbind(traveltime_ALL, traveltime)
    cat(paste0("#---", which(route==routeid), "_RouteID: ", route, " is downloaded---#\n"))
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(traveltime_ALL, out, row.names=F)
  }
  return(traveltime_ALL)
}



Rail_ODFare=function(access_token, operator, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  if (operator=="TRA"){
    cat("Please wait for a while...\n")
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/ODFare?&%24format=XML"
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))
      stop(paste0("Your access token is invalid!"))
    })

    rail_info=data.frame(OriginStationID=xml_text(xml_find_all(x, xpath=".//d1:OriginStationID")),
                         OriginStationName=xml_text(xml_find_all(x, xpath=".//d1:OriginStationName//d1:Zh_tw")),
                         DestinationStationID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationID")),
                         DestinationStationName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationName//d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")))
  }else if (operator=="THSR"){
    url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/ODFare?&%24format=XML"
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))
      stop(paste0("Your access token is invalid!"))
    })

    rail_info=data.frame(OriginStationID=xml_text(xml_find_all(x, xpath=".//d1:OriginStationID")),
                         OriginStationName=xml_text(xml_find_all(x, xpath=".//d1:OriginStationName//d1:Zh_tw")),
                         DestinationStationID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationID")),
                         DestinationStationName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationName//d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")))
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/ODFare/", operator, "?&%24format=XML")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))
      stop(paste0("Your access token is invalid!"))
    })

    rail_info=data.frame(OriginStationID=xml_text(xml_find_all(x, xpath=".//d1:OriginStationID")),
                         OriginStationName=xml_text(xml_find_all(x, xpath=".//d1:OriginStationName//d1:Zh_tw")),
                         DestinationStationID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationID")),
                         DestinationStationName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationName//d1:Zh_tw")),
                         TrainType=xml_text(xml_find_all(x, xpath=".//d1:TrainType")))
  }else if (operator=="AFR"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/ODFare?&%24format=XML")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))
      stop(paste0("Your access token is invalid!"))
    })

    rail_info=data.frame(OriginStationID=xml_text(xml_find_all(x, xpath=".//d1:OriginStationID")),
                         OriginStationName=xml_text(xml_find_all(x, xpath=".//d1:OriginStationName//d1:Zh_tw")),
                         DestinationStationID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationID")),
                         DestinationStationName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStationName//d1:Zh_tw")),
                         TrainType=xml_text(xml_find_all(x, xpath=".//d1:TrainType")))
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not allowed operator. Please check out the table of railway code above"))
  }

  if (operator=="TRA"){
    odfare_temp=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                           Price=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:Price"))))
  }else if (operator=="THSR"){
    odfare_temp=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                           FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
                           CabinClass=xml_text(xml_find_all(x, xpath=".//d1:CabinClass")),
                           Price=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:Price"))))
  }else if (operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    odfare_temp=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                           FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
                           Price=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:Price"))))
  }else if (operator=="AFR"){
    odfare_temp=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                           FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
                           CabinClass=xml_text(xml_find_all(x, xpath=".//d1:CabinClass")),
                           Price=as.numeric(xml_text(xml_find_all(x, xpath=".//d1:Price"))))
  }

  rail_info=as.data.frame(lapply(rail_info, rep, xml_length(xml_find_all(x, xpath = ".//d1:Fares"))))
  odfare=cbind(rail_info, odfare_temp)

  if (nchar(out)!=0 & out!=F){
    write.csv(odfare, out, row.names=F)
  }
  return(odfare)
}



Car_Park=function(access_token, county, street, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(street=="off"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v1/Parking/OffStreet/CarPark/City/", county, "?&%24format=XML")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))

      if (grepl("Unauthorized", conditionMessage(err))){
        stop(paste0("Your access token is invalid!"))
      }else{
        print(TDX_County)
        stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
      }
    })

    xml_node=c("CarParkID","CarParkName//d1:Zh_tw","OperatorID","Description","Telephone","PositionLat","PositionLon","Address","FareDescription","IsPublic","OperationType",
               "LiveOccuppancyAvailable","EVRechargingAvailable","MonthlyTicketAvailable","SeasonTicketAvailable","ReservationAvailable","WheelchairAccessible","OvernightPermitted")
    xml_node_name=c("CarParkID","CarParkName","OperatorID","Description","Telephone","PositionLat","PositionLon","Address","FareDescription","IsPublic","OperationType",
                    "LiveOccuppancyAvailable","EVRechargingAvailable","MonthlyTicketAvailable","SeasonTicketAvailable","ReservationAvailable","WheelchairAccessible","OvernightPermitted")

    carpark=data.frame(temp_id=c(1:length(xml_find_all(x, xpath = ".//d1:CarPark"))))
    for (i in xml_node){
      node_name=xml_node_name[which(xml_node==i)]
      temp=xml_text(xml_find_all(x, xpath=paste0(".//d1:", i)))
      temp_id=grepl(node_name, xml_find_all(x, xpath=".//d1:CarPark"))
      temp_id=which(temp_id)
      temp=data.frame(temp_id, temp)
      colnames(temp)[2]=node_name
      carpark=left_join(carpark, temp, by="temp_id")
      cat(paste0("Attribute '", node_name, "' is parsed\n"))
    }
    carpark=select(carpark, -temp_id)

    if (dtype=="text"){
      if (nchar(out)!=0 & out!=F){
        write.csv(carpark, out, row.names=F)
      }
      return(carpark)
    }else if (dtype=="sf"){
      carpark$Geometry=st_as_sfc(paste0("POINT(", carpark$PositionLon, " ", carpark$PositionLat, ")"))
      carpark=st_sf(carpark, crs=4326)

      if (grepl(".shp", out) & out!=F){
        write_sf(carpark, out, layer_options="ENCODING=UTF-8")
      }else if (!(grepl(".shp", out)) & out!=F){
        stop("The file name must contain '.shp'\n")
      }

      return(carpark)
    }else{
      stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
    }
  }else if(street=="on"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v1/Parking/OnStreet/ParkingSpot/City/", county, "?%24format=XML")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      x=read_xml(x)
    }, error=function(err){
      cat(paste0("ERROR: ", conditionMessage(err), "\n"))

      if (grepl("Unauthorized", conditionMessage(err))){
        stop(paste0("Your access token is invalid!"))
      }else{
        print(TDX_County)
        stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
      }
    })

    xml_node=c("ParkingSegmentID","ParkingSpotID","PositionLat","PositionLon","SpaceType","HasChargingPoint","Geometry")

    carpark=data.frame(temp_id=c(1:length(xml_find_all(x, xpath = ".//d1:ParkingSegmentSpot"))))
    for (i in xml_node){
      node_name=xml_node[which(xml_node==i)]
      temp=xml_text(xml_find_all(x, xpath=paste0(".//d1:", i)))
      temp_id=grepl(node_name, xml_find_all(x, xpath=".//d1:ParkingSegmentSpot"))
      temp_id=which(temp_id)
      temp=data.frame(temp_id, temp)
      colnames(temp)[2]=node_name
      carpark=left_join(carpark, temp, by="temp_id")
      cat(paste0("Attribute '", node_name, "' is parsed\n"))
    }
    carpark=select(carpark, -temp_id)%>%
      filter(!is.na(ParkingSegmentID))

    if(nrow(carpark)==0){
      stop(paste0("Data of on street parking in '", county, "' has not been uploaded up to now."))
    }

    if (dtype=="text"){
      if (nchar(out)!=0 & out!=F){
        write.csv(carpark, out, row.names=F)
      }
      return(carpark)
    }else if (dtype=="sf"){
      if(sum(is.na(carpark$Geometry))>0){
        cat("Data provides 'POINT' geometry.\n")
        carpark$Geometry=st_as_sfc(paste0("POINT(", carpark$PositionLon, " ", carpark$PositionLat, ")"))
        carpark=st_sf(carpark, crs=4326)
      }else{
        cat("Data provides 'POLYGON' geometry.\n")
        carpark$Geometry=st_as_sfc(carpark$Geometry)
        carpark=st_sf(carpark, crs=3826)
      }

      if (grepl(".shp", out) & out!=F){
        write_sf(carpark, out, layer_options="ENCODING=UTF-8")
      }else if (!(grepl(".shp", out)) & out!=F){
        stop("The file name must contain '.shp'\n")
      }

      return(carpark)
    }else{
      stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
    }
  }else{
    stop("'street' must be 'on' or 'off'.")
  }
}



Ship_Port=function(access_token, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  url="https://tdx.transportdata.tw/api/basic/v3/Ship/Port?&%24format=XML"
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    x=read_xml(x)
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))
    stop(paste0("Your access token is invalid!"))
  })

  shipport=data.frame(PortID=xml_text(xml_find_all(x, xpath=".//d1:PortID")),
                      PortName=xml_text(xml_find_all(x, xpath=".//d1:PortName//d1:Zh_tw")),
                      PositionLat=xml_text(xml_find_all(x, xpath=".//d1:PositionLat")),
                      PositionLon=xml_text(xml_find_all(x, xpath=".//d1:PositionLon")),
                      City=xml_text(xml_find_all(x, xpath=".//d1:City")))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(shipport, out, row.names=F)
    }
    return(shipport)
  }else if (dtype=="sf"){
    shipport$Geometry=st_as_sfc(paste0("POINT(", shipport$PositionLon, " ", shipport$PositionLat, ")"))
    shipport=st_sf(shipport, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(shipport, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'\n")
    }

    return(shipport)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



Ship_Route=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/Route/Domestic/City/", county, "?&%24format=XML")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    x=read_xml(x)
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))

    if (grepl("Unauthorized", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above. Or it might becasue there is no ship service in '", county, "'."))
    }
  })

  shiproute=data.frame(RouteID=xml_text(xml_find_all(x, xpath=".//d1:RouteID")),
                       RouteName=xml_text(xml_find_all(x, xpath=".//d1:RouteName//d1:Zh_tw")),
                       StartPortID=xml_text(xml_find_all(x, xpath=".//d1:StartPortID")),
                       StartPortName=xml_text(xml_find_all(x, xpath=".//d1:StartPortName")),
                       EndPortID=xml_text(xml_find_all(x, xpath=".//d1:EndPortID")),
                       EndPortName=xml_text(xml_find_all(x, xpath=".//d1:EndPortName")),
                       Description=xml_text(xml_find_all(x, xpath=".//d1:Description")),
                       TicketPriceDescription=xml_text(xml_find_all(x, xpath=".//d1:TicketPriceDescription")),
                       RouteType=xml_text(xml_find_all(x, xpath=".//d1:RouteType")),
                       RouteDistance=xml_text(xml_find_all(x, xpath=".//d1:RouteDistance")))

  if (nchar(out)!=0 & out!=F){
    write.csv(shiproute, out, row.names=F)
  }
  return(shiproute)
}



Ship_StopOfRoute=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/StopOfRoute/Domestic/City/", county, "?&%24format=XML")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    x=read_xml(x)
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))

    if (grepl("Unauthorized", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above. Or it might becasue there is no ship service in '", county, "'."))
    }
  })

  stopofroute=data.frame(RouteID=xml_text(xml_find_all(x, xpath=".//d1:RouteID")),
                         RouteName=xml_text(xml_find_all(x, xpath=".//d1:RouteName//d1:Zh_tw")),
                         Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
                         StopSequence=xml_text(xml_find_all(x, xpath=".//d1:StopSequence")),
                         PortID=xml_text(xml_find_all(x, xpath=".//d1:PortID")),
                         PortName=xml_text(xml_find_all(x, xpath=".//d1:PortName//d1:Zh_tw")))

  stopofroute=arrange(stopofroute, RouteID, Direction, StopSequence)

  if (nchar(out)!=0 & out!=F){
    write.csv(stopofroute, out, row.names=F)
  }
  return(stopofroute)
}



Bus_RouteFare=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")

  url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/RouteFare/City/", county, "?&%24format=XML")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    x=read_xml(x)
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))

    if (grepl("Unauthorized", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above. Or it might becasue there is no API service for '", county, "' up to now."))
    }
  })

  bus_info=data.frame(RouteID=xml_text(xml_find_all(x, xpath=".//d1:RouteID")),
                      RouteName=xml_text(xml_find_all(x, xpath=".//d1:RouteName")),
                      OperatorID=xml_text(xml_find_all(x, xpath=".//d1:OperatorID")),
                      SubRouteID=xml_text(xml_find_all(x, xpath=".//d1:SubRouteID")),
                      SubRouteName=xml_text(xml_find_all(x, xpath=".//d1:SubRouteName")),
                      FarePricingType=xml_text(xml_find_all(x, xpath=".//d1:FarePricingType")),
                      IsFreeBus=xml_text(xml_find_all(x, xpath=".//d1:IsFreeBus")))

  if (unique(bus_info$FarePricingType)==0){
    temp=xml_find_all(x, xpath = ".//d1:SectionFares")
    num_of_buffer=lengths(gregexpr("BufferZones", temp))/2
    num_of_fares=xml_length(xml_find_all(x, xpath = ".//d1:SectionFare"))-num_of_buffer

    route_buffer=data.frame(SectionSequence=xml_text(xml_find_all(x, xpath=".//d1:SectionSequence")),
                            Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
                            BZOStopID=xml_text(xml_find_all(x, xpath=".//d1:FareBufferZoneOrigin//d1:StopID")),
                            BZOStopName=xml_text(xml_find_all(x, xpath=".//d1:FareBufferZoneOrigin//d1:StopName")),
                            BZDStopID=xml_text(xml_find_all(x, xpath=".//d1:FareBufferZoneDestination//d1:StopID")),
                            BZDStopName=xml_text(xml_find_all(x, xpath=".//d1:FareBufferZoneDestination//d1:StopName")))

    bus_info_bz=as.data.frame(lapply(bus_info, rep, num_of_buffer))
    bus_info_bz=cbind(bus_info_bz, route_buffer)

    route_fare=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                          FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
                          Price=xml_text(xml_find_all(x, xpath=".//d1:Price")))

    bus_info_fare=as.data.frame(lapply(bus_info, rep, num_of_fares))
    bus_info_fare=cbind(bus_info_fare, route_fare)
  }else if (unique(bus_info$FarePricingType)==1){
    cat(paste0("Sorry! Data is too large. '", county, "` is recorded in OD Fare. Please try another county!", "\n"))

    # cat("Please wait for a while...\n")
    # temp=xml_find_all(x, xpath = ".//d1:BusRouteFare")
    # bus_info_od=bus_info[which(grepl("ODFares", temp)),]
    # num_of_odfare=xml_length(xml_find_all(x, xpath = ".//d1:ODFares"))
    #
    # route_fare=data.frame(Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
    #                       OStopID=xml_text(xml_find_all(x, xpath=".//d1:OriginStop//d1:StopID")),
    #                       OStopName=xml_text(xml_find_all(x, xpath=".//d1:OriginStop//d1:StopName")),
    #                       DStopID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStop//d1:StopID")),
    #                       DStopName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStop//d1:StopName")),
    #                       TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
    #                       FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
    #                       Price=xml_text(xml_find_all(x, xpath=".//d1:Price")))
    #
    # bus_info_od=as.data.frame(lapply(bus_info_od, rep, num_of_odfare))
    # bus_info_fare=cbind(bus_info_od, route_fare)
  }else if (unique(bus_info$FarePricingType)==2){
    num_of_stagefare=xml_length(xml_find_all(x, xpath = ".//d1:StageFares"))
    num_of_fare=xml_length(xml_find_all(x, xpath = ".//d1:Fares"))

    bus_info_stage=as.data.frame(lapply(bus_info, rep, num_of_stagefare))
    route_info=data.frame(Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
                          OStopID=xml_text(xml_find_all(x, xpath=".//d1:OriginStage//d1:StopID")),
                          OStopName=xml_text(xml_find_all(x, xpath=".//d1:OriginStage//d1:StopName")),
                          DStopID=xml_text(xml_find_all(x, xpath=".//d1:DestinationStage//d1:StopID")),
                          DStopName=xml_text(xml_find_all(x, xpath=".//d1:DestinationStage//d1:StopName")))

    bus_info_stage=cbind(bus_info_stage, route_info)

    route_fare=data.frame(TicketType=xml_text(xml_find_all(x, xpath=".//d1:TicketType")),
                          FareClass=xml_text(xml_find_all(x, xpath=".//d1:FareClass")),
                          Price=xml_text(xml_find_all(x, xpath=".//d1:Price")))

    bus_info_fare=as.data.frame(lapply(bus_info_stage, rep, num_of_fare))
    bus_info_fare=cbind(bus_info_stage, route_fare)
  }


  # if (nchar(out)!=0 & out!=F){
  #   write.csv(shiproute, out, row.names=F)
  # }

  if (unique(bus_info$FarePricingType)==0){
    return(list(BufferZone=bus_info_bz, ZoneFare=bus_info_fare))
  }else if (unique(bus_info$FarePricingType)==2){
    return(bus_info_fare)
  }
}



Bike_Remain_His=function(access_token, county, dates, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(httr)) install.packages("httr")

  url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bike/Availability/", county, "?Dates=", dates, "&%24format=JSONL")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  bike_remain=read.csv(textConnection(content(x, 'text')), header=F)%>%
    select(-V7)
  bike_remain$V1=substr(bike_remain$V1, regexpr(":", bike_remain$V1)+1, 1000)
  bike_remain$V2=substr(bike_remain$V2, regexpr(":", bike_remain$V2)+1, 1000)
  bike_remain$V3=substr(bike_remain$V3, regexpr(":", bike_remain$V3)+1, 1000)
  bike_remain$V4=substr(bike_remain$V4, regexpr(":", bike_remain$V4)+1, 1000)
  bike_remain$V5=substr(bike_remain$V5, regexpr(":", bike_remain$V5)+1, 1000)
  bike_remain$V6=substr(bike_remain$V6, regexpr(":", bike_remain$V6)+1, 1000)

  colnames(bike_remain)=c("StationUID","StationID","ServiceAvailable","AvailableRentBikes","AvailableReturnBikes","SrcUpdateTime")

  if (nchar(out)!=0 & out!=F){
    write.csv(bike_remain, out, row.names=F)
  }
  return(bike_remain)
}



Freeway_Shape=function(geotype, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(geotype=="section"){
    # SectionID
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/Section.xml")
    freeway_section=data.frame(SectionID=xml_text(xml_find_all(x, xpath = ".//d1:SectionID")),
                               SubAuthorityCode=xml_text(xml_find_all(x, xpath = ".//d1:SubAuthorityCode")),
                               SectionName=xml_text(xml_find_all(x, xpath = ".//d1:SectionName")),
                               RoadID=xml_text(xml_find_all(x, xpath = ".//d1:RoadID")),
                               RoadName=xml_text(xml_find_all(x, xpath = ".//d1:RoadName")),
                               RoadClass=xml_text(xml_find_all(x, xpath = ".//d1:RoadClass")),
                               RoadDirection=xml_text(xml_find_all(x, xpath = ".//d1:RoadDirection")),
                               RoadSection_start=xml_text(xml_find_all(x, xpath = ".//d1:Start")),
                               RoadSection_end=xml_text(xml_find_all(x, xpath = ".//d1:End")),
                               SectionLength=xml_text(xml_find_all(x, xpath = ".//d1:SectionLength")),
                               SectionMile_start=xml_text(xml_find_all(x, xpath = ".//d1:StartKM")),
                               SectionMile_end=xml_text(xml_find_all(x, xpath = ".//d1:EndKM")),
                               SpeedLimit=xml_text(xml_find_all(x, xpath = ".//d1:SpeedLimit")))

    # SectionID shape
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/SectionShape.xml")
    freeway_section_shape=data.frame(SectionID=xml_text(xml_find_all(x, xpath = ".//d1:SectionID")),
                                     Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))
    freeway_shape=left_join(freeway_section, freeway_section_shape)%>%
      filter(!is.na(Geometry))

    if(dtype=="sf"){
      freeway_shape=mutate(freeway_shape, Geometry=st_as_sfc(Geometry))%>%
        st_sf(crs=4326)
    }

  }else if(geotype=="link"){
    # LinkID
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/SectionLink.xml")
    freeway_shape=data.frame(SectionID=rep(xml_text(xml_find_all(x, xpath = ".//d1:SectionID")), times=xml_length(xml_find_all(x, xpath = ".//d1:LinkIDs"))),
                             LinkID=xml_text(xml_find_all(x, xpath = ".//d1:LinkID")))
    temp_id=paste0('"', freeway_shape$LinkID, '"')
    temp_id=paste0("[", paste(temp_id, collapse=","), "]")

    # LinkID
    x=POST("https://link.motc.gov.tw/v2/Road/Link/Shape/Geometry/WKT?$format=JSON",
           content_type("application/json"),
           body=temp_id)
    temp=content(x)
    temp=data.frame(LinkID=do.call(rbind, lapply(c(1:length(temp)), function (i) temp[[i]]$LinkID)),
                    Geometry=do.call(rbind, lapply(c(1:length(temp)), function (i) temp[[i]]$Geometry)))%>%
      distinct()
    freeway_shape=left_join(freeway_shape, temp)%>%
      filter(!(is.na(Geometry)))

    if(dtype=="sf"){
      freeway_shape=mutate(freeway_shape, Geometry=st_as_sfc(Geometry))%>%
        st_sf(crs=4326)
    }

  }else if(geotype=="gantry"){
    # GantryID
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/ETag.xml")
    freeway_shape=data.frame(ETagGantryID=xml_text(xml_find_all(x, xpath = ".//d1:ETagGantryID")),
                             LinkID=xml_text(xml_find_all(x, xpath = ".//d1:LinkID")),
                             LocationType=xml_text(xml_find_all(x, xpath = ".//d1:LocationType")),
                             PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:PositionLon")),
                             PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:PositionLat")),
                             RoadID=xml_text(xml_find_all(x, xpath = ".//d1:RoadID")),
                             RoadName=xml_text(xml_find_all(x, xpath = ".//d1:RoadName")),
                             RoadClass=xml_text(xml_find_all(x, xpath = ".//d1:RoadClass")),
                             RoadDirection=xml_text(xml_find_all(x, xpath = ".//d1:RoadDirection")),
                             Start=xml_text(xml_find_all(x, xpath = ".//d1:RoadSection//d1:Start")),
                             End=xml_text(xml_find_all(x, xpath = ".//d1:RoadSection//d1:End")),
                             LocationMile=xml_text(xml_find_all(x, xpath = ".//d1:LocationMile")))

    if(dtype=="sf"){
      freeway_shape=mutate(freeway_shape, Geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
        st_sf(crs=4326)
    }

  }else if(geotype=="gantryod"){
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/ETagPair.xml")
    freeway_shape=data.frame(ETagPairID=xml_text(xml_find_all(x, xpath = ".//d1:ETagPairID")),
                             StartETagGantryID=xml_text(xml_find_all(x, xpath = ".//d1:StartETagGantryID")),
                             EndETagGantryID=xml_text(xml_find_all(x, xpath = ".//d1:EndETagGantryID")),
                             Description=xml_text(xml_find_all(x, xpath = ".//d1:Description")),
                             Distance=xml_text(xml_find_all(x, xpath = ".//d1:Distance")),
                             StartLinkID=xml_text(xml_find_all(x, xpath = ".//d1:StartLinkID")),
                             EndLinkID=xml_text(xml_find_all(x, xpath = ".//d1:EndLinkID")),
                             Geometry=xml_text(xml_find_all(x, xpath = ".//d1:Geometry")))

    if(dtype=="sf"){
      freeway_shape=mutate(freeway_shape, Geometry=st_as_sfc(Geometry))%>%
        st_sf(crs=4326)
    }
  }else if(geotype=="vd"){
    x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/VD.xml")

    xml_node=c("VDID","SubAuthorityCode","BiDirectional","LinkID","Bearing","RoadDirection","LaneNum","ActualLaneNum","VDType",
               "LocationType","DetectionType","PositionLon","PositionLat","RoadID","RoadName","RoadClass","Start","End","LocationMile")

    freeway_shape=data.frame(temp_id=c(1:length(xml_find_all(x, xpath = ".//d1:VD"))))
    for (i in c(1:length(xml_node))){
      node_name=xml_node[i]
      temp=xml_text(xml_find_all(x, xpath=paste0(".//d1:", xml_node[i])))
      temp_id=grepl(node_name, xml_find_all(x, xpath=".//d1:VD"))
      temp_id=which(temp_id)
      temp=data.frame(temp_id, temp)
      colnames(temp)[2]=node_name
      freeway_shape=left_join(freeway_shape, temp, by="temp_id")
    }
    freeway_shape=dplyr::select(freeway_shape, -temp_id)

    if(dtype=="sf"){
      freeway_shape=mutate(freeway_shape, Geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
        st_sf(crs=4326)
    }
  }else{
    stop("'geotype' must be 'section', 'link', 'gantry', or, 'gantryod'.\n")
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(freeway_shape, out, row.names=F)
    }
    return(freeway_shape)
  }else if (dtype=="sf"){
    if (grepl(".shp", out) & out!=F){
      write_sf(freeway_shape, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'\n")
    }

    return(freeway_shape)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



District_Shape=function(access_token, district, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  cat("Please wait for a while...\n")

  url=paste0("https://tdx.transportdata.tw/api/basic/V3/Map/District/Boundary/", ifelse(district=="County", "City", district), "?%24format=XML")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    x=read_xml(x)
  }, error=function(err){
    cat(paste0("ERROR: ", conditionMessage(err), "\n"))

    if (grepl("Unauthorized", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }else{
      stop(paste0("Parameter 'district' should be 'County', 'Town', or 'Village'."))
    }
  })

  if(district=="County"){
    district_shape=data.frame(COUNTYNAME=xml_text(xml_find_all(x, xpath = ".//CityName")),
                              Geometry=xml_text(xml_find_all(x, xpath = ".//Geometry")))
  }else if(district=="Town"){
    district_shape=data.frame(COUNTYNAME=xml_text(xml_find_all(x, xpath = ".//CityName")),
                              TOWNCODE=xml_text(xml_find_all(x, xpath = ".//TownCode")),
                              TownName=xml_text(xml_find_all(x, xpath = ".//TownName")),
                              Geometry=xml_text(xml_find_all(x, xpath = ".//Geometry")))
  }else if(district=="Village"){
    district_shape=data.frame(COUNTYNAME=xml_text(xml_find_all(x, xpath = ".//CityName")),
                              TOWNCODE=xml_text(xml_find_all(x, xpath = ".//TownCode")),
                              TOWNNAME=xml_text(xml_find_all(x, xpath = ".//TownName")),
                              VILLCODE=xml_text(xml_find_all(x, xpath = ".//VillageCode")),
                              VILLNAME=xml_text(xml_find_all(x, xpath = ".//VillageName")),
                              Geometry=xml_text(xml_find_all(x, xpath = ".//Geometry")))
  }

  temp=data.frame(COUNTYNAME=TDX_County$County[1:22],
                  COUNTYCODE=c("63000","65000","68000","66000","67000","64000","10017","10018","10004","10005","10007","10008","10009","10010","10020","10013","10002","10015","10014","09020","10016","09007"))

  district_shape=left_join(district_shape, temp, by="COUNTYNAME")
  district_shape=dplyr::select(district_shape, all_of(c("COUNTYCODE", "COUNTYNAME", names(district_shape)[2:(ncol(district_shape)-1)])))


  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(district_shape, out, row.names=F)
    }
    return(district_shape)
  }else if (dtype=="sf"){
    district_shape$Geometry=st_as_sfc(district_shape$Geometry)
    district_shape=st_sf(district_shape, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(district_shape, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'\n")
    }

    return(district_shape)
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
}



Population=function(district, time, age=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")

  time_rev=paste0(as.numeric(substr(time, 1, regexpr("-", time)-1))-1911, "Y", substr(time, regexpr("-", time)+1, 10), "M")

  # check if the month is March, June, September, December
  if(!substr(time, regexpr("-", time)+1, 10) %in% c("03","06","09","12")){
    stop("The month must be March, June, September, or December. Note that the demographic data in Taiwan is updated every three months!")
  }
  if(as.numeric(substr(time, 1, regexpr("-", time)-1))<2008){
    stop("Only year after 2008 is provided!")
  }

  if(district=="County"){
    if(age){
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=3025FF02BBFBF102CCDA8BAA874B0F4E&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B&TYPE=CSV")
    }else{
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=3025FF02BBFBF1024A2ACA584EFA2EF4&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B&TYPE=CSV")
    }
  }else if(district=="Town"){
    if(age){
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=3025FF02BBFBF1026E41E92D5357C450&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUBBOUNDARY=&TYPE=CSV")
  }else{
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=3025FF02BBFBF1025C84B70DD22071F5&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUBBOUNDARY=&TYPE=CSV")
    }
  }else if(district=="Village"){
    if(age){
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=3025FF02BBFBF1026239FE8CBE94A3D3&STTIME=", time_rev, "&STUNIT=U01VI&BOUNDARY=%E5%85%A8%E5%9C%8B&SUBBOUNDARY=&TYPE=CSV")
    }else{
      url=paste0("https://segis.moi.gov.tw/STAT/Generic/Project/GEN_STAT.ashx?method=downloadproductfile&code=0B74191C4E5CA476A7ED44A9BE7FA28C&STTIME=", time_rev, "&STUNIT=U01VI&BOUNDARY=%E5%85%A8%E5%9C%8B&SUBBOUNDARY=&TYPE=CSV")
    }
  }else{
    stop("The argument of 'district' must be 'County', 'Town', or 'Village'.")
  }

  unlink(list.files(tempdir(), full.names=T), recursive=T)
  download.file(url, paste0(tempdir(), "/temp_pop_TDX.zip"), mode="wb", quiet=T)

  untar(paste0(tempdir(), "/temp_pop_TDX.zip"), exdir=paste0(tempdir(), "/temp_pop_TDX"))
  dir_file=dir(dir(paste0(tempdir(), "/temp_pop_TDX"), full.names=T), full.names=T)
  dir_file=dir_file[grepl(".csv", dir_file)]
  population=read.csv(dir_file, fileEncoding="Big5")
  population=population[-1, ]
  unlink(paste0(tempdir(), "/temp_pop_TDX"), recursive=T)
  file.remove(paste0(tempdir(), "/temp_pop_TDX.zip"))

  if(district=="County"){
    colnames(population)[c(1:2)]=c("COUNTYCODE","COUNTYNAME")
  }else if(district=="Town"){
    colnames(population)[c(1:4)]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME")
  }else if(district=="Village"){
    colnames(population)[c(1:6)]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME","VILLCODE","VILLNAME")
    population$VILLCODE=gsub("-", "", population$VILLCODE)
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(population, out, row.names=F)
  }
  return(population)
}



Freeway_History=function(file, date, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(progress)) install.packages("progress")

  if(sum(dir() %in% c("temp_freeway_TDX.zip","temp_freeway_TDX"))){
    stop(paste0("Please remove or rename the directory 'temp_freeway_TDX.zip' and 'temp_freeway_TDX.zip' in advance!!\n"))
  }

  freeway_data=data.frame()
  if(file %in% c("M03A","M04A","M05A","M08A")){
    url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", gsub("-", "", date), "/00/TDCS_", file, "_", gsub("-", "", date), "_000000.csv")
    if(suppressWarnings(ncol(fread(url)))!=0){
      pb=progress_bar$new(format="(:spin) [:bar] :percent  ", total=24*12, clear=F, width=80)
      for(hr in c(0:23)){
        for(minu in seq(0, 55, 5)){
          pb$tick()
          url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", gsub("-", "", date), "/",
                     ifelse(nchar(hr)==1, paste0("0", hr), hr), "/TDCS_", file, "_", gsub("-", "", date), "_",
                     ifelse(nchar(hr)==1, paste0("0", hr), hr), ifelse(nchar(minu)==1, paste0("0", minu), minu), "00.csv")
          temp=fread(url, showProgress=F)
          if(file=="M03A"){
            colnames(temp)=c("TimeInterval","GantryID","Direction","VehicleType","Flow")
          }else if(file=="M04A"){
            colnames(temp)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","TravelTime","Flow")
          }else if(file=="M05A"){
            colnames(temp)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","SpaceMeanSpeed","Flow")
          }else if(file=="M08A"){
            colnames(temp)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","Flow")
          }
          freeway_data=rbind(freeway_data, temp)
          cat(paste0(date, " ", ifelse(nchar(hr)==1, paste0("0", hr), hr), ":", ifelse(nchar(minu)==1, paste0("0", minu), minu)))
        }
      }
    }else{
      url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", file, "_", gsub("-", "", date), ".tar.gz")
      download.file(url, "./temp_freeway_TDX.zip", quiet=T)
      untar("temp_freeway_TDX.zip", exdir="temp_freeway_TDX")
      dir_file=paste0(dir("temp_freeway_TDX", full.names=T))
      dir_file=dir(dir_file, full.names=T)
      dir_file=dir(dir_file, full.names=T)
      dir_file=dir(dir_file, full.names=T)
      if(length(dir_file)==0){
        unlink("temp_freeway_TDX", recursive=T)
        file.remove("temp_freeway_TDX.zip")
        stop(paste0("Data of ", date, " is not updated to Traffic Database, Freeway Bureau, MOTC\n Or please check out if your date format is valid!"))
      }
      freeway_data=rbindlist(lapply(dir_file, fread))

      if(file=="M03A"){
        colnames(freeway_data)=c("TimeInterval","GantryID","Direction","VehicleType","Flow")
      }else if(file=="M04A"){
        colnames(freeway_data)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","TravelTime","Flow")
      }else if(file=="M05A"){
        colnames(freeway_data)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","SpaceMeanSpeed","Flow")
      }else if(file=="M08A"){
        colnames(freeway_data)=c("TimeInterval","GantryFrom","GantryTo","VehicleType","Flow")
      }

      unlink("temp_freeway_TDX", recursive=T)
      file.remove("temp_freeway_TDX.zip")
    }
  }else if(file %in% c("M06A","M07A")){
    url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", gsub("-", "", date), "/00/TDCS_", file, "_", gsub("-", "", date), "_000000.csv")

    if(suppressWarnings(ncol(fread(url)))!=0){
      pb=progress_bar$new(format="(:spin) [:bar] :percent  ", total=24, clear=F, width=80)
      for(hr in c(0:23)){
        pb$tick()
        url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", gsub("-", "", date), "/",
                   ifelse(nchar(hr)==1, paste0("0", hr), hr), "/TDCS_", file, "_", gsub("-", "", date), "_",
                   ifelse(nchar(hr)==1, paste0("0", hr), hr), "0000.csv")
        temp=fread(url, showProgress=F)

        if(file=="M06A"){
          colnames(temp)=c("VehicleType","DetectionTime_O","GantryID_O","DetectionTime_D","GantryID_D","TripLength","TripEnd","TripInformation")
        }else if(file=="M07A"){
          colnames(temp)=c("TimeInterval","GantryFrom","VehicleType","TripDistance","Flow")
        }
        freeway_data=rbind(freeway_data, temp)
        cat(paste0(date, " Hour:", ifelse(nchar(hr)==1, paste0("0", hr), hr)))
      }
    }else{
      if(file=="M06A"){
        cat("Please wait for a while...\n")
      }
      url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", file, "_", gsub("-", "", date), ".tar.gz")
      download.file(url, "./temp_freeway_TDX.zip", quiet=T)
      untar("temp_freeway_TDX.zip", exdir="temp_freeway_TDX")
      dir_file=paste0(dir("temp_freeway_TDX", full.names=T))
      dir_file=dir(dir_file, full.names=T)
      dir_file=dir(dir_file, full.names=T)
      dir_file=dir(dir_file, full.names=T)
      if(length(dir_file)==0){
        unlink("temp_freeway_TDX", recursive=T)
        file.remove("temp_freeway_TDX.zip")
        stop(paste0("Data of ", date, " is not updated to Traffic Database, Freeway Bureau, MOTC\n Or please check out if your date format is valid!"))
      }

      freeway_data=rbindlist(lapply(dir_file, fread))

      if(file=="M06A"){
        colnames(freeway_data)=c("VehicleType","DetectionTime_O","GantryID_O","DetectionTime_D","GantryID_D","TripLength","TripEnd","TripInformation")
      }else if(file=="M07A"){
        colnames(freeway_data)=c("TimeInterval","GantryFrom","VehicleType","TripDistance","Flow")
      }

      unlink("temp_freeway_TDX", recursive=T)
      file.remove("temp_freeway_TDX.zip")
    }
  }else{
    stop(paste0("Please use valid `file` parameter, including 'M03A`, 'M04A`, 'M05A`, 'M06A`, 'M07A` and 'M08A`\n"))
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(freeway_data, out, row.names=F)
  }
  return(freeway_data)
}



Bus_RealTime=function(access_token, county, format, dates, out=F){

  if (!(county %in% c(TDX_County$Code, "ALL"))){
    print(TDX_County)
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  if(format=="frequency"){
    url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/RealTimeByFrequency/City/", county, "?Dates=", dates, "&%24format=CSV")
  }else if(format=="stop"){
    url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/RealTimeNearStop/City/", county, "?Dates=", dates, "&%24format=CSV")
  }else{
    stop("Parameter 'format' should be 'frequency' or 'stop'.")
  }

  tryCatch({
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
  }, error=function(err){
    if (grepl("Unauthorized", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })

  if(x$status_code==400){
    stop("Parameter 'dates' should not be more than 7 days!")
  }

  bus_real_time=content(x)

  if (nchar(out)!=0 & out!=F){
    write.csv(bus_real_time, out, row.names=F)
  }
  return(bus_real_time)
}






# Ship_Schedule=function(access_token, county, out=F){
#   if (!require(dplyr)) install.packages("dplyr")
#   if (!require(xml2)) install.packages("xml2")
#   if (!require(httr)) install.packages("httr")
#
#   url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/GeneralSchedule/Domestic/City/", county, "?&%24format=XML")
#   x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
#
#   tryCatch({
#     x=read_xml(x)
#   }, error=function(err){
#     cat(paste0("ERROR: ", conditionMessage(err), "\n"))
#
#     if (grepl("Unauthorized", conditionMessage(err))){
#       stop(paste0("Your access token is invalid!"))
#     }else{
#       print(TDX_County)
#       stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above. Or it might becasue there is no ship service in '", county, "'."))
#     }
#   })
#
#   x=xml_find_all(x, xpath = ".//d1:GeneralSchedules")
#   route_info=data.frame(RouteID=xml_text(xml_find_all(x, xpath=".//d1:RouteID")),
#                         RouteName=xml_text(xml_find_all(x, xpath=".//d1:RouteName//d1:Zh_tw")),
#                         Direction=xml_text(xml_find_all(x, xpath=".//d1:Direction")),
#                         EffectiveDate=xml_text(xml_find_all(x, xpath=".//d1:EffectiveDate")),
#                         ExpireDate=xml_text(xml_find_all(x, xpath=".//d1:ExpireDate")),
#                         OperatorID=xml_text(xml_find_all(x, xpath=".//d1:OperatorID")))
#
#   url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/Operator/Domestic/City/", county, "?&%24format=XML")
#   x_temp=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
#   x_temp=read_xml(x_temp)
#   operator_info=data.frame(OperatorID=xml_text(xml_find_all(x_temp, xpath=".//d1:OperatorID")),
#                         OperatorName=xml_text(xml_find_all(x_temp, xpath=".//d1:OperatorName//d1:Zh_tw")))
#   route_info=left_join(route_info, operator_info)
#
#
#   # check whether the schedule is in timetable form or frequency form
#   tt=as.character(xml_children(x))
#   route_info1=route_info[grepl("Timetables", tt),]
#   route_info2=route_info[grepl("Frequencies", tt),]
#
#   if (nrow(route_info1)!=0){
#     ship_schedule_1=data.frame(temp_id=c(1:length(xml_find_all(x, xpath = ".//d1:Timetable"))))
#
#     xml_node=c("Timetable//d1:ServiceDay//d1:Sunday","Timetable//d1:ServiceDay//d1:Monday","Timetable//d1:ServiceDay//d1:Tuesday",
#                "Timetable//d1:ServiceDay//d1:Wednesday","Timetable//d1:ServiceDay//d1:Thursday","Timetable//d1:ServiceDay//d1:Friday",
#                "Timetable//d1:ServiceDay//d1:Saturday","Timetable//d1:ServiceDay//d1:NationalHolidays","Timetable//d1:StopTimes//d1:StopTime//d1:StopSequence",
#                "Timetable//d1:StopTimes//d1:StopTime//d1:StopUID","Timetable//d1:StopTimes//d1:StopTime//d1:StopName//d1:Zh_tw",
#                "Timetable//d1:StopTimes//d1:StopTime//d1:ArrivalTime","Timetable//d1:StopTimes//d1:StopTime//d1:DepartureTime")
#     xml_node_name=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","NationalHolidays","StopSequence","StopUID","StopName","ArrivalTime","DepartureTime")
#     xml_text(xml_find_all(x, xpath=paste0(".//d1:StopSequence")))
#     for (i in xml_node){
#       node_name=xml_node_name[which(xml_node==i)]
#       temp=xml_text(xml_find_all(x, xpath=paste0(".//d1:Timetables//d1:", i)))
#       temp_id=grepl(node_name, xml_find_all(x, xpath=".//d1:Timetable"))
#       temp_id=which(temp_id)
#       temp=data.frame(temp_id, temp)
#       colnames(temp)[2]=node_name
#       ship_schedule_1=left_join(ship_schedule_1, temp, by="temp_id")
#       cat(paste0("Attribute '", node_name, "' is parsed\n"))
#     }
#   }
#
#
#   xml_length(xml_find_all(x, xpath = ".//d1:Frequencies"))
#
#
#   if (nchar(out)!=0 & out!=F){
#     write.csv(stopofroute, out, row.names=F)
#   }
#   return(stopofroute)
# }





