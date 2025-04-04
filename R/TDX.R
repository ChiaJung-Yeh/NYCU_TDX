library(dplyr)
library(jsonlite)
library(XML)
library(xml2)
library(httr)
library(sf)
library(urltools)
library(cli)
library(data.table)
library(progress)
library(archive)
library(fs)
library(readODS)
library(readxl)
library(rvest)
library(tidyr)

# usethis::use_package("dplyr")
# usethis::use_package("tidyr")
# usethis::use_package("jsonlite")
# usethis::use_package("XML")
# usethis::use_package("xml2")
# usethis::use_package("httr")
# usethis::use_package("sf")
# usethis::use_package("urltools")
# usethis::use_package("cli")
# usethis::use_package("data.table")
# usethis::use_package("progress")
# usethis::use_package("archive")
# usethis::use_package("fs")
# usethis::use_package("readODS")
# usethis::use_package("readxl")
# usethis::use_package("rvest")

# TDX_County=content(GET("https://tdx.transportdata.tw/api/basic/v2/Basic/City?%24format=JSON", add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token))))
# TDX_County=data.frame(Operator=unlist(lapply(TDX_County, function(x) x$CityName)),
#                       Code=unlist(lapply(TDX_County, function(x) x$City)),
#                       CityCode=unlist(lapply(TDX_County, function(x) x$CityCode)))%>%
#   rbind(data.frame(Operator="公路客運", Code="Intercity", CityCode="THB"))
# TDX_County$Operator=factor(TDX_County$Operator, c("臺北市","新北市","基隆市","桃園市","新竹縣","新竹市","苗栗縣","臺中市","彰化縣","南投縣","雲林縣","嘉義縣","嘉義市","臺南市","高雄市","屏東縣","宜蘭縣","花蓮縣","臺東縣","澎湖縣","金門縣","連江縣","公路客運"))
# TDX_County=arrange(TDX_County, Operator)
# write.csv(TDX_County, "C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_county.txt", row.names=F)
# TDX_County=read.csv("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_county.txt")
# usethis::use_data(TDX_County, overwrite=T)
#
# TDX_Railway=read.table("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_railway.txt", encoding="UTF-8", sep=",", header=T)
# usethis::use_data(TDX_Railway, overwrite=T)
#
# TDX_RoadClass=read.table("C:/Users/USER/OneDrive - The University of Sydney (Students)/Desktop/R Transportation/R Github Project/NYCU_TDX/data/tdx_roadclass.txt", encoding="UTF-8", sep=",", header=T)
# usethis::use_data(TDX_RoadClass, overwrite=T)



# #---API Form---#
# url_all=data.frame(Mode=c("Bus","Rail","Bike","Air","Ship"),
#                    url=c("https://tdx.transportdata.tw/webapi/File/Swagger/V3/2998e851-81d0-40f5-b26d-77e2f5ac4118",
#                          "https://tdx.transportdata.tw/webapi/File/Swagger/V3/268fc230-2e04-471b-a728-a726167c1cfc",
#                          "https://tdx.transportdata.tw/webapi/File/Swagger/V3/2cc9b888-a592-496f-99de-9ab35b7fb70d",
#                          "https://tdx.transportdata.tw/webapi/File/Swagger/V3/eb87998f-2f9c-4592-8d75-c62e5b724962",
#                          "https://tdx.transportdata.tw/webapi/File/Swagger/V3/38e6a4c2-5111-4449-8f37-6347caf4a7fc"))
# name_remain=function(x){
#   all_name=c("type","title","format","description")
#   name_remain=all_name[all_name %in% x]
#   return(name_remain)
# }
#
# all_api=data.frame()
# for(k in c(1:nrow(url_all))){
#   api_content=fromJSON(url_all$url[k])
#   for(i in c(1:length(api_content$components$schemas))){
#     temp=bind_rows(mapply(function(x) list(data.frame(rbind(api_content$components$schemas[[i]]$properties[[x]])) %>% select(name_remain(names(.)))), c(1:length(api_content$components$schemas[[i]]$properties))))
#     if(nrow(temp)!=0){
#       temp=cbind(Attribute=names(api_content$components$schemas[[i]]$properties), temp)
#       temp=cbind(API=api_content$components$schemas[[i]]$title, temp)
#       all_api=bind_rows(all_api, cbind(Mode=url_all$Mode[k], temp))
#     }
#   }
# }
# for(i in c(4:7)){
#   all_api[[i]]=mapply(function(x) ifelse(is.null(all_api[[i]][[x]]), NA, all_api[[i]][[x]]), c(1:nrow(all_api)))
# }
# write.csv(all_api, "./others/all_api_attribute.csv", row.names=F)



#---need to update periodically---#
# download.file("https://segis.moi.gov.tw/FileDownload/Download.aspx?u=j0e6LYeQ1bVn3G9Pxpg5fa7y7eVLLN9lAKKOtEZ0Khj4hKtfaYF5I99aRvHukitTJ2QQ%2bfduz9CenperpgRf1w%3d%3d", paste0(tempdir(), "./STATCatalog.xlsx"), mode="wb", quiet=T)
# catalog=readxl::read_xlsx(paste0(tempdir(), "./STATCatalog.xlsx"), sheet=1, skip=1)
# colnames(catalog)=c("TYPE1","TYPE2","DATANAME","TIME","SPACE","UNIT","COLUMN")
# catalog_temp=filter(catalog, DATANAME %in% c("\u884c\u653f\u5340\u91ab\u7642\u9662\u6240\u7d71\u8a08",
#                                         "\u7d71\u8a08\u5340\u91ab\u7642\u9662\u6240\u7d71\u8a08"), !TIME %in% c("97Y","98Y"))%>%
#   group_by(DATANAME, UNIT, TIME)%>%
#   summarise(SPACE=paste(SPACE, collapse="|"))
# write.csv(catalog_temp, "./others/hospital_area_time.csv", row.names=F)
# catalog_temp=filter(catalog, DATANAME %in% c("\u884c\u653f\u5340\u4eba\u53e3\u7d71\u8a08", "\u7d71\u8a08\u5340\u4eba\u53e3\u7d71\u8a08",
#                                              "\u884c\u653f\u5340\u4e94\u6b72\u5e74\u9f61\u7d44\u6027\u5225\u4eba\u53e3\u7d71\u8a08",
#                                              "\u7d71\u8a08\u5340\u4e94\u6b72\u5e74\u9f61\u7d44\u6027\u5225\u4eba\u53e3\u7d71\u8a08"))%>%
#   mutate(DATANAME=case_when(
#     grepl("\u4e94\u6b72\u5e74\u9f61\u7d44\u6027\u5225\u4eba\u53e3\u7d71\u8a08", DATANAME) ~ "\u4e94\u6b72\u5e74\u9f61\u7d44\u6027\u5225\u4eba\u53e3\u7d71\u8a08",
#     grepl("\u4eba\u53e3\u7d71\u8a08", DATANAME) ~ "\u4eba\u53e3\u7d71\u8a08"
#   ))%>%
#   group_by(DATANAME, UNIT, TIME)%>%
#   summarise(SPACE=paste(SPACE, collapse="|"))%>%
#   mutate(SA=case_when(
#     grepl("\u6700\u5c0f", UNIT) ~ "SA0",
#     grepl("\u4e00\u7d1a", UNIT) ~ "SA1",
#     grepl("\u4e8c\u7d1a", UNIT) ~ "SA2",
#     grepl("\u6751\u91cc", UNIT) ~ "Village",
#     grepl("\u9109\u93ae\u5e02\u5340", UNIT) ~ "Town",
#     grepl("\u7e23\u5e02", UNIT) ~ "County"
#   ))
# catalog_temp$TIME_NUM=mapply(function(x) (as.numeric(strsplit(catalog_temp$TIME, "Y|M")[[x]][1])+1911)*12+as.numeric(strsplit(catalog_temp$TIME, "Y|M")[[x]][2]), c(1:nrow(catalog_temp)))
# catalog_temp$TIME_lab=paste0(as.numeric(mapply(function(x) strsplit(catalog_temp$TIME, "Y|M")[[x]][1], c(1:nrow(catalog_temp))))+1911, "-", mapply(function(x) strsplit(catalog_temp$TIME, "Y|M")[[x]][2], c(1:nrow(catalog_temp))))
# catalog_temp=arrange(catalog_temp, DATANAME, UNIT, TIME_NUM)
# write.csv(catalog_temp, "./others/pop_area_time.csv", row.names=F)
# catalog_temp=filter(catalog, grepl("\u7d71\u8a08\u5340\u570b\u571f\u5229\u7528\u8abf\u67e5\u7d71\u8a08", DATANAME))%>%
#   mutate(DATANAME="\u7d71\u8a08\u5340\u570b\u571f\u5229\u7528\u8abf\u67e5\u7d71\u8a08")%>%
#   group_by(DATANAME, UNIT, TIME)%>%
#   summarise(SPACE=paste(SPACE, collapse="|"))%>%
#   mutate(SA=case_when(
#     grepl("\u6700\u5c0f", UNIT) ~ "SA0",
#     grepl("\u4e00\u7d1a", UNIT) ~ "SA1",
#     grepl("\u4e8c\u7d1a", UNIT) ~ "SA2"
#   ))
# catalog_temp$Year=as.numeric(gsub("Y", "", catalog_temp$TIME))+1911
# catalog_temp=arrange(catalog_temp, DATANAME, UNIT, Year)
# write.csv(catalog_temp, "./others/landuse_area_time.csv", row.names=F)
# catalog_temp=filter(catalog, grepl("\u5de5\u5546\u5bb6\u6578", DATANAME), !TIME %in% c("97Y","98Y"))%>%
#   group_by(DATANAME, UNIT, TIME)%>%
#   summarise(SPACE=paste(SPACE, collapse="|"))
# write.csv(catalog_temp, "./others/business_area_time.csv", row.names=F)
# catalog_temp=filter(catalog, grepl("GML", DATANAME))%>%
#   select(DATANAME, TIME)%>%
#   mutate(SA=case_when(
#     grepl("\u6700\u5c0f", DATANAME) ~ "SA0",
#     grepl("\u4e00\u7d1a", DATANAME) ~ "SA1",
#     grepl("\u4e8c\u7d1a", DATANAME) ~ "SA2"
#   ),
#   TIME_NUM=mapply(function(x) (as.numeric(strsplit(TIME, "Y|M")[[x]][1])+1911)*12+as.numeric(strsplit(TIME, "Y|M")[[x]][2]), c(1:nrow(.))),
#   TIME_lab=paste0(as.numeric(mapply(function(x) strsplit(TIME, "Y|M")[[x]][1], c(1:nrow(.))))+1911, "-", mapply(function(x) strsplit(TIME, "Y|M")[[x]][2], c(1:nrow(.)))))
# write.csv(catalog_temp, "./others/statistical_area.csv", row.names=F)
# catalog_temp=filter(catalog, grepl("\u6821\u5225\u6982\u89bd", DATANAME), !grepl("\u7a7a\u5927", DATANAME))%>%
#   filter(grepl("\u9ad8\u7d1a\u4e2d\u7b49\u5b78\u6821|\u5927\u5c08\u6821\u9662|\u570b\u6c11\u4e2d\u5b78|\u570b\u6c11\u5c0f\u5b78", DATANAME))%>%
#   select(DATANAME, TIME, SPACE)%>%
#   mutate(Year=as.numeric(gsub("Y", "", TIME))+1911)%>%
#   mutate(Level=case_when(
#     grepl("\u9ad8\u7d1a\u4e2d\u7b49\u5b78\u6821", DATANAME) ~ "senior",
#     grepl("\u5927\u5c08\u6821\u9662", DATANAME) ~ "university",
#     grepl("\u570b\u6c11\u5c0f\u5b78", DATANAME) ~ "elementary",
#     grepl("\u570b\u6c11\u4e2d\u5b78", DATANAME) ~ "junior"
#   ))
# write.csv(catalog_temp, "./others/school_year.csv", row.names=F)
# catalog_temp=filter(catalog, grepl("\u4fe1\u4ee4\u4eba\u53e3\u7d71\u8a08", DATANAME))%>%
#   select(DATANAME, TIME, UNIT)
# write.csv(catalog_temp, "./others/cellular_year.csv", row.names=F)


# temp=Rail_Station(access_token, "TRA")%>%
#   select(StationID, StationName)%>%
#   unique()
# write.csv(temp, "./others/tra_station.csv", row.names=F)




#---get the token---#
#' @export
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



date_rev=function(dates){
  if(grepl(",", dates)){
    dates_all=unique(unlist(strsplit(dates, ",")))
  }else if(grepl("~", dates)){
    dates_all=unique(unlist(strsplit(dates, "~")))
  }else{
    dates_all=dates
  }

  tryCatch({
    dates_all=as.Date(dates_all)
    if(grepl("~", dates)){
      dates_all=as.Date(dates_all[1]:dates_all[2])
    }
  }, error=function(err){
    cat(paste0("Type", paste0(rep(" ", 12), collapse=""), "Format", paste0(rep(" ", 18), collapse=""), "Example\n", paste0(rep("=", 61), collapse=""), "\nSingle Date\tYYYY-MM-DD\t\t2023-01-01\nMultiple Dates\tYYYY-MM-DD,YYYY-MM-DD\t2023-01-01,2023-02-01\nDate Range\tYYYY-MM-DD~YYYY-MM-DD\t2023-01-01~2023-01-31\n"))
    stop("Date value is invalid! Format of the date is listed above.")
  })
  if(sum(is.na(dates_all))!=0){
    cat(paste0("Type", paste0(rep(" ", 12), collapse=""), "Format", paste0(rep(" ", 18), collapse=""), "Example\n", paste0(rep("=", 61), collapse=""), "\nSingle Date\tYYYY-MM-DD\t\t2023-01-01\nMultiple Dates\tYYYY-MM-DD,YYYY-MM-DD\t2023-01-01,2023-02-01\nDate Range\tYYYY-MM-DD~YYYY-MM-DD\t2023-01-01~2023-01-31\n"))
    stop("Date value is invalid! Format of the date is listed above.")
  }
  return(dates_all)
}



histo_data=function(access_token, mode, type, cou_ope, dates){
  if (!require(cli)) install.packages("cli")

  dates_all=date_rev(dates)
  all_data=data.frame()
  num_of_nodata=0
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]  {.emph Date: {dates_all[pb_current]}}", total=length(dates_all))
  for(i in as.character(dates_all)){
    cli_progress_update()

    if(mode %in% c("Bus","Bike")){
      if(cou_ope=="Intercity"){
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/", mode, "/", type, "/Date/", i, "/InterCity?%24format=CSV")
      }else if(cou_ope %in% TDX_County$Code){
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/", mode, "/", type, "/Date/", i, "/City/", cou_ope, "?%24format=CSV")
      }else{
        print(TDX_County)
        stop(paste0("City: '", cou_ope, "' is not valid. Please check out the parameter table above."))
      }
    }else if(mode=="Rail"){
      if(cou_ope %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/", mode, "/Metro/", type, "/Date/", i, "/", cou_ope, "?%24format=CSV")
      }else if(cou_ope %in% c("TRA","THSR")){
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/", mode, "/", cou_ope, "/", type, "/Date/", i, "?%24format=CSV")
      }else if(cou_ope=="AFR"){
        stop("'AFR' does not provide historical data up to now!")
      }else{
        print(TDX_Railway)
        stop(paste0("Railway: '", cou_ope, "' is not valid. Please check out the parameter table above."))
      }
    }

    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    if(is.na(content(x, "text", encoding="UTF-8"))){
      num_of_nodata=num_of_nodata+1
      cli_alert_info(paste0("Data of ", i, " is not avaliable!\n"))
    }else if(content(x, "text", encoding="UTF-8")==""){
      num_of_nodata=num_of_nodata+1
      cli_alert_info(paste0("Data of ", i, " is not avaliable!\n"))
    }else{
      all_data_temp=read.csv(textConnection(content(x, "text", encoding="UTF-8")), header=T)
      if("invalid.token" %in% names(all_data_temp)){
        stop(paste0("Your access token is invalid!"))
      }else if("Message" %in% names(all_data_temp)){
        stop(all_data_temp$Message)
      }else{
        all_data=rbind(all_data, cbind(all_data_temp, "InfoDate"=i))
      }
    }
  }
  cli_alert_info(ifelse(num_of_nodata==0, "All Done!", paste0("All Done!\n", num_of_nodata, " dates have no data!")))
  cli_progress_done()
  return(all_data)
}



#' @export
Bus_StopOfRoute=function(access_token, county, dates=F, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")
  if (!require(cli)) install.packages("cli")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(dates==F){
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
  }else{
    bus_stop=histo_data(access_token, "Bus", "StopOfRoute", county, dates)
  }

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bus_stop, out, row.names=F)
    }
  }else if(dtype=="sf"){
    bus_stop$geometry=st_as_sfc(ifelse(!is.na(bus_stop$PositionLon), paste0("POINT(", bus_stop$PositionLon, " ", bus_stop$PositionLat, ")"), "POINT EMPTY"))
    bus_stop=st_sf(bus_stop, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(bus_stop, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(bus_stop)
}



#' @export
Bus_Route=function(access_token, county, dates=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(dates==F){
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
    cat(paste0(length(num_of_subroute), " Routes\n"))

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
    bus_route=bus_route[,col_seq]
  }else{
    bus_route=histo_data(access_token, "Bus", "Route", county, dates)
  }

  if(nchar(out)!=0 & out!=F){
    write.csv(bus_route, out, row.names=F)
  }
  return(bus_route)
}



#' @export
Bus_Shape=function(access_token, county, dates=F, dtype="text", out=F){
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

  if(dates==F){
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
  }else{
    bus_shape=histo_data(access_token, "Bus", "Shape", county, dates)
    bus_shape=rename(bus_shape, geometry=Geometry)
  }

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



# used to retrieve the first row in function Bus_Schedule
retrieve_first=function(dat){
  temp=data.frame(t(mapply(function(x){
    first_row=t(unlist(dat[[x]][1,]))
    return(first_row)
  }, c(1:length(dat)))))
  return(temp)
}



#' @export
Bus_Schedule=function(access_token, county, dates=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(cli)) install.packages("cli")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(dates==F){
    iti_time=1
  }else{
    date_all=as.character(date_rev(dates))
    iti_time=length(date_all)
    cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]  {.emph Date: {date_all[pb_current]}}", total=iti_time)
  }

  bus_schedule=data.frame()
  for(i in c(1:iti_time)){
    if(dates!=F){
      cli_progress_update()
    }

    if(dates==F){
      if(county=="Intercity"){
        url="https://tdx.transportdata.tw/api/basic/v2/Bus/Schedule/InterCity?&$format=JSON"
      }else{
        url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/Schedule/City/", county, "?&$format=JSON")
      }
    }else{
      if(county=="Intercity"){
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/Schedule/Date/", date_all[i], "/InterCity/?%24format=JSON")
      }else{
        url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/Schedule/Date/", date_all[i], "/City/", county, "?%24format=JSON")
      }
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      if(dates==F){
        bus_info=fromJSON(content(x, as="text"))
      }else{
        bus_info=fromJSON(content(x, as="text"))$BusSchedules
      }
    }, error=function(err){
      if (grepl("invalid", conditionMessage(err))){
        stop(paste0("Your access token is invalid!"))
      }
    })
    if("Message" %in% names(bus_info)){
      if(county %in% TDX_County$Code){
        stop(paste0("Bus schedule data of '",county, "' is not avaliable.\n", bus_info$Message))
      }
      else{
        print(TDX_County)
        stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
      }
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

    bus_schedule_temp=bind_rows(bus_freq, bus_time)
    row.names(bus_schedule_temp)=NULL

    if(dates==F){
      bus_schedule=bus_schedule_temp
    }else{
      bus_schedule_temp=cbind(bus_schedule_temp, "InfoDate"=date_all[i])
      bus_schedule=rbind(bus_schedule, bus_schedule_temp)
      if(i==iti_time){
        cli_progress_done()
      }
    }
  }

  cat(paste0("#---", county, " Bus Schedule Downloaded---#\n"))

  if(nchar(out)!=0 & out!=F){
    write.csv(bus_schedule, out, row.names=F)
  }
  return(bus_schedule)
}



#' @export
Bus_Vehicle=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/Vehicle/InterCity?&$format=JSON"
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/Vehicle/City/", county, "?&$format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    bus_veh=fromJSON(content(x, as="text"))
  }, error=function(err){
    if (grepl("invalid", conditionMessage(err))){
      stop(paste0("Your access token is invalid!"))
    }
  })
  if("Message" %in% names(bus_veh)){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' bus vehicle data is not avaliable.\n", bus_veh$Message))
    }else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }

  bus_veh=select(bus_veh, -"UpdateTime")

  if(nchar(out)!=0 & out!=F){
    write.csv(bus_veh, out, row.names=F)
  }
  return(bus_veh)
}



#' @export
Rail_StationOfLine=function(access_token, operator, dates=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(dates==F){
    if(operator=="TRA"){
      url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/StationOfLine?&%24format=JSON"
    }else if(operator=="THSR"){
      stop("Please use function 'Rail_Station()' to retrieve the station of high speed rail (THSR).")
    }else if(operator=="AFR"){
      url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/StationOfLine?&%24format=JSON")
    }else if(operator %in% TDX_Railway$Code){
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/StationOfLine/", operator, "?&%24format=JSON")
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above"))
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
  }else{
    rail_station_line=histo_data(access_token, "Rail", "StationOfLine", operator, dates)
  }

  cat(paste0("#---", operator, " Station of Line Downloaded---#\n"))

  if(nchar(out)!=0 & out!=F){
    write.csv(rail_station_line, out, row.names=F)
  }
  return(rail_station_line)
}



#' @export
Rail_Station=function(access_token, operator, dates=F, dtype="text", out=F){
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

  if(dates==F){
    if(operator=="TRA"){
      url="https://tdx.transportdata.tw/api/basic/v2/Rail/TRA/Station?&%24format=JSON"
    }else if(operator=="THSR"){
      url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/Station?&%24format=JSON"
    }else if(operator=="AFR"){
      url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/Station?&%24format=JSON")
    }else if(operator %in% TDX_Railway$Code){
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/Station/", operator, "?&%24format=JSON")
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above."))
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
  }else{
    rail_station=histo_data(access_token, "Rail", "Station", operator, dates)
  }

  cat(paste0("#---", operator, " Station Downloaded---#\n"))

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_station, out, row.names=F)
    }
  }else if (dtype=="sf"){
    rail_station$geometry=st_as_sfc(ifelse(!is.na(rail_station$PositionLon), paste0("POINT(", rail_station$PositionLon, " ", rail_station$PositionLat, ")"), "POINT EMPTY"))
    rail_station=st_sf(rail_station, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(rail_station, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(rail_station)
}



#' @export
Rail_Shape=function(access_token, operator, dates=F, dtype="text", out=F){
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

  if(dates==F){
    if(operator=="TRA"){
      url="https://tdx.transportdata.tw/api/basic/v3/Rail/TRA/Shape?%24format=JSON"
    }else if(operator=="THSR"){
      url="https://tdx.transportdata.tw/api/basic/v2/Rail/THSR/Shape?%24format=JSON"
    }else if(operator=="AFR"){
      stop("AFR does not provide route geometry data up to now! Please check out other rail system.")
    }else if(operator %in% TDX_Railway$Code){
      url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/Shape/", operator, "?&%24format=JSON")
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above"))
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
  }else{
    rail_shape=histo_data(access_token, "Rail", "Shape", operator, dates)
    rail_shape=rename(rail_shape, geometry=Geometry)
  }

  cat(paste0("#---", operator, " Shape Downloaded---#\n"))

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(rail_shape, out, row.names=F)
    }
  }else if(dtype=="sf"){
    rail_shape$geometry=st_as_sfc(rail_shape$geometry)
    rail_shape=st_sf(rail_shape, crs=4326)

    if(grepl(".shp", out) & out!=F){
      write_sf(rail_shape, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(rail_shape)
}



#' @export
Bike_Station=function(access_token, county, dates=F, dtype="text", out=F){
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

  if(dates==F){
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
    bike_station=bike_station[, col_req]
  }else{
    bike_station=histo_data(access_token, "Bike", "Station", county, dates)
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(bike_station, out, row.names=F)
    }
  }else if (dtype=="sf"){
    bike_station$geometry=paste0("POINT(", bike_station$PositionLon, " ", bike_station$PositionLat, ")")
    bike_station$geometry=st_as_sfc(bike_station$geometry)
    bike_station=st_sf(bike_station, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(bike_station, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(bike_station)
}



#' @export
Geocoding=function(access_token, address, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(sf)) install.packages("sf")
  if (!require(urltools)) install.packages("urltools")
  if (!require(httr)) install.packages("httr")
  if (!require(cli)) install.packages("cli")

  if(!dtype %in% c("text","sf")){
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }
  if(!(grepl(".shp", out)) & out!=F & dtype=="sf"){
    stop("The file name must contain '.shp' when exporting shapefile.\n")
  }
  if(!(grepl(".csv|.txt", out)) & out!=F & dtype=="text"){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  # pb=progress_bar$new(format="(:spin) [:bar] :percent  ", total=length(address), clear=F, width=80)
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]", total=length(address))
  address_record=data.frame()
  record_fail=c()

  for (i in c(1:length(address))){
    cli_progress_update()
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
          cli_alert_info(paste0("CANNOT Geocode ", AddressOriginal=address[i], "\n"))
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
        if (grepl("externalptr", conditionMessage(err))){
          cat(paste0("Reconnect!!\n"))
          nexti=F
        }else if(grepl("subscript out of bounds", conditionMessage(err))){
          cli_alert_info(paste0("CANNOT Geocode ", AddressOriginal=address[i], "\n"))
          record_fail=c(record_fail, address[i])
          nexti=T
        }else{
          stop("Your access token is invalid!")
        }
      })
    }
  }
  cli_progress_done()

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
  }else if (dtype=="sf"){
    address_record$geometry=st_as_sfc(address_record$geometry)
    address_record=st_sf(address_record, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(address_record, out, layer_options="ENCODING=UTF-8")
    }else if (!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp'\n")
    }
  }
  return(list(SUCCESS=address_record, FAIL=record_fail))
}



#' @export
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
  }else if (dtype=="sf"){
    road$geometry=st_as_sfc(road$geometry)
    road=st_sf(road, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(road, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(road)
}



#' @export
Rail_TimeTable=function(access_token, operator, record, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
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
      stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above."))
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      if(operator=="TRA"){
        data_all=fromJSON(content(x, as="text", encoding="UTF-8"))$StationTimetables
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
    }else if (operator=="AFR"){
      url="https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/GeneralTrainTimetable?&%24format=JSON"
    }else{
      print(TDX_Railway)
      stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above"))
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



#' @export
Rail_TravelTime=function(access_token, operator, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/S2STravelTime/", operator, "?%24format=JSON")
  }else{
    stop(paste0("'",operator, "' travel time data is not avaliable.\n"))
  }

  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    rail_info=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })
  if("Message" %in% names(rail_info)){
    if(operator %in% TDX_Railway$Code){
      stop(paste0("'",operator, "' travel time data is not avaliable.\n", rail_info$Message))
    }
    else{
      print(TDX_Railway)
      stop(paste0("City: '", operator, "' is not valid. Please check out the parameter table above."))
    }
  }

  travel_time=data.frame(Sequence=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$Sequence), c(1:nrow(rail_info)))),
                         FromStationID=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$FromStationID), c(1:nrow(rail_info)))),
                         FromStationName=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$FromStationName$Zh_tw), c(1:nrow(rail_info)))),
                         ToStationID=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$ToStationID), c(1:nrow(rail_info)))),
                         ToStationName=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$ToStationName$Zh_tw), c(1:nrow(rail_info)))),
                         RunTime=unlist(mapply(function(x) list(rail_info$TravelTimes[[x]]$RunTime), c(1:nrow(rail_info)))),
                         StopTime=unlist(mapply(function(x) ifelse(is.null(rail_info$TravelTimes[[x]]$StopTime), list(rep(NA, times=nrow(rail_info$TravelTimes[[x]]))), list(rail_info$TravelTimes[[x]]$StopTime)), c(1:nrow(rail_info)))))

  num_of_s2s=mapply(function(x) nrow(rail_info$TravelTimes[[x]]), c(1:nrow(rail_info)))
  req_col=c("LineNo","LineID","RouteID","TrainType")
  req_col=req_col[req_col %in% names(rail_info)]
  rail_info=rail_info[rep(c(1:nrow(rail_info)), times=num_of_s2s), req_col]

  travel_time=cbind(rail_info, travel_time)
  row.names(travel_time)=NULL

  if (nchar(out)!=0 & out!=F){
    write.csv(travel_time, out, row.names=F)
  }
  return(travel_time)
}



#' @export
Rail_StationExit=function(access_token, operator, dtype="text", out=F){
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

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/StationExit/", operator, "?%24format=JSON")
  }else if(operator=="TRA"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/", operator, "/StationExit/?%24format=JSON")
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/", operator, "/StationExit/?%24format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    if(operator=="TRA"){
      rail_info=fromJSON(content(x, as="text"))$StationExits
    }else{
      rail_info=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  if("MESSAGE" %in% toupper(names(rail_info))){
    if(operator %in% TDX_Railway$Code){
      stop(paste0("'",operator, "' station exit data is not avaliable.\n", rail_info$Message))
    }
    else{
      print(TDX_Railway)
      stop(paste0("City: '", operator, "' is not valid. Please check out the parameter table above."))
    }
  }

  if(operator=="TRA"){
    station_exit=data.frame(ExitID=unlist(mapply(function(x) list(rail_info$Exits[[x]]$ExitID), c(1:nrow(rail_info)))),
                            ExitName=unlist(mapply(function(x) list(rail_info$Exits[[x]]$ExitName$Zh_tw), c(1:nrow(rail_info)))),
                            PositionLon=unlist(mapply(function(x) ifelse(is.null(rail_info$Exits[[x]]$ExitPosition$PositionLon), list(rep(NA, times=nrow(rail_info$Exits[[x]]))), list(rail_info$Exits[[x]]$ExitPosition$PositionLon)), c(1:nrow(rail_info)))),
                            PositionLat=unlist(mapply(function(x) ifelse(is.null(rail_info$Exits[[x]]$ExitPosition$PositionLat), list(rep(NA, times=nrow(rail_info$Exits[[x]]))), list(rail_info$Exits[[x]]$ExitPosition$PositionLat)), c(1:nrow(rail_info)))),
                            LocationDescription=unlist(mapply(function(x) list(rail_info$Exits[[x]]$LocationDescription), c(1:nrow(rail_info)))),
                            Stair=unlist(mapply(function(x) list(rail_info$Exits[[x]]$Stair), c(1:nrow(rail_info)))),
                            Escalator=unlist(mapply(function(x) list(rail_info$Exits[[x]]$Escalator), c(1:nrow(rail_info)))),
                            Elevator=unlist(mapply(function(x) list(rail_info$Exits[[x]]$Escalator), c(1:nrow(rail_info)))))
    num_of_exit=mapply(function(x) nrow(rail_info$Exits[[x]]), c(1:nrow(rail_info)))
    rail_info$StationName=rail_info$StationName$Zh_tw
    station_exit=cbind(rail_info[rep(c(1:nrow(rail_info)), num_of_exit), c("StationID","StationName")], station_exit)
    row.names(station_exit)=NULL
  }else{
    station_exit=data.frame(StationID=rail_info$StationID,
                            StationName=rail_info$StationName$Zh_tw,
                            ExitID=rail_info$ExitID,
                            ExitName=rail_info$ExitName$Zh_tw,
                            PositionLon=rail_info$ExitPosition$PositionLon,
                            PositionLat=rail_info$ExitPosition$PositionLat,
                            LocationDescription=unlist(ifelse(is.null(rail_info$LocationDescription), list(rep(NA, nrow(rail_info))), list(rail_info$LocationDescription))),
                            Stair=unlist(ifelse(is.null(rail_info$Stair), list(rep(NA, nrow(rail_info))), list(rail_info$Stair))),
                            Escalator=unlist(ifelse(is.null(rail_info$Escalator), list(rep(NA, nrow(rail_info))), list(rail_info$Escalator))),
                            Elevator=unlist(ifelse(is.null(rail_info$Elevator), list(rep(NA, nrow(rail_info))), list(rail_info$Elevator))))
  }

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(station_exit, out, row.names=F)
    }
  }else if (dtype=="sf"){
    station_exit$geometry=st_as_sfc(ifelse(!is.na(station_exit$PositionLon), paste0("POINT(", station_exit$PositionLon, " ", station_exit$PositionLat, ")"), "POINT EMPTY"))
    station_exit=st_sf(station_exit, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(station_exit, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(station_exit)
}



#' @export
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
      stop(paste0("'",county, "' has no cycling path or data is not avaliable.\n", bike_shape$Message))
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



#' @export
Air_Schedule=function(access_token, domestic=T, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

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



#' @export
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



#' @export
Bus_TravelTime=function(access_token, county, routeid, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(cli)) install.packages("cli")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(county=="Tainan"){
    warning("Travel time data of Tainan retrieves all the routes. Parameter 'routeid' is muted.\n")
    url="https://tdx.transportdata.tw/api/basic/v3/Bus/S2STravelTime/City/Tainan?%24format=JSON"
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
    tryCatch({
      subroute_info=fromJSON(content(x, as="text"))$S2STravelTimes
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })
    route_info=subroute_info[, c("RouteUID","RouteID","SubRouteUID","SubRouteID")]
    s2stimes=rbindlist(mapply(function(x) list(subroute_info$TravelTimes[[x]]), c(1:nrow(subroute_info))))
    num_of_s2s=mapply(function(x) nrow(subroute_info$TravelTimes[[x]]), c(1:nrow(subroute_info)))
    route_info=route_info[rep(c(1:nrow(route_info)), num_of_s2s),]
    traveltime_ALL=cbind(route_info, s2stimes)
  }else{
    routeid=unique(routeid)
    cat(paste0("Total: ", length(routeid), " Routes\n"))
    cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]  {.emph RouteID: {routeid[pb_current]}}", total=length(routeid))
    num_of_nodata=0

    traveltime_ALL=data.frame()
    for (route in routeid){
      cli_progress_update()
      if (county=="Intercity"){
        url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/S2STravelTime/InterCity/", route, "?&%24format=JSON")
      }else if(county %in% TDX_County$Code[1:22]){
        url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/S2STravelTime/City/", county, "/", route, "?&%24format=JSON")
      }else{
        print(TDX_County[1:22,])
        stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
      }
      x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

      tryCatch({
        subroute_info=fromJSON(content(x, as="text"))
      }, error=function(err){
        stop(paste0("Your access token is invalid!"))
      })
      if(length(subroute_info)==0){
        cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
        num_of_nodata=num_of_nodata+1
        next
      }else{
        if(sum(lengths(subroute_info$TravelTimes))==0){
          cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
          num_of_nodata=num_of_nodata+1
          next
        }
        subroute_info=dplyr::select(subroute_info, -UpdateTime)
      }

      route_info=subroute_info[, c("RouteUID","RouteID","SubRouteUID","SubRouteID","Direction")]
      s2stimes=mapply(function(x) list(subroute_info$TravelTimes[[x]]$S2STimes), c(1:nrow(subroute_info)))

      week_info=data.frame(Weekday=unlist(mapply(function(x) list(subroute_info$TravelTimes[[x]]$Weekday), c(1:nrow(subroute_info)))),
                           StartHour=unlist(mapply(function(x) list(subroute_info$TravelTimes[[x]]$StartHour), c(1:nrow(subroute_info)))),
                           EndHour=unlist(mapply(function(x) list(subroute_info$TravelTimes[[x]]$EndHour), c(1:nrow(subroute_info)))))
      num_of_week_info=lengths(s2stimes)
      route_info=route_info[rep(c(1:nrow(route_info)), num_of_week_info),]
      route_info=cbind(route_info, week_info)

      num_of_s2s=unlist(mapply(function(y) list(mapply(function(x) nrow(s2stimes[[y]][[x]]), c(1:length(s2stimes[[y]])))), c(1:length(s2stimes))))
      route_info=route_info[rep(c(1:nrow(route_info)), num_of_s2s),]

      s2s_df=rbindlist(mapply(function(x) list(rbindlist(subroute_info$TravelTimes[[x]]$S2STimes)), c(1:nrow(subroute_info))))
      traveltime=cbind(route_info, s2s_df)
      row.names(traveltime)=NULL
      traveltime_ALL=rbind(traveltime_ALL, traveltime)
    }
    cli_alert_info(ifelse(num_of_nodata==0, "All Done!", paste0("All Done!\n", num_of_nodata, " RouteIDs have no data!")))
    cli_progress_done()
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(traveltime_ALL, out, row.names=F)
  }
  return(traveltime_ALL)
}



#' @export
Rail_ODFare=function(access_token, operator, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(data.table)) install.packages("data.table")

  if (operator %in% c("TRA","THSR")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/", operator, "/ODFare?&%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
  }else if(operator %in% c("TRTC","KRTC","TYMC","NTDLRT","TMRT","KLRT")){
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Rail/Metro/ODFare/", operator, "?&%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
  }else if (operator=="AFR"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Rail/AFR/ODFare?&%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
  }else{
    print(TDX_Railway)
    stop(paste0("'", operator, "' is not valid operator. Please check out the table of railway code above"))
  }

  tryCatch({
    if(operator=="AFR"){
      rail_info=fromJSON(content(x, as="text"))$ODFares
    }else{
      rail_info=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  rail_info$OriginStationName=rail_info$OriginStationName$Zh_tw
  rail_info$DestinationStationName=rail_info$DestinationStationName$Zh_tw
  num_of_fare=mapply(function(x) nrow(rail_info$Fares[[x]]), c(1:nrow(rail_info)))
  rail_fare=rbindlist(rail_info$Fares)

  col_req=c("OriginStationID","OriginStationName","DestinationStationID","DestinationStationName","Direction","TrainType","TravelDistance")
  col_req=col_req[col_req %in% names(rail_info)]
  rail_info=rail_info[rep(c(1:nrow(rail_info)), num_of_fare), col_req]
  rail_fare=cbind(rail_info, rail_fare)

  if (nchar(out)!=0 & out!=F){
    write.csv(odfare, out, row.names=F)
  }
  return(rail_fare)
}



#' @export
Car_Park=function(access_token, county, street, dtype="text", out=F){
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

  if(!county %in% TDX_County$Code[1:22]){
    print(TDX_County[1:22,])
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  if(street=="off"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v1/Parking/OffStreet/CarPark/City/", county, "?&%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      carpark=fromJSON(content(x, as="text"))$CarParks
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })
    if(is.null(carpark) | length(carpark)==0){
      stop(paste0("Car parking data of '", county, "' is not avaliable."))
    }

    carpark$CarParkName=carpark$CarParkName$Zh_tw
    carpark$CarParkShortName=carpark$CarParkShortName$Zh_tw
    carpark$ParkingTypes=mapply(function(x) paste(carpark$ParkingTypes[[x]], collapse="|"), c(1:nrow(carpark)))
    carpark$ParkingSiteTypes=mapply(function(x) paste(carpark$ParkingSiteTypes[[x]], collapse="|"), c(1:nrow(carpark)))
    carpark$ChargeTypes=mapply(function(x) paste(carpark$ChargeTypes[[x]], collapse="|"), c(1:nrow(carpark)))
    carpark=cbind(carpark, carpark$CarParkPosition)%>%
      select(-CarParkPosition)
    carpark$ParkingAreaID=mapply(function(x) paste(carpark$ParkingAreas[[x]]$ParkingAreaID, collapse="|"), c(1:nrow(carpark)))
    carpark$ParkingAreaName=mapply(function(x) paste(carpark$ParkingAreas[[x]]$ParkingAreaName$Zh_tw, collapse="|"), c(1:nrow(carpark)))
    if("ParkingAreas" %in% names(carpark)){
      carpark=dplyr::select(carpark, -ParkingAreas)
    }
  }else if(street=="on"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v1/Parking/OnStreet/ParkingSpot/City/", county, "?%24format=JSON")
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      carpark=fromJSON(content(x, as="text"))$ParkingSegmentSpots
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })
    if(is.null(carpark) | length(carpark)==0){
      stop(paste0("Car parking data of '", county, "' is not avaliable."))
    }

    carpark=cbind(carpark, carpark$Position)%>%
      select(-Position)
  }else{
    stop("Parameter 'street' must be 'on' or 'off'.")
  }


  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(carpark, out, row.names=F)
    }
    return(carpark)
  }else if (dtype=="sf"){
    if("Geometry" %in% names(carpark) & street=="on"){
      cat("Data provides 'POLYGON' geometry. Note that the 'POINT' geometry of the parking area are also retrieved.\n")
      carpark=rename(carpark, geometry=Geometry)
      carpark$geometry=st_as_sfc(carpark$geometry)
      carpark=st_sf(carpark, crs=3826)%>%
        st_transform(crs=4326)
    }else{
      cat("Data provides 'POINT' geometry.\n")
      carpark$geometry=st_as_sfc(paste0("POINT(", carpark$PositionLon, " ", carpark$PositionLat, ")"))
      carpark=st_sf(carpark, crs=4326)
    }

    if (grepl(".shp", out) & out!=F){
      write_sf(carpark, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(carpark)
}



#' @export
Ship_Port=function(access_token, dtype="text", out=F){
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

  url="https://tdx.transportdata.tw/api/basic/v3/Ship/Port?&%24format=JSON"
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    shipport=fromJSON(content(x, as="text"))$Ports
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  shipport=cbind(shipport, shipport$PortPosition)%>%
    dplyr::select(-PortPosition)
  shipport$PortName=shipport$PortName$Zh_tw

  if (dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(shipport, out, row.names=F)
    }
    return(shipport)
  }else if (dtype=="sf"){
    shipport$geometry=st_as_sfc(paste0("POINT(", shipport$PositionLon, " ", shipport$PositionLat, ")"))
    shipport=st_sf(shipport, crs=4326)

    if (grepl(".shp", out) & out!=F){
      write_sf(shipport, out, layer_options="ENCODING=UTF-8")
    }

    return(shipport)
  }
}


#' @export
Ship_Route=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/Route/Domestic/City/", county, "?&%24format=JSON")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    shiproute=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })
  if("Message" %in% names(shiproute) | length(shiproute)==0){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' has no ship route or data is not avaliable.\n", shiproute$Message))
    }
    else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }

  shiproute=shiproute$Routes
  shiproute$Operators=mapply(function(x) paste(shiproute$Operators[[x]], collapse="|"), c(1:nrow(shiproute)))
  shiproute$RouteName=shiproute$RouteName$Zh_tw
  shiproute$TicketPriceDescription=shiproute$TicketPriceDescription$Zh_tw

  if (nchar(out)!=0 & out!=F){
    write.csv(shiproute, out, row.names=F)
  }
  return(shiproute)
}



#' @export
Ship_StopOfRoute=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  url=paste0("https://tdx.transportdata.tw/api/basic/v3/Ship/StopOfRoute/Domestic/City/", county, "?&%24format=JSON")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    stopofroute=fromJSON(content(x, as="text"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })
  if("Message" %in% names(stopofroute)){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' has no ship route or data is not avaliable.\n", stopofroute$Message))
    }
    else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }

  stopofroute=stopofroute$StopOfRoutes
  if(length(stopofroute)==0){
    stop(paste0("'",county, "' has no ship route or data is not avaliable.\n", stopofroute$Message))
  }
  stopofroute$RouteName=stopofroute$RouteName$Zh_tw
  stopofroute$OperatorID=mapply(function(x) paste(stopofroute$Operators[[x]]$OperatorID, collapse="|"), c(1:nrow(stopofroute)))

  all_stop=bind_rows(mapply(function(x) list(stopofroute$Stops[[x]]), c(1:nrow(stopofroute))))
  all_stop$PortName=all_stop$PortName$Zh_tw
  num_of_stop=mapply(function(x) nrow(stopofroute$Stops[[x]]), c(1:nrow(stopofroute)))

  stopofroute=cbind(stopofroute[rep(c(1:nrow(stopofroute)), num_of_stop),], all_stop)%>%
    dplyr::select(-Operators, -Stops)%>%
    arrange( RouteID, Direction, StopSequence)

  if (nchar(out)!=0 & out!=F){
    write.csv(stopofroute, out, row.names=F)
  }
  return(stopofroute)
}



#' @export
Bus_RouteFare=function(access_token, county, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(data.table)) install.packages("data.table")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  cat("Please wait for a while...\n")
  if(county=="Intercity"){
    url="https://tdx.transportdata.tw/api/basic/v2/Bus/RouteFare/InterCity?&%24format=JSON"
  }else if(county=="Tainan"){
    url=paste0("https://tdx.transportdata.tw/api/basic/v3/Bus/RouteFare/City/", county, "?&%24format=JSON")
  }else{
    url=paste0("https://tdx.transportdata.tw/api/basic/v2/Bus/RouteFare/City/", county, "?&%24format=JSON")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  tryCatch({
    if(county=="Tainan"){
      bus_info=fromJSON(content(x, as="text"))$RouteFares
    }else{
      bus_info=fromJSON(content(x, as="text"))
    }
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  if("Message" %in% names(bus_info)){
    if(county %in% TDX_County$Code){
      stop(paste0("'",county, "' has no data avaliable.\n", bus_info$Message))
    }
    else{
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
  }

  if(unique(bus_info$FarePricingType)==0){
    temp=mapply(function(x) bus_info$SectionFares[[x]]$BufferZones[[1]], c(1:nrow(bus_info)))
    route_buffer=data.frame(SectionSequence=unlist(mapply(function(x) temp[[x]]$SectionSequence, c(1:length(temp)))),
                            Direction=unlist(mapply(function(x) temp[[x]]$Direction, c(1:length(temp)))),
                            BZOStopID=unlist(mapply(function(x) temp[[x]]$FareBufferZoneOrigin$StopID, c(1:length(temp)))),
                            BZOStopName=unlist(mapply(function(x) temp[[x]]$FareBufferZoneOrigin$StopName, c(1:length(temp)))),
                            BZDStopID=unlist(mapply(function(x) temp[[x]]$FareBufferZoneDestination$StopID, c(1:length(temp)))),
                            BZDStopName=unlist(mapply(function(x) temp[[x]]$FareBufferZoneDestination$StopName, c(1:length(temp)))))
    num_of_buffer=lengths(mapply(function(x) temp[[x]]$SectionSequence, c(1:length(temp))))
    bus_info_bz=cbind(bus_info[rep(c(1:nrow(bus_info)), num_of_buffer), c("RouteID","RouteName","SubRouteID","SubRouteName")], route_buffer)

    temp=rbindlist(mapply(function(x) list(bus_info$SectionFares[[x]]$Fares[[1]]), c(1:nrow(bus_info))))
    num_of_fare=mapply(function(x) nrow(bus_info$SectionFares[[x]]$Fares[[1]]), c(1:nrow(bus_info)))
    bus_info_fare=cbind(bus_info[rep(c(1:nrow(bus_info)), num_of_fare), ], temp)%>%
      dplyr::select(-SectionFares, -UpdateTime)
  }else if(unique(bus_info$FarePricingType)==1){
    stop_info=data.frame(Direction=unlist(mapply(function(x) bus_info$ODFares[[x]]$Direction, c(1:nrow(bus_info)))),
                         OriginStopID=unlist(mapply(function(x) bus_info$ODFares[[x]]$OriginStop$StopID, c(1:nrow(bus_info)))),
                         OriginStopName=unlist(mapply(function(x) bus_info$ODFares[[x]]$OriginStop$StopName, c(1:nrow(bus_info)))),
                         DestinationStopID=unlist(mapply(function(x) bus_info$ODFares[[x]]$DestinationStop$StopID, c(1:nrow(bus_info)))),
                         DestinationStopName=unlist(mapply(function(x) bus_info$ODFares[[x]]$DestinationStop$StopName, c(1:nrow(bus_info)))))
    num_of_stop=unlist(mapply(function(x) ifelse(is.null(nrow(bus_info$ODFares[[x]])), 0, nrow(bus_info$ODFares[[x]])), c(1:nrow(bus_info))))

    stop_info=cbind(bus_info[rep(c(1:nrow(bus_info)), num_of_stop), c("RouteID","RouteName","OperatorID","OperatorNo","SubRouteID","SubRouteName","FarePricingType","IsFreeBus","IsForAllSubRoutes")], stop_info)
    bus_info=bus_info[num_of_stop!=0, ]

    fare_data=data.frame(TicketType=unlist(mapply(function(y) mapply(function(x) bus_info$ODFares[[y]]$Fares[[x]]$TicketType, c(1:nrow(bus_info$ODFares[[y]]))), c(1:nrow(bus_info)))),
                         FareClass=unlist(mapply(function(y) mapply(function(x) bus_info$ODFares[[y]]$Fares[[x]]$FareClass, c(1:nrow(bus_info$ODFares[[y]]))), c(1:nrow(bus_info)))),
                         Price=unlist(mapply(function(y) mapply(function(x) bus_info$ODFares[[y]]$Fares[[x]]$Price, c(1:nrow(bus_info$ODFares[[y]]))), c(1:nrow(bus_info)))))
    num_of_fare=unlist(mapply(function(y) mapply(function(x) nrow(bus_info$ODFares[[y]]$Fares[[x]]), c(1:nrow(bus_info$ODFares[[y]]))), c(1:nrow(bus_info))))

    bus_info_fare=cbind(stop_info[rep(c(1:nrow(stop_info)), num_of_fare),], fare_data)
  }else if (unique(bus_info$FarePricingType)==2){
    stage_info=data.frame(Direction=unlist(mapply(function(x) bus_info$StageFares[[x]]$Direction, c(1:nrow(bus_info)))),
                          OriginStage_StopID=unlist(mapply(function(x) bus_info$StageFares[[x]]$OriginStage$StopID, c(1:nrow(bus_info)))),
                          OriginStage_StopName=unlist(mapply(function(x) bus_info$StageFares[[x]]$OriginStage$StopName, c(1:nrow(bus_info)))),
                          OriginStage_Sequence=unlist(mapply(function(x) bus_info$StageFares[[x]]$OriginStage$Sequence, c(1:nrow(bus_info)))),
                          DestinationStage_StopID=unlist(mapply(function(x) bus_info$StageFares[[x]]$DestinationStage$StopID, c(1:nrow(bus_info)))),
                          DestinationStage_StopName=unlist(mapply(function(x) bus_info$StageFares[[x]]$DestinationStage$StopName, c(1:nrow(bus_info)))),
                          DestinationStage_Sequence=unlist(mapply(function(x) bus_info$StageFares[[x]]$DestinationStage$Sequence, c(1:nrow(bus_info)))))
    num_of_stage=mapply(function(x) nrow(bus_info$StageFares[[x]]), c(1:nrow(bus_info)))
    stage_info=cbind(bus_info[rep(c(1:nrow(bus_info)), num_of_stage), c("RouteID","RouteName","OperatorID","OperatorNo","SubRouteID","SubRouteName","FarePricingType","IsFreeBus","IsForAllSubRoutes")], stage_info)

    fare_data=data.frame(FareName=unlist(mapply(function(y) mapply(function(x) bus_info$StageFares[[y]]$Fares[[x]]$FareName, c(1:nrow(bus_info$StageFares[[y]]))), c(1:nrow(bus_info)))),
                         TicketType=unlist(mapply(function(y) mapply(function(x) bus_info$StageFares[[y]]$Fares[[x]]$TicketType, c(1:nrow(bus_info$StageFares[[y]]))), c(1:nrow(bus_info)))),
                         FareClass=unlist(mapply(function(y) mapply(function(x) bus_info$StageFares[[y]]$Fares[[x]]$FareClass, c(1:nrow(bus_info$StageFares[[y]]))), c(1:nrow(bus_info)))),
                         Price=unlist(mapply(function(y) mapply(function(x) bus_info$StageFares[[y]]$Fares[[x]]$Price, c(1:nrow(bus_info$StageFares[[y]]))), c(1:nrow(bus_info)))))
    num_of_fare=unlist(mapply(function(y) mapply(function(x) nrow(bus_info$StageFares[[y]]$Fares[[x]]), c(1:nrow(bus_info$StageFares[[y]]))), c(1:nrow(bus_info))))

    bus_info_fare=cbind(stage_info[rep(c(1:nrow(stage_info)), num_of_fare),], fare_data)
  }


  if (nchar(out)!=0 & out!=F){
    if(unique(bus_info$FarePricingType)==0){
      write.csv(bus_info_bz, paste0(gsub(".txt|.csv", "", out), "_BufferZone.", ifelse(grepl("csv", out), "csv", "txt")), row.names=F)
      write.csv(bus_info_fare, paste0(gsub(".txt|.csv", "", out), "_ZoneFare.", ifelse(grepl("csv", out), "csv", "txt")), row.names=F)
    }else{
      write.csv(bus_info_fare, out, row.names=F)
    }
  }

  row.names(bus_info_fare)=NULL
  if (unique(bus_info$FarePricingType)==0){
    row.names(bus_info_bz)=NULL
    warning("Please use '$BufferZone' to retrieve the buffer zone of bus route, and use '$ZoneFare' to  retrieve the fare of each buffer zone.")
    return(list(BufferZone=bus_info_bz, ZoneFare=bus_info_fare))
  }else{
    return(bus_info_fare)
  }
}



#' @export
Bike_Remain_His=function(access_token, county, dates, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bike/Availability/", county, "?Dates=", dates, "&%24format=CSV")
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

  if(content(x, "text", encoding="UTF-8")==""){
    stop(paste0("'",county, "' has no bike sharing system or data is not avaliable.\n", bike_station$Message))
  }
  bike_remain=read.csv(textConnection(content(x, "text", encoding="UTF-8")), header=T)

  if("invalid.token" %in% names(bike_remain)){
    stop(paste0("Your access token is invalid!"))
  }else if("Message" %in% names(bike_remain)){
    if(grepl("2021-06-01", bike_remain$Message)){
      stop(paste0("Historical data can only be retrived after date '2021-06-01'!"))
    }else if(grepl("City", bike_remain$Message)){
      print(print(TDX_County[1:22,]))
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }else{
      stop(paste0("Date value is invalid! Format of the date:\n", "\nType\t\tFormat\t\t\tExample\n", paste0(rep("=", 61), collapse=""), "\nSingle Date\tYYYY-MM-DD\t\t2023-01-01\nMultiple Dates\tYYYY-MM-DD;YYYY-MM-DD\t2023-01-01;2023-02-01\nDate Range\tYYYY-MM-DD~YYYY-MM-DD\t2023-01-01~2023-01-31"))
    }
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(bike_remain, out, row.names=F)
  }
  return(bike_remain)
}



#' @export
Freeway_Shape=function(geotype, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(xml2)) install.packages("xml2")
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

  if(geotype=="section"){
    # SectionID
    tryCatch({
      x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/Section.xml")
    }, error=function(err){
      stop(paste0("Original data in database contains errors!"))
    })
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
    tryCatch({
      x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/SectionShape.xml")
    }, error=function(err){
      stop(paste0("Original data in database contains errors!"))
    })
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
    tryCatch({
      x=read_xml("https://tisvcloud.freeway.gov.tw/history/motc20/SectionLink.xml")
    }, error=function(err){
      stop(paste0("Original data in database contains errors!"))
    })
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
  }else if (dtype=="sf"){
    if (grepl(".shp", out) & out!=F){
      write_sf(freeway_shape, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(freeway_shape)
}


#' @export
District_Shape=function(district, time=NULL, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(sf)) install.packages("sf")
  if (!require(archive)) install.packages("archive")
  if (!require(XML)) install.packages("XML")
  options(timeout=1000)

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(district %in% c("SA0","SA1","SA2")){
    all_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/statistical_area.csv")%>%
      filter(SA==district)
    if(!is.null(time)){
      if(nchar(time)!=7 | !grepl("-", time)){
        stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
      }
      time_num=as.numeric(unlist(strsplit(time, "-")))
      time_num=time_num[1]*12+time_num[2]
      all_data_temp=all_data[which.min(abs(all_data$TIME_NUM-time_num)),]
      if(all_data_temp$TIME_lab!=time){
        cat(paste0("Data ", time, " is not avaliable. Download data ", all_data_temp$TIME_lab, " instead.\nAll data available for ", district, " is listed below:\n"), paste(all_data$TIME_lab, collapse=", "), "\n")
      }else{
        cat(paste0("Download the data in ", all_data_temp$TIME_lab, ".\n"))
      }
    }else{
      all_data_temp=all_data[which.max(all_data$TIME_NUM),]
      cat(paste0("Download the latest data ", all_data_temp$TIME_lab, ".\nIf a specific time of data is required, please set the argument 'time'.\n"))
    }
    time_rev=all_data_temp$TIME
  }else if(district %in% c("County","Town","Village")){
    if(!is.null(time)){cat("Argument 'time' is deprecated.\n")}
  }else{
    stop(paste0("Argument 'district' should be 'County', 'Town', 'Village', 'SA0', 'SA1', or 'SA2'."))
  }

  if(district=="County"){
    url="https://maps.nlsc.gov.tw/download/%E7%B8%A3%E5%B8%82%E7%95%8C%E7%B7%9A(TWD97%E7%B6%93%E7%B7%AF%E5%BA%A6).zip"
  }else if(district=="Town"){
    url="https://maps.nlsc.gov.tw/download/%E9%84%89%E9%8E%AE%E5%B8%82%E5%8D%80%E7%95%8C%E7%B7%9A(TWD97%E7%B6%93%E7%B7%AF%E5%BA%A6).zip"
  }else if(district=="Village"){
    url="https://maps.nlsc.gov.tw/download/%E6%9D%91(%E9%87%8C)%E7%95%8C(TWD97_121%E5%88%86%E5%B8%B6).zip"
  }else if(district=="SA0"){
    url=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=B%2fAMiCXtTLpw0dsuDX3ECw%3d%3d&STTIME=", time_rev, "&STUNIT=null&BOUNDARY=%E5%85%A8%E5%9C%8B")
  }else if(district=="SA1"){
    url=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=KauzeQbZ0OwB7oPXscC47g%3d%3d&STTIME=", time_rev, "&STUNIT=null&BOUNDARY=%E5%85%A8%E5%9C%8B")
  }else if(district=="SA2"){
    url=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=w6DncCAc5Scpyqawns3cBg%3d%3d&STTIME=", time_rev, "&STUNIT=null&BOUNDARY=%E5%85%A8%E5%9C%8B")
  }

  unlink(list.files(tempdir(), full.names=T), recursive=T)
  download.file(url, paste0(tempdir(), "/shape.zip"), mode="wb", quiet=T)
  untar(paste0(tempdir(), "/shape.zip"), exdir=paste0(tempdir(), "/shape"))

  if(district %in% c("County","Town","Village")){
    dir_files=dir(paste0(tempdir(), "/shape"), full.names=T, recursive=T, pattern="shp")
    dir_files=dir_files[which.max(file.info(dir_files)$size)]
    district_shape=read_sf(dir_files)
    district_shape=st_transform(district_shape, crs=3826)
  }else{
    dir_files=dir(paste0(tempdir(), "/shape"), full.names=T, recursive=T, pattern="rar")
    # archive(dir_files)
    con=archive_read(dir_files)
    suppressWarnings({xml_file=readLines(con, ok=T)})
    close(con)
    xml_file=xmlParse(xml_file)
    saveXML(xml_file, file=paste0(tempdir(), "/xml_file.gml"), encoding="UTF-8")
    district_shape=read_sf(paste0(tempdir(), "/xml_file.gml"))
    towncode=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/TOWNCODE.csv", colClasses=c("COUNTYCODE"="character","TOWNCODE"="character"))

    if(district=="SA0"){
      colnames(district_shape)=c("temp0","SA0CODE","temp1","TOWNCODE","temp_TOWNNAME","COUNTYCODE","temp_COUNTYNAME","AREA","temp2","temp3","temp4","temp5","temp6","temp7","SA1CODE","SA2CODE","temp8","Population","Household","temp9","geometry")
    }else if(district=="SA1"){
      colnames(district_shape)=c("temp0","SA1CODE","temp1","TOWNCODE","temp_TOWNNAME","COUNTYCODE","temp_COUNTYNAME","AREA","temp2","temp3","temp4","temp5","temp6","temp7","temp8","temp9","SA2CODE","Population","Household","temp10","geometry")
    }else if(district=="SA2"){
      colnames(district_shape)=c("temp0","SA2CODE","temp1","TOWNCODE","temp_TOWNNAME","COUNTYCODE","temp_COUNTYNAME","AREA","temp2","temp3","temp4","temp5","temp6","temp7","temp8","temp9","temp10","Population","Household","temp10","geometry")
    }

    st_geometry(district_shape)="geometry"
    district_shape=district_shape[, !grepl("temp", names(district_shape))]
    district_shape$TOWNCODE=ifelse(nchar(district_shape$TOWNCODE)==7, paste0("0", district_shape$TOWNCODE), district_shape$TOWNCODE)
    district_shape$COUNTYCODE=ifelse(nchar(district_shape$COUNTYCODE)==4, paste0("0", district_shape$COUNTYCODE), district_shape$COUNTYCODE)
    district_shape=left_join(district_shape, towncode, by=c("TOWNCODE","COUNTYCODE"))
    district_shape=st_zm(district_shape, drop=T)
    district_shape=st_sf(district_shape, crs=3826)
  }


  if (grepl(".shp", out) & out!=F){
    write_sf(district_shape, out, layer_options="ENCODING=UTF-8")
  }
  return(district_shape)
}



#' @export
Population=function(district, time=NULL, age=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(cli)) install.packages("cli")
  options(timeout=1000)

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(district %in% c("County","Town","Village","SA0","SA1","SA2")){
    all_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/pop_area_time.csv")%>%
      filter(SA==district, DATANAME==ifelse(age==T, "\u4e94\u6b72\u5e74\u9f61\u7d44\u6027\u5225\u4eba\u53e3\u7d71\u8a08", "\u4eba\u53e3\u7d71\u8a08"))
  }else{
    stop(paste0("Argument 'district' should be 'County', 'Town', 'Village', 'SA0', 'SA1', or 'SA2'."))
  }

  if(!is.null(time)){
    if(nchar(time)!=7 | !grepl("-", time)){
      stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
    }
    time_num=as.numeric(unlist(strsplit(time, "-")))
    time_num=time_num[1]*12+time_num[2]
    all_data_temp=all_data[which.min(abs(all_data$TIME_NUM-time_num)),]
    if(all_data_temp$TIME_lab!=time){
      cat(paste0("Data ", time, " is not avaliable. Download data ", all_data_temp$TIME_lab, " instead.\nAll data available for ", district, " is listed below:\n"), paste(all_data$TIME_lab, collapse=", "), "\n")
    }else{
      cat(paste0("Download the data in ", all_data_temp$TIME_lab, ".\n"))
    }
  }else{
    all_data_temp=all_data[which.max(all_data$TIME_NUM),]
    cat(paste0("Download the latest data ", all_data_temp$TIME_lab, ".\nIf a specific time of data is required, please set the argument 'time'.\n"))
  }
  time_rev=all_data_temp$TIME


  if(district=="County"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=7A2dE0lGbbiQg8ggGoTmLg%3d%3d&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=Aeiv7Oil4QUv76rbeaj3Hw%3d%3d&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }
  }else if(district=="Town"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=ZA5vfYBRjOAOLs4Yfz3tCQ%3d%3d&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=QXVT3AX8GQy1NqiSsJGV4A%3d%3d&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }
  }else if(district=="Village"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=ltVp3Seiz4UacOUlJ5WlGw%3d%3d&STTIME=", time_rev, "&STUNIT=U01VI&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=4pFoRRKEKSnTDaIyzlU4rQ%3d%3d&STTIME=", time_rev, "&STUNIT=U01VI&BOUNDARY=%E5%85%A8%E5%9C%8B")
    }
  }else if(district=="SA0"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=N6mbogBbiIhLpTay1Nfzeg%3d%3d&STTIME=", time_rev, "&STUNIT=U0200&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=N5l9H0sZ9JX%2fMQp8qSxyKg%3d%3d&STTIME=", time_rev, "&STUNIT=U0200&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }
  }else if(district=="SA1"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=3FG6sa9njC%2fvY4%2f2tMYnpg%3d%3d&STTIME=", time_rev, "&STUNIT=U0201&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=yYLunxoEPygac%2fHXuXQi2w%3d%3d&STTIME=", time_rev, "&STUNIT=U0201&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }
  }else if(district=="SA2"){
    if(age){
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=xbNe6XuALyiq0Tr%2fTSmLlA%3d%3d&STTIME=", time_rev, "&STUNIT=U0202&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }else{
      url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=XjqpPDCzautWVyIadXhbpA%3d%3d&STTIME=", time_rev, "&STUNIT=U0202&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
    }
  }

  population=data.frame()
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]", total=length(url_all))
  for(url in c(1:length(url_all))){
    cli_progress_update()
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url_all[url], paste0(tempdir(), "/temp_pop_TDX.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/temp_pop_TDX.zip"), exdir=paste0(tempdir(), "/temp_pop_TDX"))
    dir_file=dir(dir(paste0(tempdir(), "/temp_pop_TDX"), full.names=T), full.names=T, pattern="csv")

    # latest data use 'UTF-8' encoding, therefore, try read again if error occurs when using 'Big-5'
    suppressWarnings({
      population_temp=read.csv(dir_file, fileEncoding="Big5")
      population_temp=population_temp[-1, ]
    })
    if(nrow(population_temp)==0){
      population_temp=read.csv(dir_file)
      population_temp=population_temp[-1, ]
    }

    population_temp[, grepl("CNT|RAT|DEN", colnames(population_temp))]=matrix(as.numeric(as.matrix(population_temp[, grepl("CNT|RAT|DEN", colnames(population_temp))])), nrow=nrow(population_temp))
    population=rbind(population, population_temp)

    unlink(paste0(tempdir(), "/temp_pop_TDX"), recursive=T)
    file.remove(paste0(tempdir(), "/temp_pop_TDX.zip"))
    rm(population_temp)
  }
  cli_progress_done()

  temp_id=as.numeric(mapply(function(x) which(c("COUNTY_ID","COUNTY","TOWN_ID","TOWN","VILLAGE","V_ID")[x]==colnames(population)), c(1:6)))
  colnames(population)[temp_id[!is.na(temp_id)]]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME","VILLNAME","VILLCODE")[!is.na(temp_id)]
  temp_id=as.numeric(mapply(function(x) which(c("CODE2","CODE1","CODEBASE")[x]==colnames(population)), c(1:3)))
  colnames(population)[temp_id[!is.na(temp_id)]]=c("SA2CODE","SA1CODE","SA0CODE")[!is.na(temp_id)]

  if (nchar(out)!=0 & out!=F){
    write.csv(population, out, row.names=F)
  }
  return(population)
}



#' @export
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

    suppressWarnings({
      tryCatch({
        fread(url, showProgress=F)
        ZIP<<-F
      }, error=function(err){
        ZIP<<-T
      })
    })

    if(!ZIP){
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
      unlink(tempdir(), recursive=T)

      url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", file, "_", gsub("-", "", date), ".tar.gz")
      download.file(url, paste0(tempdir(), "freeway_TDX.zip"), quiet=T)
      untar(paste0(tempdir(), "freeway_TDX.zip"), exdir=paste0(tempdir(), "freeway_TDX"))
      dir_file=dir(paste0(tempdir(), "freeway_TDX"), full.names=T, recursive=T)

      if(length(dir_file)==0){
        unlink(tempdir(), recursive=T)
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

      unlink(tempdir(), recursive=T)
    }
  }else if(file %in% c("M06A","M07A")){
    url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", gsub("-", "", date), "/00/TDCS_", file, "_", gsub("-", "", date), "_000000.csv")

    suppressWarnings({
      tryCatch({
        fread(url, showProgress=F)
        ZIP<<-F
      }, error=function(err){
        ZIP<<-T
      })
    })

    if(!ZIP){
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
      unlink(tempdir(), recursive=T)

      url=paste0("https://tisvcloud.freeway.gov.tw/history/TDCS/", file, "/", file, "_", gsub("-", "", date), ".tar.gz")
      download.file(url, paste0(tempdir(), "freeway_TDX.zip"), quiet=T)
      untar(paste0(tempdir(), "freeway_TDX.zip"), exdir=paste0(tempdir(), "freeway_TDX"))
      dir_file=dir(paste0(tempdir(), "freeway_TDX"), full.names=T, recursive=T)

      if(length(dir_file)==0){
        unlink(tempdir(), recursive=T)
        stop(paste0("Data of ", date, " is not updated to Traffic Database, Freeway Bureau, MOTC\n Or please check out if your date format is valid!"))
      }

      freeway_data=rbindlist(lapply(dir_file, fread))

      if(file=="M06A"){
        colnames(freeway_data)=c("VehicleType","DetectionTime_O","GantryID_O","DetectionTime_D","GantryID_D","TripLength","TripEnd","TripInformation")
      }else if(file=="M07A"){
        colnames(freeway_data)=c("TimeInterval","GantryFrom","VehicleType","TripDistance","Flow")
      }

      unlink(tempdir(), recursive=T)
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



#' @export
Bus_RealTime=function(access_token, county, format, dates, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(httr)) install.packages("httr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(format=="frequency"){
    url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/RealTimeByFrequency/City/", county, "?Dates=", dates, "&%24format=CSV")
  }else if(format=="stop"){
    url=paste0("https://tdx.transportdata.tw/api/historical/v2/Historical/Bus/RealTimeNearStop/City/", county, "?Dates=", dates, "&%24format=CSV")
  }else{
    stop("Parameter 'format' should be 'frequency' or 'stop'.")
  }
  x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))
  bus_real_time=read.csv(textConnection(content(x, "text", encoding="UTF-8")), header=T)

  if("invalid.token" %in% names(bus_real_time)){
    stop(paste0("Your access token is invalid!"))
  }else if("Message" %in% names(bus_real_time)){
    if(grepl("Dates:", bus_real_time$Message)){
      stop("Parameter 'Dates' are invalid. And it should not be more than 7 days!")
    }else if(grepl("City", bus_real_time$Message)){
      print(TDX_County)
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }else{
      stop(paste0("Date value is invalid! Format of the date:\n", "\nType\t\tFormat\t\t\tExample\n", paste0(rep("=", 61), collapse=""), "\nSingle Date\tYYYY-MM-DD\t\t2023-01-01\nMultiple Dates\tYYYY-MM-DD;YYYY-MM-DD\t2023-01-01;2023-02-01\nDate Range\tYYYY-MM-DD~YYYY-MM-DD\t2023-01-01~2023-01-31"))
    }
  }

  if (nchar(out)!=0 & out!=F){
    write.csv(bus_real_time, out, row.names=F)
  }
  return(bus_real_time)
}



#' @export
Income=function(year, out=F){
  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  tryCatch({
    url=paste0("https://eip.fia.gov.tw/data/ias/ias", year-1911, "/", year-1911, "_165-9.csv")
    income=read.csv(url)
  }, error=function(err){
    stop(paste0("Data of year '", year, "' is not provided!"))
  })
  colnames(income)=c("CountyTown","Village","Houses","Amount","MEAN","MED","Q1","Q3","SD","CV")

  if (nchar(out)!=0 & out!=F){
    write.csv(income, out, row.names=F)
  }
  return(income)
}



#' @export
gtfs=function(access_token, mode, county=F, out=F){
  gtfs_label=c("agency","calendar","calendar_dates","frequencies","routes","shapes","stop_times","stops","trips")
  cat(paste0("Note that GTFS data includes ", paste(gtfs_label, collapse=", "), ". All data would be stored as 'txt' file in a directory if they are exported.\n"))

  gtfs_list=list()
  for(i in c(1:length(gtfs_label))){
    if(mode=="Bus"){
      url=paste0("https://tdx.transportdata.tw/api/premium/v2/GTFS/Static/", mode, "/City/", county, "/", gtfs_label[i], "?%24format=txt")
    }else if(mode %in% c("TRA","THSR")){
      url=paste0("https://tdx.transportdata.tw/api/premium/v2/GTFS/Static/", mode, "/", gtfs_label[i], "?%24format=txt")
    }else{
      stop(paste0("Data of '", mode, "' is not available."))
    }

    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    if(content(x, "text", encoding="UTF-8")==""){
      cat(paste0("'", gtfs_label[i], "' data is not available.\n"))
      gtfs_list[gtfs_label[i]]=list(c())
    }else{
      all_data_temp=read.csv(textConnection(content(x, "text", encoding="UTF-8")), header=T)
      if("invalid.token" %in% names(all_data_temp)){
        stop(paste0("Your access token is invalid!"))
      }else if(sum(grepl("Message", names(all_data_temp)))!=0){
        stop(fromJSON(content(x, "text", encoding="UTF-8"))$Message)
      }else{
        gtfs_list[gtfs_label[i]]=list(all_data_temp)
        if(out!=F){
          write.table(all_data_temp, paste0(out, "/", gtfs_label[i], ".txt"), sep=",")
        }
        cat(paste0("'", gtfs_label[i], "' data downloaded.\n"))
      }
    }
  }
  return(gtfs_list)
}



#' @export
Bike_OD_His=function(bikesys, time, out=F){
  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  if(bikesys==1){
    url="https://tcgbusfs.blob.core.windows.net/dotapp/youbike_ticket_opendata/YouBikeHis.csv"
  }else if(bikesys==2){
    url="https://tcgbusfs.blob.core.windows.net/dotapp/youbike_second_ticket_opendata/YouBikeHis.csv"
  }else{
    stop("Argument 'bikesys' should be either 1 or 2!")
  }
  dir_file=read.csv(url)

  if(nchar(time)==7 & grepl("-", time)){
    time_year=substr(time, 1, regexpr("-", time)-1)
    time_month=substr(time, regexpr("-", time)+1, regexpr("-", time)+2)
  }else{
    stop("Please check out the format of 'time'! It should be 'YYYY-MM'.")
  }

  if(sum(grepl(time, dir_file$fileURL))!=0){
    url=dir_file$fileURL[which(grepl(time, dir_file$fileURL))]
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url, paste0(tempdir(), "/temp_youbike_TDX.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/temp_youbike_TDX.zip"), exdir=paste0(tempdir(), "/temp_youbike_TDX"))

    dir_file=dir(paste0(tempdir(), "/temp_youbike_TDX"), full.names=T)
    if(grepl("csv", dir_file)==F){
      dir_file=dir(dir(paste0(tempdir(), "/temp_youbike_TDX"), full.names=T), full.names=T)
    }

    if(grepl("rent", read.csv(dir_file, header=F, nrows=1)[1])){
      bike_od_his=read.csv(dir_file, header=T)
    }else{
      bike_od_his=read.csv(dir_file, header=F)
    }
    colnames(bike_od_his)=c("RentTime","RentStation","ReturnTime","ReturnStation","RideTime","Date")
    unlink(paste0(tempdir(), "/temp_youbike_TDX"), recursive=T)
    file.remove(paste0(tempdir(), "/temp_youbike_TDX.zip"))
  }else{
    stop(paste0("Data of Youbike ", bikesys, ".0 in ", time, " is not available!"))
  }

  if(nchar(out)!=0 & out!=F){
    write.csv(bike_od_his, out, row.names=F)
  }
  return(bike_od_his)
}



#' @export
Landuse=function(district, year, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(cli)) install.packages("cli")
  if (!require(fs)) install.packages("fs")
  options(timeout=1000)

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  all_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/landuse_area_time.csv")
  if(district %in% c("SA0","SA1","SA2")){
    all_data_temp=filter(all_data, Year==year, SA==district)
    dis_code=c("U0200","U0201","U0202")[which(district==c("SA0","SA1","SA2"))]
  }else{
    stop(paste0("Argument 'district' should be 'SA0', 'SA1', or 'SA2'."))
  }

  if(nrow(all_data_temp)==0){
    stop(paste0("The data is only provided in the following years:\n ", paste(sort(unique(all_data$Year)), collapse=", ")))
  }else{
    year_rev=paste0(year-1911, "Y")
    all_county=unlist(strsplit(all_data_temp$SPACE, "\\|"))
    cat(paste0("Year of ", year, " in the following ",  length(all_county), " counties will be downloaded:\n", paste(all_county, collapse=", "), "\n"))
  }

  landuse_name=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/landuse_name.csv")
  if(year %in% c(2014,2015)){
    landuse_name=landuse_name[landuse_name$CATEGORY=="95-104",]
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=iI0sdRY7vC5HS5s9QthynQ%3d%3d&STTIME=", year_rev, "&STUNIT=", dis_code, "&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
  }else if(year %in% c(2016:2019)){
    landuse_name=landuse_name[landuse_name$CATEGORY=="105-108",]
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=XkioVMS8Cor5vqlKLsBFQg%3d%3d&STTIME=", year_rev, "&STUNIT=", dis_code, "&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
  }else{
    landuse_name=landuse_name[landuse_name$CATEGORY=="109-",]
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=UobMnDnnf0xMbxoTeOgmPA%3d%3d&STTIME=", year_rev, "&STUNIT=", dis_code, "&BOUNDARY=", toupper(url_encode(unlist(strsplit(all_data_temp$SPACE, "\\|")))))
  }

  prog_text=unlist(strsplit(all_data_temp$SPACE, "\\|"))
  landuse=data.frame()
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}] {prog_text[url]}", total=length(url_all))
  for(url in c(1:length(url_all))){
    cli_progress_update()
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url_all[url], paste0(tempdir(), "/temp_landuse_TDX.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/temp_landuse_TDX.zip"), exdir=paste0(tempdir(), "/temp_landuse_TDX"))
    dir_file=dir_ls(paste0(tempdir(), "/temp_landuse_TDX"), recurse=T)
    dir_file=dir_file[grepl("csv", dir_file)]

    # latest data use 'UTF-8' encoding, therefore, try read again if error occurs when using 'Big-5'
    suppressWarnings({
      landuse_temp=read.csv(dir_file, fileEncoding="Big5")
      landuse_temp=landuse_temp[-1, ]
    })
    if(nrow(landuse_temp)==0){
      landuse_temp=read.csv(dir_file)
      landuse_temp=landuse_temp[-1, ]
    }

    landuse_temp[, grepl("L0|SUM", colnames(landuse_temp))]=matrix(suppressWarnings(as.numeric(as.matrix(landuse_temp[, grepl("L0|SUM", colnames(landuse_temp))]))), nrow=nrow(landuse_temp))
    landuse_temp=replace(landuse_temp, is.na(landuse_temp), 0)
    colnames(landuse_temp)[mapply(function(x) which(landuse_name$ORI_NAME[x]==colnames(landuse_temp)), c(1:nrow(landuse_name)))]=landuse_name$NEW_NAME

    unlink(paste0(tempdir(), "/temp_landuse_TDX"), recursive=T)
    file.remove(paste0(tempdir(), "/temp_landuse_TDX.zip"))

    landuse=rbind(landuse, landuse_temp)
    rm(landuse_temp)
  }
  cli_progress_done()

  temp_id=as.numeric(mapply(function(x) which(c("CODE2","CODE1","CODEBASE")[x]==colnames(landuse)), c(1:9)))
  colnames(landuse)[temp_id[!is.na(temp_id)]]=c("SA2CODE","SA1CODE","SA0CODE")[!is.na(temp_id)]

  if (nchar(out)!=0 & out!=F){
    write.csv(landuse, out, row.names=F)
  }
  return(landuse)
}



#' @export
House_Price=function(year, season, out=F){
  if (!require(dplyr)) install.packages("dplyr")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }
  if(year<2012){
    stop("Year is invalid. The data is available from year 2012!")
  }else{
    if(season %in% c(1:4)){
      year_sea=paste0(year-1911, "S", season)
    }else{
      stop("Season is invalid. It should be 1, 2, 3, or 4!")
    }
  }

  url=paste0("https://plvr.land.moi.gov.tw//DownloadSeason?season=", year_sea, "&type=zip&fileName=lvr_landcsv.zip")
  unlink(list.files(tempdir(), full.names=T), recursive=T)
  download.file(url, paste0(tempdir(), "/house_price_tdx.zip"), mode="wb", quiet=T)
  untar(paste0(tempdir(), "/house_price_tdx.zip"), exdir=paste0(tempdir(), "/house_price_tdx"))
  dir_file=dir(paste0(tempdir(), "/house_price_tdx"), full.names=T)
  dir_file=dir_file[grepl(paste(paste0("lvr_land_", c("a","b","c"), ".csv"), collapse="|"), dir_file)]

  houseprice_name=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/houseprice_name.csv")
  house_price=data.frame()
  for(i in dir_file){
    house_price_temp=read.csv(i)
    colnames(house_price_temp)=gsub("\\.", "", colnames(house_price_temp))
    colnames(house_price_temp)=mapply(function(x) houseprice_name$COL_NAME[which(colnames(house_price_temp)[x]==houseprice_name$ORI_NAME)], c(1:ncol(house_price_temp)))
    house_price_temp=house_price_temp[-1,]
    house_price=bind_rows(house_price, house_price_temp)
  }

  temp=c("LAND_AREA","ROOM","HALL","BATH","BUILD_TOTAL_AREA","TOTAL_PRICE","UNIT_PRICE","PARKING_AREA","PARKING_PRICE","BUILD_AREA_MAIN","BUILD_AREA_AUX","BALCONY_AREA","PARKING_UNIT_PRICE")
  temp=temp[temp %in% colnames(house_price)]
  house_price[, temp]=matrix(as.numeric(unlist(house_price[, temp])), nrow(house_price))

  # house_price=mutate(house_price, TRANSACTION_DATE=case_when(
  #   nchar(TRANSACTION_DATE)==6 ~ as.Date(paste0(as.numeric(substr(TRANSACTION_DATE, 1, 2))+1911, "-", substr(TRANSACTION_DATE, 3, 4), "-", substr(TRANSACTION_DATE, 5, 6))),
  #   nchar(TRANSACTION_DATE)==7 ~ as.Date(paste0(as.numeric(substr(TRANSACTION_DATE, 1, 3))+1911, "-", substr(TRANSACTION_DATE, 4, 5), "-", substr(TRANSACTION_DATE, 6, 7))),
  #   TRUE ~ NA
  # ), CONSTRUCTION_DATE=case_when(
  #   nchar(CONSTRUCTION_DATE)==6 ~ as.Date(paste0(as.numeric(substr(CONSTRUCTION_DATE, 1, 2))+1911, "-", substr(CONSTRUCTION_DATE, 3, 4), "-", substr(CONSTRUCTION_DATE, 5, 6))),
  #   nchar(CONSTRUCTION_DATE)==7 ~ as.Date(paste0(as.numeric(substr(CONSTRUCTION_DATE, 1, 3))+1911, "-", substr(CONSTRUCTION_DATE, 4, 5), "-", substr(CONSTRUCTION_DATE, 6, 7))),
  #   TRUE ~ NA
  # ))

  if(nchar(out)!=0 & out!=F){
    write.csv(house_price, out, row.names=F)
  }
  return(house_price)
}



#' @export
School=function(level, year, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(sf)) install.packages("sf")
  if (!require(fs)) install.packages("fs")

  if(dtype=="text"){
    if(!(grepl(".csv|.txt", out)) & out!=F){
      stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
    }
  }else if(dtype=="sf"){
    if(!(grepl(".shp", out)) & out!=F){
      stop("The file name must contain '.shp' when exporting shapefile.\n")
    }
  }else{
    stop(paste0(dtype, " is not valid format. Please use 'text' or 'sf'.\n"))
  }

  all_data=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/school_year.csv")
  all_data_temp=filter(all_data, Year==year, Level==tolower(level))
  if(nrow(all_data_temp)==0){
    if(!tolower(level) %in% c("elementary","junior","senior","university")){
      stop(paste0("'", level, "' is invalid! Argument 'level' must be 'elementary', 'junior', 'senior', or 'university'!"))
    }else{
      stop(paste0("Date of '", tolower(level), "' is only available in the following years:\n", paste(sort(unique(filter(all_data, Level==tolower(level))$Year)), collapse=", ")))
    }
  }else{
    year_rev=paste0(year-1911, "Y")
  }

  if(tolower(level)=="elementary"){
    if(year<=2015){
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=zPzp%2fhQW6gJCn0esWovJpg%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=SJCPGkQm3KtYoC3pTYtCdw%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }else{
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=Ywl8HJm92fAisiMe4xmGAw%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=uqj1F9fpF00qkT7GLYw22Q%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }
  }else if(tolower(level)=="junior"){
    if(year<=2015){
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=BoS3OzTL6YBOxfojkr3v6Q%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=cIOfve6OcAkzVfS8433JSQ%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }else{
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=wSnHwaMwjUruS14yh91lIA%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=qO17yG6gEf44WCJ3kNnqaw%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }
  }else if(tolower(level)=="senior"){
    if(year<=2013){
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=3VXKvoXzKHUw7l9w4dn7YA%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=Q49B6SRee7ql5pYobe2aBQ%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }else{
      url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=uWD8BdyU4K6oRx27MFGYbw%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                     paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=0%2fiLe8nwWzCK5k7tEJ0k4Q%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
    }
  }else if(tolower(level)=="university"){
    url_all=ifelse(all_data_temp$SPACE=="\u5168\u570b(\u4e0d\u542b\u91d1\u9580\u3001\u9023\u6c5f\u3001\u6f8e\u6e56)",
                   paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=V0hFaorLGCnjkSeiT9FIFA%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))),
                   paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=vV2xmoaMzm7yaic2cRgS9g%3d%3d&STTIME=", year_rev, "&STUNIT=null&BOUNDARY=", toupper(url_encode(all_data_temp$SPACE))))
  }

  school=data.frame()
  for(url in c(1:length(url_all))){
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url_all[url], paste0(tempdir(), "/school_tdx.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/school_tdx.zip"), exdir=paste0(tempdir(), "/school_tdx"))
    dir_file=dir_ls(paste0(tempdir(), "/school_tdx"), recurse=T)
    dir_file=dir_file[grepl("csv", dir_file)]

    suppressWarnings({
      school_temp=read.csv(dir_file, fileEncoding="Big5")
      school_temp=school_temp=school_temp[-1,]
    })
    if(nrow(school_temp)==0){
      school_temp=read.csv(dir_file)
      school_temp=school_temp=school_temp[-1,]
    }

    req_col=c(colnames(school_temp)[grepl("CNT|AREA", colnames(school_temp))], "X", "Y")
    school_temp[, req_col]=matrix(as.numeric(unlist(school_temp[, req_col])), nrow=nrow(school_temp))
    school=rbind(school, school_temp)
  }

  if(dtype=="text"){
    if (nchar(out)!=0 & out!=F){
      write.csv(school, out, row.names=F)
    }
  }else if(dtype=="sf"){
    school=mutate(school, geometry=st_as_sfc(ifelse(is.na(X) | is.na(Y), "POINT EMPTY", paste0("POINT(", X, " ", Y, ")"))))%>%
      st_sf(crs=3826)
    if (grepl(".shp", out) & out!=F){
      write_sf(school, out, layer_options="ENCODING=UTF-8")
    }
  }
  return(school)
}



#' @export
Hospital=function(district, time, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(cli)) install.packages("cli")
  if (!require(sf)) install.packages("sf")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting.\n")
  }

  # check if the month is valid
  if(nchar(time)!=7 | !grepl("-", time)){
    stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
  }
  if(as.numeric(substr(time, 1, regexpr("-", time)-1))<2010){
    stop("Only year after 2010 is provided!")
  }
  time_rev=paste0(as.numeric(substr(time, 1, regexpr("-", time)-1))-1911, "Y", substr(time, regexpr("-", time)+1, 10), "M")
  area_time=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/hospital_area_time.csv")
  area_time$TIME=factor(area_time$TIME, levels=(data.frame(TIME=unique(area_time$TIME), TIME_temp=as.numeric(gsub("Y|M", "", unique(area_time$TIME)))) %>% arrange(TIME_temp))$TIME)

  space=c("\u7e23\u5e02","\u9109\u93ae\u5e02\u5340","\u6700\u5c0f\u7d71\u8a08\u5340","\u4e00\u7d1a\u767c\u5e03\u5340","\u4e8c\u7d1a\u767c\u5e03\u5340")[which(district==c("County","Town","SA0","SA1","SA2"))]
  if(length(space)==1){
    space_all=filter(area_time, UNIT==space, TIME==time_rev)
    if(nrow(space_all)==1){
      all_county=toupper(url_encode(unlist(strsplit(space_all$SPACE, "\\|"))))
    }else{
      temp=sort(unique(filter(area_time, UNIT==space)$TIME))
      stop(paste0("Data of '", time, "' is not available. Please use the following time :\n", paste(paste0(as.numeric(substr(temp, 1, regexpr("Y", temp)-1))+1911, "-", substr(temp, regexpr("Y", temp)+1, regexpr("M", temp)-1)), collapse=", ")))
    }
  }else{
    stop("The argument of 'district' must be 'County', 'Town', 'SA0', 'SA1', or 'SA2'!\nNote that data of village is not provided!")
  }

  if(district=="County"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=uzQwsIyPLZpx5X9J6VD%2fyw%3d%3d&STTIME=", time_rev, "&STUNIT=null&BOUNDARY=%E5%85%A8%E5%9C%8B")
  }else if(district=="Town"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=4VGsl7hehrryNP89r0Y1sw%3d%3d&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUB_BOUNDARY=")
  }else if(district=="SA0"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=umFrrMnLFtHUv9N68uxNpQ%3d%3d&STTIME=", time_rev, "&STUNIT=U0200&BOUNDARY=", all_county)
  }else if(district=="SA1"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=YsLzSADPZw3vdiKpnjHHjw%3d%3d&STTIME=", time_rev, "&STUNIT=U0201&BOUNDARY=", all_county)
  }else if(district=="SA2"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=POZ16XysLPo2L69on07AvQ%3d%3d&STTIME=", time_rev, "&STUNIT=U0202&BOUNDARY=", all_county)
  }

  hospital=data.frame()
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]", total=length(url_all))
  for(url in url_all){
    cli_progress_update()
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url, paste0(tempdir(), "/hospital_TDX.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/hospital_TDX.zip"), exdir=paste0(tempdir(), "/hospital_TDX"))
    dir_file=dir(dir(paste0(tempdir(), "/hospital_TDX"), full.names=T), full.names=T)

    dir_file=dir_file[grepl(".csv", dir_file)]
    tryCatch({
      hospital_temp=read.csv(dir_file)
    }, error=function(err){
      hospital_temp <<- read.csv(dir_file, fileEncoding="Big5")
    })
    hospital_temp=hospital_temp[-1, ]
    hospital_temp[, grepl("CNT|BED|SRVP|SRVB", colnames(hospital_temp))]=matrix(as.numeric(as.matrix(hospital_temp[, grepl("CNT|BED|SRVP|SRVB", colnames(hospital_temp))])), nrow=nrow(hospital_temp))

    unlink(paste0(tempdir(), "/hospital_TDX"), recursive=T)
    file.remove(paste0(tempdir(), "/hospital_TDX.zip"))
    if(nrow(hospital)==0){
      hospital=hospital_temp
    }else{
      hospital=bind_rows(hospital, hospital_temp)
    }
    rm(hospital_temp)
  }
  cli_progress_done()

  temp_id=as.numeric(mapply(function(x) which(c("COUNTY_ID","COUNTY","TOWN_ID","TOWN","VILLAGE","V_ID")[x]==colnames(hospital)), c(1:6)))
  colnames(hospital)[temp_id[!is.na(temp_id)]]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME","VILLNAME","VILLCODE")[!is.na(temp_id)]

  if(nchar(out)!=0 & out!=F){
    write.csv(hospital, out, row.names=F)
  }
  return(hospital)
}



#' @export
Business=function(district, time, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(cli)) install.packages("cli")
  if (!require(sf)) install.packages("sf")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  # check if the month is valid
  if(nchar(time)!=7 | !grepl("-", time)){
    stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
  }
  if(as.numeric(substr(time, 1, regexpr("-", time)-1))<2010){
    stop("Only year after 2010 is provided!")
  }
  time_rev=paste0(as.numeric(substr(time, 1, regexpr("-", time)-1))-1911, "Y", substr(time, regexpr("-", time)+1, 10), "M")
  area_time=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/business_area_time.csv")
  area_time$TIME=factor(area_time$TIME, levels=(data.frame(TIME=unique(area_time$TIME), TIME_temp=as.numeric(gsub("Y|M", "", unique(area_time$TIME)))) %>% arrange(TIME_temp))$TIME)

  space=c("\u7e23\u5e02","\u9109\u93ae\u5e02\u5340","\u6700\u5c0f\u7d71\u8a08\u5340","\u4e00\u7d1a\u767c\u5e03\u5340","\u4e8c\u7d1a\u767c\u5e03\u5340")[which(district==c("County","Town","SA0","SA1","SA2"))]
  if(length(space)==1){
    space_all=filter(area_time, UNIT==space, TIME==time_rev)
    if(nrow(space_all)==1){
      all_county=toupper(url_encode(unlist(strsplit(space_all$SPACE, "\\|"))))
    }else{
      temp=sort(unique(filter(area_time, UNIT==space)$TIME))
      stop(paste0("Data of '", time, "' is not available. Please use the following time :\n", paste(paste0(as.numeric(substr(temp, 1, regexpr("Y", temp)-1))+1911, "-", substr(temp, regexpr("Y", temp)+1, regexpr("M", temp)-1)), collapse=", ")))
    }
  }else{
    stop("The argument of 'district' must be 'County', 'Town', 'SA0', 'SA1', or 'SA2'!\nNote that data of village is not provided!")
  }


  if(district=="County"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=M9Zt6AgfXSJRSfmNKnpBNw%3d%3d&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B&TYPE=CSV")
  }else if(district=="Town"){
    # https://segis.moi.gov.tw/STATCloud/reqcontroller.file?method=filedown.downloadproductfile&code=ghqVapNaCUIm4z0SkDRaUw%3d%3d&STTIME=110Y06M&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUB_BOUNDARY=
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=ghqVapNaCUIm4z0SkDRaUw%3d%3d&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUB_BOUNDARY=&TYPE=CSV")
  }else if(district=="SA0"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=LiQSGBXyaafwA6RVul0%2ffg%3d%3d&STTIME=", time_rev, "&STUNIT=U0200&BOUNDARY=", all_county, "&TYPE=CSV")
  }else if(district=="SA1"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=dGd0C47ZXrLBe7jLtYPbDw%3d%3d&STTIME=", time_rev, "&STUNIT=U0201&BOUNDARY=", all_county, "&TYPE=CSV")
  }else if(district=="SA2"){
    url_all=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=K8nmVts%2fDQ4bTxFvqYf%2fTw%3d%3d&STTIME=", time_rev, "&STUNIT=U0202&BOUNDARY=", all_county, "&TYPE=CSV")
  }
  business_name=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/business_name.csv")


  business=data.frame()
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]", total=length(url_all))
  for(url in url_all){
    cli_progress_update()
    unlink(list.files(tempdir(), full.names=T), recursive=T)
    download.file(url, paste0(tempdir(), "/business_TDX.zip"), mode="wb", quiet=T)
    untar(paste0(tempdir(), "/business_TDX.zip"), exdir=paste0(tempdir(), "/business_TDX"))
    dir_file=dir(dir(paste0(tempdir(), "/business_TDX"), full.names=T), full.names=T)

    dir_file=dir_file[grepl(".csv", dir_file)]
    tryCatch({
      business_temp=read.csv(dir_file)
    }, error=function(err){
      business_temp <<- read.csv(dir_file, fileEncoding="Big5")
    })

    business_temp=business_temp[-1, ]
    business_temp[, grepl("CNT", colnames(business_temp))]=matrix(as.numeric(as.matrix(business_temp[, grepl("CNT", colnames(business_temp))])), nrow=nrow(business_temp))
    colnames(business_temp)[mapply(function(x) which(business_name$ORI_NAME[x]==colnames(business_temp)), c(1:nrow(business_name)))]=business_name$NEW_NAME

    unlink(paste0(tempdir(), "/business_TDX"), recursive=T)
    file.remove(paste0(tempdir(), "/business_TDX.zip"))
    if(nrow(business)==0){
      business=business_temp
    }else{
      business=bind_rows(business, business_temp)
    }
    rm(business_temp)
  }
  cli_progress_done()

  temp_id=as.numeric(mapply(function(x) which(c("COUNTY_ID","COUNTY","TOWN_ID","TOWN","VILLAGE","V_ID")[x]==colnames(business)), c(1:6)))
  colnames(business)[temp_id[!is.na(temp_id)]]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME","VILLNAME","VILLCODE")[!is.na(temp_id)]

  if(nchar(out)!=0 & out!=F){
    write.csv(business, out, row.names=F)
  }
  return(business)
}



#' @export
Crash=function(access_token, crash, county, time, dtype="text", out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(httr)) install.packages("httr")

  if((grepl(".csv|.txt", out)) & out!=F){
    warning("No need to set '.csv' or '.txt' when exporting the file in this function. Just set the directory and file name!\n")
  }
  if(!crash %in% c("A1", "A2", "A30")){
    stop("Argument 'crash' must be either 'A1', 'A2' or 'A30'!")
  }

  county_code=c("TPE","NWT","TAO","TXG","TNN","KHH","KEE","HSZ","HSQ","MIA","CHA","NAN","YUN","CYQ","CYI","PIF","ILA","HUA","TTT","KIN","PEN","LIE")[which(county==TDX_County$Code)]
  if(length(county_code)==0 | is.na(county_code)){
    print(TDX_County[1:22,])
    stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
  }

  tryCatch({
    if(nchar(time)==4){
      as.Date(paste0(time, "-01-01"))
    }else{
      as.Date(paste0(time, "-01"))
    }
  }, error=function(err){
    stop(paste0("Date format is valid! It should be 'YYYY' or 'YYYY-MM'!"))
  })

  tryCatch({
    if(crash!="A30"){
      url=paste0("https://tdx.transportdata.tw/api/historical/v1/TrafficAccident/", crash, "T1/", county_code, "/DateFilter/", time, "?$format=json")
      x=GET(url, add_headers(Accept="application/json", Authorization=paste("Bearer", access_token)))
      crash_t1=fromJSON(content(x, as="text", encoding="UTF-8"))
    }else{
      crash_t1=NULL
    }
    url=paste0("https://tdx.transportdata.tw/api/historical/v1/TrafficAccident/", crash, "T2/", county_code, "/DateFilter/", time, "?$format=json")
    x=GET(url, add_headers(Accept="application/json", Authorization=paste("Bearer", access_token)))
    crash_t2=fromJSON(content(x, as="text", encoding="UTF-8"))
  }, error=function(err){
    stop(paste0("Your access token is invalid!"))
  })

  tryCatch({
    url="https://tdx.transportdata.tw/api/historical/v1/AccidentBasic/TrafficEventCode?$format=json"
    x=GET(url, add_headers(Accept="application/json", Authorization=paste("Bearer", access_token)))
    crash_code=fromJSON(content(x, as="text", encoding="UTF-8"))
  })

  if(length(crash_t1)==0 | sum(names(crash_t1) %in% "ErrorMessage")){
    warning(paste0(county, " has no crash in ", time,  " ,or data (crash data) is not avaliable."))
  }
  if(length(crash_t2)==0 | sum(names(crash_t2) %in% "ErrorMessage")){
    warning(paste0(county, " has no crash in ", time,  " ,or data (person data) is not avaliable."))
  }

  if(dtype=="sf" & length(crash_t1)!=0){
    crash_t1$geometry=st_as_sfc(ifelse(!(crash_t1$Lon==0 | is.na(crash_t1$Lon) | crash_t1$Lat==0 | is.na(crash_t1$Lat)), paste0("POINT(", crash_t1$Lon, " ", crash_t1$Lat, ")"), "POINT EMPTY"))
    crash_t1=st_sf(crash_t1, crs=4326)
    if(sum(st_is_empty(crash_t1$geometry))!=0){
      warning(paste0("Note that some of latitude and longitude are missing, the geometry is remained empty!"))
    }
    if(nchar(out)!=0 & out!=F){
      write.table(colnames(crash_t1), paste0(out, "_T1colname.txt"), sep=",", row.names=F, col.names=F)
      write_sf(crash_t1, paste0(out, "_T1.shp"), layer_options="ENCODING=UTF-8")
      warning(paste0("Some colnames have been shorten. The original column name is retrieved in, '", paste0(out, "_T1colname.txt")), "'")
    }
  }else{
    if(nchar(out)!=0 & out!=F){
      write.csv(crash_t1, paste0(out, "_T1.csv"), row.names=F)
    }
  }

  if(nchar(out)!=0 & out!=F){
    write.csv(crash_t2, paste0(out, "_T2.csv"), row.names=F)
    write.csv(crash_code, paste0(out, "_DEF.csv"), row.names=F)
  }

  crash_all=list(T1=crash_t1, T2=crash_t2, DEF=crash_code)
  warning(paste0("Please use '$T1' to retrieve the crash data, and use '$T2' to retrieve the person data."))
  return(crash_all)
}



#' @export
Bus_ScheduleEst=function(access_token, county, routename, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(tidyr)) install.packages("tidyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(cli)) install.packages("cli")
  if (!require(urltools)) install.packages("urltools")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  routename=unique(routename)
  cat(paste0("Total: ", length(routename), " Routes\n"))
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]  {.emph RouteName: {routename[pb_current]}}", total=length(routename))
  num_of_nodata=0

  sch_est_ALL=data.frame()
  for (route in routename){
    cli_progress_update()
    if (county=="Intercity"){
      url=paste0("https://tdx.transportdata.tw/api/premium/V3/Map/Estimation/Bus/Schedule/InterCity/RouteName/", toupper(url_encode(route)), "?&%24format=JSON")
    }else if(county %in% TDX_County$Code[1:22]){
      url=paste0("https://tdx.transportdata.tw/api/premium/V3/Map/Estimation/Bus/Schedule/City/", county, "/RouteName/", toupper(url_encode(route)), "?&%24format=JSON")
    }else{
      print(TDX_County[1:22,])
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      subroute_info=data.frame(fromJSON(content(x, as="text"))$Schedules)
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })
    if(length(subroute_info)==0){
      cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
      num_of_nodata=num_of_nodata+1
      next
    }

    timetable_all=tidyr::unnest(subroute_info, cols="Timetables")
    timetable_all$TripID=c(1:nrow(timetable_all))
    timetable_all=tidyr::unnest(timetable_all, cols="StopTimes")

    #
    # timetable_all=mapply(function(x) subroute_info$Timetables[[x]]$StopTimes, c(1:length(subroute_info$Timetables)))
    # for(i in c(1:length(timetable_all))){
    #   timetable_all[[i]]$StopName=timetable_all[[i]]$StopName$Zh_tw
    # }
    # timetable_all=rbindlist(timetable_all)
    #
    # rep_id=mapply(function(x) nrow(subroute_info$Timetables[[x]]$StopTimes[[1]]), c(1:length(subroute_info$Timetables)))
    # timetable_all=data.frame(timetable_all, ServiceTag=rep(mapply(function(x) subroute_info$Timetables[[x]]$ServiceDay$ServiceTag, c(1:length(subroute_info$Timetables))), times=rep_id))
    #
    # timetable_all=cbind(subroute_info[rep(c(1:nrow(subroute_info)), times=rep_id), c("TripID","SubRouteUID","SubRouteID","SubRouteName","Direction")], timetable_all)
    # row.names(timetable_all)=NULL
    #
    sch_est_ALL=rbind(sch_est_ALL, timetable_all)
  }
  cli_alert_info(ifelse(num_of_nodata==0, "All Done!", paste0("All Done!\n", num_of_nodata, " RouteIDs have no data!")))
  cli_progress_done()

  if (nchar(out)!=0 & out!=F){
    write.csv(sch_est_ALL, out, row.names=F)
  }
  return(sch_est_ALL)
}



#' @export
Bus_Distance=function(access_token, county, routeid, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(httr)) install.packages("httr")
  if (!require(cli)) install.packages("cli")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt' when exporting text.\n")
  }

  routeid=unique(routeid)
  cat(paste0("Total: ", length(routeid), " Routes\n"))
  cli_progress_bar(format="Downloading {pb_bar} {pb_percent} [{pb_eta}]  {.emph RouteID: {routeid[pb_current]}}", total=length(routeid))
  num_of_nodata=0

  busdist_ALL=data.frame()
  for (route in routeid){
    cli_progress_update()
    if (county=="Intercity"){
      url=paste0("https://tdx.transportdata.tw/api/premium/S2SDistance/InterCity/RouteID/", route, "?&%24format=JSON")
    }else if(county %in% TDX_County$Code[1:22]){
      url=paste0("https://tdx.transportdata.tw/api/premium/S2SDistance/City/", county, "/RouteID/", route, "?&%24format=JSON")
    }else{
      print(TDX_County[1:22,])
      stop(paste0("City: '", county, "' is not valid. Please check out the parameter table above."))
    }
    x=GET(url, add_headers(Accept="application/+json", Authorization=paste("Bearer", access_token)))

    tryCatch({
      subroute_info=fromJSON(content(x, as="text"))
    }, error=function(err){
      stop(paste0("Your access token is invalid!"))
    })
    if(length(subroute_info)==0){
      cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
      num_of_nodata=num_of_nodata+1
      next
    }else{
      if(sum(lengths(subroute_info$Stops))==0){
        cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
        num_of_nodata=num_of_nodata+1
        next
      }
    }

    subroute_info$RouteName=subroute_info$RouteName$Zh_tw
    subroute_info$SubRouteName=subroute_info$SubRouteName$Zh_tw

    subroute_info=subroute_info[lengths(subroute_info$Stops)!=0,]

    if(nrow(subroute_info)!=0 & sum(subroute_info$Stops %in% NA)==0){
      rep_id=mapply(function(x) nrow(subroute_info$Stops[[x]]), c(1:nrow(subroute_info)))
      busdist=cbind(subroute_info[rep(c(1:nrow(subroute_info)), rep_id), c("RouteUID","RouteID","RouteName","SubRouteUID","SubRouteID","SubRouteName","Direction")], rbindlist(subroute_info$Stops))
      row.names(busdist)=NULL
      busdist_ALL=rbind(busdist_ALL, busdist)
    }else{
      cli_alert_info(paste0("Data of RouteID: ", route, " is not available."))
    }
  }

  cli_alert_info(ifelse(num_of_nodata==0, "All Done!", paste0("All Done!\n", num_of_nodata, " RouteIDs have no data!")))
  cli_progress_done()

  if (nchar(out)!=0 & out!=F){
    write.csv(busdist_ALL, out, row.names=F)
  }
  return(busdist_ALL)
}



#' @export
Rail_Patronage=function(operator, ym=NULL, OD=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(httr)) install.packages("httr")
  if (!require(rvest)) install.packages("rvest")
  if (!require(data.table)) install.packages("data.table")
  if (!require(jsonlite)) install.packages("jsonlite")
  if (!require(readODS)) install.packages("readODS")
  if (!require(readxl)) install.packages("readxl")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  if(operator=="TRA"){
    warning("Argument 'ym' is deprecated. All data from 2005 to the latest avialable data are downloaded!")
  }else if(operator=="THSR"){
    warning("Argument 'ym' is deprecated. All data from 2017 to the latest avialable data are downloaded!")
  }else if(operator=="TYMC"){
    warning("Argument 'ym' is deprecated. All data from 2019 to the latest avialable data are downloaded!")
  }else{
    if((nchar(ym)!=7 | !grepl("-", ym))){
      stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
    }else{
      YEAR=unlist(strsplit(ym, "-"))[1]
      MONTH=unlist(strsplit(ym, "-"))[2]
    }
  }

  if(OD==T){
    if(!operator %in% c("TRTC")){
      warning(paste0("Argument 'OD' is deprecated. OD data is not available for ", operator, "!"))
    }
  }

  if(operator=="TRA"){
    tra_station=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/refs/heads/main/others/tra_station.csv", colClasses=c("StationID"="character"))
    url_all=c("https://ods.railway.gov.tw/tra-ods-web/ods/download/dataResource/8ae4cabe6924dcdb016927b0dade013a",
              "https://ods.railway.gov.tw/tra-ods-web/ods/download/dataResource/8ae4cac3799a9b6b01799d00fd37014b")
    unlink(paste0(tempdir(), "/tra_patronage"), recursive=T)

    all_patronage=data.frame()
    for(i in url_all){
      download.file(i, paste0(tempdir(), "/tra_patronage.zip"), mode="wb")
      untar(paste0(tempdir(), "/tra_patronage.zip"), exdir=paste0(tempdir(), "/tra_patronage"))
      dir_files=dir(paste0(tempdir(), "/tra_patronage"), pattern=".csv", full.names=T)
      dir_files=dir_files[grepl("\u6bcf\u65e5", dir_files)]
      patronage_temp=rbindlist(lapply(dir_files, fread))
      if(ncol(patronage_temp)==5){
        names(patronage_temp)=c("trnOpDate","staCode","STOP_NAME","gateIn","gateOut")
        patronage_temp$staCode=as.character(patronage_temp$staCode)
      }else{
        patronage_temp$staCode=ifelse(nchar(patronage_temp$staCode)==3, paste0("0", patronage_temp$staCode), patronage_temp$staCode)
      }
      patronage_temp$trnOpDate=as.character(patronage_temp$trnOpDate)

      all_patronage=bind_rows(all_patronage, patronage_temp)
      unlink(paste0(tempdir(), "/tra_patronage"), recursive=T)
      file.remove(paste0(tempdir(), "/tra_patronage.zip"))
    }
    patronage_temp=fromJSON("https://ods.railway.gov.tw/tra-ods-web/ods/download/dataResource/8ae4cabf6973990e0169947ed32454b9")%>%
      mutate(gateInComingCnt=as.numeric(gateInComingCnt),
             gateOutGoingCnt=as.numeric(gateOutGoingCnt))
    all_patronage=bind_rows(all_patronage, patronage_temp)

    temp=filter(all_patronage, is.na(STOP_NAME))%>%
      dplyr::select(-STOP_NAME)%>%
      left_join(tra_station, by=c("staCode"="StationID"))
    all_patronage$STOP_NAME[is.na(all_patronage$STOP_NAME)]=temp$StationName

  }else if(operator=="TRTC"){
    if(OD==T){
      download.file(paste0("https://data.taipei/api/dataset/63f31c7e-7fc3-418b-bd82-b95158755b4d/resource/eb481f58-1238-4cff-8caa-fa7bb20cb4f4/download"), paste0(tempdir(), "/taipei_mrt_data.csv"), mode="wb", quiet=T)
      mrt_od_data=read.csv(paste0(tempdir(), "/taipei_mrt_data.csv"))
      names(mrt_od_data)[2]="YM"
      mrt_od_data=filter(mrt_od_data, YM==gsub("-", "", ym))
      if(nrow(mrt_od_data)==0){
        stop(paste0("Data of ", ym, " is not available!"))
      }

      options(timeout=1000)
      download.file(mrt_od_data$URL, paste0(tempdir(), "/taipei_mrt_data.csv"), mode="wb", quiet=T)
      all_patronage=fread(paste0(tempdir(), "/taipei_mrt_data.csv"))
      names(all_patronage)=c("Date","Time","gateIn","gateOut","Patronage")
      unlink(list.files(tempdir(), full.names=T), recursive=T)
    }else{
      tryCatch({
        download.file(paste0("https://web.metro.taipei/RidershipPerStation/", YEAR, MONTH,"_cht.ods"), paste0(tempdir(), "/trtc_patronage.ods"), mode="wb", quiet=T)
      }, error=function(err){
        stop(paste0("Data of ", ym, " is not available!"))
      })
      temp1=read_ods(paste0(tempdir(), "/trtc_patronage.ods"), sheet=1)%>%
        data.table()
      names(temp1)[1]="Date"
      temp1=melt(temp1, id.vars="Date", measure.vars=2:ncol(temp1), variable.name="StationName", value.name="Patronage")%>%
        mutate(TYPE="gateOutGoingCnt")
      temp2=read_ods(paste0(tempdir(), "/trtc_patronage.ods"), sheet=1)%>%
        data.table()
      names(temp2)[1]="Date"
      temp2=melt(temp2, id.vars="Date", measure.vars=2:ncol(temp2), variable.name="StationName", value.name="Patronage")%>%
        mutate(TYPE="gateInComingCnt")
      all_patronage=rbind(temp1, temp2)
      all_patronage$Date=as.Date(all_patronage$Date)
      all_patronage=dcast(all_patronage, Date+StationName ~ TYPE, value.var="Patronage")
    }
  }else if(operator=="KRTC"){
    url=paste0("https://kcgdg.kcg.gov.tw/CWSSLWEB/Attachment/Download.ashx?VP_FileName=%e9%ab%98%e9%9b%84%e9%83%bd%e6%9c%83%e5%8d%80%e5%a4%a7%e7%9c%be%e6%8d%b7%e9%81%8b%e7%b3%bb%e7%b5%b1%e5%90%84%e7%ab%99%e6%97%85%e9%81%8b%e9%87%8f%e7%b5%b1%e8%a8%88%e8%a1%a8(", as.numeric(YEAR)-1911, ".", as.numeric(MONTH), ".).xlsx")
    tryCatch({
      download.file(url, paste0(tempdir(), "/krtc_patronage.xlsx"), mode="wb")
      }, error=function(err){
      stop(paste0("Data of ", ym, " is not available!"))
    })
    all_patronage=read_excel(paste0(tempdir(), "/krtc_patronage.xlsx"))%>%
      data.frame()
    temp1=all_patronage[grepl("R", all_patronage[,1]), 1:3]
    temp2=all_patronage[grepl("O", all_patronage[,5]), 5:7]
    names(temp1)=names(temp2)=c("Station","gateInComingCnt","gateOutGoingCnt")
    all_patronage=rbind(temp1, temp2)%>%
      mutate(StationID=substr(Station, 1, regexpr(" ", Station)-1),
             StationName=substr(Station, regexpr(" ", Station)+1, 100),
             gateInComingCnt=as.numeric(gateInComingCnt),
             gateOutGoingCnt=as.numeric(gateOutGoingCnt))%>%
      select(StationID, StationName, gateInComingCnt, gateOutGoingCnt)
    unlink(list.files(tempdir(), full.names=T))
  }else if(operator=="THSR"){
    html_content=read_html("https://www.thsrc.com.tw/corp/9571df11-8524-4935-8a46-0d5a72e6bc7c")
    station_name=gsub(" ", "", unlist(strsplit(html_text(html_nodes(html_content, xpath='//*[@id="fixTable01"]/thead/tr')), "\n")))
    station_name=station_name[2:13]
    temp1=data.frame(Month=html_text(html_nodes(html_content, ".passengers th")),
                     lapply(2:13, function(x) as.numeric(gsub(",", "", html_text(html_nodes(html_content, paste0("#fixTable td:nth-child(", x, ")")))))))%>%
      data.table()%>%
      unique()
    names(temp1)[2:ncol(temp1)]=station_name
    temp1=melt(temp1, id.vars="Month", variable.name="StationName", value.name="gateInComingCnt")
    temp2=data.frame(Month=html_text(html_nodes(html_content, ".passengers th")),
                     lapply(2:13, function(x) as.numeric(gsub(",", "", html_text(html_nodes(html_content, paste0("#fixTable01 td:nth-child(", x, ")")))))))%>%
      data.table()%>%
      unique()
    names(temp2)[2:ncol(temp2)]=station_name
    temp2=melt(temp2, id.vars="Month", variable.name="StationName", value.name="gateOutGoingCnt")
    all_patronage=left_join(temp1, temp2, by=c("StationName", "Month"))%>%
      mutate(Patronage=gateInComingCnt+gateOutGoingCnt)%>%
      data.frame()
  }else if(operator=="TYMC"){
    url=html_nodes(html_content, xpath='//*[@id="__nuxt"]/div/div[1]/main/div[3]/div[3]/div[2]/div[2]')%>%
      html_nodes("a")%>%
      html_attr("href")

    all_patronage=data.frame()
    for(i in url){
      all_patronage=rbind(all_patronage, read.csv(i, fileEncoding="Big5"))
    }
    names(all_patronage)=c("Year","Month","Station","AveDailyPatronage")
    all_patronage=mutate(all_patronage, Year=Year+1911)%>%
      mutate(StationID=substr(Station, 1, regexpr(" ", Station)-1),
             StationName=substr(Station, regexpr(" ", Station)+2, 100))%>%
      select(Year, Month, StationID, StationName, Patronage)
  }else{
    stop("This function is currently available for downloading patronage of Taiwan Railway (TRA), Taiwan High Speed Rail (THSR), Taipei MRT (TRTC), Kaoshiung MRT (KRTC), and Tayouan Airport MRT (TYMC)!")
  }

  if(nchar(out)!=0 & out!=F){
    write.csv(all_patronage, out, row.names=F)
  }
  return(all_patronage)
}



#' @export
CellularPopulation=function(district, time=NULL, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(urltools)) install.packages("urltools")
  if (!require(cli)) install.packages("cli")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  area_time=read.csv("https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/cellular_year.csv")
  area_time$TIME=factor(area_time$TIME, levels=(data.frame(TIME=unique(area_time$TIME), TIME_temp=as.numeric(gsub("Y|M", "", unique(area_time$TIME)))) %>% arrange(TIME_temp))$TIME)

  space=c("\u7e23\u5e02","\u9109\u93ae\u5e02\u5340")[which(district==c("County","Town"))]
  if(length(space)==1){
    # check if the month is valid
    if(is.null(time)){
      temp=sort(unique(filter(area_time, UNIT==space)$TIME))
      time_rev=temp[length(temp)]
      cat(paste0("Download the latest data ", time_rev, ".\nIf a specific time of data is required, please set the argument 'time'.\n"))
    }else if(nchar(time)!=7 | !grepl("-", time)){
      stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
    }else{
      time_rev=paste0(as.numeric(substr(time, 1, regexpr("-", time)-1))-1911, "Y", substr(time, regexpr("-", time)+1, 10), "M")
    }

    space_all=filter(area_time, UNIT==space, TIME==time_rev)
    if(nrow(space_all)!=1){
      temp=sort(unique(filter(area_time, UNIT==space)$TIME))
      stop(paste0("Data of '", time, "' is not available. Please use the following time :\n", paste(paste0(as.numeric(substr(temp, 1, regexpr("Y", temp)-1))+1911, "-", substr(temp, regexpr("Y", temp)+1, regexpr("M", temp)-1)), collapse=", ")))
    }
  }else{
    stop("The argument of 'district' must be 'County' or 'Town'!")
  }

  if(district=="County"){
    url=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=aMLV7QwPwx1LNTMGyiEaGQ%3d%3d&STTIME=", time_rev, "&STUNIT=U01CO&BOUNDARY=%E5%85%A8%E5%9C%8B")
  }else if(district=="Town"){
    url=paste0("https://segis.moi.gov.tw/STATCloud/reqcontroller.file/?method=filedown.downloadproductfile&code=HguGfOFL5JA%2b7zqnoudkJQ%3d%3d&STTIME=", time_rev, "&STUNIT=U01TO&BOUNDARY=%E5%85%A8%E5%9C%8B&SUB_BOUNDARY=")
  }

  unlink(list.files(tempdir(), full.names=T), recursive=T)
  download.file(url, paste0(tempdir(), "/cellular_TDX.zip"), mode="wb", quiet=T)
  untar(paste0(tempdir(), "/cellular_TDX.zip"), exdir=paste0(tempdir(), "/cellular_TDX"))
  dir_file=dir(dir(paste0(tempdir(), "/cellular_TDX"), full.names=T), full.names=T)

  dir_file=dir_file[grepl(".csv", dir_file)]
  tryCatch({
    cellular_pop=read.csv(dir_file)
  }, error=function(err){
    cellular_pop <<- read.csv(dir_file, fileEncoding="Big5")
  })

  unlink(paste0(tempdir(), "/cellular_TDX"), recursive=T)
  file.remove(paste0(tempdir(), "/cellular_TDX.zip"))
  cat("The column name is summarised below:\n")
  print(t(data.frame(cellular_pop[1,], row.names="Column Name")))

  cellular_pop=cellular_pop[-1,]
  temp_id=as.numeric(mapply(function(x) which(c("COUNTY_ID","COUNTY","TOWN_ID","TOWN","VILLAGE","V_ID")[x]==colnames(cellular_pop)), c(1:6)))
  colnames(cellular_pop)[temp_id[!is.na(temp_id)]]=c("COUNTYCODE","COUNTYNAME","TOWNCODE","TOWNNAME","VILLNAME","VILLCODE")[!is.na(temp_id)]

  if(nchar(out)!=0 & out!=F){
    write.csv(cellular_pop, out, row.names=F)
  }

  return(cellular_pop)
}



#' @export
VehicleOwn=function(ym, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(tidyr)) install.packages("tidyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(rvest)) install.packages("rvest")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  tryCatch({
    as.Date(paste0(ym, "-01"))
    YEAR=as.numeric(unlist(strsplit(ym, "-"))[1])
    MONTH=unlist(strsplit(ym, "-"))[2]
  }, error=function(err){
    stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
  })

  url=paste0("https://stat.thb.gov.tw/hb01/webMain.aspx?sys=220&ym=", YEAR-1911, MONTH, "&ymt=", YEAR-1911, MONTH, "&kind=21&type=1&funid=1110007&cycle=1&outmode=0&compmode=0&outkind=1&fldspc=3,4,8,2,11,5,17,4,22,5,28,1,33,1,&codspc0=1,400,1")
  tryCatch({
    html_content=read_html(url)
  }, error=function(err){
    stop(paste0("Data for ", ym, " is not available! Only date later than year 2016 is allowed to download."))
  })
  county=html_text(html_nodes(html_content, "tr+ tr .stytitle+ .stytitle"))
  type=html_text(html_nodes(html_content, "tr:nth-child(1) .stytitle+ .stytitle"))
  veh_own=suppressWarnings(data.frame(matrix(as.numeric(gsub(",", "", html_text(html_nodes(html_content, ".stydata")))), ncol=length(county)/length(type), byrow=T)))
  names(veh_own)=county[1:(length(county)/length(type))]
  veh_own$VehicleType=type
  veh_own=melt(data.table(veh_own), id.vars="VehicleType", variable.name="TOWNNAME", value.name="VehicleCount")%>%
    data.frame()%>%
    mutate(TOWNNAME=as.character(TOWNNAME),
           COUNTYNAME=ifelse(TOWNNAME %in% TDX_County$Operator, TOWNNAME, NA),
           VehicleCount=ifelse(is.na(VehicleCount), 0, VehicleCount))%>%
    fill(COUNTYNAME, .direction="down")%>%
    filter(COUNTYNAME!=TOWNNAME | COUNTYNAME %in% c("\u65b0\u7af9\u5e02","\u5609\u7fa9\u5e02"))%>%
    select(COUNTYNAME, TOWNNAME, VehicleType, VehicleCount)

  warning(paste0("\u65b0\u7af9\u5e02 & \u5609\u7fa9\u5e02 only have statistics in county level!"))

  if(nchar(out)!=0 & out!=F){
    write.csv(veh_own, out, row.names=F)
  }

  return(veh_own)
}



#' @export
Air_Patronage=function(ym, domestic=F, out=F){
  if (!require(dplyr)) install.packages("dplyr")
  if (!require(tidyr)) install.packages("tidyr")
  if (!require(data.table)) install.packages("data.table")
  if (!require(rvest)) install.packages("rvest")
  if (!require(readxl)) install.packages("readxl")

  if(!(grepl(".csv|.txt", out)) & out!=F){
    stop("The file name must contain '.csv' or '.txt'.\n")
  }

  tryCatch({
    as.Date(paste0(ym, "-01"))
  }, error=function(err){
    stop(paste0("Date format is valid! It should be 'YYYY-MM'!"))
  })

  if(!domestic){
    html_content=read_html("https://www.caa.gov.tw/article.aspx?a=1752&lang=1")
    html_content=html_nodes(html_content, ".download-filebase:nth-child(1)")
    temp=gsub("\\.\\.", "", html_attr(html_content, "href"))
    all_url=suppressWarnings(data.frame(url=paste0("https://www.caa.gov.tw/FileAtt.ashx?", substr(temp, regexpr("lang", temp)+1, 100)),
                                        title=gsub(".xls", "", html_attr(html_content, "title")))%>%
                               separate(title, c("Year","Month"), sep="\u5e74|\u6708")%>%
                               mutate(Year=as.numeric(Year)+1911,
                                      Month=as.numeric(Month),
                                      MON=paste0(Year, "-", ifelse(nchar(Month)==1, paste0(0, Month), Month))))%>%
      arrange(Year, Month)
    all_url$MON=factor(all_url$MON, levels=all_url$MON, ordered=T)

    all_url_temp=filter(all_url, MON==ym)
    if(nrow(all_url_temp)==0){
      stop(paste0("Data for ", ym, " is not available! And note that only date later than year 2009 is allowed to download."))
    }

    download.file(all_url_temp$url[1], paste0(tempdir(), "/air_pat.xls"), mode="wb", quite=T)


    if(all_url_temp$MON>factor("2015-01", levels=all_url$MON, ordered=T)){
      all_sheet=excel_sheets(paste0(tempdir(), "/air_pat.xls"))
      all_sheet=all_sheet[grepl("36", all_sheet)]
      all_sheet=gsub(" ", "", all_sheet)
      all_sheet=all_sheet[all_sheet!="36" & all_sheet!="36-0"]

      air_pat_all=data.frame()
      for(i in all_sheet){
        air_pat=suppressMessages(read_excel(paste0(tempdir(), "/air_pat.xls"), sheet=i)%>%
                                   data.frame())
        names(air_pat)=c("Airport_O","Airport_D","Airline",
                         "Flight_Total","Seat_Total","Patronage_Total","LoadFactor_Total",
                         "Flight_Arrival","Seat_Arrival","Patronage_Arrival","LoadFactor_Arrival",
                         "Flight_Departure","Seat_Departure","Patronage_Departure","LoadFactor_Departure")
        airport_name=air_pat[min(which(grepl("\u886836", air_pat[,1]))), 1]
        airport_name=substr(airport_name, regexpr("　", airport_name)+1, regexpr("\u570b\u969b\u53ca\u5169\u5cb8", airport_name)-1)

        air_pat=fill(air_pat, Airport_D, .direction="down")%>%
          filter(!is.na(Airline),
                 Airport_O!="\u5e8f\u865f",
                 Airport_D!="\u822a    \u7dda")
        air_pat[, grepl("Total|Arrival|Departure", names(air_pat))]=matrix(as.numeric(as.matrix(air_pat[, grepl("Total|Arrival|Departure", names(air_pat))])), ncol=12)
        air_pat$Airport_O=airport_name
        air_pat_all=rbind(air_pat_all, air_pat)
      }
    }else{
      air_pat_list=suppressMessages(read_excel(paste0(tempdir(), "/air_pat.xls"))%>%
                                      data.frame())
      names(air_pat_list)=c("Airport_O","Airport_D","Airline",
                            "Flight_Total","Seat_Total","Patronage_Total","LoadFactor_Total",
                            "Flight_Arrival","Seat_Arrival","Patronage_Arrival","LoadFactor_Arrival",
                            "Flight_Departure","Seat_Departure","Patronage_Departure","LoadFactor_Departure")

      temp=grepl("\u8868[0-9][0-9]", air_pat_list$Airport_O)
      air_pat_list$TabID[temp]=substr(air_pat_list$Airport_O[temp], 1, regexpr("　", air_pat_list$Airport_O[temp])-1)
      temp=unique(substr(air_pat_list$Airport_O[temp], 1, regexpr("　", air_pat_list$Airport_O[temp])-1))
      air_pat_list=fill(air_pat_list, TabID, .direction="down")%>%
        filter(!is.na(TabID), TabID!="")
      air_pat_list=split(air_pat_list, air_pat_list$TabID)
      if(length(air_pat_list)!=1){
        air_pat_list[[1]]=NULL
      }

      air_pat_all=data.frame()
      for(i in air_pat_list){
        air_pat=fill(i, Airport_D, .direction="down")%>%
          filter(!is.na(Airline),
                 Airport_O!="\u5e8f\u865f",
                 Airport_D!="\u822a    \u7dda")%>%
          select(-TabID)

        if(length(air_pat_list)==1){
          airport_name="\u81fa\u7063\u6843\u5712\u570b\u969b\u6a5f\u5834\u6216\u9ad8\u96c4\u570b\u969b\u6a5f\u5834"
        }else{
          airport_name=i[min(which(grepl("\u8868", i$Airport_O))), 1]
          airport_name=substr(airport_name, regexpr("　", airport_name)+1, regexpr("\u570b\u969b\u53ca\u5169\u5cb8|\u570b\u969b\u822a\u7dda", airport_name)-1)
          airport_name=gsub("\u81fa\u7063\u5730\u5340|\u53f0\u7063\u5730\u5340", "", airport_name)
        }

        air_pat[, grepl("Total|Arrival|Departure", names(air_pat))]=matrix(as.numeric(as.matrix(air_pat[, grepl("Total|Arrival|Departure", names(air_pat))])), ncol=12)
        air_pat$Airport_O=airport_name
        air_pat_all=rbind(air_pat_all, air_pat)
      }
    }
  }else{
    html_content=read_html("https://www.caa.gov.tw/article.aspx?a=1739&lang=1")
    html_content=html_nodes(html_content, ".download-filebase:nth-child(1)")
    temp=gsub("\\.\\.", "", html_attr(html_content, "href"))
    all_url=suppressWarnings(data.frame(url=paste0("https://www.caa.gov.tw/FileAtt.ashx?", substr(temp, regexpr("lang", temp)+1, 100)),
                                        title=gsub(".xls", "", html_attr(html_content, "title")))%>%
                               separate(title, c("Year","Month"), sep="\u5e74|\u6708")%>%
                               mutate(Year=as.numeric(Year)+1911))%>%
      arrange(Year, Month)

    all_url_temp=filter(all_url, Year==substr(ym, 1, 4))
    if(nrow(all_url_temp)==0){
      stop(paste0("Data for ", ym, " is not available! And note that only date later than year 2000 is allowed to download."))
    }
    warning(paste0("All months in year ", substr(ym, 1, 4), " are extracted, please filter the data by argument 'Year' and 'Month' on you own."))

    download.file(all_url_temp$url[1], paste0(tempdir(), "/air_pat.xls"), mode="wb", quite=T)

    all_sheet=excel_sheets(paste0(tempdir(), "/air_pat.xls"))
    all_sheet=all_sheet[!grepl("-|\u8f09\u5ba2\u7387", all_sheet)]

    air_pat_all=data.frame()
    for(j in all_sheet){
      air_pat=suppressMessages(read_excel(paste0(tempdir(), "/air_pat.xls"), sheet=j)%>%
                                 data.frame())

      if(all_url_temp$Year>2002){
        all_airline=unique(as.character(air_pat[which(air_pat[,2]=="\u822a\u7a7a\u516c\u53f8"),]))
        all_airline=all_airline[!is.na(all_airline) & !all_airline %in% c("\u822a\u7a7a\u516c\u53f8","\u7e3d\u8a08")]

        air_pat_mon=data.frame()
        for(i in c(1:length(all_airline))){
          air_pat_temp=air_pat[, c(1:2, (3+(i-1)*4):(6+(i-1)*4))]
          names(air_pat_temp)=c("ID","OD","Flight","Seat","Patronage","LoadFactor")
          air_pat_temp$Airline=all_airline[i]
          air_pat_mon=rbind(air_pat_mon, air_pat_temp)
        }
        air_pat_mon=suppressWarnings(filter(air_pat_mon, !is.na(as.numeric(ID)))%>%
                                       separate(OD, c("Airport_O","Airport_D"), sep="—"))
        if(grepl("\u5e74|_|-", j)){
          air_pat_mon=suppressWarnings(mutate(air_pat_mon, ym=j)%>%
                                         separate(ym, c("Year","Month"), sep="\u5e74|\u6708|_|-")%>%
                                         mutate(Year=as.numeric(Year)+1911, Month=as.numeric(Month)))
        }else{
          air_pat_mon$Year=as.numeric(substr(ym, 1, 4))
          air_pat_mon$Month=as.numeric(gsub("[^0-9]", "", j))
        }
      }else{
        air_pat_mon=air_pat[,1:6]
        names(air_pat_mon)=c("Airline","Airport","Flight","Seat","Patronage","LoadFactor")
        suppressWarnings({air_pat_mon$Airline[!is.na(as.numeric(air_pat_mon$Airline))]=NA})
        air_pat_mon=suppressWarnings(fill(air_pat_mon, Airline, .direction="down")%>%
                                       filter(grepl("—", Airport))%>%
                                       separate(Airport, c("Airport_O","Airport_D"), sep="—")%>%
                                       mutate(Airline=gsub(" |\u5c0f\u8a08", "", Airline)))
        air_pat_mon$Year=as.numeric(substr(ym, 1, 4))
        air_pat_mon$Month=as.numeric(gsub("[^0-9]", "", j))
      }

      air_pat_mon=select(air_pat_mon, Year, Month, Airline, Airport_O, Airport_D, Flight, Seat, Patronage, LoadFactor)
      air_pat_mon[, grepl("Flight|Seat|Patronage|LoadFactor", names(air_pat_mon))]=matrix(as.numeric(as.matrix(air_pat_mon[, grepl("Flight|Seat|Patronage|LoadFactor", names(air_pat_mon))])), ncol=4)
      air_pat_all=rbind(air_pat_all, air_pat_mon)
    }

    air_pat_all=arrange(air_pat_all, Year, Month, Airline, Airport_O, Airport_D)
  }

  if(nchar(out)!=0 & out!=F){
    write.csv(air_pat_all, out, row.names=F)
  }
  return(air_pat_all)
}


