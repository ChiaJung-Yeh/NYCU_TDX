## Overview

This package can be used to connect transportation data from TDX
(Transport Data eXchange) in a neat and efficient function. TDX platform
is supported by Ministry of Transportation and Communications (MOTC) in
Taiwan, which provides lots of API for the members download the
transportation data. Before using the function provided in this package,
the authentication key is a must, which can be applied from [PTX
platform](https://ptx.transportdata.tw/PTX/Management/AccountApply).

## Installation

Please install the package from Github (ChiaJung-Yeh/NYCU\_TDX).

    install.packages("devtools")
    devtools::install_github("ChiaJung-Yeh/NYCU_TDX")
    library(TDX)

## Usage

All functions provided in this package are summarised in the table
below.

<table class="table table-striped table-hover" style="font-size: 14px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料
</th>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料細目
</th>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料來源
</th>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料型態
</th>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
函式
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
軌道
</td>
<td style="text-align:left;">
軌道站點
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="12">
PTX
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
點
</td>
<td style="text-align:left;">
`Rail_Shape()`
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線站點
</td>
<td style="text-align:left;">
`Rail_Station()`
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線線型
</td>
<td style="text-align:left;">
線
</td>
<td style="text-align:left;">
`Rail_StationOfLine()`
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道班表
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
`Rail_TimeTable()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
公車
</td>
<td style="text-align:left;">
公車站點
</td>
<td style="text-align:left;">
點
</td>
<td style="text-align:left;">
`Bus_StopOfRoute()`
</td>
</tr>
<tr>
<td style="text-align:left;">
公車路線
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
`Bus_Route()`
</td>
</tr>
<tr>
<td style="text-align:left;">
公車路線線型
</td>
<td style="text-align:left;">
線
</td>
<td style="text-align:left;">
`Bus_Shape()`
</td>
</tr>
<tr>
<td style="text-align:left;">
公車班表
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
`Bus_Schedule()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="2">
自行車
</td>
<td style="text-align:left;">
公共自行車站點
</td>
<td style="text-align:left;">
點
</td>
<td style="text-align:left;">
`Bike_Station()`
</td>
</tr>
<tr>
<td style="text-align:left;">
自行車路網
</td>
<td style="text-align:left;">
線
</td>
<td style="text-align:left;">
`Bike_Shape()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
航空
</td>
<td style="text-align:left;">
航空班表
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
`Air_Schedule()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
觀光景點
</td>
<td style="text-align:left;">
觀光景點點位
</td>
<td style="text-align:left;">
點
</td>
<td style="text-align:left;">
`ScenicSpot()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
道路路網
</td>
<td style="text-align:left;">
道路路網線型
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
GIS-T
</td>
<td style="text-align:left;">
線
</td>
<td style="text-align:left;">
`Road_Network()`
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
地理編碼
</td>
<td style="text-align:left;">
地理編碼服務
</td>
<td style="text-align:left;">
點
</td>
<td style="text-align:left;">
`Geocoding()`
</td>
</tr>
</tbody>
</table>

Take retrieving MRT stations of Taipei Metro System for example. The
code is shown below. Note that the parameter `app_id` and `app_key` is
the authentication key applied from PTX.

    TRTC_station=Rail_Station(app_id, app_key, "TRTC")
    head(TRTC_station)

The result is shown as followings.

    ## [1] "Success: (200) OK"
    ## [1] "#---TRTC Station Downloaded---#"

    ##   StationName StationUID StationID LocationCity LocationTown LocationTownCode
    ## 1        頂埔  TRTC-BL01      BL01       新北市       土城區         65000130
    ## 2        永寧  TRTC-BL02      BL02       新北市       土城區         65000130
    ## 3        土城  TRTC-BL03      BL03       新北市       土城區         65000130
    ## 4        海山  TRTC-BL04      BL04       新北市       土城區         65000130
    ## 5    亞東醫院  TRTC-BL05      BL05       新北市       板橋區         65000010
    ## 6        府中  TRTC-BL06      BL06       新北市       板橋區         65000010
    ##   PositionLon PositionLat
    ## 1    121.4205    24.96012
    ## 2   121.43613    24.96682
    ## 3   121.44432    24.97313
    ## 4   121.44873   24.985305
    ## 5  121.452465    24.99828
    ## 6  121.459276   25.008465

Usages of other functions can be found in the *[TDX
Guide](https://chiajung-yeh.github.io/TDX_Guide/)* website.

## Support

This package takes advantage of API service provided by TDX, MOTC.

<img src="./TDX.png" width="60%" style="display: block; margin: auto;" />

## Contact

For questions, bugs, and other discussion, please feel free to contact
the package maintainer, Chia Jung, Yeh.  
Email:
<a href="mailto:1328robert@gmail.com"><u><1328robert@gmail.com></u></a>
