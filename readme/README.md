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
Rail\_Shape()
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線站點
</td>
<td style="text-align:left;">
Rail\_Station()
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
Rail\_StationOfLine()
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
Rail\_TimeTable()
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
Bus\_StopOfRoute()
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
Bus\_Route()
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
Bus\_Shape()
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
Bus\_Schedule()
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
Bike\_Station()
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
Bike\_Shape()
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
Air\_Schedule()
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
ScenicSpot()
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
Road\_Network()
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
code and its result are shown below. Note that the parameter `app_id`
and `app_key` is the

    TRTC_station=Rail_Station(app_id, app_key, "TRTC", dtype="sf")

## Contact

For questions, bugs, and other discussion, please feel free to contact
the package maintainer, Chia Jung, Yeh.  
Email:
