## Overview

This package can be used to connect transportation data from TDX
(Transport Data eXchange) in a neat and efficient function. TDX platform
is supported by Ministry of Transportation and Communications (MOTC) in
Taiwan, which provides lots of API for the members download the
transportation data. Before using the function provided in this package,
the authentication key is a must, which can be applied from PTX
[platform](https://ptx.transportdata.tw/PTX/Management/AccountApply).

## Installation

Please install the package from Github (ChiaJung-Yeh/NYCU\_TDX).

    install.packages("devtools")
    devtools::install_github("ChiaJung-Yeh/NYCU_TDX")
    library(TDX)

## Usage

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
[`Rail_Shape()`](#軌道路線線型資料)
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線站點
</td>
<td style="text-align:left;">
[`Rail_Station()`](#軌道站點資料)
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
[`Rail_StationOfLine()`](#軌道路線站點資料)
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
[`Rail_TimeTable()`](#軌道班表資料)
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
[`Bus_StopOfRoute()`](#公車路線站點資料)
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
[`Bus_Route()`](#公車路線資料)
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
[`Bus_Shape()`](#公車路線線型資料)
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
[`Bus_Schedule()`](#公車班表資料)
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
[`Bike_Station()`](#公共自行車站點資料)
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
[`Bike_Shape()`](#自行車線型資料)
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
[`Air_Schedule()`](#航空班表資料)
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
[`ScenicSpot()`](#觀光景點點位)
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
[`Road_Network()`](#道路路網線型)
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
[`Geocoding()`](#地理編碼服務)
</td>
</tr>
</tbody>
</table>
