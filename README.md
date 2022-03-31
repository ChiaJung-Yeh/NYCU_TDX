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
[Rail\_Shape()](https://chiajung-yeh.github.io/TDX_Guide/%E8%BB%8C%E9%81%93%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E8%BB%8C%E9%81%93%E8%B7%AF%E7%B7%9A%E7%B7%9A%E5%9E%8B%E8%B3%87%E6%96%99)
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線站點
</td>
<td style="text-align:left;">
[Rail\_Station()](https://chiajung-yeh.github.io/TDX_Guide/%E8%BB%8C%E9%81%93%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E8%BB%8C%E9%81%93%E7%AB%99%E9%BB%9E%E8%B3%87%E6%96%99)
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
[`Rail_StationOfLine()`](https://chiajung-yeh.github.io/TDX_Guide/%E8%BB%8C%E9%81%93%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E8%BB%8C%E9%81%93%E8%B7%AF%E7%B7%9A%E7%AB%99%E9%BB%9E%E8%B3%87%E6%96%99)
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
[Rail\_TimeTable()](https://chiajung-yeh.github.io/TDX_Guide/%E8%BB%8C%E9%81%93%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E8%BB%8C%E9%81%93%E7%8F%AD%E8%A1%A8%E8%B3%87%E6%96%99)
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
[Bus\_StopOfRoute()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%AC%E8%BB%8A%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E5%85%AC%E8%BB%8A%E8%B7%AF%E7%B7%9A%E7%AB%99%E9%BB%9E%E8%B3%87%E6%96%99)
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
[Bus\_Route()](#公車路線資料)
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
[Bus\_Shape()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%AC%E8%BB%8A%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E5%85%AC%E8%BB%8A%E8%B7%AF%E7%B7%9A%E7%B7%9A%E5%9E%8B%E8%B3%87%E6%96%99)
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
[\`Bus\_Schedule()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%AC%E8%BB%8A%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E5%85%AC%E8%BB%8A%E7%8F%AD%E8%A1%A8%E8%B3%87%E6%96%99)
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
[Bike\_Station()\`](https://chiajung-yeh.github.io/TDX_Guide/%E8%87%AA%E8%A1%8C%E8%BB%8A%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E5%85%AC%E5%85%B1%E8%87%AA%E8%A1%8C%E8%BB%8A%E7%AB%99%E9%BB%9E%E8%B3%87%E6%96%99)
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
[Bike\_Shape()](https://chiajung-yeh.github.io/TDX_Guide/%E8%87%AA%E8%A1%8C%E8%BB%8A%E9%81%8B%E8%BC%B8%E8%B3%87%E6%96%99.html#%E8%87%AA%E8%A1%8C%E8%BB%8A%E7%B7%9A%E5%9E%8B%E8%B3%87%E6%96%99)
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
[Air\_Schedule()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%B6%E4%BB%96-tdx-%E8%B3%87%E6%96%99.html#%E8%88%AA%E7%A9%BA%E7%8F%AD%E8%A1%A8%E8%B3%87%E6%96%99)
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
[ScenicSpot()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%B6%E4%BB%96-tdx-%E8%B3%87%E6%96%99.html#%E8%A7%80%E5%85%89%E6%99%AF%E9%BB%9E%E9%BB%9E%E4%BD%8D)
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
[Road\_Network()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%B6%E4%BB%96-tdx-%E8%B3%87%E6%96%99.html#%E9%81%93%E8%B7%AF%E8%B7%AF%E7%B6%B2%E7%B7%9A%E5%9E%8B)
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
[Geocoding()](https://chiajung-yeh.github.io/TDX_Guide/%E5%85%B6%E4%BB%96-tdx-%E8%B3%87%E6%96%99.html#%E5%9C%B0%E7%90%86%E7%B7%A8%E7%A2%BC%E6%9C%8D%E5%8B%99)
</td>
</tr>
</tbody>
</table>
