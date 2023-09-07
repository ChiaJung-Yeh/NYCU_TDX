## Overview

<img src="./figure/TDX_icon.png" width="30%" style="display: block; margin: auto;" />

This package can be used to connect transportation data from TDX
(Transport Data eXchange) in a neat and efficient way. TDX platform is
supported by Ministry of Transportation and Communications (MOTC) in
Taiwan, which provides lots of API for the members download the
transportation data. Before using the function provided in this package,
the authentication key is a must, which can be applied from [TDX
platform](https://tdx.transportdata.tw/register). After being a member
of TDX, you will soon get the Client Id and Client Secret, please check
out in the [API key
Dashboard](https://tdx.transportdata.tw/user/dataservice/key).

## Installation

Please install the package from Github (ChiaJung-Yeh/NYCU\_TDX).  
If you are Python user, please refer to
[here](https://pypi.org/project/nycu-tdx-py/) for Python package via
PyPi.

    install.packages("devtools")
    devtools::install_github("ChiaJung-Yeh/NYCU_TDX")
    library(TDX)

## Usage

All functions provided in this package are summarized in the table
below.

<table class="table table-striped table-hover" style="font-size: 14px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
本套件函式綜覽
</caption>
<thead>
<tr>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料
</th>
<th style="text-align:left;font-weight: bold;color: white !important;background-color: #8E8E8E !important;">
資料細目
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
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="7">
軌道
</td>
<td style="text-align:left;">
軌道站點
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="3">
點
</td>
<td style="text-align:left;">
`Rail_Station()`
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道路線站點
</td>
<td style="text-align:left;">
Rail\_StationOfLine()
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道站點出口
</td>
<td style="text-align:left;">
Rail\_StationExit()
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
Rail\_Shape()
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道班表
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="3">
文字
</td>
<td style="text-align:left;">
Rail\_TimeTable()
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道票價
</td>
<td style="text-align:left;">
Rail\_ODFare()
</td>
</tr>
<tr>
<td style="text-align:left;">
軌道站間旅行時間
</td>
<td style="text-align:left;">
Rail\_TravelTime()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="8">
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
<td style="text-align:left;vertical-align: middle !important;" rowspan="4">
文字
</td>
<td style="text-align:left;">
Bus\_Schedule()
</td>
</tr>
<tr>
<td style="text-align:left;">
公車站間旅行時間
</td>
<td style="text-align:left;">
Bus\_TravelTime()
</td>
</tr>
<tr>
<td style="text-align:left;">
公車票價
</td>
<td style="text-align:left;">
Bus\_RouteFare()
</td>
</tr>
<tr>
<td style="text-align:left;">
公車車輛
</td>
<td style="text-align:left;">
Bus\_Vehicle()
</td>
</tr>
<tr>
<td style="text-align:left;">
公車動態
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
點
</td>
<td style="text-align:left;">
Bus\_RealTime()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
自行車
</td>
<td style="text-align:left;">
公共自行車站點
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
<td style="text-align:left;">
公共自行車站點歷史動態
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="3">
文字
</td>
<td style="text-align:left;">
Bike\_Remain\_His()
</td>
</tr>
<tr>
<td style="text-align:left;">
臺北市公共自行車租借紀錄
</td>
<td style="text-align:left;">
Bike\_OD\_His()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="4">
航空與航運
</td>
<td style="text-align:left;">
航空班表
</td>
<td style="text-align:left;">
Air\_Schedule()
</td>
</tr>
<tr>
<td style="text-align:left;">
港口點位
</td>
<td style="text-align:left;">
點
</td>
<td style="text-align:left;">
Ship\_Port()
</td>
</tr>
<tr>
<td style="text-align:left;">
航運航線
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
文字
</td>
<td style="text-align:left;">
Ship\_Route()
</td>
</tr>
<tr>
<td style="text-align:left;">
航線靠港順序
</td>
<td style="text-align:left;">
Ship\_StopOfRoute()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;vertical-align: middle !important;" rowspan="2">
高速公路
</td>
<td style="text-align:left;">
高快速公路線型
</td>
<td style="text-align:left;">
線
</td>
<td style="text-align:left;">
Freeway\_Shape()
</td>
</tr>
<tr>
<td style="text-align:left;">
高速公路 etag 與 VD
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
Freeway\_History()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
停車場
</td>
<td style="text-align:left;">
停車場點位資訊
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
點
</td>
<td style="text-align:left;">
Car\_Park()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
觀光點位
</td>
<td style="text-align:left;">
景點、餐廳、旅館點位
</td>
<td style="text-align:left;">
Tourism()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
道路路網
</td>
<td style="text-align:left;">
道路路網線型
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
Geocoding()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
人口
</td>
<td style="text-align:left;">
人口（含各年齡層與性別資料）
</td>
<td style="text-align:left;vertical-align: middle !important;" rowspan="2">
文字
</td>
<td style="text-align:left;">
Population()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
所得
</td>
<td style="text-align:left;">
所得（各村里）
</td>
<td style="text-align:left;">
Income()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
行政區
</td>
<td style="text-align:left;">
行政區疆域
</td>
<td style="text-align:left;">
面
</td>
<td style="text-align:left;">
District\_Shape()
</td>
</tr>
<tr>
<td style="text-align:left;font-weight: bold;">
GTFS
</td>
<td style="text-align:left;">
公共運輸標準格式
</td>
<td style="text-align:left;">
文字
</td>
<td style="text-align:left;">
gtfs()
</td>
</tr>
</tbody>
</table>

Data retrieving process requires an access token to obtain the data from
TDX platform. Most function in this package should use function
`get_token()` in advance to obtain the token by entering your Client ID
and Client Secret first. Note that the access token will expire in 1
day.

Take retrieving MRT stations of Taipei Metro System for example. The
code is shown below. Here the argument `CLIENT_ID` and `CLIEN_SECRET` is
the authentication key applied from TDX.

    # get the access token first
    access_token=get_token("CLIENT_ID", "CLIEN_SECRET")

    # retrieve Taipei MRT station
    TRTC_station=Rail_Station(access_token, "TRTC")

    # historical data
    # TRTC_station=Rail_Station(access_token, "TRTC", dates="2023-01-01")

    head(TRTC_station)

The result is shown as followings.

    ## #---TRTC Station Downloaded---#

    ##   StationUID StationID StationName                       StationAddress
    ## 1  TRTC-BL01      BL01        頂埔 236040新北市土城區中央路4段51之6號B3
    ## 2  TRTC-BL02      BL02        永寧   236036新北市土城區中央路3段105號B1
    ## 3  TRTC-BL03      BL03        土城   236017新北市土城區金城路1段105號B1
    ## 4  TRTC-BL04      BL04        海山       236023新北市土城區海山路39號B2
    ## 5  TRTC-BL05      BL05    亞東醫院  220056新北市板橋區南雅南路2段17號B1
    ## 6  TRTC-BL06      BL06        府中 220052新北市板橋區縣民大道1段193號B1
    ##   BikeAllowOnHoliday LocationCity LocationCityCode LocationTown
    ## 1               TRUE       新北市              NWT       土城區
    ## 2               TRUE       新北市              NWT       土城區
    ## 3               TRUE       新北市              NWT       土城區
    ## 4               TRUE       新北市              NWT       土城區
    ## 5               TRUE       新北市              NWT       板橋區
    ## 6               TRUE       新北市              NWT       板橋區
    ##   LocationTownCode PositionLon PositionLat
    ## 1         65000130    121.4187    24.95935
    ## 2         65000130    121.4361    24.96682
    ## 3         65000130    121.4443    24.97313
    ## 4         65000130    121.4488    24.98545
    ## 5         65000010    121.4525    24.99828
    ## 6         65000010    121.4593    25.00847

Usages of other functions can be found in the **[TDX
Guide](https://chiajung-yeh.github.io/TDX_Guide/)** website.

## Support

This package takes advantage of API service provided by TDX, MOTC.

<img src="./figure/TDX.png" width="60%" style="display: block; margin: auto;" />

## Contact

For questions, bugs, and other discussion, please feel free to contact
the package maintainer, Chia Jung, Yeh.  
Email:
<a href="mailto:chia-jung.yeh@sydney.edu.au"><u><chia-jung.yeh@sydney.edu.au></u></a>
