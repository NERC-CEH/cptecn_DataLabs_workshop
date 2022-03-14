library(feather)
library(tibble)
library(dplyr)
library(tidyverse)

daily_weather <- function(tbl_moth_abundance, weather_types=NULL)
{
    
    sites <- unique(tbl_moth_abundance$SITECODE)
    
    date_range <- tbl_moth_abundance %>%
    group_by(SITECODE, station) %>%
    summarize(date_min = min(date), date_max = max(date)) %>%
    ungroup()
    
    inputDataMA = read_feather("/data/ecn/ECNmet_daily.feather")
    if( is.null(class(weather_types)) )
    {
        weather_types <- unique(inputDataMA$FIELDNAME)
    }
    
    tbl_weather <- inputDataMA %>%
    dplyr::filter((SITECODE %in% sites) & FIELDNAME %in% weather_types) %>%
    right_join(date_range, by=c("SITECODE")) %>%
    filter((DATE >= date_min) & (DATE <= date_max)) %>%
    mutate(date = DATE, value=daily, value_type = FIELDNAME) %>%
    select(date, SITECODE, station, value, value_type)
    
    return(tbl_weather)
}

get_daily_moth_rate <- function(in_stations, date_min, date_max)
{    
    mothdata <- as_tibble(read.csv("/data/ecn/ECN_IM1.csv"))
  
    # process
    return(
        mothdata %>%
        # remove rows with "Q" in FIELDNAME
        dplyr::filter(!str_detect(FIELDNAME, "Q")) %>%
        # change year format yy to yyyy
        dplyr::mutate(YEAR=as.integer(str_sub(SDATE, start= -2))) %>%
        dplyr::mutate(YEAR=if_else( YEAR <= 50,YEAR+2000,YEAR+1900)) %>%
        dplyr::mutate(DATE= as.Date(SDATE, format= "%d-%b-%y")) %>%
        # drop YEAR and SDATE columns
        dplyr::select(-YEAR,-SDATE) %>%
        # get dates in specified date range
        dplyr::filter(
            DATE >= as.Date(date_min) 
            & 
            DATE <= as.Date(date_max) 
        ) %>%
        # create station column
        dplyr::mutate(station = paste0(SITECODE, "-", LCODE)) %>%
        # select only relevant stations
        dplyr::filter(station %in% in_stations) %>%
        # get daily station count
        dplyr::group_by(SITECODE, DATE, station) %>%
        dplyr::summarize(count = sum(VALUE)) %>%
        dplyr::ungroup() %>%
        # calculate daily moth rate
        dplyr::arrange(station, DATE) %>%
        mutate(
            lag_station = lag(station,1),
            lag_DATE = lag(DATE,1), 
            interval = as.double(DATE-lag_DATE), 
            useful_row = station == lag_station,
            month = lubridate::month(DATE),
            value_type = "daily_abun_rate"
        ) %>%
        # take only useful rows
        filter(useful_row == TRUE) %>%
        mutate(value = count/interval) %>%
        select(SITECODE, DATE, station, value, value_type)
    )
}

get_monthly_moth_rate <- function(in_stations, date_min, date_max)
{
    return(
        get_daily_moth_rate(in_stations, date_min, date_max) %>%
        mutate(
            year=lubridate::year(DATE),
            monthname = lubridate::month(DATE, label=TRUE, abbr=FALSE),
            days_in_month = lubridate::days_in_month(DATE), 
        ) %>%
        group_by(SITECODE, station,  year, monthname, days_in_month) %>%
        summarize(sum_rate = sum(value)) %>%
        ungroup() %>%
        mutate(
            value = sum_rate/days_in_month,
            date = as.Date(paste(year,monthname,days_in_month,sep='-'), format="%Y-%B-%d"),
            value_type = "monthly_abun_rate"
        ) %>%
        select(SITECODE, station, value, date, value_type) %>%
        arrange(station,date) 
    )
}


interpolate_monthly <- function(tbl_moth)
{
    # calculate linear interpolation parameters
    interp_model <- 
        tbl_moth %>%
        dplyr::group_by(station) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            diffy = (value - lag(value)),
            diffx = lubridate::time_length(date - lag(date), "day"),
            numeric_x = as.numeric(date),
            m = diffy/diffx,
            c = value - m*numeric_x
        ) %>%
        ungroup() %>%
        dplyr::select(SITECODE, station, date, m, c)
    
    # interpolate values
    tbl_interpolated <-
    tbl_moth %>%
    dplyr::group_by(station, SITECODE, value_type) %>%
    tidyr::complete(
        date = seq(
            min(date)+lubridate::days(1), 
            max(date)+lubridate::days(1), 
            by = "1 month"
        ) - lubridate::days(1)
    ) %>%
    ungroup() %>%
    left_join(
        interp_model,
    by=c("station","SITECODE","date")
    ) %>%
    dplyr::arrange(station,date) %>%
    tidyr::fill(m,c,.direction='up') %>%
    mutate(
        missing = is.na(value),
        value = ifelse(missing, m*as.numeric(date)+c, value)
    )
    
    return(tbl_interpolated)
}

deseason <- function(tbl)
{
    season_means <- tbl %>%
    dplyr::select(-c(m,c)) %>%
    dplyr::group_by(station) %>%
    dplyr::mutate(
        ma = stats::filter(
        value, 
        filter=c(1/24, rep(1/12, 11), 1/24), 
            method='convolution', 
            sides=2),
        detrend_ma = value-ma,
        month = lubridate::month(date)
    ) %>%
    dplyr::ungroup()%>%
    dplyr::group_by(
        station,
        month
    ) %>%
    dplyr::summarize(season_mean = mean(detrend_ma, na.rm=TRUE)) %>%
    dplyr::ungroup()

    return(
        tbl %>% 
        dplyr::select(-c(m,c)) %>%
        mutate(month = lubridate::month(date)) %>%
        left_join(
            season_means,
            by=c("station", "month")
        ) %>%
        dplyr::mutate(deseason = value - season_mean)
    )
}