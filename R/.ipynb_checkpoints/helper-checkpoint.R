
## R script of useful functions for the moth/environment analysis
library(tidyverse)
library(feather)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(readxl)
library(reticulate)

setwd("/data/notebooks/jupyterlab-workshop/")
#source_python("cptMF/cpt/call.py")

# useful data
# (Michael T)
inputDataMA = read_feather("/data/ecn/ECNmet_daily.feather")
list_interesting_moth <-  read_csv("/data/ecn/interesting_moths.csv")   
moth_groups <-  read_csv("/data/ecn/moth_groups.csv")


#read_excel('/data/ecn/MOTH_TRAIT_CODES_MASTER_4_Peter.xlsx') # metadata
moth_traits = read_excel('/data/ecn/MOTH_TRAIT_CODES_MASTER_4_Peter.xlsx', sheet='MOTH_TRAIT_CODES_MASTER') %>%
    mutate(IM_CODE = substr(IM_CODE,4,nchar(IM_CODE)))


# add column indicating months
# (Aaron)
add_month <- function(df)
{
    return(
        df %>% dplyr::mutate(month=lubridate::month(DATETIME))
    )
}

# ma2x12 moving average
# (Aaron)
add_ma2x12 <- function(df)
{    
    return(
        df %>% 
            dplyr::mutate(ma2x12 = stats::filter(counts, filter=c(1/24, rep(1/12, 11), 1/24), method='convolution', sides=2))
    )
}

# add detrended (ma2x12) time series
# (Aaron)
add_detrend_ma2x12 <- function(df)
{
    return( 
        df %>% mutate(detrend_ma2x12 = counts - ma2x12) 
    )
}

# add seasonal components from classicial composition
# (Aaron)
add_decomp_seascomp <- function(df)
{
    decomp_seascomp <- df %>%
        dplyr::group_by(month) %>% 
        dplyr::summarize(decomp_seas_comp=mean(counts)) %>% 
        dplyr::mutate(std_decomp_seas=decomp_seas_comp - mean(decomp_seas_comp)) %>%
        dplyr::ungroup()
    
    return(
        df %>% 
        dplyr::left_join(decomp_seascomp)
    )
}

# add deseasonalised series using classical decomposition
# (Aaron)
add_decomp_deseason <- function(df)
{
    return( 
    df %>% 
        dplyr::mutate(decomp_deseason = counts-std_decomp_seas)
    )
}

add_decomp_deseason_std <- function(data)
{
    sds <- data %>% 
    dplyr::group_by(month, site) %>%
    dplyr::summarize( decomp_deseason_sd = sd(decomp_deseason) ) %>%
    dplyr::ungroup()

    return( 
        data <- data %>% 
            dplyr::left_join(sds) %>%
            dplyr::group_by(site) %>% 
        dplyr::mutate(decomp_deseason_std = (decomp_deseason - mean(decomp_deseason) )/decomp_deseason_sd) %>%
        dplyr::ungroup()
    )
}

# deseasonalise the series using classical decomposition 
# (trend accounted for when estimating seasonal components)
# https://otexts.com/fpp2/classical-decomposition.html
# (Aaron)
add_seasonal_decomp <- function(df)
{
    df <- add_month(df)
    df <- add_ma2x12(df)
    df <- add_detrend_ma2x12(df)
    df <- add_decomp_seascomp(df)
    df <- add_decomp_deseason(df)

    return(df)
}

# add (12 month) moving average annual trend estimation and detrend
# (Aaron)
add_ma12_detrend <- function(df)
{    
    return(
        df %>% 
            dplyr::mutate(ma12 = stats::filter(counts, filter=rep(1/12,12), method='convolution', sides=1)) %>%
            dplyr::mutate(detrend_ma12 = counts-ma12)
    )
}

# add median seasonal component and de-seasonalise
# (Aaron)
add_median_deseason <- function(df)
{
    # calculate monthly seasonal components estimated by median
    seasonal_comp <- df %>%
        dplyr::mutate(month=lubridate::month(DATETIME)) %>% 
        dplyr::group_by(month) %>% 
        dplyr::summarize(median_seas_comp=median(counts))%>% 
        dplyr::mutate(median_std_seas_comp=median_seas_comp - mean(median_seas_comp)) %>%
        dplyr::ungroup()
    
    # remove monthly seasonal components
    df_out <- df %>% 
        dplyr::mutate(month=lubridate::month(DATETIME)) %>% 
        dplyr::left_join(seasonal_comp) %>% 
        dplyr::mutate(deseason = counts-median_std_seas_comp)

    return( df_out )
}

# add seasonal difference (12 month)
# (Aaron)
add_monthly_difference <- function(df)
{    
    return( 
        df %>% 
        mutate(diff12 = c(rep(NA,12), diff(counts,12)))
    )
}

# add changepoints of ma2x12 segment estimates
# (Aaron)
add_cpts <- function(df)
{
    selected <- df %>% 
        tidyr::drop_na() %>%
        dplyr::select(DATETIME, ma2x12)

    cpt <- changepoint::cpt.meanvar(selected$ma2x12,
                                    method='PELT', 
                                    test.stat='Normal', 
                                    param.estimates=TRUE,
                                    class=TRUE)

    selected$cpts <- do.call(
        c, 
        mapply(rep,
               as.list(slot(cpt, "param.est")$mean),
               as.list(c(slot(cpt, "cpts")[1], diff(slot(cpt, "cpts"))))
              )
    )
    
    return( df %>% dplyr::left_join(selected) )
}

# check for missing dates between range
# (Aaron)
check_alldates <- function(df)
{
    print("check_alldates: assuming dates are recorded same day each month")
    date_min <- min(df$DATETIME)
    date_max <- max(df$DATETIME)
    
    dates_all <- seq.Date(date_min, date_max, by='month')
    
    logical_missing_dates <- all(dates_all %in% df$DATETIME) 
    
    print( c("missing dates:", logical_missing_dates) )
    if(logical_missing_dates){ return(T) }
    return(F)
}

# fill missing date count with 0
# (Aaron)
infill_missingdates_0 <- function(df)
{
    date_min <- min(df$DATETIME)
    date_max <- max(df$DATETIME)
    
    dates_all <- seq.Date(date_min, date_max, by='month')
    
    df_new <- data.frame(DATETIME = dates_all) %>%
    dplyr::left_join(df) %>%
    tidyr::replace_na(list(counts=0)) %>%
    dplyr::arrange(DATETIME)
    
    return( df_new )
}


# function to extract moth data by group and infill missing data
## (Michael T)
inputData <- function(input)
{
  if(input$dataset == "butterfly")
  {
    filename = "/data/ecn/ECN_IB1.csv"  # butterflies
  } else if(input$dataset == "moth")
  {
    filename = "/data/ecn/ECN_IM1.csv"  # moths
  } else 
  {
    print('error! type butterfly or moth')
  }
  
  # read the file
  dd = read.csv(filename) %>%  
    dplyr::mutate(station = paste0(SITECODE,'-',LCODE)) %>%
    dplyr::mutate(YEAR=as.integer(str_sub(SDATE, start= -2))) %>%
    dplyr::mutate(YEAR=if_else( YEAR <= 50,YEAR+2000,YEAR+1900)) %>%
    dplyr::mutate(DATE= as.Date(SDATE, format= "%d-%b-%y")) %>%
    dplyr::mutate(FLAGID = ifelse(grepl( paste0("Q",1:5,collapse = "|"),FIELDNAME),VALUE,NA)) %>% 
    dplyr::select(-YEAR,-SDATE) 
  if(input$dataset == "moth") dd = dplyr::left_join(dd,moth_groups,by=c("FIELDNAME"="IM_SPEC"))  
  if(input$dataset == "moth") dd = dplyr::left_join(dd,moth_traits,by=c("FIELDNAME"="IM_CODE"))  
  
  # find out days with NA
  na_days = dd %>% 
    dplyr::filter(SITECODE == input$site) %>%
    dplyr::filter(as.Date(DATE) >= as.Date(input$daterange[1]) &
                    as.Date(DATE) <= as.Date(input$daterange[2]) ) %>%
    dplyr::filter(!str_detect(FIELDNAME, "Q"))    %>%
    dplyr::right_join(data.frame(DATE=seq.Date(min(.$DATE), max(.$DATE), by='day'))) %>%
    dplyr::arrange(DATE) %>%
    dplyr::filter(is.na(VALUE)) %>%
    dplyr::select(DATE,VALUE)  %>% 
    group_by(seq_id = cumsum(c(1, as.numeric(diff(DATE))) != 1) + 1) %>% # find consecutive periods
    mutate(STARTDATE=min(DATE), ENDDATE=max(DATE), duration=n(), next_meas = as.numeric(ENDDATE-DATE+1))
  
  na_days_summary = na_days %>%
    distinct(seq_id,STARTDATE,ENDDATE,duration)
  
  message(paste0('The number of unsampled days during the selected date range at this site is: ', nrow(na_days), '.'))
  message(paste0('The top 10 unsampled periods during the selected date range at this site is: '))
  message(paste0(capture.output(na_days_summary %>% arrange(desc(duration)) %>% head(10)), collapse   = "\n")) 
  
  # filtering
  dd = dd %>% 
    dplyr::filter(SITECODE == input$site) %>%
    dplyr::filter(as.Date(DATE) >= as.Date(input$daterange[1]) &
                    as.Date(DATE) <= as.Date(input$daterange[2]) ) %>%
    dplyr::filter(!str_detect(FIELDNAME, "Q"))    %>%
    #dplyr::right_join(data.frame(DATE=seq.Date(min(dd$DATE), max(dd$DATE), by='day'))) %>% # fill in no count days (8/12/2020)
    dplyr::group_by(DATE) %>% 
    #dplyr::filter(!str_detect(FIELDNAME, "XX"))    %>%       # XX marks days with no moths observed, add FILEDNAME =='XX' to show zero counts (8/12/2020)
    {if(input$plotPredOpt %in% list_interesting_moth$DESC_COMMON) dplyr::filter(.,DESC_COMMON == input$plotPredOpt ) else .} %>%
    {if(input$plotPredOpt %in% c("G","M","N","O")) dplyr::filter(.,IM_GROUP == input$plotPredOpt ) else .} %>%
    {if(!is.na(input$plotPredOpt) & 
        !(input$plotPredOpt %in% c("G","M","N","O")) & 
        !(input$plotPredOpt %in% list_interesting_moth$DESC_COMMON) ) dplyr::filter(.,eval(rlang::parse_expr(input$plotPredOpt))) else .} 
  
  message(paste0(capture.output(dd), collapse = "\n"))
  
  # dd = dd %>%
  #   dplyr::group_by(DATE) %>%
  #   dplyr::select(DATE,FIELDNAME,VALUE) %>%
  #   dplyr::right_join(na_days %>% select(DATE,VALUE) ) %>%  # fill in na days
  #   dplyr::right_join(data.frame(DATE=seq.Date(min(dd$DATE), max(dd$DATE), by='day'),VALUE=0))  # fill in zero count days / needed？？
  message(paste0(capture.output(dd %>% filter(is.na(VALUE))  ), collapse = "\n"))
  message(paste0(capture.output(dd), collapse = "\n"))
  
  # fill missing data and summation (daily)
  dd = dd %>%
    #dplyr::group_by(DATE) %>% # corrected, moved above (8/12/2020)
    dplyr::summarise(counts = sum(VALUE,na.rm = T) , 
                     n_species = ifelse(counts==0, 0, n_distinct(FIELDNAME)))  %>%
    #dplyr::select( -FIELDNAME_XX,-previous) %>%
    dplyr::rename(DATETIME=DATE)
  
  message(paste0(capture.output(na_days %>% head(10)), collapse = "\n"))
  
  # instead of filling all daterange with zeros, fill only values in observation period
  datemin <- max( c(min(as.Date(dd$DATETIME),na.rm = T), as.Date(input$daterange[1])) )
  datemax <- min( c(max(as.Date(dd$DATETIME),na.rm = T), as.Date(input$daterange[2])) )
  
  # first insert NA days, then pad zeros for the remainder
  dd = dd %>%
    dplyr::full_join(na_days %>% select(DATE,next_meas,duration,ENDDATE) %>% mutate(NA_FLAG = 1) , by=c('DATETIME'='DATE')) %>%   # pad NA
    dplyr::right_join(data.frame(DATETIME=seq.Date(datemin, datemax, by='day')))  %>%   # pad zeros
    arrange(DATETIME) %>%
    ###mutate(counts = ifelse(is.na(NA_FLAG) & is.na(counts),0,counts),           # write NA
    ###       n_species = ifelse(is.na(NA_FLAG) & is.na(n_species),0,n_species)  ) %>%
    select(-NA_FLAG)

  # handling NA for daily counts (TO ADD)
  if(input$avg_missing_data == TRUE) {
    
    dd = dd %>%
      dplyr::mutate(duration = ifelse(is.na(duration),0,duration)) %>% 
      dplyr::ungroup() %>%  # needed for fill
      tidyr::fill(counts,.direction = "up") %>% #fill missing data
      dplyr::ungroup() %>% 
      dplyr::mutate(counts = ifelse(duration <= 3, counts, NA)) %>%  # put NA back for long duration
      dplyr::mutate(counts = ifelse(duration <= 3, counts/(duration+1), counts))  # only do averaging for 3 days or less
    
    message("WARNING！`avg_missing_data = TRUE`. Averging missing moth counts may result non-integer.")
    
    # for all ENDDATES within the duration criteria, use counts(ENDDATE+1) = counts(ENDDATE)
    NA_ENDDATES = distinct(dd,ENDDATE) %>% mutate(ENDDATE=ENDDATE+1) %>% filter(!is.na(ENDDATE)) %>% pull()
    
    dd = dd %>% dplyr::mutate(counts=ifelse( DATETIME %in% NA_ENDDATES,lag(counts,1),counts))
  }

  # get monthly sums of counts
  monthly = dd %>% dplyr::ungroup() %>%
    dplyr::select(DATETIME,counts) %>%
    dplyr::group_by(year(DATETIME),month(DATETIME)) %>%
    dplyr::summarise(total = sum(counts,na.rm=T), NA_per_mo = sum(is.na(counts) )) %>%
    dplyr::rename(counts = total) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DATETIME = make_date(`year(DATETIME)`,`month(DATETIME)`,1)) %>%
    dplyr::select(DATETIME,counts,NA_per_mo)

  return(dd) # monthly or[ dd or na_days] use the latter two for checks
}



multi_site_analysis <- function(sites, dataset, daterange, avg_missing_data, plotPredOpt)
{

    data <- list()

    for( site in sites )
    {
        print(c("site:", site))
    
        data[[site]] <- inputData(
            list(dataset = dataset,
                 site = site, 
                 daterange = daterange,
                 avg_missing_data = avg_missing_data,
                 plotPredOpt = plotPredOpt  # either groups (G,M,N,O) or species (`list_interesting_moth %>% distinct(DESC_COMMON)`)
                )) 
    
        # check and correct if dates are missing
        check_alldates(data[[site]])
        data[[site]] <- infill_missingdates_0(data[[site]])
    
        # add columns
            # yearmonth
            # month
            # seasonal decomposition
            # site
        data[[site]] <- data[[site]] %>% 
            dplyr::mutate(yearmonth = tsibble::yearmonth(data[[site]]$DATETIME)) %>%
            add_month() %>% 
            add_seasonal_decomp() %>%
            dplyr::mutate(site=site) %>%
            #add_cpts() %>%
            add_decomp_deseason_std()
    }


    return( data %>% purrr::reduce(dplyr::full_join) )
}
     
multi_site_changepoints <- function(data, pen, cost, y)
{
    y <- dplyr::enquo(y)
    
    sites <- unique(data$site)
    
    cpt_data <- list()
        
    for( i_site in 1:length(sites) )
    {
        cpt_data[[i_site]] <- data %>% 
            dplyr::filter(site == sites[i_site]) %>%
            tidyr::drop_na() %>%
            dplyr::select(DATETIME, !!y) 
    }

    cpts <- as.Date(CPT_MF(cpt_data, cost, pen), format="%d-%m-%Y")
    
    tbl_cpt <- tibble::tibble(site=sites[1], cpt.type='multifreq', loc=cpts)
    if(length(sites) == 1){ return(tbl_cpt) }
    
    for( i_site in 2:length(sites) )
    {
        tbl_cpt <- tbl_cpt %>% 
            dplyr::full_join(
                tibble::tibble(site = sites[i_site], 
                               cpt.type = 'multifreq', 
                               loc = cpts)
            )
    }
    
    return(tbl_cpt)
    
}
     
multi_site_uni_changepoints <- function(data, pen, cost, y)
{
    y <- dplyr::enquo(y)
    sites <- unique(data$site)
    
    cpt_data <- list(
        data %>% 
            dplyr::filter(site == sites[1]) %>%
            tidyr::drop_na() %>%
            dplyr::select(DATETIME, !!y)
    )
        
    cpts <- as.Date(CPT_MF(cpt_data, cost[1], pen[1]), format="%d-%m-%Y")
        
    tbl_cpt <- tibble::tibble(site=sites[1], cpt.type='uni', loc=cpts)
    if(length(sites) == 1){ return(tbl_cpt) }
    
    for( i_site in 2:length(sites) )
    {
        cpt_data <- list(
            data %>% 
            dplyr::filter(site == sites[i_site]) %>%
            tidyr::drop_na() %>%
            dplyr::select(DATETIME, !!y)
        )
        
        cpts <- as.Date(CPT_MF(cpt_data, cost[i_site], pen[i_site]), format="%d-%m-%Y")
        
        tbl_cpt <- tbl_cpt %>% 
            dplyr::full_join(
                tibble::tibble(site = sites[i_site], 
                               cpt.type = 'uni',
                               loc = cpts)
            )
        
    }
    
    return( tbl_cpt )    
}
