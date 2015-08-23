get_budget_est <- function(fcs_fy){
    
    main <- get_formul_main(fcs_fy = fcs_fy)
    assum <- get_formul_assum(fcs_fy = fcs_fy)    
    
#     prog_mismatch <- union(
#         setdiff(main$prog, assum$prog)
#         ,
#         setdiff(assum$prog, main$prog) 
#     ) %>% paste(., collapse ='\n')
#     cat('Programs Mismatches:\n',prog_mismatch)
    
    no_assum <- anti_join(
        main,
        assum %>% select(-h2,-h3),
        by=c('h1'='h1','prog'='prog','fy'='fy','type'='type','sr'='sr')
    )
    
    no_main <- anti_join(
        assum %>% select(-h2,-h3),
        main,
        by=c('h1'='h1','prog'='prog','fy'='fy','type'='type','sr'='sr')
    )
    
    budget_est <- inner_join(
        main,
        assum %>% select(-h2,-h3),
        by=c('h1'='h1','prog'='prog','fy'='fy','type'='type','sr'='sr')
    ) %>% 
        # add unique Program IDs
        group_by(fy, type) %>% 
        mutate(id = paste(type, fy, row_number() %>% 
                              stringr::str_pad(., 3, "left", "0"),
                          sep = '_')) %>% 
        ungroup()
    
    budget_est <- get_moodys_ratings(budget_est)
    
    L <- list(
        budget_est = budget_est,
        no_main = no_main,
        no_assum = no_assum
    )
    
    list2env(L, envir = parent.frame())
    
}


get_formul_main <- function(fcs_fy){

    # FY split function
    get_tbl_main2 <- function(fcs_fy, fcs_tbl){
        
        stopifnot(fcs_tbl %in% 1:2)
        
        type <- ifelse(fcs_tbl==1,'DL','LG')
        
        df <- get_clean_df(fcs_fy = fcs_fy, fcs_tbl = fcs_tbl)
        
        df <- merge(data_frame(fy=NA, type=type),df)
        
        df <- bind_rows(
            # current yr
            df %>% select(-py_rate,-py_amt,-py_ln_size) %>% 
                rename(sr=cy_rate,amt=cy_amt,ln_size=cy_ln_size) %>% 
                mutate(fy = fcs_fy),
            # prev yr
            df %>% select(-cy_rate,-cy_amt,-cy_ln_size) %>% 
                rename(sr=py_rate,amt=py_amt,ln_size=py_ln_size) %>% 
                mutate(fy = fcs_fy-1)
        ) %>% 
            filter(!is.na(sr))
        
        return(df)
    }
    
        
    df <- bind_rows(
        # Direct Loans
        get_tbl_main2(fcs_fy = fcs_fy,fcs_tbl = 1),
        # Loan Gtys
        get_tbl_main2(fcs_fy = fcs_fy,fcs_tbl = 2)
    )
}


get_formul_assum <- function(fcs_fy){
    
    df <- bind_rows(
        merge(
            data_frame(fy=fcs_fy-1,type='DL'),
            get_clean_df(fcs_fy = fcs_fy, fcs_tbl = 3)
        ),
        merge(
            data_frame(fy=fcs_fy-1,type='LG'),
            get_clean_df(fcs_fy = fcs_fy, fcs_tbl = 4)
        ),
        merge(
            data_frame(fy=fcs_fy,type='DL'),
            get_clean_df(fcs_fy = fcs_fy, fcs_tbl = 5)
        ),
        merge(
            data_frame(fy=fcs_fy,type='LG'),
            get_clean_df(fcs_fy = fcs_fy, fcs_tbl = 6)
        )
    )
    
    # default, net of recoveries rate
    df <- mutate(
        df,
        net_def = def * (1-ifelse(is.na(recov),0,recov)/100),
        net_def = round(net_def, 2)
    )

    return(df)
}



get_moodys_ratings <- function(budget_est){
    
    detach_DT <- loadedNamespaces() %>% grepl('data\\.table',.) %>% sum==0
    
    library(data.table)
    
    moody_cache <- 'data/cache/moodys.RData'
    if(!file.exists(moody_cache))
        source('code/prep_data_moodys.R')
    load(moody_cache)
    
    
    moody <- data.table(moody)
    setkey(moody, moody_yr, net_def)
    
    dt <- data.table(budget_est)
    dt[, moody_yr := mat]
    dt[moody_yr>20 |is.na(moody_yr), moody_yr:=20]
    dt[is.na(net_def),net_def:=0]
    
    setkey(dt, moody_yr, net_def)
    
    dt <- moody[dt, roll=-Inf]
    
    setkey(dt, fy, id)
    
    setcolorder(
        dt, 
        c(colnames(budget_est), 
          c('moody','moody_num','moody_yr')))
    
    
    budget_est <- as_data_frame(dt)
    
    if(detach_DT) detach("package:data.table")
    
    return(budget_est)
}



