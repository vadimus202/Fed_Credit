



# Get a clean Fed Credit Supplement Table
##################################################################################################
get_clean_df <- function(fcs_fy, fcs_tbl, cache_dir='data'){
    stopifnot(fcs_tbl %in% 1:6)
    
    raw <- get_raw_file(fcs_fy, fcs_tbl, cache_dir)
    
    # table metadata
    if(fcs_tbl %in% 1:2){
        # subsidy rates tables
        col_names <- c('txt','bea','py_rate','py_amt','py_ln_size','cy_rate','cy_amt','cy_ln_size')
        col_heads <- 1:2
        col_data_check <- 2
    } else if(fcs_tbl %in% 3:6){
        # assumption tables
        col_names <- c('txt','sr','sr_def','sr_int','sr_fee','sr_oth','mat','bor_int','grc','fee','ann_fee','oth_fee','def','recov')
        col_heads <- 1
        col_data_check <- 2
        # add column for loan gty pct
        if(fcs_tbl %in% c(4,6)) col_names <- c(col_names, "gty_pct")
    }
    
    
    names(raw) <- col_names
    
    is_data <- !is.na(raw[,col_data_check] %>% .[[1]])
    
    # get headers and values
    heads <- get_heads(raw, is_data)
    vals <- get_vals(raw, is_data, col_heads)
    
    # final data = headers + values
    df <- bind_cols(filter(heads, is_data), vals)
    
    # remove programs
    df <- filter(
        df,
        !grepl('Legislative Proposal|Weighted Average',
               prog,
               ignore.case = T)
    )
    
    
    return(df)
}


# Read in Excel file
##################################################################################################
get_raw_file <- function(fcs_fy, fcs_tbl, cache_dir){
    library(readxl)
    
    xl_file <- paste0('BUDGET-', fcs_fy, '-FCS-', fcs_tbl, '.xlsx')
    cache_path <- paste(cache_dir,xl_file,sep = '/')
    
    if(!file.exists(cache_path)){
        url <- paste0('http://www.gpo.gov/fdsys/pkg/BUDGET-',
                      fcs_fy,
                      '-FCS/xls/',
                      xl_file)  
        download.file(url, cache_path, mode = 'wb' )
    }
    
    raw <- read_excel(cache_path, skip = 2, col_names = F)
    raw <- raw[, colSums(is.na(raw))<nrow(raw)]
    
    return(raw)
}





# Get Headers 
##################################################################################################
get_heads <- function(raw, is_data){
    
    # clean headers descr
    txt <-  raw$txt %>% 
        gsub('\\.{2,}','', .) %>% 
        gsub('^ +','', .) %>% 
        gsub(':$','', .) %>% 
        gsub('[1-9 ,]+$','', .) %>% 
        gsub('Department of( the)? ','', ., ignore.case = T) %>% 
        gsub('Agriculture','USDA',., ignore.case = T) %>% 
        gsub('Agency for International Development','USAID',., ignore.case = T) %>% 
        gsub('Health and Human Services','HHS',., ignore.case = T) %>% 
        gsub('Homeland Security','DHS',., ignore.case = T) %>% 
        gsub('Housing and Urban Development','HUD',., ignore.case = T) %>% 
        gsub('Export-Import Bank of the United States','EXIM Bank',., ignore.case = T) %>% 
        gsub('Overseas Private Investment Corporation','OPIC',., ignore.case = T) %>% 
        gsub('Small Business Administration','SBA',., ignore.case = T) %>% 
        gsub('Veterans Affairs','VA',., ignore.case = T)
    
    
    
    is_blank <- apply(raw, 1, function(x) sum(is.na(x)))==ncol(raw)
    is_h1 <- !is_blank & !is_data & !grepl("^ ", raw$txt) & !grepl(":$", raw$txt)
    is_h1 <- is_h1 | grepl(paste('Export-Import Bank',
                                 'National Infrastructure Bank',
                                 'Agency for International Development',
                                 'Overseas Private Investment',
                                 sep='|'), raw$txt)
    is_h2 <- !is_data & !is_h1 & !grepl("^ ", raw$txt)  
    is_h3 <- !is_data & !is_h1 & grepl("^ ", raw$txt)
    
    stopifnot(sum(is_data, is_h1, is_h2, is_h3)==nrow(raw))
    
    # setup empty grid
    heads <- matrix(nrow = nrow(raw), ncol = 4, 
                    dimnames = list(NULL, c('h1','h2','h3','prog'))) %>% 
        as.data.frame()
    
    for(i in 1:nrow(raw)){
        if(!is_blank[i]){
            
            if(is_h1[i]){
                h1 <- txt[i]
                h2 <- h3 <- NA
            } else if(is_h2[i]){
                h2 <- txt[i]
                h3 <- NA
            } else if(is_h3[i]){
                h3 <- txt[i]
            }
            
            if(txt[i]=='Direct Loans'){
                txt[i] <- paste(h3, txt[i])
                h3 <- NA
            }
            
            heads$h1[i] <- h1
            heads$h2[i] <- h2
            heads$h3[i] <- h3
            heads$prog[i] <- txt[i]
        }
    }
    
    return(heads)    
}



# Get Values 
##################################################################################################
get_vals <- function(raw, is_data, col_heads){
    
    # cleaner function
    clean_amt <- function(col){
        if(is.numeric(col)) return(col)
        
        chr <- col %>% magrittr::extract2(1) %>% 
            #remove dots ......
            gsub('\\.{2,}', '', .)
        
        
        num <- chr %>% 
            # remove commas
            gsub(',', '', .)  %>% 
            # remove footnotes
            gsub('^[1-9] +', '', .)  %>% 
            # convert * to 0
            gsub('\\*', '0', .)  %>% 
            as.numeric()
        
        # check for conversion errors
        if(num[chr!="" & !is.na(chr)] %>% is.na(.) %>% sum()>0){
            nas <- chr[is.na(num) & chr!="" & !is.na(chr)] %>% paste(., sep='\n')
            cat('Conversion Errors:\n', nas, '\n')
        }
        
        return(num)
    }
    
    df <- raw[is_data, -col_heads]
    for(j in seq_along(df))  df[,j] <- clean_amt(df[,j])
    
    return(df)
}
    