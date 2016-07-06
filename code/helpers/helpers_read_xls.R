



# Get a clean Fed Credit Supplement Table
##################################################################################################
get_clean_df <- function(fcs_fy, fcs_tbl, cache_dir='data/xls'){
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
    
    raw <- clean_clean_txt(raw)
    
    is_data <- !is.na(raw[,col_data_check] %>% .[[1]])
    
    # get headers and values
    heads <- get_heads(raw, is_data)
    if(fcs_tbl %in% 1:2) heads <- bind_cols(heads, raw[,2])
    heads <- filter(heads, is_data)
    
    # clean values    
    vals <- get_vals(raw, is_data, col_heads)
    
    # final data = headers + values
    df <- bind_cols(heads, vals)
    
    # remove programs
    df <- filter(
        df,
        !grepl('Legislative Proposal|Weighted Average',
               prog,
               ignore.case = T)
    )
    
    
    return(df)
}

# Clean Descriptions
##################################################################################################
clean_clean_txt <- function(raw){
    
    
    raw$txt <- raw$txt %>% 
        # remove dots (.....)
        gsub('\\.{2,}','', .) %>% 
        # remove trailing footnotes
        gsub('[1-9 ,]+$','', .) %>% 
        gsub('Department of( the)? ', '', ., ignore.case = T) %>% 
        gsub('Agriculture','USDA',., ignore.case = T) %>% 
        gsub('Agency for International Development:?','USAID',., ignore.case = T) %>% 
        gsub('Health and Human Services','HHS',., ignore.case = T) %>% 
        gsub('Homeland Security','DHS',., ignore.case = T) %>% 
        gsub('Housing and Urban Development','HUD',., ignore.case = T) %>% 
        gsub('Export-Import Bank of the United States','EXIM Bank',., ignore.case = T) %>% 
        gsub('Overseas Private Investment Corporation:?','OPIC',., ignore.case = T) %>% 
        gsub('Small Business Administration','SBA',., ignore.case = T) %>% 
        gsub('Veterans Affairs','VA',., ignore.case = T) %>% 
        gsub('   Business Loans:','Business Loans:',., ignore.case = T) %>% 
        gsub('Transportation, Infrastructure','Transportation Infrastructure',., ignore.case = T) %>% 
        gsub('Risk Category$','Risk Category 4 Guarantees',., ignore.case = T)
    
    
    raw <- paste_split_lines(
        raw,
        line_1 = '504 Commercial Real Estate \\(CRE\\) Refinance',
        line_2 = '^ *?Program')
    
    raw <- paste_split_lines(
        raw,
        line_1 = 'Section 504 Certified Development Companies',
        line_2 = 'Debentures')
    
    raw <- paste_split_lines(
        raw,
        line_1 = 'Community Development Loan Guarantee',
        line_2 = '\\(Section 108\\)')
    
    raw <- paste_split_lines(
        raw,
        line_1 = 'Minority Business Resource Center',
        line_2 = 'Loan Guarantees')
    
    raw <- paste_split_lines(
        raw,
        line_1 = 'Transportation, Infrastructure, Finance \\& Innovation \\(TIFIA\\)',
        line_2 = 'Direct Loans')
    
    ######## 2017 ##############
    raw <- paste_split_lines(
        raw,
        line_1 = 'Section 108 Community Development Loan',
        line_2 = 'Guarantee \\(Fee\\)')

    return(raw)    
}

# Function to combine split descr lines
paste_split_lines <- function(raw, line_1, line_2){
    
    from <- grep(line_1, raw$txt,ignore.case = T)
    to <- grep(paste0('^ *?',line_2), raw$txt,ignore.case = T)
    
    to <- intersect(to,from+1)
    from <- to-1
    
    raw$txt[to] <- paste(
        raw$txt[from],
        raw$txt[to] %>% gsub('^ *','', .)
    )
    raw$txt[from] <- NA
    
    return(raw)
}



# Read in Excel file
##################################################################################################
get_raw_file <- function(fcs_fy, fcs_tbl, cache_dir){
    library(readxl)
    
    xl_file <- paste0('BUDGET-', fcs_fy, '-FCS-', fcs_tbl, ifelse(fcs_fy>=2016, '.xlsx', '.xls'))
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
        # remove leading spaces
        gsub('^ +','', .) %>% 
        # remove trailing ":"
        gsub(':$','', .)
    
    
    is_blank <- apply(raw, 1, function(x) sum(is.na(x)))==ncol(raw)
    is_h1 <- !is_blank & !is_data & !grepl("^ ", raw$txt) & !grepl(":$", raw$txt)
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
            # remove footnotes like "1.00 (*)" 
            gsub(' +\\(.+\\) *$', '', .)  %>% 
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
