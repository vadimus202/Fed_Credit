
stopifnot(file.exists('data/txt/moodys.txt'))

moody_txt <- readLines('data/txt/moodys.txt',warn = F)

moody_txt <- moody_txt %>% 
    # remove empty lines
    .[.!=""] %>% 
    # remove headers
    .[!grepl('Rating', .)]


rates <- bind_cols(
    # years 1-10 (first 10 rows)
    gsub('^[[:alpha:] -]+', '', moody_txt[1:7]) %>% textConnection() %>% read.table(),
    # years 11-20 (last 10 rows)
    gsub('^[[:alpha:] -]+', '', moody_txt[11:17]) %>% textConnection() %>% read.table()
) %>% 
    bind_cols(
        .[,20]
    )

colnames(rates) <- c(1:20, 45)

moody <- data_frame(
    # ratings rank
    moody_num = 1:7,
    # ratings (first column)
    moody = gsub(' [[:digit:]\\. ]+', '', moody_txt)[1:7]
) %>% 
    bind_cols(rates) %>% 
    tidyr::gather(key = moody_yr, value = net_def, -moody, -moody_num) %>%
    mutate(moody_yr = as.numeric(moody_yr)) %>% 
    arrange(moody_num,moody_yr) %>% 
    mutate(moody = factor(moody, levels=unique(moody)))

save(moody, file = 'data/cache/moodys.RData')



