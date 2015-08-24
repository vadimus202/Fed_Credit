library(dplyr)


budget_cache <- 'data/cache/budget_est.RData'

if(!file.exists(budget_cache)){
    source('code/prep_budget_estimates.R')
}
load(budget_cache)

cy <- budget_est %>% filter(fy==2016)



library(ggplot2)
ggplot(data = cy, aes(net_def))+
    theme_bw()+
    geom_histogram(binwidth=10, color='black', fill='firebrick')

ggplot(
    data = budget_est %>% 
        group_by(fy, moody_num, moody) %>% 
        summarise(amt=sum(amt)) %>% 
        ungroup() %>% 
        arrange(moody_num) %>% 
        mutate(moody=factor(moody, levels=unique(moody))), 
       aes(moody, amt, fill=factor(fy)))+
    theme_bw()+
    geom_bar(stat = 'identity',position = 'dodge')

ggplot(
    data = budget_est %>% 
        group_by(fy, purp) %>% 
        summarise(amt=sum(amt)), 
    aes(purp, amt, fill=factor(fy)))+
    theme_bw()+
    geom_bar(stat = 'identity',position = 'dodge')

ggplot(
    data = budget_est %>% 
        group_by(fy, purp) %>% 
        summarise(Subsidy=sum(sr/100*amt/1000,na.rm = T) %>% round(0)), 
    aes(purp, Subsidy, fill=factor(fy)))+
    theme_bw()+
    geom_bar(stat = 'identity',position = 'dodge')


# Sankey
library(googleVis)
df_sankey <- bind_rows(
    cy %>% 
        group_by(from=purp, to=h1) %>% 
        summarise(amount = sum(amt,na.rm = T),
                  exposure = sum(sr_def*amt/100,na.rm = T) %>% 
                      round(0)),
    cy %>% 
        group_by(from=h1, to=moody) %>% 
        summarise(amount = sum(amt,na.rm = T),
                  exposure = sum(sr_def*amt/100,na.rm = T) %>% 
                      round(0)),
    cy %>% 
        group_by(from=moody, to=type) %>% 
        summarise(amount = sum(amt,na.rm = T),
                  exposure = sum(sr_def*amt/100,na.rm = T) %>% 
                      round(0)) %>% 
        mutate(to = ifelse(to=='DL','Direct Loans','Loan Guarantees'))
)




sk_exp <- gvisSankey(data = df_sankey %>% select(from,to,exposure), 
                     from = 'from', to = 'to', weight = 'exposure',
                     options=list(width=700, height=500))

sk_amt <- gvisSankey(data = df_sankey %>% select(from,to,amount), 
                     from = 'from', to = 'to', weight = 'amount',
                     options=list(width=700, height=500))


plot(sk_amt)
plot(sk_cost)
plot(sk_exp)



# Borrower Int ~ Int Subsidy
ggplot(data = cy %>% filter(type=='DL'), 
       aes(bor_int, sr_int, color=mat))+
    theme_classic() +
    geom_point()+
    geom_smooth(method="lm")

# Def Rate ~ Def Subsidy
ggplot(data = budget_est %>% filter(fy==2016), 
       aes(net_def, sr_def))+
    theme_classic() +
    #     scale_x_log10()+
    #     scale_y_log10()+
    geom_point(aes(color=moody, size=amt/1000000))+
    scale_size_continuous(range = c(3, 10))+
    geom_smooth(method="lm")+
    labs(x='% Default Rate, Net of Recoveries',y='Net Defaults Subsidy Rate Component')



