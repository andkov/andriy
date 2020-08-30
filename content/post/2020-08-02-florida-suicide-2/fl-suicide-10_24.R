rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages ---------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # pipes %>% 
library(ggplot2)  # graphs
library(dplyr)    # data wrangling
requireNamespace("tidyr")  # data tidying

#----- load-sources -------------------------------

# ---- declare-globals ----------------------------------------
library(ggplot2)
# the copy of the data could be retrived from :
# https://github.com/dss-hmi/suicide-prevention-2019/commit/d72ee51834ab48ab84547e20a6109a13aa88d2a2
path_file_input <- "../../../../dss-hmi/suicide-prevention-2019/data-unshared/derived/9-population-suicide.rds"
path_file_input2 <- "../../../../dss-hmi/suicide-prevention-2019/data-unshared/derived/9-population-suicide-2.rds"

# to help with sorting the levels of the `age_group` factor
lvl_age_groups <- c(
  "less_than_1"         =   "<1"          
  ,"1_4"                =   "1-4"  
  ,"5_9"                =   "5-9"  
  ,"10_14"              =   "10-14"    
  ,"15_19"              =   "15-19"    
  ,"20_24"              =   "20-24"    
  ,"25_34"              =   "25-34"    
  ,"35_44"              =   "35-44"    
  ,"45_54"              =   "45-54"    
  ,"55_64"              =   "55-64"    
  ,"65_74"              =   "65-74"    
  ,"75_84"              =   "75-84"    
  ,"85_plus"            =   "85+"      
)
age_groups_in_focus <-   lvl_age_groups[4:12]
age_groups_10_24    <-   lvl_age_groups[4:6]

#set default ggplot theme
#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------
# data prepared by "./manipulation/9-aggregator.R" combining population estimates and suicide counts

ds_population_suicide <-   readr::read_rds(path_file_input)
ds_population_suicide_2 <-   readr::read_rds(path_file_input2)


# map of florida counties
florida_counties_map <- ggplot2::map_data("county") %>% 
  dplyr::filter(region == "florida") %>% 
  dplyr::mutate_at(
    "subregion"
    , ~stringr::str_replace_all(
      .
      ,c(
        "de soto" = "Desoto"
        ,"st johns" ="Saint Johns"
        ,"st lucie" = "Saint Lucie"
      )
    )
  ) %>% tibble::as_tibble()

# ---- inspect-data --------------
ds_population_suicide %>% glimpse()
ds_population_suicide_2 %>% glimpse()

ds1 <- ds_population_suicide
ds2 <- ds_population_suicide_2


ds1 %>% distinct(race, ethnicity)
ds1 %>% distinct(year)
ds1 %>% distinct(age_group)

ds2 %>% distinct(race3, ethnicity)
ds2 %>% distinct(year)


# ---- tweak-data-1 -----------------------------------------------------

#mutate and filter data to include only ages 10-24

ds1 <- ds_population_suicide %>%
  dplyr::mutate(
    county = forcats::as_factor(county)
    ,year          = as.integer(year)
    ,sex           = factor(sex,levels = c("Male", "Female"))
    ,race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group
                            ,levels = names(lvl_age_groups)
                            ,labels = lvl_age_groups
    )
    ,n_population  = as.integer(n_population)
    ,n_suicides    = as.integer(n_suicides)
  ) %>% 
  filter(age_group %in% age_groups_10_24) %>% # !!! Only YOUTH
  select(
    county, year, sex, age_group, race, ethnicity, 
    n_population, n_suicides, everything()
  ) 

ds2 <- ds_population_suicide_2 %>% 
  mutate(
    race = forcats::fct_recode(race3,
                               "Black & Other" = "Black",
                               "Black & Other" = "Other"
    )
  ) %>% 
  group_by(county, year, sex, age, race, ethnicity ) %>% 
  summarize(
    n_population = sum(n_population, na.rm = T)
    ,n_suicides = sum(n_suicides, na.rm = T)
    ,n_firearms = sum(n_firearms, na.rm = T)
    ,n_other = sum(n_other, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    race_ethnicity = factor(paste0(race, " + ", ethnicity))
  )

# ---- inspect-data-1 ---------------------------
ds1 %>% glimpse()
ds2 %>% glimpse()
# ---- inspect-data-2 ---------------------------
ds1 %>% distinct(race, ethnicity, race_ethnicity)
ds2 %>% distinct(race, ethnicity, race_ethnicity)

# ---- declare-functions----------------------------------------

#updated to new compute rate function, that includes option for wide data

compute_rate <- function( d  ,grouping_frame  ,wide = FALSE ){
  # d <- ds_population_suicide
  # grouping_frame <- c("year")
  # 
  d_wide <- d %>%
    dplyr::group_by(.dots = grouping_frame) %>%
    dplyr::summarize(
      n_population      = sum(n_population, na.rm = T)
      ,n_suicide        = sum(n_suicides, na.rm = T)
      ,n_gun            = sum(`Firearms Discharge`, na.rm=T)
      ,n_drug           = sum(`Drugs & Biological Substances`, na.rm=T)
      ,n_hanging        = sum(`Hanging, Strangulation, Suffocation`, na.rm=T)
      ,n_jump           = sum(`Jump From High Place`, na.rm=T)
      ,n_other_seq      = sum(`Other & Unspec & Sequelae`, na.rm = T)
      ,n_other_liq      = sum(`Other & Unspec Sol/Liq & Vapor`, na.rm = T)
      ,n_other_gas      = sum(`Other Gases & Vapors`, na.rm = T)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # n_other = n_suicide - n_drug - n_gun -n_hanging - n_jump
      # n_non_gun = n_suicide - n_gun
      n_non_gun_hang   = n_suicide - n_gun - n_hanging 
      
      ,rate_suicide                 = (n_suicide/n_population)*100000
      ,rate_gun                     = (n_gun/n_population)*100000
      ,rate_drug                    = (n_drug/n_population)*100000
      ,rate_hanging                 = (n_hanging/n_population)*100000
      ,rate_jump                    = (n_jump/n_population)*100000
      # ,rate_other                 = (n_other/n_population)*100000
      ,rate_other_seq               = (n_other_seq/n_population)*100000
      ,rate_other_liq               = (n_other_liq/n_population)*100000
      ,rate_other_gas               = (n_other_gas/n_population)*100000
      # ,rate_non_gun                 = (n_non_gun/n_population)*100000
      ,rate_non_gun_hang            = (n_non_gun_hang/n_population)*100000
      
    )
  # d_wide %>% glimpse()
  col_select <- c("n_suicide"
                  ,"n_drug"
                  ,"n_gun"
                  ,"n_hanging"
                  ,"n_jump"
                  ,"n_other_seq" 
                  ,"n_other_liq" 
                  ,"n_other_gas"
                  # ,"n_non_gun"
                  ,"n_non_gun_hang")
  d_n <- d_wide %>% dplyr::select(c(grouping_frame , col_select)) %>% 
    tidyr::pivot_longer(
      cols       = col_select
      ,names_to  = "suicide_cause"
      ,values_to = "n_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "n_suicides", n_suicide, n_drug,n_gun, n_hanging, n_jump, n_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^n_","",suicide_cause)
    )
  d_rate <- d_wide %>% dplyr::select(
    c(grouping_frame, gsub("^n_","rate_",col_select))
  ) %>%
    tidyr::pivot_longer(
      cols = gsub("^n_","rate_",col_select)
      ,names_to  = "suicide_cause"
      ,values_to = "rate_suicides"
    ) %>% 
    # tidyr::gather("suicide_cause", "rate_per_100k", rate_suicide, rate_drug,rate_gun, rate_hanging, rate_jump, rate_other) %>% 
    dplyr::mutate(
      suicide_cause = gsub("^rate_","",suicide_cause)
    )
  
  d_long <- d_wide %>% dplyr::select( c(grouping_frame,"n_population") ) %>% 
    dplyr::left_join(d_n) %>% 
    dplyr::left_join(d_rate)
  
  d_out <- d_long
  if(wide){
    d_out <- d_wide
  }
  
  return(d_out)
}


#how to use
# ls_compute_rate <- ds0 %>% compute_rate("year")


# ---- q1-1 ---------------------------------------------------------
# > Q1 - What is the overall trajectory of youth suicides in FL between 2006 and 2017? 
d1 <- ds1 %>% 
  compute_rate("year") %>% 
  filter(suicide_cause == "suicide") %>% 
  select(-suicide_cause) %>% 
  mutate(
    n_out_of      = round(n_population/n_suicides,0)
    ,n_population = n_population/1000000
  ) %>% 
  tidyr::pivot_longer(
    cols       = c("n_suicides","n_population", "rate_suicides","n_out_of")
    ,names_to  = "metric"
    ,values_to = "value"
  ) 

labels <- c(
  "n_suicides"     = "Number of Suicides"
  ,"n_population"  = "Population (in Millions)"
  ,"rate_suicides" = "Rate per 100K"
  ,"n_out_of"      = "1 out of"
)

g1 <- d1 %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3), minor_breaks = seq(2006,2017,1)) +
  facet_wrap(~metric, scales = "free_y",nrow = 2 , labeller = as_labeller(labels)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse = TRUE
                        ,label.x = 0.9
                        ,label.y = 0.05) +
  geom_text(
    data = d1 %>% dplyr::filter(year %in% c(2006, 2017))
    , aes(label = round(value,2)), vjust =-0
  )+
  labs(
    x  = NULL
    ,y = NULL
  )
g1


# ---- q1-2 -----------------------------------------------------------

d2 <- ds1 %>% 
  compute_rate(c("year","age_group")) %>% 
  filter(suicide_cause == "suicide") %>% 
  select(-suicide_cause) %>% 
  mutate(
    n_out_of     = n_population/n_suicides
    ,n_population = n_population/1000000
  ) %>% 
  tidyr::pivot_longer(
    cols       = c("n_suicides","n_population", "rate_suicides" ,"n_out_of")
    ,names_to  = "metric"
    ,values_to = "value"
  ) 

labels <- c(
  "n_suicides"     = "Suicides"
  ,"n_population"  = "Population \n (in Millions)"
  ,"rate_suicides" = "Rate per 100k"
  ,"n_out_of"      = "1 Out of"
  ,"10-14"         =   "10-14"    
  ,"15-19"         =   "15-19"    
  ,"20-24"         =   "20-24" 
  
)

g2 <- d2 %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  facet_grid(metric ~ age_group, scales = "free_y", labeller = as_labeller(labels)) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult=c(.2,.5))) +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3), minor_breaks = seq(2006,2017,1)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse   = TRUE
                        ,label.x = 0.05
                        ,label.y = 1
                        ,color   = "#D95F02") +
  labs(
    x  = NULL
    ,y = NULL
  )

g2

# ---- q1-3 -----------------------------------------------------------

g3 <- d2 %>% 
  dplyr::filter(age_group %in% c("15-19","20-24")) %>% 
  dplyr::filter(metric %in% c("n_out_of")) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(alpha = 0.5) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#1B9E77") +
  facet_wrap(~ age_group, scales = "free_y", labeller = as_labeller(labels)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3), minor_breaks = seq(2006,2017,1)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse   = TRUE
                        ,label.x = 0.05
                        ,label.y = 1
                        ,color   = "#D95F02") +
  labs(
    x  = NULL
    ,y = NULL
  )

g3+labs(title = "One ouf of how many commit suicide? ")

# ---- q1-4 --------
d3 <- ds1 %>% 
  compute_rate(c("year","age_group","race_ethnicity")) %>% 
  filter(suicide_cause == "suicide") %>% 
  select(-suicide_cause) %>% 
  mutate(
    n_population = n_population/1000000
  ) %>% 
  distinct()

labels <- c(
  "Black & Other + Hispanic"     = "Black & Other \n Hispanic"
  ,"Black & Other + Non-Hispanic"  = "Black & Other \n Non-Hispanic"
  ,"White + Hispanic" = "White \n Hispanic"
  ,"White + Non-Hispanic"      = "White \n Non-Hispanic"
  ,"10-14"         =   "10-14"   
  ,"15-19"         =   "15-19"    
  ,"20-24"         =   "20-24" 
  
)

g2 <- d3 %>% 
  ggplot(aes(x = year, y = n_population, color = race_ethnicity)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey70",alpha =.5) +
  geom_line(alpha = 0.99) +
  geom_point(shape = 21, size = 3, alpha = 1) +
  facet_grid(race_ethnicity ~ age_group, scales = "free_y", labeller = as_labeller(labels)) +
  # scale_color_viridis_d(begin = .0, end = .7, option = "viridis")+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2007,2017,3), minor_breaks = seq(2006,2017,1)) +
  ggpmisc::stat_poly_eq(formula = y ~ + x 
                        ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
                        ,parse   = TRUE
                        ,label.x = 0.05
                        ,label.y = 1
                        ,color   = "#D95F02") +
  labs(
    x  = NULL
    ,y = "Population in millions"
    ,color = NULL
    ,title = "Population growth among Florida youth (10-24)"
    # scale_color_viridis_d(option = "volcano", begin = .0 , end = .8)
  )+theme(legend.position = "bottom")

g2




# ---- q2-1 -------------------------------------
# Hypothesis: does entering high-school associated with increased suicide events?
# Can we see the spike in mortality at 13-14 years of age?
# For that we need to view by year mortality event? 

# d4 <- ds_suicide_by_age %>%

d4 <- ds2 %>% 
  mutate(age = as.integer(age)) %>% 
  filter(age %in% c(10:85)) %>%
  filter(year %in% 2006:2018) %>% 
  group_by(year, age) %>% 
  summarize(
    n_suicide        = sum(n_suicides, na.rm = T)
  ) %>% 
  ungroup()
# d4 %>% glimpse()



g4 <- d4 %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA)+
  scale_x_continuous(breaks = seq(10,85,5))+
  # scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among persons of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g4

# ---- q2-2 --------------
g4 <- d4 %>% 
  dplyr::filter(age %in% 10:40) %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA)+
  scale_x_continuous(breaks = seq(10,40,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among person of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g4
# ---- q2-3 -------------------------------------
# among 10-24 the increase across age is very linear

g5 <- d4 %>% 
  filter(age %in% c(10:24)) %>% 
  ggplot(aes(x = age, y = n_suicide))+
  geom_point(shape = 21, alpha = .4, size = 2, position = position_jitter(width = .1))+
  geom_smooth(method = "lm", se= F, size = 1,color = "salmon")+
  geom_smooth(method = "loess", se= F, size = 1,color = "cyan3")+
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE, color = "salmon"
    # , vjust = 7
  )+
  geom_boxplot(aes( group = age), fill = NA, outlier.shape = NA)+
  scale_x_continuous(breaks = seq(10,40,1))+
  scale_y_continuous(breaks = seq(0,100,10))+
  geom_vline(xintercept = 24.5, size = 4, alpha = .1)+
  geom_vline(xintercept = 17.5, size = 1, linetype = "dashed", color = "grey80")+
  theme(
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Suicide events among person of the same age (2006-2018)"
    ,x = "Age in years", y = "Count of suicides (all causes)"
  )
g5


# ---- q3-1 -------------------------------------------

#yearly count of suicide means 

suicide_cause_order <- c(
  "drug"        = "Drug"
  ,"gun"          = "Gun"
  ,"hanging"      = "Hanging"
  ,"jump"         = "Jump"
  ,"other_seq"    = "Other Sequelae"
  ,"other_liq"    = "Other Liqud"
  ,"other_gas"    = "Other Gas & Vapor"
)




d6 <- ds1 %>% 
  compute_rate("year")


g6 <- d6 %>%  
  filter(!suicide_cause %in% c("suicide","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(suicide_cause_order)
                           ,labels = suicide_cause_order)
  ) %>% 
  ggplot(aes(x = reorder(suicide_cause,desc(suicide_cause)), y = n_suicides)) +
  geom_col(alpha = 0.4, fill = "#1B9E77", color = "#666666") +
  geom_text(aes(label = n_suicides), hjust = 1) +
  coord_flip() +
  facet_wrap(~year) +
  labs(
    x        = "Number of cases"
    ,y       = NULL
    ,title   = "Breakdown of Yearly Suicide Counts"
  )
g6

# ---- q3-2 -------------------------------------------

g7 <-  d6 %>%  
  filter(!suicide_cause %in% c("suicide","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(suicide_cause_order)
                           ,labels = suicide_cause_order)
  ) %>% 
  ggplot(aes(x = reorder(suicide_cause,desc(suicide_cause)), y = rate_suicides)) +
  geom_col(alpha = 0.4, fill = "#1B9E77", color = "#666666") +
  geom_text(aes(label = round(rate_suicides,1)), hjust = 1) +
  coord_flip() +
  facet_wrap(~year) +
  labs(
    x        = "Rate per 100,000"
    ,y       = NULL
    ,title   = "Breakdown of Yearly Suicide Rates"
  )

g7


# ---- q4-1 --------------------------------------------------------

major_causes <- c(
  "gun"           = "Gun"
  ,"hanging"      = "Hanging"
  ,"non_gun_hang" = "Non Gun/Hang")

major_causes_colors <- c(
  
)

g8 <- d6 %>% 
  filter(suicide_cause %in% names(major_causes)) %>%
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(major_causes)
                           ,labels = major_causes)
  ) %>% 
  ggplot(aes(x = year,y = n_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", se = F) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) +
  scale_x_continuous(breaks = seq(2006,2017,2), minor_breaks = seq(2006,2017,1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL
    ,y = "Count"
    , title = "Florida suicide mortality among youth (10-24) by means of death"
    , color = "Method"
  ) 

g8

# ---- q4-2 -----------------------------------------------------------------

g9 <- d6 %>% 
  filter(suicide_cause %in% names(major_causes)) %>%
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = names(major_causes)
                           ,labels = major_causes)
  ) %>% 
  ggplot(aes(x = year,y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = "lm", se = F) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
  ) +
  scale_x_continuous(breaks = seq(2006,2017,2), minor_breaks = seq(2006,2017,1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL
    ,y = "Rate per 100,000"
    , title = "Florida suicide mortality among youth (10-24) by means of death"
    , color = "Method"
  )

g9

# ---- q5-1---------------------------------------------

d10 <- ds1 %>% 
  compute_rate(c("year","sex","race_ethnicity")) %>% 
  filter(suicide_cause %in% c("gun","hanging","non_gun_hang")) %>% 
  mutate(
    suicide_cause = factor(suicide_cause
                           ,levels = c("gun","hanging","non_gun_hang")
                           ,labels = c("Gun","Hang","Non Gun/Hang"))
  )


g10 <- d10 %>% 
  ggplot(aes(x = year, y = rate_suicides, color = sex)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5), minor_breaks = seq(2006,2017,1)) +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_viridis_d(option = "magma",begin = .2, end = .65)+
  facet_grid(suicide_cause ~race_ethnicity
             # , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) +
  labs(
    x      = NULL
    ,y     = "Rate per 100,000"
    ,title = "Rate of Suicides by Race and Sex"
    ,color = "Sex"
  )
g10

# ---- q5-2 -----------

g11 <- d10 %>% 
  ggplot(aes(x = year, y = rate_suicides, color = suicide_cause)) +
  geom_line() +
  geom_point(shape = 21) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(2007,2017,5), minor_breaks = seq(2006,2017,1)) +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(sex ~race_ethnicity
             , scales = "free"
  ) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    # , vjust = 7
  ) +
  labs(
    x      = NULL
    ,y     = "Rate per 100,000"
    ,title = "Rate of Suicides by Race and Sex"
    ,color = "Method"
  )
g11
