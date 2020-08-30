# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./stitched-output/manipulation/0-greeter.md")
# knitr::stitch_rmd(script = "./manipulation/0-greeter.R", output = "./manipulation/stitched-output/0-greeter.md", )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified
# see http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(ggplot2)  # graphs
library(dplyr)
requireNamespace("tidyr")    # data tidying
requireNamespace("ggpubr")   # publication plots
requireNamespace("readxl")   # data import

# ---- declare-globals ---------------------------------------------------------
# you will need to replace this path to the location where you stored your data file
path_file_input <- "./content/post/2020-03-27-florida-demographic-growth/data/FloridaPopulation.xlsx"

# ---- load-data ---------------------------------------------------------------
ds0 <-  readxl::read_excel(path_file_input, col_names = FALSE, skip = 3)
# ---- tweak-data-1 -----------------------------------------------------
ds1 <- ds0 # duplicate to preserve the original
# because we removed the row with columns names during import:
names(ds1) <- c("race","ethnicity","sex","age_group","age",as.character(c(2006:2020)),"total")
ds1 %>% dplyr::glimpse(90)

# Note, we will create a separate name (e.g. `ds1`, `ds2`, `ds3`, etc) only for the dataframes 
#we intend to keep, otherwise we will overwrite the existing to augment with trivial changes

# ---- tweak-data-2 -------------------------------------
ds1 <- ds1 %>%
  # carry observations forward to fill cells missing due to Excel structure ()
  tidyr::fill(race, ethnicity,sex,age_group, age) %>% 
  dplyr::filter(!age == "Total")  # because makes data untidy, we'll compute later
ds1 %>% dplyr::glimpse(90)

# ---- tweak-data-4 -------------------------------------
ds1 %>% dplyr::distinct(age_group) # to find out what strings got misinterpreted
ds1 %>% 
  dplyr::filter(age_group %in% c("43834","43960","44118")) %>% 
  dplyr::distinct(age_group, age) # to identify how to recode
# imlement the recoding
ds1 <- ds1 %>%   
  dplyr::mutate(
    age_group = stringr::str_replace(age_group, "43834", "1-4")
    ,age_group = stringr::str_replace(age_group, "43960", "5-9")
    ,age_group = stringr::str_replace(age_group, "44118", "10-14")
  ) %>%
  dplyr::select(-total) # because it makes no sense in this context (adds up across the rows)
ds1 %>% glimpse(90)
  
# ---- tweak-data-5 ---------------
ds1$age_group5 <- ds1$age_group # because we want to keep the original for quality check
ds1$age_group5[ds1$age %in% c(0:4)]   <- "0-4"
ds1$age_group5[ds1$age %in% c(25:29)] <- "25-29"
ds1$age_group5[ds1$age %in% c(30:34)] <- "30-34"
ds1$age_group5[ds1$age %in% c(35:39)] <- "35-39"
ds1$age_group5[ds1$age %in% c(40:44)] <- "40-44"
ds1$age_group5[ds1$age %in% c(45:49)] <- "45-49"
ds1$age_group5[ds1$age %in% c(50:54)] <- "50-54"
ds1$age_group5[ds1$age %in% c(55:59)] <- "55-59"
ds1$age_group5[ds1$age %in% c(60:64)] <- "60-64"
ds1$age_group5[ds1$age %in% c(65:69)] <- "65-69"
ds1$age_group5[ds1$age %in% c(70:74)] <- "70-74"
ds1$age_group5[ds1$age %in% c(75:79)] <- "75-79"
ds1$age_group5[ds1$age %in% c(80:84)] <- "80-84"

ds1 %>% dplyr::distinct(age_group, age_group5) # to inspect the result

# ---- tweak-data-6 -------------------
# to translate into a longer form with respect to year
ds2 <- ds1 %>%
  tidyr::pivot_longer(cols = as.character(2006:2020),names_to = "year", values_to = "count") 
ds2 %>% dplyr::glimpse(90)
# `ds2` is the source set for computing totals because it has both `age_group`` categories

# ---- tweak-data-7 ------------------
lvl_age_groups <-c(
  "<1"
  ,"1-4"
  ,"5-9"
  ,"10-14"
  ,"15-19"
  ,"20-24"
  ,"25-34"
  ,"35-44"
  ,"45-54"
  ,"55-64"
  ,"65-74"
  ,"75-84"
  ,"85+"
)

lvl_age_groups5 <- c(
  "0-4"
  ,"5-9"
  ,"10-14"
  ,"15-19"
  ,"20-24"
  ,"25-29"
  ,"30-34"
  ,"35-39"
  ,"40-44"
  ,"45-49"
  ,"50-54"
  ,"55-59"
  ,"60-64"
  ,"65-69"
  ,"70-74"
  ,"75-79"
  ,"80-84"
  ,"85+"
)
ds2 <- ds2 %>%
  dplyr::mutate(
    race_ethnicity = factor(paste0(race, " + ", ethnicity))
    ,race          = factor(race)
    ,ethnicity     = factor(ethnicity)
    ,age_group     = factor(age_group, levels = lvl_age_groups)
    ,age_group5    = factor(age_group5, levels = lvl_age_groups5)
    ,age           = as.integer(age)
    ,sex           = factor(sex)
    ,year          = as.integer(year)
  ) %>% 
  dplyr::select(race, ethnicity, age_group, age_group5, dplyr::everything())
ds2 %>% dplyr::glimpse(90)


# ---- tweak-data-8 -----------------------
ds_age_group <- ds2 %>% 
  dplyr::group_by(race, ethnicity, sex, age_group, year) %>% 
  dplyr::summarize(
    count = sum(count, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    race_ethnicity = paste0(race, " + ", ethnicity),
    race_ethnicity = factor(race_ethnicity)
  )

ds_age_group5 <- ds2 %>% 
  dplyr::group_by(race, ethnicity, sex, age_group5, year) %>% 
  dplyr::summarize(
    count = sum(count, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    race_ethnicity = paste0(race, " + ", ethnicity),
    race_ethnicity = factor(race_ethnicity)
  )

# ---- save-to-disk -------------------
list(
  "ds_wide"        = ds1
  ,"ds_long"       = ds2
  ,"ds_age_group"  = ds_age_group
  ,"ds_age_group3" = ds_age_group5
) %>% 
  saveRDS("./content/post/2020-03-27-florida-demographic-growth/data/clean_data.rds")
# ---- g0 -----------------------------
# Total population of Florida over the years
ds_age_group5 %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(
    count = sum(count, na.rm = T)
  ) %>% 
  ggplot(aes(x=year, y = count))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()

# ---- g1 -----------------------------
# total population of Florida by broken down by 4 ethnic groups (race_ethnicity)
d1 <- ds_age_group5 %>% 
  dplyr::group_by(race_ethnicity, year) %>% 
  dplyr::summarize(
    n_people = sum(count, rm.na = T)
  )
g1 <- d1 %>% 
  ggplot(aes(x = year,  y = n_people, color = race_ethnicity))+
  geom_line(aes(group = race_ethnicity))+
  geom_point(shape = 21, fill = NA, size =2)+
  scale_y_continuous(labels = scales::comma)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = - 90,vjust =.5, hjust = -0)
    #https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  )+
  labs(
    title = "Population growth in Florida over last 15 years \n  broken down by ethnic groups"
    ,color = "Ethnic Group"
    ,x = "Calendar Year"
    ,y = "Population Count"
  )
g1

# ----- q1 ------------------------------------
# Q: what Ethnic group is most dissimilar from the other three in their dynamics?
g1 + facet_wrap(~race_ethnicity, scale = "free_y")
# A: "White + Non-Hispanic" because of a "dip" in late 2000's

# ---- g2 -------------------------------------
# Build a graph showing age composition of all ethnic groups in 2019
g2 <- ds_age_group5 %>%
  dplyr::filter(year == 2019) %>%
  ggplot(aes(x = age_group5, y = count, fill = race_ethnicity)) +
  geom_col()+
  facet_grid(sex ~ race_ethnicity)+
  scale_y_continuous(labels = scales::comma)+
  # https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2 also https://r-graphics.org/recipe-axes-tick-label
  theme_bw()+
  theme(
     axis.text.x = element_text(angle = - 90,vjust =.5, hjust = -0)
     #https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  )+
  labs(
    title = "Population in Florida in 2019 broken down by age groups and gender"
    ,color = "Ethnic Group"
    ,x = "Calendar Year"
    ,y = "Population Count"
  )
g2
# ----- q2 -------------------------
g2a <- ds_age_group %>%
  dplyr::filter(year == 2019) %>%
  ggplot(aes(x = age_group, y = count, fill = race_ethnicity)) +
  geom_col()+
  facet_grid(sex ~ race_ethnicity)+
  scale_y_continuous(labels = scales::comma)+
  # https://stackoverflow.com/questions/14563989/force-r-to-stop-plotting-abbreviated-axis-labels-e-g-1e00-in-ggplot2 also https://r-graphics.org/recipe-axes-tick-label
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = - 90,vjust =.5, hjust = -0)
    #https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
  )+
  labs(
    title = "Population in Florida in 2019 broken down by age groups and gender"
    ,color = "Ethnic Group"
    ,x = "Calendar Year"
    ,y = "Population Count"
  )
g2a

# ---- publish ---------------------------------
rmarkdown::render(
  input = "./analysis/blogposts/florida-demographic-growth/fl-demo-growth.Rmd"
  ,output_format = c(
    "html_document"
    # ,"pdf_document"
    # ,"md_document"
    # "word_document"
  )
  ,clean=TRUE
)



