# This script inputs the raw data and prepares it for tuning.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 

# ---- load-packages -----------------------------------------------------------
library(magrittr) # pipes
requireNamespace("dplyr")
requireNamespace("readr")


# ---- load-data ---------------------------------------------------------------
# consider the following data as the starting point for this exercise
ds_wide <- structure(
  list(
    SUBNO        = c(1001, 1002, 1003)
    , FEMALE     = c(0, 1,0)
    , RACE       = c(2, 3, 1)
    , AGE_0      = c(15, 13, 12)
    , AGE_1      = c(NA, 14,13)
    , AGE_2      = c(NA, 15, 14)
    , AGE_3      = c(NA, 16, 15)
    , BULLYING_0 = c(3.8,2.3, 0.6)
    , BULLYING_1 = c(NA, 4.6, 1.6)
    , BULLYING_2 = c(NA, NA,3)
    , BULLYING_4 = c(NA, 0.66, 1.8)
  )
  , class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame")
  , row.names = c(NA, -3L)
  )

# ---- inspect-data -----------------------------------------------------------
ds_wide %>% dplyr::glimpse()
ds_wide %>% as.data.frame %>%  print()

# ----- scenario-1-chunk-1 ------------------
ds_wide %>% 
  dplyr::select_(.dots = c("SUBNO",paste0("AGE_",0:3)))


# ----- scenario-1-chunk-2 ------------------
ds_long_1 <- ds_wide %>% 
  dplyr::select_(.dots = c("SUBNO",paste0("AGE_",0:3))) %>%
  # new lines:
  tidyr::gather(
    key    = "key"   # name of new variable to store the key
    ,value = "value" # name of new variable to store the values
    ,AGE_0, AGE_1, AGE_2, AGE_3 # selection of columns to gather
  ) %>% 
  dplyr::arrange(SUBNO)
ds_long_1 %>% print()

# ----- scenario-1-chunk-3 ------------------
ds_long_2 <- ds_long_1 %>% 
  dplyr::mutate(
    age  = value, # the variable the will store the value of age
    wave = gsub(pattern = "AGE_(\\d+)",replacement = "\\1", x = key)
  ) 
ds_long_2 %>% print()

# ----- scenario-1-chunk-4 ------------------
ds_long_3 <- ds_long_2 %>% 
  dplyr::select(-key, -value) # because no need in technical variables anymore
ds_long_3 %>% print()

# ----- scenario-1-chunk-5 ------------------
ds_long <- ds_wide %>% 
  dplyr::select_(.dots = c("SUBNO",paste0("AGE_",0:3))) %>% 
  tidyr::gather(
    key    = "key"   # name of new variable to store the key
    ,value = "value" # name of new variable to store the values
    ,AGE_0, AGE_1, AGE_2, AGE_3 # selection of columns to gather
  ) %>% 
  dplyr::mutate(
    age  = value, # the variable the will store the value of age
    wave = gsub(pattern = "AGE_(\\d+)",replacement = "\\1", x = key)
  ) %>% 
  dplyr::select(-key, -value) %>% 
  dplyr::arrange(SUBNO)
ds_long %>% print()
# ----- scenario-1-chunk-6 ------------------
ds_wide %>% dplyr::glimpse()


# ---- scenario-1a -----------------------------------
ds_long_1a <- ds_wide_1 %>% 
  tidyr::gather(
    key    = "key"   # name of new variable to store the key
    ,value = "value" # name of new variable to store the values
    ,paste0("AGE_",0:6) # selection of columns to gather
  ) 
# however, this is not optimal, because we would have to adjust this code
# if more waves or variables are added later on
# instead, we would like to make this process automatic

(varnames <- names(ds_wide_1))
# hard code the names of the variables that are "static"
# i.e. they do not change with wave (what the ds is wide with respect to)
(variables_static <- c("SUBNO")) # only one variable doesn't change with wave
# all other variables will be defined as "dynamic", changing with wave
(variables_dynamic <- setdiff(varnames, variables_static))
# this allows us to express the above code more optimally:
ds_long_1a <- ds_wide_1 %>% 
  tidyr::gather(
    key    = "key"   # name of new variable to store the key
    ,value = "value" # name of new variable to store the values
    ,variables_dynamic # selection of columns to gather
  )
ds_long_1a %>% print()
# the rest of the transformations are not affected
ds_long_1a <- ds_long_1a %>% 
  dplyr::mutate(
    age = value, # the variable the will store the value of age
    wave = gsub(pattern = "AGE_(\\d+)",replacement = "\\1", x = key)
  ) %>% 
  dplyr::select(-key) # because it is a technical variable (not so if more than 2)
ds_long_1a %>% print()

# ----- scenario-2 ---------------------
# now we will increase the complexity of the task by adding a new dynamic variable
ds_wide_2 <- ds_wide %>%   # 3 static and 2x7 dynamic variables
  dplyr::filter(SUBNO == 1003)
ds_wide_2 %>% dplyr::glimpse()
# we need to redefine the groups of variables

(varnames <- names(ds_wide_2))
(variables_static <- c("SUBNO","FEMALE","RACE")) 
# notice that must specify ALL vars that do not change with wave
(variables_dynamic <- setdiff(varnames, variables_static))
# keep those in the first that are NOT in the second

ds_long_2 <- ds_wide_2 %>%
  # we can simplify the notation to save space (compare to scenario 1 and 1a)
  tidyr::gather("key","value", variables_dynamic)  # key, value, list of vars
ds_long_2 %>% print()

# now we are ready to separate the two dynamic variables
ds_long_2 <- ds_long_2 %>% 
  dplyr::mutate(
    # 1st capturing group = any number of word characters
    # 2nd capturing group = any number of digit characters
    # capturing groups separated by a literal "_"
    wave = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\2", x = key)
    ,key = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\1", x = key)
  )
ds_long_2 %>% print()
# finally we spread the two dynamic variables stored in the "key" column
ds_long_2 <- ds_long_2 %>% 
  tidyr::spread(key = "key", value = "value")
ds_long_2



  
  
  
  
  
  
  
  

