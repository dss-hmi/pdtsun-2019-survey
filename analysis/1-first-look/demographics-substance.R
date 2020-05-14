# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(ggplot2)
library(dplyr)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("readr")   # data input
requireNamespace("tidyr")   # data manipulation
requireNamespace("dplyr")   # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # For asserting conditions meet expected patterns.
requireNamespace("corrplot")  # For asserting conditions meet expected patterns.
# requireNamespace("car")     # For it's `recode()` function.
config <- config::get()

varname_n_scale       <- config$varname_n_scale
varname_chu_scale     <- config$varname_chu_scale
varname_warwick_scale <- config$varname_warwick_scale
varname_e_scale       <- config$varname_e_scale

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes
baseSize = 8
# ---- declare-globals ---------------------------------------------------------
describe_item <- function(d, varname){
  # d <- ds %>% select(id, Q9)
  # d %>% glimpse()
  # varname <- "Q9"

  (variable_label <- labelled::var_label(d[,varname])[[1]])
  d %>% histogram_discrete(varname)+labs(title = paste0(varname,": ",variable_label))
}

# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data-unshared/derived/dto.rds")
names(dto)
# 1st element - dto[["unitData"]] - unit(person) level data; all original variables
# 2nd element - dto[["metaData"]] - meta data, info about variables
meta <- dto[["metaData"]]
# 3rd element - dto[["analytic"]] - small, groomed data to be used for analysis
ds <- dto[["microData"]]
ds %>% names()

# ---- inspect-data -------------------------------------------------------------

# ---- print-meta-1 ---------------------------
ds_e <- dto$microData %>% dplyr::select( c("id",varname_e_scale)) #%>% tibble::as_tibble()
ds_n <- dto$microData %>% dplyr::select( c("id",varname_n_scale))


# ---- tweak-data --------------------------------------------------------------
# ds <- dto$microData %>% dplyr::select( c("id",c(varname_n_scale,varname_e_scale))) #%>% tibble::as_tibble()
# varname_n_scale <- tolower(varname_n_scale)
# names(ds) <- tolower(names(ds))
# glimpse(ds, 50)
convert_back_to_integers <- function(x){
  x = as.numeric(x)
}
keep_only_scale_numbers <- function(x){
  x = ifelse(x %in% c(1:5), x, NA)
}
convert_to_symmetric_scale <- function(x){
  x = x -3
}
ds1 <- c(
  ds %>% dplyr::select(id),
  ds %>% dplyr::select(c(varname_n_scale,varname_e_scale)) %>%
    dplyr::mutate_all(convert_back_to_integers) %>%
    dplyr::mutate_all(keep_only_scale_numbers) %>%
    dplyr::mutate_all(convert_to_symmetric_scale)
) %>%
  tibble::as_tibble()
# ds1 %>% glimpse(50)

# for(i in setdiff(names(ds),c("id","race_ethnicity" )) ){
#   # if( !is.null(labelled::var_label(ds[[i]])) ){
#     i_label <- labelled::var_label(ds[[i]])
#     if(!is.null(i_label)){
#       labelled::var_label(ds1[[i]]) <- i_label
#     }
#   # }
# }
# dsn
# ds1 %>% glimpse()

# keep only complete cases
ds1 <- ds1[complete.cases(ds1),]

# ----- useful-sample ---------------
ds <- ds %>%
  dplyr::filter(id %in% (ds1 %>% dplyr::pull(id))  )

# ---- basic-table --------------------------------------------------------------

# ---- basic-graph --------------------------------------------------------------


# ---- demographics -----------------------------------------
cat("\n Sample size: ")
ds$id %>% length() %>% unique()

cat("\n\n")
cat("## Sample characteristics\n")
ds %>% describe_item("gender")
ds %>% describe_item("race")
ds %>% describe_item("ethnicity")
ds %>% describe_item("race_ethnicity")
ds %>% describe_item("class_standing")

# ---- substance-use ---------------------
cat("\n\n")
cat("## Concerned about use\n")
ds %>% describe_item("Q9")

cat("\n\n")
cat("## Met my goal\n")
ds %>% describe_item("Q11")

# item Q12
cat("\n\n")
cat("## What helped - items \n")
ds %>% describe_item("Q12_1")
ds %>% describe_item("Q12_2")
ds %>% describe_item("Q12_3")
ds %>% describe_item("Q12_4")
ds %>% describe_item("Q12_5")
ds %>% describe_item("Q12_6")
ds %>% describe_item("Q12_7")
ds %>% describe_item("Q12_8")
ds %>% describe_item("Q12_9")
ds %>% describe_item("Q12_10")
ds %>% describe_item("Q12_11")
ds %>% describe_item("Q12_12")
ds %>% describe_item("Q12_13")
ds %>% describe_item("Q12_14")

cat("\n\n")
cat("## What helped - summary\n")
vars_helped_goal <- paste0("Q12_",1:14)
d1 <- ds %>%
  dplyr::select(c("id",vars_helped_goal)) %>%
  # dplyr::filter(id == "R_0Oi2kFZHx1kSxMd") %>%
  # dplyr::filter(id %in% c("R_0Oi2kFZHx1kSxMd","R_0GMDW5Vmy3q3fHj")) %>%
  tidyr::gather(key = "item", value = "response", vars_helped_goal) %>%
  dplyr::select(-item) %>%
  dplyr::distinct() %>%
  dplyr::arrange(id) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate( n_responses = n() ) %>%
  dplyr::ungroup()
# d1
only_missing_response <- d1 %>%
  dplyr::filter( n_responses == 1 & response == "(Missing)")
only_nonmissing_response <- d1 %>%
  dplyr::filter( n_responses > 1) %>%
  dplyr::filter(!response == "(Missing)")

d_q12 <- only_nonmissing_response %>%
  dplyr::group_by(response) %>%
  dplyr::summarize(n_freq = n()) %>%
  dplyr::arrange(desc(n_freq))
factor_levels <- d_q12$response %>% as.character()
d_q12 <- d_q12 %>% dplyr::mutate(
  response = factor(response, levels = factor_levels),
  response = factor(response, levels = rev(levels(response)))
  )

d_q12 %>%
  ggplot(aes(x = response, y = n_freq)) +
  geom_bar(stat = "identity", alpha =  .5, fill = "salmon")+
  geom_text(aes(label = n_freq))+
  coord_flip()+
  theme_bw()+
  labs(title = paste0("Q12: ", labelled::var_label(ds$Q12_1), "\n (frequency of non-unique responses)") )

cat("\n\n")
cat("## What helped - Comments\n")
# comments to Q12
ds %>% dplyr::distinct(Q14) %>% neat()


# item Q12

ds %>% describe_item("Q13_1")
ds %>% describe_item("Q13_2")
ds %>% describe_item("Q13_3")
ds %>% describe_item("Q13_4")
ds %>% describe_item("Q13_5")
ds %>% describe_item("Q13_6")
ds %>% describe_item("Q13_7")
ds %>% describe_item("Q13_8")
ds %>% describe_item("Q13_9")
ds %>% describe_item("Q13_10")
ds %>% describe_item("Q13_11")
ds %>% describe_item("Q13_12")
ds %>% describe_item("Q13_13")

cat("\n\n")
cat("## What hindered - Summary\n")
vars_hindered_goal <- paste0("Q13_",1:13)
d1 <- ds %>%
  dplyr::select(c("id",vars_hindered_goal)) %>%
  # dplyr::filter(id == "R_0Oi2kFZHx1kSxMd") %>%
  # dplyr::filter(id %in% c("R_0Oi2kFZHx1kSxMd","R_0GMDW5Vmy3q3fHj")) %>%
  tidyr::gather(key = "item", value = "response", vars_hindered_goal) %>%
  dplyr::select(-item) %>%
  dplyr::distinct() %>%
  dplyr::arrange(id) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate( n_responses = n() ) %>%
  dplyr::ungroup()
# d1
only_missing_response <- d1 %>%
  dplyr::filter( n_responses == 1 & response == "(Missing)")
only_nonmissing_response <- d1 %>%
  dplyr::filter( n_responses > 1) %>%
  dplyr::filter(!response == "(Missing)")

d_q13 <- only_nonmissing_response %>%
  dplyr::group_by(response) %>%
  dplyr::summarize(n_freq = n()) %>%
  dplyr::arrange(desc(n_freq))
factor_levels <- d_q13$response %>% as.character()
d_q13 <- d_q13 %>% dplyr::mutate(
  response = factor(response, levels = factor_levels),
  response = factor(response, levels = rev(levels(response)))
)

d_q13 %>%
  ggplot(aes(x = response, y = n_freq)) +
  geom_bar(stat = "identity", alpha =  .5, fill = "lightblue")+
  geom_text(aes(label = n_freq))+
  coord_flip()+
  theme_bw()+
  labs(title = paste0("Q13: ", labelled::var_label(ds$Q13_1), "\n (frequency of non-unique responses)") )

cat("\n\n")
cat("## What hindered - Comments\n")
# comments to Q13
ds %>% dplyr::distinct(Q15) %>% neat()

cat("\n\n")
cat("## Craving (New)\n")
# Q16
ds %>% describe_item("Q16")


# ---- q40 ------------------
# ds %>% distinct(Q40)
# cat("\n\n")
# cat("## Craving (old)\n")
# custom_levels <- c(
#   "Moderate urge"
#   ,"None at all"
#   ,"MIld urge"
#   ,"Slight, that is, a very mild urge"
#   ,"Strong urge, but easily controlled"
#   ,"Strong urge, would have used if available"
#   ,"Strong urge and difficult to control"
#   ,"(Missing)"
# )
# var_label <- labelled::var_label(ds$Q40)
# ds <- ds %>%
#   dplyr::mutate(
#      Q40 = factor(Q40, levels = custom_levels)
#     ) %>%
#   dplyr::filter(!is.na(Q40))
# labelled::var_label(ds$Q40) <- var_label

ds %>% describe_item("Q40")


# ----- publisher --------------------
path <- "./analysis/1-first-look/demographics-substance.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)
