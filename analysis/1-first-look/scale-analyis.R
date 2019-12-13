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


  # d1 <- d %>%
  #   dplyr::rename(temp = varname ) %>%
  #   dplyr::mutate(
  #     temp = as.numeric(factor(temp)),
  #     temp = ifelse(temp %in% c(1:5), temp, NA)
  #   ) %>%
  #   plyr::rename(c("temp" = varname))
  #
  # d1 %>% group_by(temp) %>% summarize(n = n())
  #
  # psych::summary.psych(d)
  # d1 %>% histogram_continuous(varname)


  # cat("\n")
  # cat("\nMean: ",round(mean( as.numeric( factor(d[,varname]) ),na.rm = T),2),"\n")
  # cat("\nSD: ", round(sd(as.numeric(d[,varname]), na.rm = T),2),"\n")
  # cat("\nMissing: ",sum(is.na(d[,varname])),"\n")

}

varname_n_scale <- c(
  "Q4_1"
  ,"Q4_2"
  ,"Q4_3"
  ,"Q4_4"
  ,"Q4_5"
  ,"Q4_6"
  ,"Q4_7"
  ,"Q4_8"
  ,"Q4_9"
  ,"Q4_10"
  ,"Q4_11"
  ,"Q4_12"
  ,"Q4_13"
  ,"Q4_14"
  ,"Q4_15"
  ,"Q4_16"
  ,"Q6_1"
  ,"Q6_2"
  ,"Q6_3"
  ,"Q6_4"
)

varname_e_scale <- c(
  "Q17"
  ,"Q18"
  ,"Q19"
  ,"Q20"
  ,"Q21"
  ,"Q22"
  ,"Q23"
  ,"Q24"
  ,"Q25"
  ,"Q26"
  ,"Q27"
  ,"Q28"
  ,"Q29"
  ,"Q30"
  ,"Q31"
  ,"Q33"
  # ,"Q34"
  ,"Q35"
  ,"Q36"
  ,"Q37"
  ,"Q38"
  ,"Q39"
)


describe_item <- function(d, varname){
  # d <- ds %>% select(id, Q9)
  # d %>% glimpse()
  # varname <- "Q9"
  (variable_label <- labelled::var_label(d[,varname])[[1]])
  d %>% histogram_discrete(varname)+labs(title = paste0(varname,": ",variable_label))
}

make_corr_matrix <- function(d,metaData,item_names){
  # d <- dsn
  # metaData <- dto$metaData
  # item_names <- varname_n_scale
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select_(.dots=item_names)
  d2 <- d1[complete.cases(d1),] %>%
    dplyr::mutate(
      total_score = rowSums(.)
    )
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = paste0(item_name,"\n",item_label))

  rownames <- rownames[,"display_name"]
  rownames[nrow(rownames)+1,1]<- "total\nscore"
  rownames <- rownames %>% as.list() %>% unlist() %>% as.character()

  d3 <- sapply(d2, as.numeric)
  # d3 %>% glimpse()
  cormat <- cor(d3)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
  return(cormat)
}

make_corr_plot <- function (
  corr,
  lower="number",
  upper="circle",
  tl.pos=c("d","lt", "n"),
  diag=c("n", "l", "u"),
  bg="white",
  addgrid.col="gray", ...
){

  diag <- match.arg(diag)
  tl.pos <- match.arg(tl.pos)
  n <- nrow(corr)
  # corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
  corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos)
  # corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
  corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n")
  if (diag == "n" & tl.pos != "d") {
    symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
  }
}
# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data-unshared/derived/dto.rds")
# names(dto)
# 1st element - dto[["unitData"]] - unit(person) level data; all original variables
# 2nd element - dto[["metaData"]] - meta data, info about variables
meta <- dto[["metaData"]]
# 3rd element - dto[["analytic"]] - small, groomed data to be used for analysis
ds <- dto[["microData"]]
# ds %>% names()
# ---- inspect-data -------------------------------------------------------------

#
# ds_e <- dto$microData %>% dplyr::select( c("id",varname_e_scale)) #%>% tibble::as_tibble()
# ds_n <- dto$microData %>% dplyr::select( c("id",varname_n_scale))

# ---- tweak-data --------------------------------------------------------------
ds <- dto$microData %>% dplyr::select( c("id",c(varname_n_scale,varname_e_scale))) #%>% tibble::as_tibble()
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
ds1 %>% glimpse(50)

for(i in setdiff(names(ds),"id")){
  i_label <- labelled::var_label(ds[[i]])
  labelled::var_label(ds1[[i]]) <- i_label

}
# dsn
# ds1 %>% glimpse()

ds_labels <- dto$metaData %>%
  dplyr::filter(item_name %in% c(varname_n_scale, varname_e_scale)) %>%
  dplyr::mutate(display_name = paste0(item_name,"\n",item_label))

# ---- item-distributions ---------------------
# ds %>% glimpse()
# ds %>% describe_item("Q17")

cat("\n\n# Item Analysis: New")
# for(item_i in varname_e_scale[1:3]){
for(item_i in varname_n_scale){
  # item_i <- "Q17"
  item_label <- ds_labels %>% filter(item_name == item_i) %>% select(item_label) %>%
    as.list() %>% unlist() %>% as.character()
  item_description <- ds_labels %>% filter(item_name == item_i) %>% select(item_description) %>%
    as.list() %>% unlist() %>% as.character()

  cat("\n\n")
  cat("## ", item_i," - ", item_label)
  # labelled::var_label(ds[item_i])
  cat("\n\n")
  item_description %>% print()
  cat("\n\n")
  ds %>% describe_item(item_i) %>% print()
  cat("\n\n")
}

cat("\n\n# Item Analysis: Existing")
# for(item_i in varname_e_scale[1:3]){
for(item_i in varname_e_scale){
  # item_i <- "Q17"
  item_label <- ds_labels %>% filter(item_name == item_i) %>% select(item_label) %>%
    as.list() %>% unlist() %>% as.character()

  cat("\n\n")
  cat("## ", item_i," - ", item_label)
  labelled::var_label(ds[item_i])
  cat("\n\n")
  ds %>% describe_item(item_i) %>% print()
  cat("\n\n")
}





# ---- total-scores -------------

# dsn <- dsn %>%
#   dplyr::mutate(
#     total_score = rowSums(.[2:length(names(dsn))])
#   )

ds1 <- ds1 %>%
  dplyr::mutate(
    score_e = rowSums(.[varname_e_scale]),
    score_n = rowSums(.[varname_n_scale])
  )
# ds1 %>% glimpse()
# distribution of scale scores
ds1 %>% histogram_continuous("score_n",main_title = "Distribution of total Scores",
                             sub_title = "New Scale", x_title = "Total score (New Scale)")
ds1 %>% histogram_continuous("score_e",main_title = "Distribution of total Scores",
                             sub_title = "Existing Scale",x_title = "Total score (Existing Scale)")


ds1 <- ds1 %>%
  dplyr::mutate(
     Q4_4   = Q4_4 * -1
    ,Q4_7   = Q4_7 * -1
    ,Q4_11  = Q4_11 * -1
    ,Q4_9   = Q4_9 * -1
    ,Q4_15  = Q4_15 * -1
    ,Q20    = Q20 * -1
    ,Q21    = Q21 * -1
    ,Q22    = Q22 * -1
    ,Q23    = Q23 * -1
    ,Q24    = Q24 * -1
    ,Q25    = Q25 * -1
    ,Q28    = Q28 * -1
    ,Q29    = Q29 * -1
    ,Q30    = Q30 * -1
  )



# ---- make-cor ----------------
# select_variables <- dto$metaData %>%
#   dplyr::filter(select==TRUE) %>%
#   dplyr::select(name)
# (select_variables <- as.character(select_variables$name))
# # subset selected variables
# ds_small <- unitData %>%
#   dplyr::select_(.dots = select_variables)
# # rename selected variables
# d_rules <- metaData %>%
#   dplyr::filter(name %in% names(ds_small)) %>%
#   dplyr::select(name, name_new ) # leave only collumn, which values you wish to append
# names(ds_small) <- d_rules[,"name_new"]

ds2 <- ds1[complete.cases(ds1),]
# R <- cor( as.matrix(dsna %>% select(-id)) ) # correlation matrix R of variables in foc
# eigen <- eigen(R) # eigen decomposition of R      #  VDV' : $values -eigenvalues, $vectors
# svd <- svd(R)   # single value decomposition of R #  UDV' : $d      -eigenvalues, $u,$v
# cormat <- cor( as.matrix(dsna %>% select(-id)) )
# colnames(cormat) <- names(dsna %>% select(-id))


# ---- new-scale ----------
cormat <- make_corr_matrix(ds1, dto$metaData, varname_n_scale)
make_corr_plot(cormat, upper="pie")

ds_labels %>% select(item_name, item_description)
cormat[,21]

# ---- old-scale ----------
cormat <- make_corr_matrix(ds1, dto$metaData, varname_e_scale)
make_corr_plot(cormat, upper="pie")



# ----- publisher --------------------
path <- "./analysis/1-first-look/scale-analyis.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)
