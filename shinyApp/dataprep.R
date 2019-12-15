library(magrittr)
library(datasets)
library(ggplot2) # load ggplot
library(psych)
library(plotrix)
library(sem)
library(GPArotation)
library(dplyr)


# ---- declare-globals ---------------------------------------------------------
# sample_size <- 643
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

varname_warwick_scale <- c(
  "Q17"
  ,"Q18"
  ,"Q19"
  # ,"Q20"
  # ,"Q21"
  # ,"Q22"
  # ,"Q23"
  # ,"Q24"
  # ,"Q25"
  ,"Q26"
  ,"Q27"
  # ,"Q28"
  # ,"Q29"
  # ,"Q30"
  ,"Q31"
  ,"Q33"
  ,"Q35"
  ,"Q36"
  ,"Q37"
  ,"Q38"
  ,"Q39"
)

varname_chu_scale <- c(
  # "Q17"
  # ,"Q18"
  # ,"Q19"
   "Q20"
  ,"Q21"
  ,"Q22"
  ,"Q23"
  ,"Q24"
  ,"Q25"
  # ,"Q26"
  # ,"Q27"
  ,"Q28"
  ,"Q29"
  ,"Q30"
  # ,"Q31"
  # ,"Q33"
  # ,"Q35"
  # ,"Q36"
  # ,"Q37"
  # ,"Q38"
  # ,"Q39"
)
varname_e_scale <- c(varname_chu_scale, varname_warwick_scale)

make_corr_matrix <- function(d,metaData,item_names){
  # d <- dsn
  # metaData <- dto$metaData
  # item_names <- varname_n_scale
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select_(.dots=item_names)
  d2 <- d1[complete.cases(d1),]
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = paste0(item_name,"\n",item_label))

  rownames <- rownames[,"display_name"]
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
metaData <- dto[["metaData"]]
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

# ---- reverse-code -------------
ds1 <- ds1[complete.cases(ds1),]

sample_size <- nrow(ds1)
n_obs <- sample_size
ds_reversed <- ds1 %>%
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

ds_original <- ds1


# ---- data-phase-0 ------------------
items_phase_0 <- varname_n_scale
R0 <- make_corr_matrix(ds_reversed, metaData, items_phase_0)
# saveRDS(R0,"./data/shared/derived/R0.rds")
# Phase_0 <- ds
items_0 <- R0
n.items_0 <- sample_size
p.items_0 <- nrow(R0)
# Scree.Plot(R0)
# FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:6,RMSEA.cutoff = .08)
# ---- data-phase-1 ------------------
items_phase_1 <- varname_n_scale
# items = items_phase_1
R1 <- make_corr_matrix(ds_original, metaData, items_phase_1)
# Phase_1 <- ds
items_1 <- R1
n.items_1 <- sample_size
p.items_1 <- nrow(R1)

# ---- data-phase-2 ------------------

items_phase_2 <- varname_e_scale
# items = items_phase_2
R2 <- make_corr_matrix(ds_reversed, metaData, items_phase_2)
# Phase_2 <- ds
items_2 <- R2
n.items_2 <- sample_size
p.items_2 <- nrow(R2)

# ---- data-phase-3 ------------------

items_phase_3 <- varname_e_scale
# items = items_phase_3
R3 <- make_corr_matrix(ds_original, metaData, items_phase_3)
# Phase_2 <- ds
items_3 <- R3
n.items_3 <- sample_size
p.items_3 <- nrow(R3)



# rm(list=setdiff(ls(), c("cognitive", "emotional", "physical",
#                        "vars.cognitive", "vars.emotional","vars.physical",
#                        "n.cognitive", "n.emotional", "n.physical",
#                        "p.cognitive", "p.emotional", "p.physical")))


