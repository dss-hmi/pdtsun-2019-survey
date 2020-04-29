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
library(psych)
library(plotrix)
library(sem)
library(GPArotation)
library(corrplot)
library(corrgram)

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/SteigerRLibraryFunctions.txt")
source("./scripts/AdvancedFactorFunctions_CF.R")
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes
source("./scripts/fa-utility-functions.R") # to graph factor patterns
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


make_corr_matrix <- function(d,metaData,item_names, add_short_label = T){
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
    dplyr::mutate(display_name = paste0(item_name))
  if(add_short_label){
    rownames <- metaData %>%
      dplyr::filter(item_name %in% item_names) %>%
      # dplyr::mutate(display_name = paste0(item_name,"\n",item_label))
      dplyr::mutate(display_name = paste0(item_name,"_",item_label))
  }
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

# keep only complete cases
ds1 <- ds1[complete.cases(ds1),]

ds_labels <- dto$metaData %>%
  dplyr::filter(item_name %in% c(varname_n_scale, varname_e_scale)) %>%
  dplyr::mutate(display_name = paste0(item_name,"\n",item_label))


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

ds1 <- ds1 %>%
  dplyr::mutate(
    score_e = rowSums(.[varname_e_scale]),
    score_chu = rowSums(.[varname_chu_scale]),
    score_warwick = rowSums(.[varname_warwick_scale]),
    score_n = rowSums(.[varname_n_scale])
  )

# R <- cor( as.matrix(dsna %>% select(-id)) ) # correlation matrix R of variables in foc
# eigen <- eigen(R) # eigen decomposition of R      #  VDV' : $values -eigenvalues, $vectors
# svd <- svd(R)   # single value decomposition of R #  UDV' : $d      -eigenvalues, $u,$v
# cormat <- cor( as.matrix(dsna %>% select(-id)) )
# colnames(cormat) <- names(dsna %>% select(-id))
n_obs <- nrow(ds1)
sample_size <- n_obs


item_names <- c(varname_n_scale,varname_e_scale, c("score_n","score_chu","score_warwick","score_e"))
ds_matrix <- ds1 %>% dplyr::select_(.dots=item_names)
ds_matrix %>% glimpse()

rownames <- dto$metaData %>%
  dplyr::filter(item_name %in% item_names) %>%
  dplyr::mutate(display_name1 = paste0(item_name),
                display_name2 = paste0(item_name,"\n",item_label)#,
                # display_name3 = paste0(item_name,"_",item_label)
  )

rownames <- rownames[,"display_name2"]
rownames <- c(rownames %>% as.list() %>% unlist() %>% as.character(),  c("score_n","score_chu","score_warwick","score_e"))

cormat <- cor(ds_matrix)
colnames(cormat) <- rownames; rownames(cormat) <- rownames



# make_corr_plot(R0, upper="pie")

# ---- diagnose-0a ------------------------------------------------------
R0 <- make_corr_matrix(ds1, dto$metaData, varname_n_scale)
# Diagnosing number of factors
Scree.Plot(R0)
#The first 15 eigen values
data.frame(
  eigen = c(1:nrow(R0)),
  value = eigen(R0)$values
) %>%
  dplyr::filter(eigen < 16) %>%
  print()
# ---- diagnose-0b -------------------------
# MAP
psych::nfactors(R0,n.obs = n_obs)
# ---- diagnose-0c -------------------------
pa_results <- psych::fa.parallel(R0,n_obs,fm = "ml",fa="fa")
ds_pa <- data.frame(
  observed_eigens = pa_results$fa.values,
  simulated_eigens = pa_results$fa.sim
) %>% head(15) %>% print()
# ---- diagnose-0d ------------------------------------------------------
ls_solution <- solve_factors(R0,min=1,max=6,sample_size = n_obs)
ds_index <- get_indices(ls_solution)
ds_index %>% print()
# ---- diagnose-0e -------------------------
FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:6,RMSEA.cutoff = .08)

FA.Stats(Correlation.Matrix = R0,n.obs = n_obs,n.factors = 1:6,RMSEA.cutoff = .05)
# ---- estimate-0 ---------------------------------
fit_efa_0 <- MLFA(
  Correlation.Matrix = R0,
  n.factors = 3,
  n.obs = n_obs,
  sort = FALSE
)
#Loadings from the EFA solution\n")
f_pattern <- fit_efa_0[['Bifactor']]$F # fit_efa_0$
f_pattern %>% plot_factor_pattern(factor_width = 3)
# Loadings above threashold (.3) are masked to see the simpler structure
cat("\nLoadings above threashold (.3) are masked to see the simpler structure\n")
f_pattern[f_pattern<.30] <- NA
f_pattern %>% plot_factor_pattern(factor_width = 3)

# ----- confirm-0 ------------------------
# These values are translated into CFA model and used as starting values
model_0 <- FAtoSEM(
  x                 = fit_efa_0[["Bifactor"]] ,
  cutoff            = 0.30,
  factor.names      = c("General","Selfcare","Distress"),
  make.start.values = TRUE,
  cov.matrix        = FALSE, # TRUE - oblique, FALSE - orthogonal
  num.digits        = 4
)
# the model is estimated using sem package
fit_0 <- sem::sem(model_0,R0,sample_size)
# the pattern of the solution
m <- GetPattern(fit_0)$F
m[m==0] <- NA
m %>% plot_factor_pattern(factor_width=3)
# Summary of the fitted model
sem_model_summary(fit_0)
#Relative contribudion of items
sort(summary(fit_0)$Rsq) %>% dot_plot()


# ---- write-up-0 -----------------------
d <- ds1 %>% select(varname_n_scale)
alpha(d)
solution <- ls_solution

# ---- print-solution --------------------
display_solution <- function(R,k, sample_size,rotation_,mainTitle=NULL){
  A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
  L <- A$loadings
  if(rotation_=="oblimin"  ){rotation_string <- "(L, Tmat=diag(ncol(L)), gam=0,               normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="quartimin"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="targetT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="targetQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),         Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="pstT"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="pstQ"     ){rotation_string <- "(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="oblimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="entropy"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="quartimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="Varimax"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="simplimax"){rotation_string <- "(L, Tmat=diag(ncol(L)),           k=nrow(L), normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bentlerT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bentlerQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="tandemI"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="tandemII" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="geominT"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="geominQ"  ){rotation_string <- "(L, Tmat=diag(ncol(L)),           delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="cfT"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="cfQ"      ){rotation_string <- "(L, Tmat=diag(ncol(L)),             kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="infomaxT" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="infomaxQ" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="mccammon" ){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bifactorT"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}
  if(rotation_=="bifactorQ"){rotation_string <- "(L, Tmat=diag(ncol(L)),                      normalize=FALSE, eps=1e-5, maxit=1000)"}

  rotated_solution <- eval(parse(text=paste0(rotation_,rotation_string)))
  p <- nrow(R)

  FPM <- rotated_solution$loadings # FPM - Factor Pattern Matrix
  FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
  colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
  FPM  # THE OUTPUT
  Phi <- rotated_solution$Phi # factor correlation matrix
  if( is.null(Phi)) {Phi <- diag(k)} else{Phi}
  colnames(Phi) <- paste0("F", 1:k)
  rownames(Phi) <- paste0("F", 1:k)
  Phi
  solution <- list("FPM"=FPM,"Phi"=Phi)
  # load the function to gread the graph, needs k value
  source("./scripts/factor-pattern-plot.R") # to graph factor patterns
  g <- fpmFunction(FPM.matrix=solution$FPM, mainTitle=mainTitle) #Call/execute the function defined above.
  # print(g) #Print graph with factor pattern
  # file_name <- paste0("./data/shared/derived/FPM/",rotation_,"_",k,".csv")
  #browser()
  # save_file <- as.data.frame(FPM[,1:k])
  # readr::write_csv(save_file,file_name)

  return(g)
}


phase <- "0"
R <- R0 # correlation matrix for items at phase 0
sample_size <- n_obs
k <- 3
nfactors_ <- k
# rotation_ <- "Varimax"

# rotation_ <- "geominT"
# for(rotation_ in c("oblimin","geominQ")){   # },"quartimin","geominQ","bifactorQ")){
for(rotation_ in c("Varimax","geominT","bifactorT","quartimax","oblimin","geominQ","bifactorQ")){

  cat("\n\n")
  cat(paste0("## ",rotation_))
  # for(nfactors_ in c(4:10)){
    # for(nfactors_ in c(4:10)){
    mainTitle <- paste0(rotation_,", New scale ")
    cat("\n\n")
    # cat(paste0("### ",nfactors_));
    # cat("\n\n")
    solution <- display_solution(R,k=nfactors_,sample_size,rotation_,mainTitle=mainTitle) %>%
      print()
    cat("\n\n")


  # }
}

# ---- more-cor-1 ----------------------------

corrplotCustom <- function (corr, lower="number", upper="circle", tl.pos=c("d",
                                                                           "lt", "n"), diag=c("n", "l", "u"), bg="white", addgrid.col="gray", ...)  {

  diag <- match.arg(diag)
  tl.pos <- match.arg(tl.pos)
  n <- nrow(corr)
  corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
  corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
  if (diag == "n" & tl.pos != "d") {
    symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
  }
}


ds1 <- ds1 %>%
  dplyr::rename(
    new = score_n,
    chu = score_chu,
    warwick = score_warwick,
    old = score_e
  )

# item_names <- c(varname_n_scale,varname_e_scale, c("score_n","score_chu","score_warwick","score_e"))
item_names <- c(varname_n_scale, c("new","chu","warwick","old"))
ds_matrix <- ds1 %>% dplyr::select_(.dots=item_names)
# ds_matrix %>% glimpse(80)

rownames <- dto$metaData %>%
  dplyr::filter(item_name %in% item_names) %>%
  dplyr::mutate(display_name1 = paste0(item_name),
                display_name2 = paste0(item_name,"\n",item_label),
                display_name3 = paste0(item_name,"_",item_label)
  )

rownames <- rownames[,"display_name2"]
rownames <- c(rownames %>% as.list() %>% unlist() %>% as.character(),  c("new","chu","warwick","old"))

cormat <- cor(ds_matrix)
colnames(cormat) <- rownames; rownames(cormat) <- rownames

cormat %>% corrplotCustom(
  # order="AOE",
  lower="pie", upper="number",
                          title="Correlation Among Observed Variables", line=-1,
                          tl.col="black", addCoef.col="black", cl.cex=1.7)


# ----- publisher --------------------
path <- "./analysis/2-efa/2-efa.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)
