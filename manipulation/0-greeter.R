rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio

# load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R")
# load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library("magrittr") #Pipes
library("ggplot2")  # graphs
requireNamespace("dplyr")

# ---- declare-globals ---------------------------------------------------------
path_input <- "data-unshared/raw/Pilot-Testing Mental Health and Well-Being Questions_December 11, 2019_07.11.sav"

# ---- load-data ---------------------------------------------------------------
# ds0 <- haven::read_sav(path_input, )
ds0 <- haven::read_sav(path_input)

# inspect-data -------------------------------------------------------------
ds0 %>% distinct(ResponseId) %>% count()
# ---- define-content --------------------------
column_names <- c(
#  "StartDate"             = "Start Date"
# ,"EndDate"               = "End Date"
# ,"Status"                = "Response Type"
# ,"IPAddress"             = "IP Address"
# ,"Progress"              = "Progress"
# ,"Duration__in_seconds_" = "Duration (in seconds)"
# ,"Finished"              = "Finished"
# ,"RecordedDate"          = "Recorded Date"
 "ResponseId"            = "Response ID"
# ,"RecipientLastName"     = "Recipient Last Name"
# ,"RecipientFirstName"    = "Recipient First Name"
# ,"RecipientEmail"        = "Recipient Email"
# ,"ExternalReference"     = "External Data Reference"
# ,"LocationLatitude"      = "Location Latitude"
# ,"LocationLongitude"     = "Location Longitude"
# ,"DistributionChannel"   = "Distribution Channel"
# ,"UserLanguage"          = "User Language"
,"Q4_1"                  = "Today I feel able to meet my responsibilities (e.g. work, school)."
,"Q4_2"                  = "Today I feel optimistic about the future."
,"Q4_3"                  = "Today I feel loved."
,"Q4_4"                  = "Today I feel annoyed/irritable."
,"Q4_5"                  = "Today I feel rested."
,"Q4_6"                  = "Today I feel happy."
,"Q4_7"                  = "Today I feel in physical pain."
,"Q4_8"                  = "Today I feel confident."
,"Q4_9"                  = "Today I feel worried/anxious."
,"Q4_10"                 = "Today I feel that my life has a purpose."
,"Q4_11"                 = "Today I feel sad."
,"Q4_12"                 = "Today I feel useful."
,"Q4_13"                 = "Today I feel energetic."
,"Q4_14"                 = "Today I feel supported."
,"Q4_15"                 = "Today I feel angry."
,"Q4_16"                 = "Today I feel in charge of my life."
,"Q6_1"                  = "Today I woke up feeling well-rested."
,"Q6_2"                  = "Today I ate well."
,"Q6_3"                  = "Today I took good care of myself."
,"Q6_4"                  = "Today I was helpful to others."
,"Q7_1"                  = "Today I ... Ate healthy foods"
,"Q7_2"                  = "Today I ... Got enough sleep"
,"Q7_3"                  = "Today I ... Engaged in a physical activity (e.g., walking, sports)"
,"Q7_4"                  = "Today I ... Spent time with people I like"
,"Q7_5"                  = "Today I ... Engaged in a hobby"
,"Q7_6"                  = "Today I ... Meditated or prayed"
,"Q7_7"                  = "Today I ... Participating in a spiritual community"
,"Q7_8"                  = "Today I ... Volunteered or contributed to a cause I believe in"
,"Q7_9"                  = "Today I ... Spent time in nature"
,"Q7_10"                 = "Today I ... Quiet time"
,"Q8"                    = "Comments to Q7"
,"Q9"                    = "At some point in my life, I was concerned about my use of alcohol, tobacco, or drugs."
,"Q11"                   = "With respect to using alcohol, tobacco, or drugs, I have met my goal today."
,"Q12_1"                 = "Helped goal ... Medication"
,"Q12_2"                 = "Helped goal ... Support group"
,"Q12_3"                 = "Helped goal ... Counselling"
,"Q12_4"                 = "Helped goal ... Family"
,"Q12_5"                 = "Helped goal ... Friends"
,"Q12_6"                 = "Helped goal ... Significant other"
,"Q12_7"                 = "Helped goal ... Religious/spiritual activity"
,"Q12_8"                 = "Helped goal ... Mindfulness/meditation"
,"Q12_9"                 = "Helped goal ... Avoiding negative patterns of thinking"
,"Q12_10"                = "Helped goal ... Doing things for others"
,"Q12_11"                = "Helped goal ... Setting healthy boundaries"
,"Q12_12"                = "Helped goal ... Doing things I enjoy"
,"Q12_13"                = "Helped goal ... No access to substance"
,"Q12_14"                = "Helped goal ... Other:"
,"Q12_14_TEXT"           = "Helped goal ... Free text"
,"Q13_1"                 = "Hindered goal ... Family"
,"Q13_2"                 = "Hindered goal ... Friends"
,"Q13_3"                 = "Hindered goal ... Significant other"
,"Q13_4"                 = "Hindered goal ... School"
,"Q13_5"                 = "Hindered goal ... Work"
,"Q13_6"                 = "Hindered goal ... Stress"
,"Q13_7"                 = "Hindered goal ... Depression"
,"Q13_8"                 = "Hindered goal ... Boredom"
,"Q13_9"                 = "Hindered goal ... Tiredness"
,"Q13_10"                = "Hindered goal ... Social pressure"
,"Q13_11"                = "Hindered goal ... Availability of the substance"
,"Q13_12"                = "Hindered goal ... Unstable Housing"
,"Q13_13"                = "Hindered goal ... Other"
,"Q13_13_TEXT"           = "Hindered goal ... Free text"
,"Q14"                   = "Comments to Q12"
,"Q15"                   = "Comments to Q13"
,"Q16"                   = "My urge/craving to use tobacco, alcohol, or drugs today has been:"
,"Q17"                   = "I've been feeling relaxed."
,"Q18"                   = "I've been feeling cheerful."
,"Q19"                   = "I've been thinking clearly."
,"Q20"                   = "How worried are you today?"
,"Q21"                   = "How sad are you today?"
,"Q22"                   = "How much pain are you in today?"
,"Q23"                   = "How did you sleep last night?"
,"Q24"                   = "How annoyed are you today?"
,"Q25"                   = "How tired are you today?"
,"Q26"                   = "I've had energy to spare..."
,"Q27"                   = "I've been dealing with problems well..."
,"Q28"                   = "How are you doing with your work today?"
,"Q29"                   = "How are you doing with your daily routine today?"
,"Q30"                   = "Are you able to join in with activities today?"
,"Q31"                   = "I've been feeling close to other people."
,"Q33"                   = "I've been feeling loved."
# ,"Q34"                   = "I've been feeling loved."
,"Q35"                   = "I've been interested in other people."
,"Q36"                   = "I've been feeling useful."
,"Q37"                   = "I've been feeling good about myself."
,"Q38"                   = "I've been feeling confident."
,"Q39"                   = "I've been feeling optimistic about the future."
,"Q40"                   = "My urge/craving to use alcohol, tobacco, or drugs has been..."
,"Q41"                   = "What is your gender?"
,"Q42"                   = "What is your race?"
,"Q43"                   = "What is your ethnicity?"
,"Q44"                   = "What is your class standing?"
# ,"Q14___Parent_Topics"   = "Q14 - Parent Topics"
# ,"Q14___Topics"          = "Q14 - Topics"
)

ds0 <- ds0 %>% select_(.dots = names(column_names))
# ds0 %>% distinct(Q40)
# -----tweak-data ---------------------------------------------------------------
ds1 <- ds0 %>%
  dplyr::rename(
    id               = ResponseId
    ,gender          = Q41
    ,race            = Q42
    ,ethnicity       = Q43
    ,class_standing  = Q44
  )

ds1 %>% glimpse()

# ---- ---------------

#
# for(i in names(ds1)){
#   # ds1 %>% distinct_(.dots = c(i) ) %>% print()
#   labelled::var_label(ds1[,i]) %>% print()
#   labelled::val_labels(ds1[,i]) %>% print()
# }


# d %>% explore::explore()
# ---- convert-to-factors -----------------------

convert_to_factors <- function(d,varname){
  # d <- ds1 %>% dplyr::select(id, Q4_1)
  # varname <- "Q4_1"
  (factor_labels <- labelled::val_labels(d[,varname]))
  (variable_label <- labelled::var_label(d[,varname])[[1]])

  d <- d %>% dplyr::rename(temp = varname )
  d <- d %>% dplyr::mutate(
    temp = factor(temp, levels = factor_labels[[1]], labels = names(factor_labels[[1]])),
    temp = forcats::fct_explicit_na(temp)
  )
  d <- d %>% plyr::rename( c("temp" = varname))
  labelled::var_label(d[,varname]) <- variable_label
  return(d)
}

# d1 <- ds1 %>% select(id,gender, race, ethnicity)

ds2 <- ds1
for(i in setdiff(names(ds1), c("id", "Q8", "Q13_13_TEXT", "Q14","Q15","Q14___Parent_Topics", "Q14___Topics")) ){
  ds2 <- ds2 %>% convert_to_factors(i)
}

ds2 <- ds2 %>%
  dplyr::mutate(
    race_ethnicity  = paste0( as.character(levels(race)), " - ", as.character(ethnicity))
  )

# ds2 %>% glimpse()

# ds1 %>% group_by(gender) %>% summarize(n = n())
# ds2 %>% group_by(gender) %>% summarize(n = n())

# ds2 %>% explore::explore()



# ---- create-meta --------------------
ds_labels <- ds0 %>% names_labels()

ds_labels <- ds_labels %>%
  dplyr::mutate(
    label_raw = label,
    label = gsub("^Please indicate how strongly you agree or disagree with the following statements. - "
                 ,"",label),

    label = gsub("^I did the following things for myself today \\(please choose all that apply\\): "
                 ,"Today I ... ",label),
    label = gsub("^The following activities helped me to meet my goal \\(please choose all that apply\\): - Selected Choice "
                 ,"Helped goal ... ",label),
    label = gsub("^The following activities contributed to not meeting my goal \\(please choose all that apply\\): - Selected Choice "
                 ,"Hindered goal ... ",label),
    label = gsub("^To what extent do you agree with the following statement: \\\n"
                 ,"",label),
    label = gsub("^The following activities helped me to meet my goal \\(please choose all that apply\\): - Other: - Text"
                 ,"Helped goal ... Free text",label),
    label = gsub("^The following activities contributed to not meeting my goal \\(please choose all that apply\\): - Other - Text"
                 ,"Hindered goal ... Free text",label),
    label = gsub('^Do you have any comments about the question above \\("I did the following things for myself today"\\); was there anything unclear or confusing, or any choices missing that should be included\\?'
                 ,"Comments to Q7",label),
    label = gsub('^Do you have any comments about the prior question \\("The following activities helped me to meet my goal"\\); is anything unclear or confusing, or are any answer choices missing that should be included\\?'
                 ,"Comments to Q12",label),
    label = gsub('^Do you have any comments about the prior question \\("The following activities contributed to not meeting my goal"\\); is anything unclear or confusing, or are any answer choices missing that should be included\\?'
                 ,"Comments to Q13",label)

  )

# quick inspection
# ds_labels[81, 2]
ds_labels %>% readr::write_csv("./data-unshared/derived/labels.csv")

# ---- load-meta -------------------
# after adding some manual changes to the metadata outside of this script:
# ds_meta <- readxl::read_xlsx("./data-unshared/raw/meta.xlsx")
ds_meta <- readr::read_csv("./data-unshared/raw/meta.csv")


# ----- reverse-coding --------------
# the following items are contributing to the negative/adverse state we are measuring

# ds0 %>% select(ResponseId, Q4_4) %>% head()
# ds0 %>% select(ResponseId, Q4_4) %>% glimpse()
# ds0 %>% distinct(Q4_4)
#
# ds1 <- ds0 %>%
#   dplyr::mutate(
#     Q4_4r = ifelse(Q4_4 == 1, 5,
#                    ifelse(Q4_4 == 2, 4,
#                           ifelse(Q4_4 == 4, 2,
#                                  ifelse(Q4_4 == 5, 1, Q4_4))))
#   )
# ds1 %>% select(ResponseId, Q4_4r) %>% head()
# ds1 %>% select(ResponseId, Q4_4r) %>% glimpse()
# ds1 %>% distinct(Q4_4r)
#
#
# ds0 %>% select(ResponseId, Q4_4) %>% head()
# ds0 %>% select(ResponseId, Q4_4) %>% glimpse()
# ds0 %>% distinct(Q4_4)

# Q4_4
# Q4_7
# Q4_11
# Q4_9
# Q4_15
#
# Q20
# Q21
# Q22
# Q23
# Q24
# Q25
# Q28
# Q29
# Q30




# --- rename-variables ------------------------------

# ds1 <- ds0
# names_old <- names(ds0) %>% tibble::enframe(value = "name_old")
# var_rename <- ds_meta %>% dplyr::select(item_name, item_label, section) %>%
#   dplyr::left_join(
#     names_old, by = c("item_name" = "name_old")
#   )

#
# for(i in unique(colnames(ds1))){
#   d_rules <- metaData %>%
#     dplyr::filter(name_new %in% names(ds1)) %>%
#     dplyr::select(name_new, label_short )
#   attr(ds_small[,i], "label") <-  as.character(d_rules[d_rules$name_new==i,"label_short"])
# }



# # basic-table --------------------------------------------------------------
#
# ds0 %>% dplyr::distinct(Q14___Parent_Topics)
#
# d1 <- ds0 %>%
#   dplyr::filter(Q14___Topics == "Unknown") %>%
#   dplyr::select(Q14___Topics, Q14)
# # basic-graph --------------------------------------------------------------
#
# ds0 %>%
#   dplyr::select(Q4_2, Q39) %>%
#   explore::describe_all()

# save-to-disk -------------------------------------------------------------
names(ds0)
dto <- list(
   "raw0"     = ds0
  ,"microData" = ds2
  ,"metaData" = ds_meta
)
dto %>% saveRDS("./data-unshared/derived/dto.rds")
