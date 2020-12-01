# helpers.R

suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(rlang))

# Useful helper functions used throughout MADC database specialist work

# _ REDCap API URL ----
REDCAP_API_URI <- "https://redcap-p-a.umms.med.umich.edu/api/"


# _ Text that hinders JSON parsing
json_busters <- "\r\n|\r|\t|<br/>|<br />|<b>|</b>|<i>|</i>|<u>|</u>"


# _ REDCap API helper function
export_redcap_records <-
  function(
    uri = REDCAP_API_URI
    , token
    , fields = ""
    , forms = ""
    , records = NULL
    , rawdata = TRUE
    , rawhead = TRUE
    , vp = TRUE
    , vb = FALSE
    , ...
  ) {

    post_body <-
      list(
        token = token
        , content = "record"
        , format = "json"
        , type = "flat"
        , fields = fields
        , forms = forms
        , records = records
        , rawOrLabel = ifelse(rawdata, "raw", "label")
        , rawOrLabelHeaders = ifelse(rawhead, "raw", "label")
        , exportCheckboxLabel = "false"
        , exportSurveyFields = "false"
        , exportDataAccessGroups = "false"
        , returnFormat = "json"
        , ...
        # Examples for filterLogic arg to pass at function call:
        # , filterLogic = "([exam_date] >= '2017-03-01')"
        # , filterLogic = paste0("(",
        #                        "[ptid] >= 'UM00000001'",
        #                        " AND ",
        #                        "[ptid] <= 'UM00009999'",
        #                        " AND ",
        #                        "[form_date] >= '2017-03-15'",
        #                        ")")
      )

    httr::POST(
      url = uri
      , body = post_body
      , config = httr::config(ssl_verifypeer = vp, verbose = vb)
    ) %>%
      httr::content(as = "text") %>%
      str_replace_all(pattern = json_busters, replacement = " ")
  }

# _ REDCap API helper function
export_redcap_data_dictionary <-
  function(
    uri = REDCAP_API_URI
    , token
    , fields = ""
    , forms = ""
    # , records = NULL
    # , rawdata = TRUE
    # , rawhead = TRUE
    , vp = TRUE
    , vb = FALSE
    # , ...
  ) {

    post_body <-
      list(
        token = token
        , content = "metadata"
        , format = "json"
        , type = "flat"
        , fields = fields
        , forms = forms
        # , records                = records
        # , rawOrLabel             = ifelse(rawdata, "raw", "label")
        # , rawOrLabelHeaders      = ifelse(rawhead, "raw", "label")
        # , exportCheckboxLabel    = "false"
        # , exportSurveyFields     = "false"
        # , exportDataAccessGroups = "false"
        , returnFormat = "json"
        # , ...
        # Examples for filterLogic arg to pass at function call:
        # , filterLogic = "([exam_date] >= '2017-03-01')"
        # , filterLogic = paste0("(",
        #                        "[ptid] >= 'UM00000001'",
        #                        " AND ",
        #                        "[ptid] <= 'UM00009999'",
        #                        " AND ",
        #                        "[form_date] >= '2017-03-15'",
        #                        ")")
      )

    httr::POST(
      url = uri
      , body = post_body
      , config = httr::config(ssl_verifypeer = vp, verbose = vb)
    ) %>%
      httr::content(as = "text") %>%
      str_replace_all(pattern = json_busters, replacement = " ")
  }

# Helper function
## Collapse IVP / FVP / TVP fields based on IVP base name
## Uses rlang non-standard evalution
###
coalesce_ift_cols <- function(df) {

  # Get collapsible fields and the correpsonding
  # follow-up visit `fu_` and telephone visit `tele_` fields
  i_cols <- get_ift_dups(names(df)) # collapsible_fields
  f_cols <- paste0("fu_", i_cols)
  t_cols <- paste0("tele_", i_cols)

  # Iterate over collapsible fields `i_cols`,
  # coalescing `fu_` and/or `tele_` fields into `i_cols`
  for (i in seq_along(i_cols)) {
    # print(paste(i, i_cols[i], f_cols[i], t_cols[i]))
    # IVP, FVP (fu_), and TVP (tele_) columns are in df
    if (!is.null(df[[i_cols[i]]]) &
      !is.null(df[[f_cols[i]]]) &
      !is.null(df[[t_cols[i]]])) {

      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ f_cols[i] ]]))[[1]]
      df[[ f_cols[i] ]] <-
        promote_vectors(list(df[[ f_cols[i] ]], df[[ t_cols[i] ]]))[[1]]
      df[[ t_cols[i] ]] <-
        promote_vectors(list(df[[ t_cols[i] ]], df[[ i_cols[i] ]]))[[1]]
      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ f_cols[i] ]]))[[1]]
      df[[ f_cols[i] ]] <-
        promote_vectors(list(df[[ f_cols[i] ]], df[[ t_cols[i] ]]))[[1]]

      df <- df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[f_cols[i]]],
                                       df[[t_cols[i]]])) %>%
        select(-!!f_cols[[i]], -!!t_cols[[i]])
    }
      # IVP and FVP (fu_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
      !is.null(df[[f_cols[i]]]) &
      is.null(df[[t_cols[i]]])) {

      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ f_cols[i] ]]))[[1]]
      df[[ f_cols[i] ]] <-
        promote_vectors(list(df[[ f_cols[i] ]], df[[ i_cols[i] ]]))[[1]]
      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ f_cols[i] ]]))[[1]]

      df <- df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[f_cols[i]]])) %>%
        select(-!!f_cols[i])
    }
      # IVP and TVP (tele_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
      is.null(df[[f_cols[i]]]) &
      !is.null(df[[t_cols[i]]])) {

      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ t_cols[i] ]]))[[1]]
      df[[ t_cols[i] ]] <-
        promote_vectors(list(df[[ t_cols[i] ]], df[[ i_cols[i] ]]))[[1]]
      df[[ i_cols[i] ]] <-
        promote_vectors(list(df[[ i_cols[i] ]], df[[ t_cols[i] ]]))[[1]]

      df <- df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[t_cols[i]]])) %>%
        select(-!!t_cols[i])
    }
  }

  # Return df
  df
}


# Helper function
# Get UDS 3 field names that already exist in IVP form,
# but also exist in FVP and/or TVP forms (prefixed with "fu_" and/or "tele_").
# input:  c("dob", "sex", "fu_sex", "maristat", "fu_maristat", "tele_maristat")
# output: c("sex", "maristat")
get_ift_dups <- function(x) {
  unique(
    str_replace_all(
      string = x[str_detect(string = x, pattern = "^fu_|^tele_")],
      pattern = "^fu_|^tele_",
      replacement = ""
    )
  )
}


# Helper function
# Calculate age from DOB to Visit Date
# Example function call:
#   calculate_age(df_u3, birth_date, form_date)
calculate_age <- function(df, birth_date, age_date) {
  enquo_birth_date <- enquo(birth_date)
  enquo_age_date <- enquo(age_date)
  df %>%
    mutate(
      age_years = as.period(interval(!!enquo_birth_date, !!enquo_age_date),
                            unit = "years")$year,
      age_exact = time_length(
        interval(start = !!enquo_birth_date,
                 end = !!enquo_age_date),
        unit = "year"
      ),
      age_units = as.period(interval(start = !!enquo_birth_date,
                                     end = !!enquo_age_date),
                            unit = "years"))
}

## Helper function
# Propograte a value across rows where value is missing for a given
# participant ID
propagate_value <- function(df, id_field, date_field, value_field) {

  if (nrow(df) < 2) {
    stop("passed dataframe needs at least 2 rows")
  }

  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)
  # enquo_value_field <- enquo(value_field)

  quo_name_enquo_id_field <- quo_name(enquo_id_field)
  quo_name_enquo_value_field <- quo_name(enquo(value_field))

  df <- df %>%
    arrange(!!enquo_id_field, !!enquo_date_field)

  for (i in 2:nrow(df)) {
    prev_id <- df[[i - 1, quo_name_enquo_id_field]]
    curr_id <- df[[i, quo_name_enquo_id_field]]
    prev_value <- df[[i - 1, quo_name_enquo_value_field]]
    curr_value <- df[[i, quo_name_enquo_value_field]]

    if (prev_id == curr_id &&
      !is.na(prev_value) &&
      is.na(curr_value)) {
      df[[i, quo_name_enquo_value_field]] <-
        df[[i - 1, quo_name_enquo_value_field]]
    }
  }

  df <- df %>%
    arrange(!!enquo_id_field, desc(!!enquo_date_field))

  for (i in 2:nrow(df)) {
    prev_id <- df[[i - 1, quo_name_enquo_id_field]]
    curr_id <- df[[i, quo_name_enquo_id_field]]
    prev_value <- df[[i - 1, quo_name_enquo_value_field]]
    curr_value <- df[[i, quo_name_enquo_value_field]]

    if (prev_id == curr_id &&
      !is.na(prev_value) &&
      is.na(curr_value)) {
      df[[i, quo_name_enquo_value_field]] <-
        df[[i - 1, quo_name_enquo_value_field]]
    }
  }

  df <- df %>%
    arrange(!!enquo_id_field, !!enquo_date_field)

  df
}

# test_df <- tibble(
#   id = c("A001", "A001", "A001",
#          "A002", "A002", "A002",
#          "A003", "A003", "A003"),
#   date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
#                    "2019-02-01", "2020-02-01", "2021-02-01",
#                    "2019-03-01", "2020-03-01", "2021-03-01")),
#   value = c(NA_integer_, 1L, NA_integer_, 
#             NA_integer_, 2L, NA_integer_,
#             NA_integer_, 3L, NA_integer_)
# )
# test_df
# propogate_value(test_df, id, date, value)

## Helper function
# Add a `visit_num` field after `id_field` and `date_field`
calculate_visit_num <- function(df, id_field, date_field) {
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)

  df %>%
    arrange(!!enquo_id_field, !!enquo_date_field) %>%
    mutate(visit_unit = 1L) %>%
    group_by(!!enquo_id_field) %>%
    mutate(visit_num = case_when(
      !is.na(!!enquo_date_field) ~ as.integer(cumsum(visit_unit)),
      TRUE ~ NA_integer_
    )) %>%
    select(-visit_unit) %>%
    ungroup() %>%
    select(!!enquo_id_field, !!enquo_date_field, visit_num, everything())
}

# test_df <- tibble(
#   id = c("A001", "A001", "A001",
#          "A002", "A002", "A002",
#          "A003", "A003", "A003"),
#   date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
#                    "2019-02-01", NA_character_, "2021-02-01",
#                    "2019-03-01", "2020-03-01", "2021-03-01")),
#   value = c(NA_integer_, 1L, NA_integer_,
#             NA_integer_, 2L, NA_integer_,
#             NA_integer_, 3L, NA_integer_)
# )
# test_df
# calculate_visit_num(test_df, id, date)


## Helper function
# Get the nth visit from a longitudinal dataframe using ID and date fields
# n is the visit number
# Use n = -Inf for the earliest visit
# Use n = Inf for the latest visit
# Example function call:
#   get_visit_n(df_u3, ptid, form_date, 1)
get_visit_n <- function(df, id_field, date_field, n) {
  # Capture what user passed as `id_field` and `date_field`
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)

  # Handle n:
  #   if n is finite, capture the number passed by user
  #   if n is negative infinity, set expression to get earliest visit
  #   if n is positive infinity, set expression to get latest visit
  if (is.finite(n)) { vis_cnt_fltr <- enquo(n) }
  else if (n < 0) { vis_cnt_fltr <- expr(min(visit_count)) }
  else { vis_cnt_fltr <- expr(max(visit_count)) }

  df %>%
    filter(!is.na(!!enquo_id_field)) %>%
    filter(!is.na(!!enquo_date_field)) %>%
    arrange(!!enquo_id_field, !!enquo_date_field) %>%
    mutate(visit_unit = 1L) %>%
    group_by(!!enquo_id_field) %>%
    mutate(visit_count = cumsum(visit_unit)) %>%
    filter(visit_count == !!vis_cnt_fltr) %>%
    ungroup() %>%
    select(-visit_unit, -visit_count)
}


## Helper function
# adds "fu_" and "tele_" strings to a character vector
# add_fu_tele(c("foo", "bar")) => 
#   c("foo", "bar", "fu_foo", "fu_bar", "tele_foo", "tele_bar")
add_fu_tele <- function(x) {
  if (!is.character(x)) {
    stop("x must be a character vector", call. = FALSE)
  }

  c(x, paste0("fu_", x), paste0("tele_", x))
}


## Helper function
# Function that returns non-empty fields of a df
get_nonempty_fields <- function(df) {
  df %>%
    select_if(function(x) any(!is.na(x)))
}

## Helper function
# Function that returns a logical vector of which rows are non-empty
locate_nonempty_records <- function(df) {
  has_data_lgl <- logical(length = nrow(df))
  for (i in seq_len(nrow(df))) {
    row_list <- as.list(df[i,])
    if (any(!is.na(row_list))) {
      has_data_lgl[i] <- TRUE
    } else {
      has_data_lgl[i] <- FALSE
    }
  }
  has_data_lgl
}

## Helper function
# Function that cleans out rows where all relevant fields are empty
get_nonempty_records <- function(df, relevant_fields = names(df)) {
  nonempty_rows <- locate_nonempty_records(df[, relevant_fields])
  df[nonempty_rows,]
}

## Helper function
# Function that derives "MADC Consensus Diagnosis"
# df passed must have the following Form D1 fields:
#   - normcog
#   - demented
#   - mciamem, mciaplus, mcinon1, mcinon2
#   - impnomci
#   - amndem, pca, ppasyn, ftdsyn, lbdsyn, namndem
#   - alzdis, alzdisif
#   - psp, pspif 
#   - cort, cortif
#   - ftldmo, ftldmoif 
#   - ftldnos, ftldnoif
#   - cvd, cvdif
derive_consensus_dx <- function(df) {
  df %>%
    # create MADC diagnosis field
    rowwise() %>%
    mutate(madc_dx = case_when(
      sum(amndem, pca, ppasyn, ftdsyn, lbdsyn, namndem, na.rm = TRUE) > 1 ~
        "Mixed dementia",
      normcog == 1 ~ "NL",
      normcog == 0 &
        demented == 0 &
        (mciamem == 1 | mciaplus == 1) ~ "aMCI",
      normcog == 0 &
        demented == 0 &
        (mcinon1 == 1 | mcinon2 == 1) ~ "naMCI",
      normcog == 0 &
        demented == 0 &
        impnomci == 1 ~ "Impaired not MCI",
      normcog == 0 &
        demented == 1 &
        (amndem == 1 | namndem == 1) &
        alzdis == 1 &
        alzdisif == 1 ~ "AD",
      normcog == 0 & demented == 1 & lbdsyn == 1 ~ "LBD",
      normcog == 0 &
        demented == 1 &
        (ftdsyn == 1 |
          (psp == 1 & pspif == 1) |
          (cort == 1 & cortif == 1) |
          (ftldmo == 1 & ftldmoif == 1) |
          (ftldnos == 1 & ftldnoif == 1)) ~ "FTD",
      normcog == 0 &
        demented == 1 &
        cvd == 1 &
        cvdif == 1 ~ "Vascular dementia",
      normcog == 0 &
        demented == 1 &
        ppasyn == 1 &
        (is.na(psp) |
          is.na(cort) |
          is.na(ftldmo) |
          is.na(ftldnos)) ~ "PPA",
      demented == 1 ~ "Other dementia",
      TRUE ~ NA_character_
    ),
           fu_madc_dx = case_when(
             sum(fu_amndem, fu_pca, fu_ppasyn, fu_ftdsyn, fu_lbdsyn, fu_namndem, na.rm = TRUE) > 1 ~
               "Mixed dementia",
             fu_normcog == 1 ~ "NL",
             fu_normcog == 0 &
               fu_demented == 0 &
               (fu_mciamem == 1 | fu_mciaplus == 1) ~ "aMCI",
             fu_normcog == 0 &
               fu_demented == 0 &
               (fu_mcinon1 == 1 | fu_mcinon2 == 1) ~ "naMCI",
             fu_normcog == 0 &
               fu_demented == 0 &
               fu_impnomci == 1 ~ "Impaired not MCI",
             fu_normcog == 0 &
               fu_demented == 1 &
               (fu_amndem == 1 | fu_namndem == 1) &
               fu_alzdis == 1 &
               fu_alzdisif == 1 ~ "AD",
             fu_normcog == 0 &
               fu_demented == 1 &
               fu_lbdsyn == 1 ~ "LBD",
             fu_normcog == 0 &
               fu_demented == 1 &
               (fu_ftdsyn == 1 |
                 (fu_psp == 1 & fu_pspif == 1) |
                 (fu_cort == 1 & fu_cortif == 1) |
                 (fu_ftldmo == 1 & fu_ftldmoif == 1) |
                 (fu_ftldnos == 1 & fu_ftldnoif == 1)) ~ "FTD",
             fu_normcog == 0 &
               fu_demented == 1 &
               fu_cvd == 1 &
               fu_cvdif == 1 ~ "Vascular dementia",
             fu_normcog == 0 &
               fu_demented == 1 &
               fu_ppasyn == 1 &
               (is.na(fu_psp) |
                 is.na(fu_cort) |
                 is.na(fu_ftldmo) |
                 is.na(fu_ftldnos)) ~ "PPA",
             fu_demented == 1 ~ "Other dementia",
             TRUE ~ NA_character_
           )) %>%
    ungroup()
}

derive_consensus_dx_ivp <- function(df) {
  df %>%
    # create MADC diagnosis field
    rowwise() %>%
    mutate(madc_dx = case_when(
      sum(amndem, pca, ppasyn, ftdsyn, lbdsyn, namndem, na.rm = TRUE) > 1 ~
        "Mixed dementia",
      normcog == 1 ~ "NL",
      normcog == 0 &
        demented == 0 &
        (mciamem == 1 | mciaplus == 1) ~ "aMCI",
      normcog == 0 &
        demented == 0 &
        (mcinon1 == 1 | mcinon2 == 1) ~ "naMCI",
      normcog == 0 &
        demented == 0 &
        impnomci == 1 ~ "Impaired not MCI",
      normcog == 0 &
        demented == 1 &
        (amndem == 1 | namndem == 1) &
        alzdis == 1 &
        alzdisif == 1 ~ "AD",
      normcog == 0 & demented == 1 & lbdsyn == 1 ~ "LBD",
      normcog == 0 &
        demented == 1 &
        (ftdsyn == 1 |
          (psp == 1 & pspif == 1) |
          (cort == 1 & cortif == 1) |
          (ftldmo == 1 & ftldmoif == 1) |
          (ftldnos == 1 & ftldnoif == 1)) ~ "FTD",
      normcog == 0 &
        demented == 1 &
        cvd == 1 &
        cvdif == 1 ~ "Vascular dementia",
      normcog == 0 &
        demented == 1 &
        ppasyn == 1 &
        (is.na(psp) |
          is.na(cort) |
          is.na(ftldmo) |
          is.na(ftldnos)) ~ "PPA",
      demented == 1 ~ "Other dementia",
      TRUE ~ NA_character_
    )) %>%
    ungroup()
}

## Helper function
# Function that takes two vectors and ensures their types match.
# If one vector's class is "lower" than another, 
# it's promoted to the higher class.
# Class hierarchy:
#   1. factor
#   2. character
#   3. numeric
#   4. integer
#   5. logical (boolean)
promote_vectors <- function(vct_list) {

  if (!is.vector(vct_list[[1]])) stop("vct_list[[1]] is not a vector")
  if (!is.vector(vct_list[[2]])) stop("vct_list[[2]] is not a vector")

  class_v1 <- class(vct_list[[1]])
  class_v2 <- class(vct_list[[2]])

  if (identical(class_v1, class_v2)) return(vct_list)

  vct_hierarchy <- c("logical", "integer", "numeric", "character", "factor")
  max_class <- max(which(vct_hierarchy == class_v1),
                   which(vct_hierarchy == class_v2))
  highest_class <- vct_hierarchy[max_class]

  switch(highest_class,
         "logical" = {
           vct_list[[1]] <- as.logical(vct_list[[1]])
           vct_list[[2]] <- as.logical(vct_list[[2]])
         },
         "integer" = {
           vct_list[[1]] <- as.integer(vct_list[[1]])
           vct_list[[2]] <- as.integer(vct_list[[2]])
         },
         "numeric" = {
           vct_list[[1]] <- as.numeric(vct_list[[1]])
           vct_list[[2]] <- as.numeric(vct_list[[2]])
         },
         "character" = {
           vct_list[[1]] <- as.character(vct_list[[1]])
           vct_list[[2]] <- as.character(vct_list[[2]])
         },
         "factor" = {
           vct_list[[1]] <- as.factor(vct_list[[1]])
           vct_list[[2]] <- as.factor(vct_list[[2]])
         }
  )

  return(vct_list)
}


load_libraries <- function(libs) {
  purrr::walk(libs,
              function(x) {
                suppressMessages(library(x, character.only = TRUE))
              })
}


pad_with <- function(x, char, n, side = "left") {
  if (!is.vector(x))
    stop("x is not a vector")

  max_nchar <- max(nchar(x), na.rm = TRUE)
  if (max_nchar > n)
    stop("max nchar is greater than n")

  if (side == "left")
    paste0(strrep(char, n - nchar(x)), x) %>% na_if("NANA")
  else
    paste0(x, strrep(char, n - nchar(x))) %>% na_if("NANA")
}

###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
