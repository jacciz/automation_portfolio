# 
# fields_to_summarise use these variables: (i.e. c("age_group_10yr","R_RACE","R_SEX"))


# Create factor levels so they get displayed in this order
age_group_10yr_factor_level <- c("<18", "18-20","21-29","30-39","40-49","50-59","60-69", "70+") 
race_factor_level <- c('American Indian or Alaska Native', 'Asian or Pacific Islander', 'Black or African American', 'Hispanic', 'White', 'Other/Unknown') # Took out 'Native Hawaiian or other Pacific Islander'
percents_factor_levels <- c("percent_of_drivers", "percent_of_vmt", "percent_of_crashes", "percent_of_arrests")
ratios_factor_levels <- c("crash_drivers_ratio", "crash_vmt_ratio", "arrest_driver_ratio", "arrest_vmt_ratio")


# Custom ggplot2 theme
theme_bar_charts <- function(base_size = 12) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = base_size, family = "Calibri"),
      legend.position = "none",
      strip.background=element_rect(fill="#CCD2D4", colour="transparent"),
      strip.text  =element_markdown(size = base_size, hjust = 0, margin=margin(0,0,2,2)),
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.title = element_markdown(lineheight = 1.1, size = base_size + 2, family = "Calibri", hjust = 0, margin=margin(0,0,5,0)), # for title colors
      plot.caption = element_text(hjust = 0, face= "italic", family = "Calibri")
      # legend.text = element_markdown(size = 11)
    )
}

# -------------- Get age groups -----------------------------

get_age_groups_5yr <- function(dataframe, age_column) {
  dataframe %>% mutate(age_group_5yr = cut(
    age_column, right = FALSE,
    c(0,4,9,13,14,15,20,25,29,34,39,44,49,54,59,64,69,74,79,84,88,120),
    labels = c("0-4","5-9","10-13","14", "15", "16-20","21-25","26-29","30-34","35-39","40-44", "45-49","50-54","55-59","60-64","65-69","70-74", "75-79", "80-84", "85-88", "89+"),
    include.lowest = T
  )) # age_group_5yr = ifelse(is.na(age_group_5yr), "Unknown", levels(age_group_5yr))
}

get_age_groups_10yr <- function(dataframe, age_column) {
  dataframe <- dataframe %>% mutate(age_group_10yr = cut(
    age_column,
    right = FALSE,
    c(0, 16, 26, 36, 46, 56, 66, 76, 120),
    labels = c(
      "<16",
      "16-25",
      "26-35",
      "36-45",
      "46-55",
      "56-65",
      "66-75",
      "76+"
    ),
    include.lowest = T
  ))
  # OLD WAY
  # dataframe <- dataframe %>% mutate(age_group_10yr = cut(
  #   age_column,
  #   right = FALSE,
  #   c(0, 17, 20, 29, 39, 49, 59, 69, 120),
  #   labels = c(
  #     "<18",
  #     "18-20",
  #     "21-29",
  #     "30-39",
  #     "40-49",
  #     "50-59",
  #     "60-69",
  #     "70+"
  #   ),
  #   include.lowest = T
  # ))
  # Get levels of age_group factor and add Unknown
  levels <- levels(dataframe$age_group_10yr)
  levels[length(levels) + 1] <- "Unknown"
  # refactor Species to include "None" as a factor level
  # and replace NA with "None"
  dataframe$age_group_10yr <-
    factor(dataframe$age_group_10yr, levels = levels)
  dataframe$age_group_10yr[is.na(dataframe$age_group_10yr)] <- "Unknown"
  return(dataframe)
}

# -------------- Crash Data - summarize crash count -----------------------------
# Returns average crash count based on fields_to_summarise
person_summarise_crash_count <-
  function(crash_person_df, fields_to_summarise) {
    # number_of_years is to get the average crash counts
    # number_of_years = year(person_df$CRSHDATE) %>% unique() %>% length()
    number_of_years = crash_person_df[unique(year(CRSHDATE)), .N]
    crash_person_df[, .(crash_count = .N/number_of_years), by = fields_to_summarise]
  }
# -------------- Arrest Data - summarize arrest count -----------------------------
owi_summarise_arrest_count <- 
  function(arrest_df, fields_to_summarise){
    number_of_years = arrest_df[unique(year(violation_date)), .N]
    arrest_df[, .(total_arrests = .N/number_of_years), by = intersect(fields_to_summarise, names(arrest_df))]
  }
# -------------- Census Data - summarize pop count -----------------------------
census_summarise_pop_count <- 
  function(census_df, fields_to_summarise){
    # number_of_years = arrest_df[unique(year(violation_date)), .N]
    census_df[, .(total_population = sum(POP)), by = intersect(fields_to_summarise, names(census_df))]
  }

# -------------- NHTS Data - summarize pmd -----------------------------

# This is the unweighted data for 'Average Annual Vehicle Miles of Travel Per Driver' - weight is WTPERFIN - total number of persons
nhts_summarise_pmd <- function(df, fields_to_summarise){
  df[HHSTATE %in% c("MI","IA","IL","MN","WI") & DRIVER == "01" & YEARMILE >= 0,c(.(PMD=mean(YEARMILE)), nhts_n = .N), by = intersect(fields_to_summarise, names(df))]
}
# -------------- NHTH Data - summarize VT -----------------------------
# 'Vehicle Trips (Travel Day VT, annualized, weighted)'
nhts_summarise_vt <- function(df, fields_to_summarise){
  df[HHSTATE == "WI" & DRVR_FLG == "01" & TRPTRANS %in% c("03", "04", "05", "06", "08", "09", "18"),c(.(v_trips=sum(WTTRDFIN)), nhts_n = .N), by = intersect(fields_to_summarise, names(df))]
}

# -------------- NHTS Data - summarize drivers -----------------------------
nhts_summarise_total_drivers <- function(df, fields_to_summarise){
  df[HHSTATE %in% "WI" & DRIVER == "01", .(total_drivers=sum(WTPERFIN)), by = intersect(fields_to_summarise, names(df))]
}

# -------------- DMV Data - summarize drivers -----------------------------
dmv_driver_summarise_count <- function(df, fields_to_summarise){
  df[, .(total_drivers=sum(n)), by = intersect(fields_to_summarise, names(df))]
}
# -------------- Find if Passenger in a crash -----------------------------
# Returns passenger_flag column which finds if a passenger was in that unit
find_if_passenger <- function(person_df) {
  # First get all the passengers
  passenger <-
    person_df %>% filter(ROLE == "Passenger") %>% select(CRSHNMBR, UNITNMBR)
  # Then join back to df Drivers to add flag to matching crsh/unit nmbrs
  pass_join <- person_df %>% filter(ROLE == "Driver") %>% semi_join(., passenger, by = c("CRSHNMBR","UNITNMBR")) %>% mutate(passenger_flag = "Y") %>% select(CRSHNMBR, UNITNMBR, passenger_flag)
  # join back to original
  left_join(person_df, pass_join, by = c("CRSHNMBR","UNITNMBR")) %>% mutate(passenger_flag = replace_na(passenger_flag, "N"))
}

# -------------- Census Relabel Race, Hispanic, Sex -----------------------------
relabel_census <- function(raw_census) {
  raw_census %>% setnames(., c("AGE", "RACE", "SEX", "HISP"),
                          c("R_AGE", "R_RACE", "R_SEX", "R_HISP"))
  raw_census %>% as.data.table() %>% mutate(
    R_SEX = dplyr::recode(
      R_SEX,
      "2" = "Female",
      "1" = "Male",
      "0" = "Unknown"
    ),
    R_RACE = dplyr::recode(
      R_RACE,
      # combo
      "6" = "Other/Unknown",
      # This is 2 or more races
      # "5" = "Other/Unknown",
      "5" = "Asian or Pacific Islander",
      "4" = "Asian or Pacific Islander",
      "3" = "American Indian or Alaska Native",
      "2" = "Black or African American",
      "1" = "White",
      "0" = "Other/Unknown"
    )
  ) %>%
    mutate(R_RACE = ifelse(R_HISP %in% c("2"), "Hispanic", R_RACE),
           R_AGE = as.integer(R_AGE))
  # "2" = "Hispanic",
  # "1" = "Non Hispanic",
  # "0" = "Both Hispanic origins"
}

# -------------- Crash Relabel Race, Hispanic, Sex -----------------------------
relabel_crash_persons <- function(person_df){
  person_df <-
    person_df %>% mutate(
      RACE = dplyr::recode(
        RACE,
        "A" = "Asian or Pacific Islander",
        "B" = "Black or African American",
        "H" = "Hispanic",
        "I" = "American Indian or Alaska Native",
        "W" = "White",
        .missing = "Other/Unknown",
        .default = "Other/Unknown" # for empty values
      ),
      SEX = dplyr::recode(
        SEX,
        "F" = "Female",
        "M" = "Male",
        "U" = "Unknown",
        .missing = "Unknown",
      )
    ) %>% get_age_groups_10yr(., age_column = person_df$AGE) 
  
  # Rename columns so to match with NHTS data
  person_df %>% setnames(., c("RACE", "SEX", "AGE"),
                         c("R_RACE", "R_SEX", "R_AGE"))
}

# -------------- Arrest Relabel Race, Hispanic, Sex -----------------------------
adjustCentury <- function(d, threshold=1930){
  y <- year(d) %% 100
  if(y > threshold %% 100) year(d) <- 1900 + y
  d
}
x %>% mutate(dob =adjustCentury(dob),
# x %>% mutate(dob = ifelse(year(dob) %% 100 > threshold %% 100,  1900 + year(dob) %% 100, dob) )
             age = time_length(interval(as.Date(dob), as.Date(violation_date)), "years"))

relabel_arrest <- function(raw_arrest) {
  raw_arrest %>% as.data.table() %>%
    mutate_at(c("dob", "violation_date", "consensus_adjud__date"), dmy) %>%
    # fix dates, year should be 1960, not 2060
    mutate(dob = adjustCentury(dob)) %>%
    mutate(
      Race = dplyr::recode(
        Race,
        "A" = "Asian or Pacific Islander",
        "B" = "Black or African American",
        "H" = "Hispanic",
        "I" = "American Indian or Alaska Native",
        "W" = "White",
        .default = "Other/Unknown",
        # for empty values
        .missing = "Other/Unknown"
      ),
      Sex = dplyr::recode(
        Sex,
        "F" = "Female",
        "M" = "Male",
        "U" = "Unknown"
      ),
      age = time_length(interval(as.Date(dob), as.Date(violation_date)), "years")
    ) %>%
    get_age_groups_10yr(., age_column = raw_arrest$age) %>%
    setnames(., c("Race", "Sex"),
             c("R_RACE", "R_SEX"))
}
# -------------- NHTS Relabel Race, Hispanic, Sex -----------------------------
# This is according to the codebook
nhts_relabel_race <- function(df) {
  df %>%
    mutate(R_RACE = dplyr::recode(
      R_RACE,
      '-8' = 'Dont know',
      '-7' = 'Refused',
      '01' = 'White',
      '02' = 'Black or African American',
      '03' = 'Asian or Pacific Islander',
      '04' = 'American Indian or Alaska Native',
      # '05' = 'Native Hawaiian or other Pacific Islander',
      '05' = 'Asian or Pacific Islander',
      '06' = 'Multiple responses',
      '97' = 'Some other race'
    ))
}

nhts_combine_unknown_race <- function(df) {
  df %>%
    mutate(R_RACE = dplyr::recode(
      R_RACE,
      'Dont know'= 'Other/Unknown',
      'Refused' = 'Other/Unknown',
      'Multiple responses' = 'Other/Unknown',
      'Some other race' = 'Other/Unknown'
    ))
}

nhts_relabel_hispanic <- function(df) {
  df %>%
    mutate(R_HISP = dplyr::recode(
      R_HISP, 
      '-8' = 'I dont know',
      '-7' = 'I prefer not to answer',
      '01' = 'Yes, Hispanic or Latino',
      '02' = 'No, Not Hispanic or Latino'
    ))
}

# Changes the R_RACE column to Hispanic if Hispanic was found
nhts_relabel_race_if_hispanic <- function(df) {
  df %>%
    mutate(R_RACE = ifelse(R_HISP %in% c("01", "Yes, Hispanic or Latino"), "Hispanic", R_RACE)) #%>% 
  # nhts_relabel_race_integ()
}

nhts_relabel_sex <- function(df) {
  df %>%
    mutate(R_SEX = dplyr::recode(
      R_SEX,
      # '-8' = 'I dont know',
      # '-7' = 'I prefer not to answer',
      '-8' = 'Unknown',
      '-7' = 'Unknown',
      '01' = 'Male',
      '02' = 'Female'
    ))
}
# -------------- Puts all data together -----------------------------
# These combine all 'summarize' functions and returns a df with all 'summarise' columns
# @ parameters are the datasets and fields_to_summarise
# crash_person_df - get crash count
# arrest_df
# nhts_person_df - get PMD and driver count
# census_df - get census count

# Returns a df of basic data counts. Will then be used to calculate ratios
get_counts_of_PMD_crash_drivers <-
  function(use_dmv_drivers = FALSE, crash_person_df = persons_new, arrest_df = arrests1718, nhts_person_df = nhts_person, driver_data = dmv_drivers, census_df = census17, fields_to_summarise) {
    crash_counts <- person_summarise_crash_count(crash_person_df, fields_to_summarise)
    owi_counts <- owi_summarise_arrest_count(arrest_df, fields_to_summarise = fields_to_summarise)
    pmd_counts <- nhts_summarise_pmd(nhts_person_df, fields_to_summarise = fields_to_summarise)
    nhts_totdrivers <- nhts_summarise_total_drivers(nhts_person_df, fields_to_summarise = fields_to_summarise)
    census_pop <- census_summarise_pop_count(census_df, fields_to_summarise = fields_to_summarise)
    
    if (use_dmv_drivers == TRUE){ # If using DMV data for drivers, replace with this
      nhts_totdrivers <- dmv_driver_summarise_count(driver_data, fields_to_summarise = fields_to_summarise)
      nhts_totdrivers
    }
    
    return(Reduce(function(x, y) merge(x, y, all=TRUE, by = intersect(fields_to_summarise, names(nhts_person))), list(crash_counts,owi_counts,pmd_counts,nhts_totdrivers,census_pop)))
    # return(cbind(crash_counts,pmd_counts,owi_counts,nhts_totdrivers, .id = intersect(fields_to_summarise, names(nhts_person))))
  }

# -------------- Summarize all data and make final tables -----------------------------
get_column_colors <- function(table_of_data) {
  table_of_data %>%
    mutate(
      crash_drivers_ratio = color_tile("#deebf7", "#63aacf")(crash_drivers_ratio),
      crash_vmt_ratio = color_tile("#deebf7", "#63aacf")(crash_vmt_ratio),
      arrest_driver_ratio = color_tile("#deebf7", "#63aacf")(arrest_driver_ratio),
      arrest_vmt_ratio = color_tile("#deebf7", "#63aacf")(arrest_vmt_ratio)
    )
}

get_data_of_percents_and_ratios_for_table <-
  function(summarized_data,
           fields_that_were_summarised) {
    round_to_0_digits <-
      c("nhts_n",
        "total_drivers",
        "crash_count",
        "total_arrests",
        "total_population")
    round_to_2_digits <-
      c(
        "total_vmt",
        "percent_of_crashes",
        "percent_of_drivers",
        "percent_of_vmt",
        "percent_of_arrests",
        # "percent_of_arrests_by_pop",
        "percent_of_population",
        "crash_drivers_ratio",
        "crash_vmt_ratio",
        # "arrest_pop_ratio",
        "arrest_driver_ratio",
        "arrest_vmt_ratio"
      )
    total_these_variables <-
      c(
        "nhts_n",
        "total_drivers",
        "crash_count",
        "total_arrests",
        "total_population",
        "total_vmt",
        "percent_of_crashes",
        "percent_of_drivers",
        "percent_of_vmt",
        "percent_of_arrests",
        "percent_of_population"
        # "percent_of_arrests_by_pop"
      )
    data <- summarized_data %>%
      select(
        dplyr::all_of(fields_that_were_summarised),
        nhts_n,
        total_drivers,
        PMD,
        crash_count,
        total_arrests,
        total_population
      ) %>%
      mutate(
        total_vmt = (total_drivers * PMD) / 1000000000,
        percent_of_crashes = crash_count * 100 / sum(crash_count, na.rm = TRUE),
        percent_of_drivers = total_drivers * 100  / sum(total_drivers, na.rm = TRUE),
        percent_of_vmt = total_vmt * 100 / sum(total_vmt, na.rm = TRUE),
        percent_of_arrests = total_arrests * 100 / sum(total_arrests, na.rm = TRUE),
        percent_of_population = total_population * 100 / sum(total_population, na.rm = TRUE),
        # percent_of_arrests_by_pop = total_arrests* total_population / sum(total_population),
        # percent_of_arrests_by_pop = (total_population * total_arrests) * 100 / sum(total_population * total_arrests, na.rm = TRUE),
        crash_drivers_ratio = percent_of_crashes / percent_of_drivers,
        crash_vmt_ratio = percent_of_crashes / percent_of_vmt,
        # arrest_pop_ratio =  percent_of_arrests / percent_of_population,
        arrest_driver_ratio =  percent_of_arrests / percent_of_drivers,
        arrest_vmt_ratio = percent_of_arrests / percent_of_vmt,
        PMD = format(round(PMD, 0), big.mark = ",", scientific = FALSE)
      ) %>%
      mutate_at(round_to_0_digits,         # Rounds these fields
                round,
                digits = 0) %>%
      
      mutate_at(round_to_2_digits,         # Rounds these fields
                round,
                digits = 2) %>%
      # Adds columns to some colors
      adorn_totals(# Columns to TOTAL
        "row",
        .... = all_of(total_these_variables), na.rm = TRUE)
    data[data == Inf] <- 0
    data
  }

make_summarized_data_table <- function(data_to_make_table, name_of_fields_that_were_summarised, caption){
  # Number of table heading must match number of columns in the table
  table_headings = c(
    paste0(name_of_fields_that_were_summarised),
    paste0("Sample size (n)", footnote_marker_number(1)),
    # paste0("Drivers", footnote_marker_number(1)),
    paste0("Drivers", footnote_marker_number(5)),
    paste0("Average PMD", footnote_marker_number(1)), #, footnote_marker_alphabet(1)
    paste0("Drivers in a crash", footnote_marker_number(2)),
    paste0("Average annual OWI arrests", footnote_marker_number(3)),
    paste0("Population", footnote_marker_number(4)),
    "Annual VMT (100 million)",
    "Share of drivers in a crash",
    "Share of drivers",
    "Share of VMT",
    "Share of arrests",
    "Share of population",
    # "Share of arrests by weighted population",
    "Ratio of crash to drivers",
    "Ratio of crash to VMT",
    "Ratio of arrests to drivers",
    "Ratio of arrests to VMT"
  )
  total_row_number = which(data_to_make_table == "Total") # Find total row to bold
  data_to_make_table %>% 
    kbl(
      caption = caption,
      col.names = table_headings,
      format.args = list(big.mark = ','),  # Format - add a comma to all numbers
      escape = F
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
    kable_classic(full_width = T, html_font = "Calibri") %>% row_spec(total_row_number, bold = T) %>% # Total row format
    # column_spec(7, width  = "6em") %>%
    # column_spec(8, width  = "5em") %>%
    # column_spec(16, color = "white") %>%
    footnote(
      general = "Average PMD is derived by taking driving data in four neighboring states (IA, IL, MI, MN, WI). VMT is derived by multplying PMD by total drivers.",
      number = c("Source: 2017 National Household Travel Survey\n",
                 "Source: 2017 and 2018 Crash Database from TOPS Lab\n",
                 "Source: 2017 and 2018 Arrest data from Wisconsin Department of Justice\n",
                 "Source: 2017 Census Bureau Population Estimates Program\n",
                 "Source: 2017 and 2018 WisDOT DMV\n"),
      footnote_as_chunk = T
    )
}

