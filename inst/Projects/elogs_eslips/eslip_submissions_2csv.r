
library(tidyverse)
library(readxl)
library(janitor)
library(stringr)

folder_path <- "R:\\Science\\Population Ecology Division\\Shared\\!PED_Unit17_Lobster\\Lobster Unit Shared\\eSlips as of 2026-05-13"


files <- list.files(folder_path, full.names = TRUE)

# ---- HEADER DETECTION ----
find_header_row <- function(df_raw) {
  for (i in 1:nrow(df_raw)) {
    
    row_vals <- tolower(trimws(as.character(unlist(df_raw[i, ]))))
    
    # remove empty values
    row_vals <- row_vals[row_vals != "" & !is.na(row_vals)]
    
    if (length(row_vals) == 0) next
    
    hits <- sum(grepl("lic|vessel|date|quant|vrn", row_vals))
    
    if (hits >= 2 && length(row_vals) >= 5) {
      return(i)
    }
  }
  
  return(1)
}

# ---- MAIN FUNCTION ----
read_and_clean <- function(file) {
  ext <- tools::file_ext(file)
  
  tryCatch({
    
    # ---- READ FILE ----
    if (ext %in% c("xlsx", "xls")) {
      
      df_raw <- read_excel(file, col_names = FALSE, col_types = "text")
      header_row <- find_header_row(df_raw)
      
      df <- read_excel(file, skip = header_row - 1, col_types = "text")
      
    } else if (ext == "csv") {
      
      temp <- read_csv(file, n_max = 20, col_names = FALSE, show_col_types = FALSE)
      header_row <- find_header_row(temp)
      
      df <- read_csv(file, skip = header_row - 1, show_col_types = FALSE)
      
    } else {
      return(NULL)
    }
    
    message("Processed: ", basename(file), " | header row: ", header_row)
    
    # ---- CLEAN COLUMN NAMES ----
    df <- df %>% clean_names()
    species_col <- names(df)[grepl("species|product", names(df), ignore.case = TRUE)][1]
    
    
    if (!is.na(species_col)) {
      df <- df %>%
        filter(grepl("lobster", !!sym(species_col), ignore.case = TRUE))
      if(nrow(df)==0) next()
    }
    
    # ---- SELECT LICENCE COLUMN ----
    licence_num_col <- names(df)[grepl("lic.*(number|id)", names(df), ignore.case = TRUE)][1]
    
    licence_any_col <- names(df)[
      grepl("lic", names(df), ignore.case = TRUE) &
        !grepl("holder|name", names(df), ignore.case = TRUE)
    ][1]
    
    if (!is.na(licence_num_col)) {
      df <- df %>% rename(licence = all_of(licence_num_col))
    } else if (!is.na(licence_any_col)) {
      df <- df %>% rename(licence = all_of(licence_any_col))
    }
    
    # ---- SELECT DATE COLUMN ----
    date_landed_col <- names(df)[grepl("date.*land", names(df), ignore.case = TRUE)][1]
    any_date_col    <- names(df)[grepl("date", names(df), ignore.case = TRUE)][1]
    
    if (!is.na(date_landed_col)) {
      df <- df %>% rename(date = all_of(date_landed_col))
    } else if (!is.na(any_date_col)) {
      df <- df %>% rename(date = all_of(any_date_col))
    }
    
    # ---- RENAME OTHER FIELDS (SAFE) ----
    df <- df %>%
      rename(
        vessel_name = matches("vessel"),
        vrn = matches("vrn"),
        quantity = matches("quant"),
        unit = matches("measure|unit")
      )
    
    required_cols <- c("date", "licence", "vessel_name", "vrn", "quantity", "unit")
    
    for (col in required_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA
      }
    }
    
    # ---- KEEP ONLY IMPORTANT COLUMNS ----
    df <- df %>%
      select(any_of(c("date", "licence", "vessel_name",
                      "vrn", "quantity", "unit")))
    
    # ---- ADD FILE NAME ----
    df$source_file <- basename(file)
    
    # ---- FIX TYPES ----
    df <- df %>%
      mutate(
        vrn = as.character(vrn),
        licence = as.character(licence),
        vessel_name = as.character(vessel_name),
        #unit = as.character(unit),
        quantity = suppressWarnings(as.numeric(quantity)),
        date = as.character(date)
      )
    
    return(df)
    
  }, error = function(e) {
    message("FAILED: ", file)
    print(e)
    return(NULL)
  })
}

# ---- RUN ----
#cd2 =list()
for(i in 1:length(files)){
print(i)  
  cd2[[i]] <- read_and_clean(files[[i]])
  
}
# remove failed files
combined_data <- bind_rows(cd2[!sapply(cd2, is.null)])


combined_data <- combined_data %>%
  mutate(
    date = case_when(
      # numeric Excel dates
      grepl("^[0-9]+$", date) ~ as.Date(as.numeric(date), origin = "1899-12-30"),
      
      # text dates
      TRUE ~ as.Date(date, format = "%d-%b-%Y")
    )
  )

cs = subset(combined_data, !is.na(date))
write.csv(cs,file=file.path(project.datadirectory('bio.lobster'),'data','eslips_may2026.csv'))

####compare to logs

lobster.db('logs')
elog <- logs %>% filter(SRC  %in%  "ELOG")
elog$YEAR = year(elog$DATE_FISHED)
elog <- elog %>% dplyr::select(-CUNNER_WEIGHT_GRID_A,-CUNNER_WEIGHT_GRID_B,-CUNNER_WEIGHT_GRID_C, 
                               -JONAH_CRAB_WEIGHT_GRID_A, -JONAH_CRAB_WEIGHT_GRID_B, -JONAH_CRAB_WEIGHT_GRID_C,
                               -GREEN_CRAB_WEIGHT_GRID_A, -GREEN_CRAB_WEIGHT_GRID_B, -GREEN_CRAB_WEIGHT_GRID_C,
                               -SCULPIN_WEIGHT_GRID_A, -SCULPIN_WEIGHT_GRID_B, -SCULPIN_WEIGHT_GRID_C,
                               -ROCK_CRAB_WEIGHT_GRID_A, -ROCK_CRAB_WEIGHT_GRID_B, -ROCK_CRAB_WEIGHT_GRID_C)  ## can bring any of these back in if we want to look at them

elog <- elog %>% tidyr::pivot_longer(cols = c(GRID_NUM, GRID_NUM_B, GRID_NUM_C,
                                              WEIGHT_LBS, WEIGHT_LBS_B, WEIGHT_LBS_C, NUM_OF_TRAPS, NUM_OF_TRAPS_B, NUM_OF_TRAPS_C),
                                     names_to = c(".value", "set"),
                                     names_pattern = "(.*?)(_B|_C)?$")
elog <- elog %>% mutate(set = ifelse(set %in% "", "A", gsub("_", "", set)))
elog <- elog %>% filter(!(set == "B" & is.na(GRID_NUM)), !(set == "C" & is.na(GRID_NUM)))
elog <- aggregate(WEIGHT_LBS~LICENCE_ID+LFA+DATE_FISHED+VR_NUMBER,data=elog,FUN=sum)
cl = merge(cs,elog,by.x=c('date','licence'),by.y=c('DATE_FISHED','LICENCE_ID'))
require(ggplot2)
ggplot(cl,aes(x=WEIGHT_LBS,y=quantity, colour=as.factor(LFA)))+geom_point()+labs(x='Logbook Weight',y='Eslip Weight')+xlim(0,6000)+geom_abline(slope=1,intercept=0)
