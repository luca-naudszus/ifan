# ---------------------------------------------------------
# Preprocesses longitudinal dream data for one participant
# (c) Luca A. Naudszus, Essen, 27.03.2024 / Vang Vieng, 22.07.2025 / Cat Ba 05.08.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# ---------------------------------------------------------
#
#
# Input: 
### id
### df_dreams, containing dream item data for all participants
### segmentations and social content analysis (SCA) ratings for id
#-----------
#  
#-----------
#-----------
# Output: 
### df_ratings, containing SCA data for id for all dreams on situation level 
### df_daily_dreams, containing dream item and SCA data for id on day level
# ---------------------------------------------------------

# ---------------------------------------------------------

preprocess_dreams <- function(id, df_dreams) {
  ### use data for this participant
  df_dreams <- df_dreams[df_dreams$id == id,]
  # ---------------------------------------------------------
  # manually correct values
  ### KT07HN recorded a dream from 31/10/2023 after midnight
  if(id == "KT07HN") {
    df_dreams$datetime[df_dreams$datetime == "2023-11-01 00:23:00 CET"] <- "2023-10-31 23:59:59 CET"
  } 
  ### SB12KR remembered a dream from 09/11/2023 on the next day
  if(id == "SB12KR") {
    df_dreams$datetime[df_dreams$datetime == "2023-11-10 07:43:00 CET"] <- "2023-11-09 23:59:59 CET"
  }
  ### TN10HL remembered a dream from 04/08/2023 on the next day
  if (id == "TN10HL") {
    df_dreams$datetime[df_dreams$datetime == "2023-08-05 21:15:00 CET"] <- "2023-08-04 23:59:59 CET"
  }
  ### HI01IG used two questionnaires for episodes from one night on two occurrences
  if(id == "HI01IG") {
    problematic_datetimes <- c(as.POSIXct("2023-10-26 04:30:00 CEST"), as.POSIXct("2023-11-02 05:35:00 CET"))
    respective_original_dates <- c(as.POSIXct("2023-10-26 04:27:00 CEST"), as.POSIXct("2023-11-02 05:32:00 CET"))
    for (in_date in 1:2) {
      df_dreams$mood[
        df_dreams$datetime == respective_original_dates[in_date]] <- mean(
          df_dreams$mood[
            df_dreams$datetime == respective_original_dates[in_date]], 
          df_dreams$mood[
            df_dreams$datetime == problematic_datetimes[in_date]]
        )
      df_dreams$content[
        df_dreams$datetime == respective_original_dates[in_date]] <- paste0(
          df_dreams$content[df_dreams$datetime == respective_original_dates[in_date]],
          df_dreams$content[df_dreams$datetime == problematic_datetimes[in_date]]
        )
    }
    df_dreams <- df_dreams[!(df_dreams$datetime %in% problematic_datetimes),]
  }
  # ---------------------------------------------------------
  # remove duplicates without any dream
  duplicated_date <- df_dreams$datetime[(
        duplicated(as.Date(as.POSIXct(df_dreams$datetime, format = "%d/%m/%Y %H:%M"))) | 
          duplicated(as.Date(as.POSIXct(df_dreams$datetime, format = "%d/%m/%Y %H:%M")), fromLast = TRUE))]
  if(length(duplicated_date) > 0) {
    duplicated_table <- NULL
    for (entry in duplicated_date) {
      adj_date <- as.Date(as.POSIXct(entry, format = "%d/%m/%Y %H:%M"))
      dream <- df_dreams$dream[df_dreams$datetime == entry]
      duplicated_table <- rbind(duplicated_table,
                                cbind(entry, adj_date, dream))
    }
    duplicated_table <- as_tibble(duplicated_table)
    duplicated_table$deletion <- TRUE
    for (prdate in unique(duplicated_table$adj_date)) {
      duplicated_table$deletion[duplicated_table$adj_date == prdate & 
                                  duplicated_table$dream == 1] = FALSE
      if (all(duplicated_table$deletion[duplicated_table$adj_date == prdate])) {
        duplicated_table$deletion[duplicated_table$adj_date == prdate & 
                                    duplicated_table$entry == min(duplicated_table$entry[duplicated_table$adj_date == prdate])] = FALSE
      }
      if (all(duplicated_table$deletion[duplicated_table$adj_date == prdate])) {
        stop('Error in duplicates removal.')
      }
    }
    dates_for_removal = duplicated_table$entry[duplicated_table$deletion]
    df_dreams <- df_dreams[!(df_dreams$datetime %in% dates_for_removal),]
  }
  # ---------------------------------------------------------
  # make dataframe
  ### get number of days
  startDate = min(as.Date(as.POSIXct(df_dreams$datetime, format = "%d/%m/%Y %H:%M")))
  #TODO: Find out where NAs are introduced by coercion
  nDays = as.numeric(max(as.Date(as.POSIXct(df_dreams$datetime, format = "%d/%m/%Y %H:%M"))) - startDate) + 1
  ### initialize frame
  df_data <- as.data.frame(matrix(ncol = 8, nrow = nDays))
  colnames(df_data) <- c('id', 'dream', 'nightmare', 'mood', 'content', 'datetime', 'date', 'dayCounter')
  # ---------------------------------------------------------
  # fill in information from IFAN_dream
  df_data$id = id
  for (in_day in 1:nDays) {
    df_data$date[in_day] = as.Date(startDate + in_day - 1)
    df_data$dayCounter[in_day] = in_day
    nEntries = dim(df_dreams[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1,])[1]
    if (nEntries == 1 || (nEntries == 2 && df_dreams$dream[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][1] == 2)) {
      df_data$dream[in_day] = df_dreams$dream[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][nEntries]
      df_data$nightmare[in_day] = df_dreams$nightmare[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][nEntries]
      df_data$mood[in_day] = df_dreams$mood[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][nEntries]
      df_data$content[in_day] = df_dreams$content[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][nEntries]
      df_data$datetime[in_day] = df_dreams$datetime[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][nEntries]
    } else if (nEntries == 2) {
      if (df_dreams$dream[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][2] == 2) {
        stop(sprintf("Error in participant %s. Two reports for %s, of which the second one declares no memory.", id, startDate + in_day - 1))
      } 
      # !!! check whether both reports have been in the morning
      # !!! if one was in the morning, another one in the evening, there might be another problem
      df_data$dream[in_day] = 1
      df_data$nightmare[in_day] = mean(df_dreams$nightmare[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1])
      df_data$mood[in_day] = mean(df_dreams$mood[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1])
      df_data$content[in_day] = paste(df_dreams$content[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][1], 
                                      df_dreams$content[as.Date(df_dreams$datetime, format = "%d/%m/%Y %H:%M") == startDate + in_day - 1][2], sep = " ")
      df_data$datetime[in_day] = NA
      warning(sprintf("Warning for participant %s. Two reports for %s. Averaging nightmare and mood. Rendering NA for submission time.", id, startDate + in_day - 1))
    } else if (nEntries > 2) {
      stop(sprintf("Error in participant %s. More than two reports for %s.", id, startDate + in_day - 1))
    }
  }
  df_data$date <- as.Date(df_data$date)
  
  # ---------------------------------------------------------
  # load ratings and segmentations and check if everything is okay
  ### first load data
  fn_template <- list.files(path = file.path("data", "2_preprocessed-data", "social-content-analysis", "4_Templates"), pattern = id, full.names = T)
  if (length(fn_template) == 0) {
    print(sprintf("No segmentation for participant %s.", id))
    return(list(NULL, NULL))
    }
  df_segmentation <- as_tibble(read_xlsx(fn_template))
  fn_rating <- list.files(path = file.path("data", "2_preprocessed-data", "social-content-analysis", "complete"), pattern = id, full.names = T)
  if (length(fn_rating) == 0) {
    stop(sprintf("Error in participant %s. There is a segmentation, but no rating.", id))
  }
  df_ratings <- as_tibble(read_xlsx(fn_rating))
  # ---------------------------------------------------------
  ### fill in data
  df_segmentation$code <- df_segmentation$code[1]
  df_segmentation <- rename(df_segmentation, id = code)
  df_segmentation <- fill(df_segmentation, date)
  df_ratings$code <- df_ratings$code[1]
  df_ratings <- rename(df_ratings, id = code)
  df_ratings$rater <- df_ratings$rater[1]
  df_ratings <- fill(df_ratings, date)
  
  # ---------------------------------------------------------
  ### check if all data matches
  if (is.na(df_segmentation$id[1])) {
    stop(sprintf("Error in participant %s. There is no id code in the segmentation.", id))
  } else if (is.na(df_ratings$id[1])) {
    stop(sprintf("Error in participant %s. There is no id code in the rating.", id))
  } else if (id != df_segmentation$id[1]) {
    stop(sprintf("Error in participant %s. ID codes are not matching.", id))
  } else if (is.na(df_ratings$rater[1])) {
    stop(sprintf("Error in participant %s. There is no information on the rater.", id))
  } else if (nrow(df_segmentation) != nrow(df_ratings)) {
    stop(sprintf("Error in participant %s. Segmentation and ratings differ in length.", id))
  } else if (!all(df_segmentation$episode == df_ratings$episode)) {
    idx <- which(df_segmentation$episode != df_ratings$episode)
    stop(sprintf("Error in participant %s. Segmentation and ratings differ in episodal structure. This occurs on %s.", id, paste(as.Date(df_segmentation$date[idx]), collapse = ", ")))
  } else if (!all(df_segmentation$event == df_ratings$event)) {
    idx <- which(df_segmentation$event != df_ratings$event)
    stop(sprintf("Error in participant %s. Segmentation and ratings differ in event structure. This occurs on %s.", id, paste(as.Date(df_segmentation$date[idx]), collapse = ", ")))
  } else if (!all(df_segmentation$situation == df_ratings$situation)) {
    idx <- which(df_segmentation$situation != df_ratings$situation)
    stop(sprintf("Error in participant %s. Segmentation and ratings differ in situational structure. This occurs on %s.", id, paste(as.Date(df_segmentation$date[idx]), collapse = ", ")))
  }
  

  # ---------------------------------------------------------
  ### tidy up ratings 
  # correct avatar entries
  df_ratings$init_char_1 <- as.character(df_ratings$`init-char`)
  df_ratings$init_no_1 <- as.character(df_ratings$`init-no`)
  df_ratings$init_gender_1 <- as.character(df_ratings$`init-gender`)
  df_ratings$reci_char_1 <- as.character(df_ratings$`reci-char`)
  df_ratings$reci_no_1 <- as.character(df_ratings$`reci-no`)
  df_ratings$reci_gender_1 <- as.character(df_ratings$`reci-gender`)
  df_ratings$init_char_2 <- NA
  df_ratings$init_no_2 <- NA
  df_ratings$init_gender_2 <- NA
  df_ratings$reci_char_2 <- NA
  df_ratings$reci_no_2 <- NA
  df_ratings$reci_gender_2 <- NA
  df_ratings$init_char_3 <- NA
  df_ratings$init_no_3 <- NA
  df_ratings$init_gender_3 <- NA
  df_ratings$reci_char_3 <- NA
  df_ratings$reci_no_3 <- NA
  df_ratings$reci_gender_3 <- NA
  ### take rows with multiple initiating avatars
  for (irow in grep('.', df_ratings$init_char_1, fixed = T)){
    elements_char <- length(strsplit(df_ratings$init_char_1[irow], "\\.")[[1]])
    elements_no <- length(strsplit(df_ratings$init_no_1[irow], "\\.")[[1]])
    elements_gender <- length(strsplit(df_ratings$init_gender_1[irow], "\\.")[[1]])
    if (elements_char > 3){
      break 
      print('An error occurred while sorting characters: there are more than three initiating or recipient characters in at least one dream.')
    }
    # write first element before comma into init_char_1, second element into init_char_2, ...
    df_ratings$init_char_2[irow] <- strsplit(df_ratings$init_char_1[irow], "\\.")[[1]][2]
    df_ratings$init_char_3[irow] <- strsplit(df_ratings$init_char_1[irow], "\\.")[[1]][3]
    df_ratings$init_char_1[irow] <- strsplit(df_ratings$init_char_1[irow], "\\.")[[1]][1]
    if (elements_char == elements_no){
      df_ratings$init_no_2[irow] <- strsplit(df_ratings$init_no_1[irow], "\\.")[[1]][2]
      df_ratings$init_no_3[irow] <- strsplit(df_ratings$init_no_1[irow], "\\.")[[1]][3]
      df_ratings$init_no_1[irow] <- strsplit(df_ratings$init_no_1[irow], "\\.")[[1]][1]
    } else if (elements_no == 1){
      df_ratings$init_no_2[irow] <- df_ratings$init_no_1[irow]
      if (elements_char == 3){
        df_ratings$init_no_3[irow] <- df_ratings$init_no_1[irow]
      }
    } else {
      break
      print('An error occurred while sorting characters: inconsistent numbers of char and no.')
    }
    if (elements_char == elements_gender){
      df_ratings$init_gender_2[irow] <- strsplit(df_ratings$init_gender_1[irow], "\\.")[[1]][2]
      df_ratings$init_gender_3[irow] <- strsplit(df_ratings$init_gender_1[irow], "\\.")[[1]][3]
      df_ratings$init_gender_1[irow] <- strsplit(df_ratings$init_gender_1[irow], "\\.")[[1]][1]
    } else if (elements_gender == 1){
      df_ratings$init_gender_2[irow] <- df_ratings$init_gender_1[irow]
      if (elements_char == 3) {
        df_ratings$init_gender_3[irow] <- df_ratings$init_gender_1[irow]
      }
    } else {
      break
      print('An error occurred while sorting characters: inconsistent numbers of char and gender.')
    }
  }
  ### take only rows with multiple recipients
  for (irow in grep('.', df_ratings$reci_char_1, fixed = T)){
    elements_char <- length(strsplit(df_ratings$reci_char_1[irow], "\\.")[[1]])
    elements_no <- length(strsplit(df_ratings$reci_no_1[irow], "\\.")[[1]])
    elements_gender <- length(strsplit(df_ratings$reci_gender_1[irow], "\\.")[[1]])
    if (elements_char > 3){
      break 
      print('An error occurred while sorting characters: there are more than three reciiating or recipient characters in at least one dream.')
    }
    # write first element before comma into reci_char_1, second into reci_char_2, ...
    df_ratings$reci_char_2[irow] <- strsplit(df_ratings$reci_char_1[irow], "\\.")[[1]][2]
    df_ratings$reci_char_3[irow] <- strsplit(df_ratings$reci_char_1[irow], "\\.")[[1]][3]
    df_ratings$reci_char_1[irow] <- strsplit(df_ratings$reci_char_1[irow], "\\.")[[1]][1]
    if (elements_char == elements_no){
      df_ratings$reci_no_2[irow] <- strsplit(df_ratings$reci_no_1[irow], "\\.")[[1]][2]
      df_ratings$reci_no_3[irow] <- strsplit(df_ratings$reci_no_1[irow], "\\.")[[1]][3]
      df_ratings$reci_no_1[irow] <- strsplit(df_ratings$reci_no_1[irow], "\\.")[[1]][1]
    } else if (elements_no == 1){
      df_ratings$reci_no_2[irow] <- df_ratings$reci_no_1[irow]
      if (elements_char == 3){
        df_ratings$reci_no_3[irow] <- df_ratings$reci_no_1[irow] 
      }
    } else {
      break
      print('An error occurred while sorting characters: inconsistent numbers of char and no.')
    }
    if (elements_char == elements_gender){
      df_ratings$reci_gender_2[irow] <- strsplit(df_ratings$reci_gender_1[irow], "\\.")[[1]][2]
      df_ratings$reci_gender_3[irow] <- strsplit(df_ratings$reci_gender_1[irow], "\\.")[[1]][3]
      df_ratings$reci_gender_1[irow] <- strsplit(df_ratings$reci_gender_1[irow], "\\.")[[1]][1]
    } else if (elements_gender == 1){
      df_ratings$reci_gender_2[irow] <- df_ratings$reci_gender_1[irow]
      if (elements_char == 3) {
        df_ratings$reci_gender_3[irow] <- df_ratings$reci_gender_1[irow] 
      }
    } else {
      break
      print('An error occurred while sorting characters: inconsistent numbers of char and gender.')
    }
  }
  # the same for quality
  df_ratings$quality <- as.character(df_ratings$quality)
  df_ratings$quality_2 <- NA
  for (irow in grep('.', df_ratings$quality, fixed = T)){
    df_ratings$quality_2[irow] <- strsplit(df_ratings$quality[irow], "\\.")[[1]][2]
    df_ratings$quality[irow] <- strsplit(df_ratings$quality[irow], "\\.")[[1]][1]
  }
  # make all columns numerical
  ### TODO: six warnings for AE08TO, two warnings for AG06DE
  cols = c('init_char_1', 'init_no_1', 'init_gender_1', 'reci_char_1', 'reci_no_1', 'reci_gender_1', 'init_char_2', 'init_no_2', 'init_gender_2', 'reci_char_2', 'reci_no_2', 'reci_gender_2', 'init_char_3', 'init_no_3', 'init_gender_3', 'reci_char_3', 'reci_no_3', 'reci_gender_3', 'quality', 'quality_2')
  for (col in cols){
    df_ratings[[col]] <- as.numeric(df_ratings[[col]])
  }
  # add gender for dream-self
  char_cols <- c('init_char_1', 'reci_char_1', 'init_char_2', 'reci_char_2', 'init_char_3', 'reci_char_3')
  gend_cols <- c('init_gender_1', 'reci_gender_1', 'init_gender_2', 'reci_gender_2', 'init_gender_3', 'reci_gender_3')
  for (ichar in 1:length(char_cols)){
    mask = select(df_ratings, char_cols[ichar]) == 1
    mask[is.na(mask)] <- 0
    if (sum(mask) > 0){
      index = which(colnames(df_ratings) == gend_cols[ichar])
      df_ratings[mask == 1,index] <- as.numeric(df_initial$gender[df_initial$id == id])
    }
  }
  # ---------------------------------------------------------
  ### get wordcount
  df_ratings$wordcount <- str_count(df_ratings$content, "\\S+")
  # ---------------------------------------------------------
  ### combine dreams and SCA on daily level
  dataC <- df_ratings[!is.na(df_ratings$content),]
  character_cols <- c("init_char_1", "init_char_2", "init_char_3", 
                      "reci_char_1", "reci_char_2", "reci_char_3")
  number_cols <- c("init_no_1", "init_no_2", "init_no_3", 
                   "reci_no_1", "reci_no_2", "reci_no_3")
  familiar <- c(2, 21, 211, 212, 213, 214, 22, "22X", 231, 232)
  dataC$ava2single <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row %in% familiar)) & apply(dataC[, number_cols], 1, function (row) any(row == 1))
  dataC$ava2groups <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row %in% familiar)) & apply(dataC[, number_cols], 1, function (row) any(row == 2))
  dataC$ava3single <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 3)) & apply(dataC[, number_cols], 1, function (row) any(row == 1))
  dataC$ava3groups <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 3)) & apply(dataC[, number_cols], 1, function (row) any(row == 2))
  dataC$ava4single <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 4)) & apply(dataC[, number_cols], 1, function (row) any(row == 1))
  dataC$ava4groups <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 4)) & apply(dataC[, number_cols], 1, function (row) any(row == 2))
  dataC$ava5single <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 5)) & apply(dataC[, number_cols], 1, function (row) any(row == 1))
  dataC$ava5groups <- dataC$social == 1 & apply(dataC[, character_cols], 1, function(row) any(row == 5)) & apply(dataC[, number_cols], 1, function (row) any(row == 2))
  dataC$avaUns <- dataC$social == 1 & apply(dataC[, number_cols], 1, function (row) any(row == 3))
  dataC <- dataC %>%
    mutate(eventr = as.numeric(episode)*100+as.numeric(event)) 
  df_sca <- dataC %>%
    group_by(id, date) %>%
    summarize(
      n_epi = n_distinct(episode), #number of dream episodes
      n_eve = n_distinct(eventr), #number of dream events
      n_sit = n(), #number of dream situations
      dreamSocSit = sum(as.numeric(social), na.rm = T), #number of social situations
      dreamSocInt = sum((type == 3 | type == 4 | type == 5) & !is.na(type)), #number of social interactions
      multi = sum((type == 4 | type == 5) & !is.na(type)), #number of multilateral social interactions
      realMulti = sum((type == 5) & !is.na(type)), #number of reciprocal social interactions
      inter = sum((type == 3 | type == 5) & !is.na(type)), #number of unilateral and reciprocal social interactions
      # isabellasMultiIdee (Interaktionen = 5er und alle 3er mit Reaktion)
      nAva2sin = sum(ava2single, na.rm = T), #number of familiar single avatars
      nAva3sin = sum(ava3single, na.rm = T), #number of single avatars known by profession or role and celebrities
      nAva4sin = sum(ava4single, na.rm = T), #number of stranger single avatars
      nAva5sin = sum(ava5single, na.rm = T), #number of unspecified single avatars
      nAva2gro = sum(ava2groups, na.rm = T), #number of groups of familiar avatars
      nAva3gro = sum(ava3groups, na.rm = T), #number of groups of avatars known by profession or role and celebrities
      nAva4gro = sum(ava4groups, na.rm = T), #number of groups of stranger avatars
      nAva5gro = sum(ava5groups, na.rm = T), #number of groups of unspecified avatars
      nAvaUns = sum(avaUns, na.rm = T), #number of avatars unspecified in terms of number
      famAva = sum(nAva2sin, nAva2gro),
      unfAva = sum(nAva3sin, nAva3gro, nAva4sin, nAva4gro),
      unsAva = sum(nAva5sin, nAva5gro),
      wordcount = sum(wordcount),
      # calculate number of social events
      .groups = "keep"
    ) %>%
    mutate(
      wordPerSit = wordcount / n_sit,
      sitPerEve = n_sit / n_eve,
      evePerEpi = n_eve / n_epi
    )
  df_sca$n_sit[is.na(df_sca$n_epi)] <- NA
  df_sca$wordcount[is.na(df_sca$n_epi)] <- NA
  
  # ---------------------------------------------------------
  ### add event-level information
  df_event <- dataC %>% 
    mutate(eventr = as.numeric(episode)*100+as.numeric(event)) %>% 
    group_by(id, date) %>% 
    filter(social == 1) %>% 
    summarize(n_socEve = n_distinct(eventr), .groups = "keep")
  df_sca <- merge(df_sca, df_event, by = c("id", "date"), all = TRUE)
  
  ### CN10MC was rated on a Mac and has different time format
  if(id == "CN10MC") {
    df_sca$date <- as.Date(df_sca$date, format = "%d/%m/%Y %H:%M")
  } else {
    df_sca$date <- as.Date(df_sca$date, format = "%Y-%m-%d %H:%M:%S")
  }
  
  # ---------------------------------------------------------
  ### return df_ratings and df_dreams_daily
  df_dreams_daily <- merge(df_data, df_sca, by = c("id", "date"), all = TRUE)
  results <- list(df_ratings, df_dreams_daily)
}