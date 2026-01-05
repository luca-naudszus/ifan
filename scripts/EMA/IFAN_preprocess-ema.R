# ----------------------------------------------------------
# Preprocessing of EMA data for the IFAN project. 
# (c) Luca A. Naudszus, Essen, 28.08.2023/03.10.2023
#     Vang Vieng, 22.07.2025
# Institute for Psychology, University of Duisburg-Essen, Essen
# Social Brain Sciences Lab, ETH Zurich, Zurich
# ----------------------------------------------------------
#
#
# Input: 
### 
#-----------
# 
#-----------
#-----------
# Output: 
### 
# ---------------------------------------------------------

# define EMA beep times
beeptimes <- array(c(
  c("09:00", "11:00", "13:00", "15:00", "17:00", "19:00", "21:00"),
  c("10:30", "12:30", "14:30", "16:30", "18:30", "20:30", "22:30"),
  c("15:30", "17:30", "19:30", "21:30", "23:30", "01:30", "03:30")),
  dim = c(7, 3))
# participants were able to choose between 07:00 and 08:30 as start time for EMA, 
# having the first beep that concerns the last two hours at 09:00 or 10:30 respectively
# one participant was in another time zone and got the first beep at 15:30

preprocess_ema <- function(id, onset, start, end, df.ema) {
  # get individual EMA data
  data <- df.ema[df.ema$id == id,]
  # make data frame
  dates = as.Date(data$date[!(is.na(data$rsesema_1))], format = "%d/%m/%Y")
  startDate = min(dates)
  if (startDate < start) {
    print(sprintf('Warning for id %s: Recorded start date is before start of EMA phase.', id))
    startDate = start
  }
  if (max(dates) > end) {
    print(sprintf('Warning for id %s: Recorded end date is after end of EMA phase.', id))
    endDate = end
  } else {
    endDate = max(dates)
  }
  nDays = as.numeric(endDate - startDate) + 1
  df.data <- as.data.frame(matrix(ncol = 12, nrow = nDays*7))
  colnames(df.data) <- c('id', 'date',
                         'rsesema_1', 'rsesema_2', 'rsesema_3', 'rsesema_4', 'socialSituations', 'socialInteractions', 'exclusion', 'rsesema_total', 
                         'beepCounter', 'dayCounter')
  # fill id and counters
  df.data$id <- id
  df.data$dayCounter <- rep(1:nDays, each=7)
  df.data$beepCounter <- rep(1:7, times=nDays)
  # fill in date
  df.data$date <- startDate + df.data$dayCounter - 1
  # fill in EMA information
  for (day in 1:nDays) {
    for (beep in 1:7) {
      row = data[data$time == beeptimes[beep, onset] &
                   as.Date(as.POSIXct(data$scheduledTime, format = "%d/%m/%Y %H:%M")) == df.data$date[df.data$dayCounter == day & df.data$beepCounter == beep] & 
                   !(is.na(data$rsesema_1)),]
      if (nrow(row) == 1) {
        df.data$rsesema_1[df.data$dayCounter == day & df.data$beepCounter == beep] = row$rsesema_1
        df.data$rsesema_2[df.data$dayCounter == day & df.data$beepCounter == beep] = row$rsesema_2
        df.data$rsesema_3[df.data$dayCounter == day & df.data$beepCounter == beep] = row$rsesema_3
        df.data$rsesema_4[df.data$dayCounter == day & df.data$beepCounter == beep] = row$rsesema_4
        df.data$socialSituations[df.data$dayCounter == day & df.data$beepCounter == beep] = row$socialSituations
        df.data$socialInteractions[df.data$dayCounter == day & df.data$beepCounter == beep] = row$socialInteractions
        df.data$exclusion[df.data$dayCounter == day & df.data$beepCounter == beep] = row$exclusion
      } else if (nrow(row) >= 1) {
        stop('Error: multiple rows for same beep detected.')
      }
    }
  }
  df.data <- df.data %>%
    mutate(rsesema_2r = 8 - rsesema_2,
           rsesema_4r = 8 - rsesema_4,
           rses_total = (rsesema_1 + rsesema_2r + rsesema_3 + rsesema_4r)/4)
  return(df.data)
  
}

daily_ema <- function(df.data) {
  # create df.daily 
  df.daily <- df.data %>%
    group_by(id, date, dayCounter) %>%
    summarize(rses_1 = mean(rsesema_1, na.rm = T), 
              rses_2 = mean(rsesema_2r, na.rm = T),
              rses_3 = mean(rsesema_3, na.rm = T),
              rses_4 = mean(rsesema_4r, na.rm = T),
              socSit = mean(socialSituations, na.rm = T),
              socInt = mean(socialInteractions, na.rm = T),
              excl = mean(exclusion, na.rm = T),
              rses_totalMean = mean(rses_total, na.rm = T),
              # standard deviations are consistent with previous research (cf. Zeigler-Hill & Abraham, 2006)
              # calculate Mean Squared Successive Differences instead of/in addition to standard deviations!
              rses_1sd = sd(rsesema_1, na.rm = T),
              rses_2sd = sd(rsesema_2r, na.rm = T),
              rses_3sd = sd(rsesema_3, na.rm = T),
              rses_4sd = sd(rsesema_4r, na.rm = T),
              socSitSd = sd(socialSituations, na.rm = T),
              socIntSd = sd(socialInteractions, na.rm = T),
              exclSd = sd(exclusion, na.rm = T),
              rses_totalSd = sd(rses_total, na.rm = T),
              compliance = 1-sum(is.na(rsesema_1))/7,
              .groups = "keep")
}