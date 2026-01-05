# ----------------------------------------------------------
# Serves as key for the IFAN project. 
# (c) Luca A. Naudszus, Porto, 08.08.2023
# Institute for Psychology, University of Duisburg-Essen, Essen
# ----------------------------------------------------------
#
#
# Input: 
### tibble with data including id
#-----------
# Corrects and unifies all participant codes, removes duplicated data. 
#-----------
#-----------
# Output: 
### tibble with corrected (and unique) ids
# ---------------------------------------------------------

key <- function(data, unique = T){
  # unify all participant codes
  data <- data %>%
    mutate(id = str_to_upper(id)) %>% #capitalize all participant codes
    mutate(id = substr(id, 1, 6)) %>%
    mutate(id = case_when( #known misspellings of participant codes
      id == 'AD06 K' ~ 'AD06KA',
      id == 'AD12HK' ~ 'AD12HU',
      id == 'AE03T0' ~ 'AE03TO',
      id == 'AE08T0' ~ 'AE08TO',
      id == 'AJ19TO' ~ 'AJ10TO',
      id == 'AN06DR' ~ 'AN07DR',
      id == 'CR20BR' ~ 'CR10BR',
      id == 'DN12MN' ~ 'DN12MS',
      id == 'DN23MS' ~ 'DN12MS',
      id == 'EI11RC' ~ 'EI11CR',
      id == 'EK10GO' ~ 'EK01GO',
      id == 'EL11CR' ~ 'EI11CR',
      id == 'GB10TG' ~ 'GB10TO',
      id == 'HI06AE' ~ 'HL07AE',
      id == 'HN02RY' ~ 'HN05YS',
      id == 'HN02YR' ~ 'HN05YS',
      id == 'HN05YR' ~ 'HN05YS',
      id == 'HN06KK' ~ 'HN06KN',
      id == 'IE16MR' ~ 'IE07MR',
      id == 'JN05AM' ~ 'JN06AM',
      id == 'JS05KS' ~ 'JS05KR',
      id == 'KR8EI' ~ 'KR08EI',
      id == 'KR8EI ' ~ 'KR08EI',
      id == 'KR94SB' ~ 'KR04SB',
      id == 'KR09SF' ~ 'KR09SE',
      id == 'KR91DS' ~ 'KR01DS',
      id == 'LD19VK' ~ 'LD11VK',
      id == 'MA20JA' ~ 'MN09JA',
      id == 'MA09JA' ~ 'MN09JA',
      id == 'MH06HM' ~ 'HM06MH',
      id == 'MROISE' ~ 'MR12SE',
      id == 'NA04AD' ~ 'ND04AD',
      id == 'PT01  ' ~ 'PT01KU',
      id == 'PT01' ~ 'PT01KU',
      id == 'SB03CR' ~ 'SB02CR',
      id == 'SE01DT' ~ 'SE01DE',
      id == 'SMMR15' ~'SM01MR',
      id == 'SM15MR' ~ 'SM01MR',
      id == 'SN01 W' ~ 'SN01WL',
      id == 'SN11HN' ~ 'SM11HN',
      id == 'SSJH09' ~ 'SS09JH',
      id == 'TN10HL!' ~ 'TN10HL', 
      id == 'UA14TO' ~ 'UA12TO',
      id == 'UR07PL' ~ 'UR07RL',
      id == 'WB01MC' ~ 'WB02MC',
      id == 'WN02MC' ~ 'WB02MC',
      TRUE ~ id
    ))
  # remove duplicated data
  if (unique){
    data <- data[!(duplicated(data$id, fromLast = T)),] #in case of multiple entries by code, remove previous ones and retain last entry
  }
  return(data)
}