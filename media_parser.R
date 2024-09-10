library(tidyverse) # Import tidyverse for readr, pipes, and dplyr
library(stringr) # Import stringr for string manipulation
library(writexl)
library(openxlsx)


get_media <- function(x) {
  x <- x %>% mutate(Name_of_Media = case_when(
    str_starts(text, "АиФ") ~ "АиФ - Москва",
    str_starts(text, "Известия") ~ "Известия",
    str_starts(text, "МК") ~ "МК",
    str_starts(text, "Российская газета") ~ "Российская газета",
    str_starts(text, "Комсомольская правда") ~ "Комсомольская правда",
    TRUE ~ "Something is wrong here" 
  ))
  return(x)
} # Function for getting the name of publication from a string

get_date <- function(x){
  x <- x %>% mutate(Date = str_extract(text, "\\d{2}\\.\\d{2}\\.\\d{4}"))
  return(x)
} # Function for getting date of publication of an article

get_title <- function(x){
  x <- x %>% mutate(Title = str_extract(text, "(?<=Заглавие:).*(?=\r\n)"))
  x <- x %>% mutate(Title = trimws(Title, "left"))
  return(x)
}


get_df <- function(string){
  path <- paste0("/Users/mac12/Downloads/ЮАР/final/", string, ".txt")
  lines <- read_file(path)
  split_strings <- strsplit(lines, "Источник:")
  return(split_strings)
}

add_df_rows <- function(countries, df){
  for (country in countries){
    for (string in get_df(country)) {
      string = trimws(string, "left")
      df = df %>% add_row(text = string)
    }
    print(nrow(df))
  }
  return(df)
}

check_countries <- function(x){ 
  x <- x %>% mutate(Serbia = if_else(str_detect(text, regex("Серби", ignore_case = TRUE)), 1, 0))
  x <- x %>% mutate(SCO = if_else(str_detect(text, regex("ШОС", ignore_case = TRUE)), 1, 0))
  x <- x %>% mutate(Pakistan = if_else(str_detect(text, regex("Пакист", ignore_case = TRUE)), 1, 0))
  x <- x %>% mutate(Singapore = if_else(str_detect(text, regex("Сингапур", ignore_case = TRUE)), 1, 0))
  x <- x %>% mutate(South_Africa =if_else(str_detect(text, regex("ЮАР", ignore_case = TRUE)), 1, 0))
}
get_rubric <- function(df){
  df <- df %>% mutate(rubric = str_extract(text, "(?<=Рубрика:).*(?=\r\n)"))
  df <- df %>% mutate(rubric = trimws(rubric, "both"))
  df <- df %>% mutate(rubric = tolower(rubric))
  df <- df %>% mutate(rubric2 = sapply(strsplit(rubric, "\\."), function(x) x[1]),
                      rubric3 = sapply(strsplit(rubric, "\\."), function(x) x[2]))
  
  return(df)
}


prepare_dataset <- function(countries, df){
  df <- add_df_rows(countries, df)
  df <- df %>% filter(text != "")
  df <- df %>% get_title()
  df <- df %>% get_media()
  df <- df %>% get_date()
  df <- df %>% get_rubric()
  df <- df %>% mutate(rubric2 = trimws(tolower(rubric2)))
  
  df <- df %>% mutate(text = case_when(
    Name_of_Media == "АиФ - Москва" ~ sapply(strsplit(text, "\r\n \r\n"), function(x) x[2]),
    Name_of_Media == "Известия" ~ sapply(strsplit(text, "\r\n\r\n"), function(x) x[2]),
    Name_of_Media == "МК" ~ sapply(strsplit(text, "Заглавие:"), function(x) x[2]),
    Name_of_Media == "Российская газета" ~ sapply(strsplit(text, "\r\n \r\n"), function(x) x[2]),
    Name_of_Media == "Комсомольская правда" ~ sapply(strsplit(text, "\r\n \r\n"), function(x) x[2]),
    TRUE ~ text
  ))
  
  df <- df %>% mutate(text = case_when(
    Name_of_Media == "АиФ - Москва" ~ sapply(strsplit(text, "\\*\\*\\*"), function(x) x[1]),
    Name_of_Media == "МК" ~ if_else(str_detect(text, Title), sapply(strsplit(text,  Title), function(x) x[2]), text),
    TRUE ~ text
  ))
  df <- df %>% mutate(text = trimws(text), Title =  trimws(Title))
  df <- df %>% mutate(length = nchar(text), truncated = 0)
  df <- df %>% mutate(Observation = str_detect(text, lead(Title)))
  df <- df %>% mutate(Link = lead(Title))
  df <- df %>% mutate(text = if_else(Observation == FALSE | is.na(Observation), text, sapply(str_split(text, Link), function(x) x[1])))
  df <- df %>% distinct(text, .keep_all = TRUE)
  df <- df %>% distinct(Name_of_Media, Title, Date, .keep_all = TRUE)
  df <- df %>% mutate(last_line = case_when(
    !is.na(str_extract(text, "(?<=\\r\\n).*$")) | str_extract(text, "(?<=\\r\\n).*$")  != " " ~ str_extract(text, "(?<=\\r\\n).*$"),
    TRUE ~ str_extract(text, "(?<=\\r\\n)[^\r\n]*(?=\\r\\n[^\r\n]*$)")
    )
  )
  df <- df %>% mutate(text = trimws(text), Title =  trimws(Title))
  patterns = c("Полный текст интервью читайте на iz.ru", "Полный текст читайте на iz.ru")
  df <- df %>% mutate(text = str_remove_all(text, paste0(patterns, collapse = "|")))
  df <- df %>% mutate(text = case_when(
    str_detect(last_line, regex("подготовил", ignore_case = TRUE)) ~ str_remove(text, paste0(".*", "Подготовил", ".*")),
    TRUE ~ text)
    )
  df <- df %>% mutate(text = trimws(text), Title =  trimws(Title))
  return(df)
}



df <- tibble(text = character()) %>% mutate(Observation = NA, Name_of_Media = NA, Title = NA, Link = NA, 
                    Date = NA, topics = NA,
                    Serbia = 0, SCO = 0, Pakistan = 0, Singapore = 0, South_Africa = 0) # Create column names
df_brazil <- df 
df_argentina <- df
df_all <- df

countries2 = c("Serbia", "SCO", "Pakistan", "Singapore", "South_Africa")




df_all <- prepare_dataset(countries2, df_all)


df_all %>% mutate(Observation = str_detect(text, lead(Title)))




clean_names <- function(df_all){
  df_all_small <- df_all %>% filter(Name_of_Media == "Комсомольская правда" & str_detect(last_line, "^[А-Я]"))
  df_all_small <- df_all_small %>% mutate(text = str_remove(text, last_line))
  df_all <- df_all %>% filter(Name_of_Media != "Комсомольская правда" | !str_detect(last_line, "^[А-Я]"))
  df_all <- df_all %>% bind_rows(df_all_small)
  return(df_all)
}

df_all <- clean_names(df_all) 

df_all <- df_all %>% mutate(new_last_line = case_when(
  !is.na(str_extract(text, "(?<=\\r\\n).*$")) | str_extract(text, "(?<=\\r\\n).*$")  != " " ~ str_extract(text, "(?<=\\r\\n).*$"),
  TRUE ~ str_extract(text, "(?<=\\r\\n)[^\r\n]*(?=\\r\\n[^\r\n]*$)")
)
)

df_all <- df_all %>% filter(length < 30000)
df_all <- df_all %>% filter(!str_detect(Title, "Официальные курсы валют") & !str_detect(Title, "АНОНСЫ") & Title != "Кинообзор" & Title != "Датский уголок")

df_mull <- df_all %>% filter(rubric2 %in% c("светская жизнь", "культура", "регион"))

# df_all <- df_all %>% check_countries
# df <- df %>% mutate(Link = lead(Title))
# df <- df %>% mutate(Observation = str_detect(text, lead(Title)))
# df <- df %>% mutate(text = if_else(Observation == FALSE | is.na(Observation), text, sapply(str_split(text, Link), function(x) x[1])))
# df <- df %>% mutate(Observation = str_detect(text, lead(Title)))
# df_all <- df_all %>% mutate(Observation = NA, Link = NA) %>% relocate(text, .after = Observation) %>% 
#   rownames_to_column() %>% rename(quatsch = Observation, Observation = rowname) %>% mutate(Observation = as.integer(Observation), Topics = NA,
#                                                                                            neutral = NA, negative = NA, positive = NA) %>%  select(-quatsch)
df_all <- df_all %>% mutate(Observation = NA)

# Filter out rows where str_detect encounters an error
df_all <- df_all %>% filter(is.na(lead(Title)) | !is.na(str_detect(text, lead(Title))))
df_all <- df_all %>% filter(length < 32000 & length > 5)

output_file <- "/parsed_articles.xlsx"
write_xlsx(df_all, output_file)
