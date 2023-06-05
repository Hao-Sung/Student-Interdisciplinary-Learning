library(tidyverse)

# read metadata
sep = .Platform$file.sep
.department_code = read.table(
  paste(c(
    "..", "data", "code", "department code.csv"
  ), collapse = sep),
  sep = ",",
  header = TRUE,
  fileEncoding = "BIG5"
)

# read metadata stored in "list" format
source(paste(c("..", "data", "code", "college_code.R"), collapse = sep))
source(paste(c(
  "..", "data", "course", "compulsory_sub.R"
), collapse = sep))


.get_bachelor = function(data) {
  #' get college students' data only
  valid_status = c("註冊(在學)", "畢業")
  is_valid = data$在學狀態 %in% valid_status
  is_bachelor = grepl("學士", data$系名稱)
  
  res = data[(is_valid & is_bachelor), ]
  return(res)
}


.validate = function(data) {
  #' validate the data
  data$課程名稱 = gsub("\\（.+\\）|\\(.+\\)", "",  data$課程名稱)
  
  ge_code = college_codes$`通識教育`
  res = data %>%
    filter(系名稱 != "醫學系學士班") %>%
    filter(院名稱 != "全校不分系學士學位學程") %>%
    filter(課程名稱 != "操行" , 課程名稱 != "服務學習") %>%
    filter(課程名稱 != "微積分", 課程名稱 != "專題討論") %>%
    filter(課程名稱 != "體育") %>%
    filter(課程成績 >= 60, 課程成績 <= 100) %>%
    filter((substr(課程編號, 1, 2) %in% unlist(college_codes)))
  return(res)
}

.get_interdiscip = function(data) {
  #' get interdisciplinary learning data based on course number
  college_name = unique(data$院名稱)
  college_code = unlist(college_codes[college_name])
  res = data[!(substr(data$課程編號, 1, 2) %in% college_code),]
  return(res)
}


.delete_compulsory = function(data) {
  #' delete compulsory subjects from interdisciplinary learning data
  college_name = unique(data$院名稱)
  college_comps = unlist(compulsory_sub[college_name])
  if (is.null(college_comps)) {
    return(data)
  } else {
    target = data$課程名稱
    row_num = dim(data)[1]
    criteria = rep(0, row_num)
    for (comp in college_comps) {
      is_compulsory = grepl(comp, target)
      criteria = criteria + is_compulsory
    }
    res = data[criteria == 0, ]
    return(res)
  }
}

.add_course_info_c = function(course_code) {
  #' add college of the course
  code = substr(course_code, 1, 2)
  if (code  %in% college_codes$`工學院`) {
    return("工學院")
  }
  else if (code  %in% college_codes$`醫學院`) {
    return("醫學院")
  }
  else if (code  %in% college_codes$`電機資訊學院`) {
    return("電機資訊學院")
  }
  else if (code  %in% college_codes$`管理學院`) {
    return("管理學院")
  }
  else if (code  %in% college_codes$`理學院`) {
    return("理學院")
  }
  else if (code  %in% college_codes$`社會科學院`) {
    return("社會科學院")
  }
  else if (code  %in% college_codes$`規劃與設計學院`) {
    return("規劃與設計學院")
  }
  else if (code  %in% college_codes$`文學院`) {
    return("文學院")
  }
  else if (code  %in% college_codes$`生物科學與科技學院`) {
    return("生物科學與科技學院")
  }
  else {
    stop("course code out of valid range")
  }
}

# .get_department_code = function(data) {
#   lookup = c("開課系所代碼" = "系代碼",
#              "開課系所名稱" = "系名稱")
#   department_codes = data %>%
#     select("系名稱" , "系代碼") %>%
#     distinct() %>%
#     filter(!grepl("博士", 系名稱)) %>%
#     filter(!grepl("在職專班", 系名稱)) %>%
#     rename(all_of(lookup))
#   return(department_codes)
# }


.add_course_info_d = function(data) {
  #' add department/institute office of course
  res = data %>%
    mutate("開課代碼" = substr(課程編號, 1, 2)) %>%
    left_join(.department_code, by = join_by(開課代碼 == 開課系所代碼)) %>%
    select(-開課代碼)
  
  is_dep_exist = is.na(res$開課系所名稱)
  res$開課系所名稱[is_dep_exist] = res$開課單位[is_dep_exist]
  return(res)
}

get_interdiscip = function(data) {
  #' get interdisciplinary learning data of college students
  #'
  #' @param data A dataframe of students' course record over four years
  #' @return Returns a list containing following components:
  #'  - `inter` interdisciplinary learning data,
  #'  - `college` course record of college students
  #'  - `freq` frequency
  bachelor_data = data %>%
    .get_bachelor() %>%
    .validate() %>%
    rowwise() %>%
    mutate(開課單位 = .add_course_info_c(課程編號)) %>%
    ungroup() %>%
    .add_course_info_d()
  
  interdiscip_data = bachelor_data %>%
    group_by(院名稱) %>%
    do(.get_interdiscip(.)) %>%
    do(.delete_compulsory(.)) %>%
    ungroup()
  
  student_ls = bachelor_data %>%
    select(學號, 院名稱) %>%
    distinct() %>%
    tibble()
  
  interdiscip_freq = interdiscip_data %>%
    group_by(學號, 院名稱) %>%
    count(name = "個人跨域次數") %>%
    mutate("是否跨域" = "Y") %>%
    right_join(student_ls, by = c("學號", "院名稱")) %>%
    ungroup()
  
  interdiscip_freq$個人跨域次數[is.na(interdiscip_freq$個人跨域次數)] = 0
  interdiscip_freq$是否跨域[is.na(interdiscip_freq$是否跨域)] = "N"
  
  return(list(
    inter = interdiscip_data,
    freq = interdiscip_freq,
    college = bachelor_data
  ))
}

get_general_edu = function(data) {
  ge_data = data %>%
    .get_bachelor() %>%
    filter(課程成績 >= 60, 課程成績 <= 100) %>%
    filter((substr(課程編號, 1, 2) == "A9"))
  return(ge_data)
}

get_course_info = function(file_vec) {
  info_list = lapply(
    file_vec,
    read.csv,
    sep = ",",
    header = TRUE,
    fill = TRUE,
    strip.white = TRUE,
    fileEncoding = "BIG5"
  ) %>%
    map(
      ~ .x %>% select(
        "學年",
        "學期",
        "系所簡稱",
        "課程碼",
        "科目名稱",
        "選必修",
        "學分數",
        "選課人數.含合班.校際.",
        "限選人數",
        "教室最大容量"
      )
    )
  
  res = do.call("rbind", info_list) %>%
    filter(substr(課程碼, 1, 2) %in% unlist(college_codes))
  res$科目名稱 = gsub("\\（.+\\）|\\(.+\\)", "",  res$科目名稱)
  
  return(res)
}
