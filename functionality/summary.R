library(tidyverse)

flow_long_table = function(interdiscip) {
  inter = interdiscip$inter
  flow_data = inter %>%
    group_by(開課單位, 課程名稱, 院名稱) %>%
    count(name = "修課人數") %>%
    group_by(院名稱, 開課單位) %>%
    mutate(修課次數 = sum(修課人數)) %>%
    ungroup() %>%
    select(院名稱, 開課單位, 修課次數) %>%
    distinct() %>%
    arrange(desc(院名稱))
  
  return(flow_data)
}

flow_table = function(interdiscip, regularize = FALSE) {
  flow_data = interdiscip %>% flow_long_table()
  college_ls = names(college_codes)
  len = length(college_ls)
  res = matrix(0, len, len)
  for (i in 1:len) {
    for (j in 1:len) {
      if (i != j) {
        data = filter(flow_data, 院名稱 == college_ls[i], 開課單位 == college_ls[j])
        value = data[["修課次數"]]
        if (length(value) != 0) {
          res[i, j] = value
        }
      }
    }
  }
  res = res %>% as.data.frame()
  colnames(res) = college_ls
  rownames(res) = college_ls
  if (regularize) {
    res = round(res / rowSums(res), 2)
  }
  
  return(res)
}

.interdiscip_degree = function(data_by_grade, total_stu_num) {
  get_row_num = function(data) {
    return(dim(data)[1])
  }
  zero = total_stu_num - get_row_num(data_by_grade)
  one_to_two = data_by_grade %>%
    filter(跨域學習次數 > 0, 跨域學習次數 < 3) %>%
    get_row_num()
  
  three_to_four = data_by_grade %>%
    filter(跨域學習次數 > 2, 跨域學習次數 < 5) %>%
    get_row_num()
  
  above_five = data_by_grade %>%
    filter(跨域學習次數 > 4) %>%
    get_row_num()
  
  res = c(
    "0堂" = zero,
    "1~2堂" = one_to_two,
    "3~4堂" = three_to_four,
    "5堂以上" = above_five
  ) %>%
    as_tibble_row()
  return(res)
}

contengency_table = function(interdiscip) {
  inter = interdiscip$inter
  freq = interdiscip$freq
  
  total_stu_num = dim(freq)[1]
  res = inter %>%
    filter(修課學年 < 108) %>%
    group_by(學號, 修課學年) %>%
    count(name = "跨域學習次數") %>%
    group_by(修課學年) %>%
    do(.interdiscip_degree(., total_stu_num)) %>%
    ungroup() %>%
    column_to_rownames(var = "修課學年")
  
  return(res)
}

.inter_degree_additional = function(data, stu_num) {
  college = unique(data$院名稱)
  school_year = unique(data$修課學年)
  row = c(院名稱 = college, 修課學年 = school_year)
  
  degree = c("1~2堂", "3~4堂", "5堂以上")
  data_degree = data$跨域程度
  if (length(data_degree) != 3) {
    miss_degree = degree[!degree %in% data_degree]
    for (d in miss_degree) {
      data = data %>% rbind(c(row, 跨域程度 = d, 人數 = 0))
    }
  }
  
  college_num = filter(stu_num, 院名稱 == college)[["總人數"]]
  inter_num = sum(as.integer(data$人數))
  no_inter_num = college_num - inter_num
  res = data %>% rbind(c(row, 跨域程度 = "0堂", 人數 = no_inter_num)) %>%
    mutate(人數 = as.integer(人數)) %>%
    mutate(比例 = 人數 / college_num)
  return(res)
}

time_based_interdiscip = function(interdiscip) {
  inter = interdiscip$inter
  college = interdiscip$college
  
  stu_num = college %>%
    group_by(院名稱) %>%
    select("學號") %>%
    distinct() %>%
    count(name = "總人數")
  
  res = inter %>%
    filter(修課學年 < 108) %>%
    group_by(院名稱, 學號, 修課學年) %>%
    count(name = "跨域學習次數") %>%
    mutate(跨域程度 = case_when(
      跨域學習次數 > 0 & 跨域學習次數 < 3 ~ "1~2堂",
      跨域學習次數 > 2 & 跨域學習次數 < 5 ~ "3~4堂",
      跨域學習次數 >= 5 ~ "5堂以上",
    )) %>%
    group_by(院名稱, 修課學年, 跨域程度) %>%
    count(name = "人數") %>%
    group_by(院名稱, 修課學年) %>%
    do(.inter_degree_additional(., stu_num)) %>%
    mutate(
      修課學年 = case_when(
        修課學年 == 104 ~ "104學年(大一)",
        修課學年 == 105 ~ "105學年(大二)",
        修課學年 == 106 ~ "106學年(大三)",
        修課學年 == 107 ~ "107學年(大四)",
      )
    )
  return(res)
}

stu_performance = function(interdiscip, department, course) {
  college = interdiscip$college
  course_data = college %>%
    filter(開課系所名稱 == department, 課程名稱 == course)
  if (dim(course_data)[1] == 0) {
    stop("Course can't be found in the given data.")
  }
  
  res = course_data %>%
    group_by(院名稱) %>%
    summarise(AVG = mean(課程成績),
              STD = sd(課程成績),
              N = n()) %>%
    filter(!is.na(STD))
  return(res)
}

room_capacity_check = function(attractive_course, course_info, degree = 3) {
  limit_info = course_info %>%
    group_by(課程碼) %>%
    summarise("是否超過最大容量" = mean(選課人數.含合班.校際.) >= mean(教室最大容量)) %>%
    ungroup()
  
  res = attractive_course %>%
    filter(修課院數 >= degree) %>%
    left_join(limit_info, by = join_by(課程編號 == 課程碼)) %>%
    group_by(修課院數) %>%
    summarise("超過最大容量-比例" = mean(是否超過最大容量)) %>%
    arrange(desc(修課院數))
  
  return(res)
}

.credit_transform = function(data) {
  cates = c("必修", "選修", "跨域學習")
  res = list()
  for (cate in cates) {
    value = filter(data, 選必修 == cate)[["總學分"]]
    value = if (length(value) == 0)
      0
    else
      value
    res[[cate]] = value
  }
  return(as_tibble(res))
}

.add_course_credit = function(data) {
  course_strict = course_info %>%
    select("學年", "學期", "課程碼", "選必修", "學分數") %>%
    distinct()
  course_moderate = course_info %>%
    group_by(課程碼) %>%
    summarise(選必修 = unique(選必修), 學分數 = mean(學分數)) %>%
    ungroup()
  
  res = data %>%
    left_join(course_strict , by = join_by(修課學年 == 學年, 修課學期 == 學期, 課程編號 ==
                                             課程碼)) %>%
    filter(!is.na(學分數))
  
  res_miss_credit = res %>% filter(is.na(學分數)) %>%
    select(-選必修, -學分數) %>%
    left_join(course_moderate, join_by(課程編號 == 課程碼)) %>%
    filter(!is.na(學分數))
  
  res_success = res %>% rbind(res_miss_credit)
  return(res_success)
}

stu_credit_allocate = function(interdiscip, course_info) {
  inter = interdiscip$inter %>% .add_course_credit()
  college = interdiscip$college %>% .add_course_credit()
  
  not_inter_long_form = college %>%
    group_by(修課學年, 修課學期, 學號, 院名稱) %>%
    anti_join(inter) %>%
    group_by(選必修, .add = TRUE) %>%
    summarise("總學分" = sum(學分數)) %>%
    ungroup()
  inter_long_form = inter %>%
    group_by(修課學年, 修課學期, 學號, 院名稱) %>%
    summarise("總學分" = sum(學分數)) %>%
    mutate(選必修 = "跨域學習") %>%
    ungroup()
  
  long_form = rbind(not_inter_long_form, inter_long_form)
  res = long_form %>%
    group_by(修課學年, 修課學期, 學號, 院名稱) %>%
    do(.credit_transform(.))
  
  return(res)
}