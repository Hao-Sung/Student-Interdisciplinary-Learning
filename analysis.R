# import necessary library
library(tidyverse)
library(ggrepel)
library(ggExtra)
library(viridis)
library(networkD3)
library(docstring)
library(encryptr)


# import scripts for pre-process, visualization and statistic analysis
sep = .Platform$file.sep
source(paste(c(".", "functionality", "preprocess.R"), collapse = sep), chdir = TRUE)
source(paste(c(".", "functionality", "summary.R"), collapse = sep), chdir = TRUE)
source(paste(c(".", "functionality", "graphic.R"), collapse = sep), chdir = TRUE)

# read students' course record
data = read.table(
  paste(c(".", "data", "student", "batch.csv"), collapse = sep),
  sep = ",",
  header = TRUE,
  fill = TRUE,
  strip.white = TRUE,
  fileEncoding = "BIG5"
)

# get interdisciplinary learning data
interdiscip = get_interdiscip(data)

# get general education data
general_edu = get_general_edu(data)

# read course information dataset
course_files = list.files(paste(c(".", "data", "course"), collapse = sep),
                          pattern = "*.csv",
                          full.names = TRUE)
course_info = get_course_info(course_files)



# Task.1: Exploratory data analysis
# ====================================================================

# Bar chart
plot_freq_dist(interdiscip)
plot_freq_dist(interdiscip, group_by_college = TRUE)

# Pie chart
plot_inter_ratio(interdiscip)
plot_inter_ratio(interdiscip, group_by_college = TRUE)


# Task.2: Most popular course in interdisciplinary learning
# ====================================================================

attractive_course = interdiscip$inter %>%
  group_by(開課系所名稱, 課程名稱, 課程編號) %>%
  select(院名稱) %>%
  distinct() %>%
  count(name = "修課院數") %>%
  arrange(desc(修課院數)) %>%
  ungroup()

view(attractive_course)


# Task.3: display students' course selection in interdisciplinary learning
# ====================================================================

# dataframe
flow = flow_table(interdiscip, regularize = TRUE)
view(flow)

# sankey plot
plot_sankey(interdiscip)


# Task.4: unpopular general education
# ====================================================================

unattractive_ge = general_edu %>%
  group_by(課程名稱) %>%
  select(院名稱) %>%
  distinct() %>%
  count(name = "修課院數") %>%
  ungroup() %>%
  arrange(修課院數) %>%
  filter(修課院數 >= 3)

view(unattractive_ge)


# Task.5: time effect in interdisciplinary learning
# ====================================================================

# test of independence
tbl = contengency_table(interdiscip)
chisq_test = chisq.test(tbl)
chisq_test

# Time Based Heatmap
interdiscip %>% time_based_interdiscip() %>% time_based_heatmap()


# Task.6: student performance in the given course
# ====================================================================

dep = "心理學系學士班"
course = "心理學概論"
stu_performance(interdiscip, dep, course)
# todo: bar chart

# Task.7: the more popular a course is,
#         the more likely that student number exceeds its classroom capacity
# ====================================================================

room_capacity_check(attractive_course, course_info)
# todo: bar chart

# Task.8: correlation between "required credits" and "interdisciplinary learning credits"
# ====================================================================


alloc = stu_credit_allocate(interdiscip, course_info)

outliner = filter(interdiscip$freq, 個人跨域次數 < 1)[["學號"]]
alloc = alloc %>%
  filter(!學號 %in% outliner)

with(alloc, cor(必修, 跨域學習, method = "pearson"))
with(alloc, cor(必修, 跨域學習, method = "spearman"))

p = ggplot(alloc, aes(x = 必修, y = 跨域學習)) +
  geom_point(colour = "#0c4c8a", size = 2) +
  geom_smooth(method = "loess",
              color = "#8B008B",
              size = 1) +
  labs(title = "跨域學習與必修學分 負相關", x = "必修學分", y = "跨域學習學分") +
  theme(plot.title = element_text(size = 14, face = "bold"))
graph_export(p, 4, 3, "required_cr_effect.png")
