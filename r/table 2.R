# 安装和加载必要的包
if(!require(compareGroups)) install.packages("compareGroups")
if(!require(flextable)) install.packages("flextable")
if(!require(officer)) install.packages("officer")
if(!require(dplyr)) install.packages("dplyr")
library(compareGroups)
library(flextable)
library(officer)
library(dplyr)

# 准备元数据
metadata_tableone <- metadata %>%
  filter(table2 == "Y") %>%
  arrange(order) %>%
  mutate(
    label = ifelse(is.na(label) | label == "", variable_name, label)
  )

# 准备数据
# 将分类变量转为因子
for (var in metadata_tableone$variable_name[metadata_tableone$flag == "Categorical"]) {
  imputed_data[[var]] <- factor(imputed_data[[var]])
}

# 单独设置is_dead的因子水平（关键修改）
if ("is_dead" %in% names(imputed_data)) {
  imputed_data$is_dead <- factor(
    imputed_data$is_dead,
    levels = c(0, 1),
    labels = c("Survivor", "Non-survivor")
  )
}

# 设置变量标签
for (i in 1:nrow(metadata_tableone)) {
  var_name <- metadata_tableone$variable_name[i]
  var_label <- metadata_tableone$label[i]
  attr(imputed_data[[var_name]], "label") <- var_label
}

# 定义分组变量（修改为is_dead）
group_var <- "is_hosp_dead"  # 修改点

# 创建compareGroups对象
cg <- compareGroups(
  as.formula(paste(group_var, "~ .")),
  data = imputed_data[, c(group_var, metadata_tableone$variable_name)],
  method = NA,  # 自动选择方法
  alpha = 0.05,  # 显著性水平
  include.label = TRUE  # 包含变量标签
)

# 提取 hide = "0" 的变量名
xx0 <- metadata$variable_name[metadata$hide0 == "1" & !is.na(metadata$hide0)]
xx0 <- as.character(xx0)
hide_list <- setNames(rep("0", length(xx0)), xx0)

# 创建表格对象
table1 <- createTable(
  cg,
  show.all = TRUE,
  show.p.overall = TRUE,
  show.p.trend = FALSE,
  show.ratio = FALSE,
  hide.no = "no", hide = hide_list,
  digits = 2,
  digits.ratio = 2,
  sd.type = 2,
  q.type = c(1, 3)
)

# 修改列名以适应新分组（关键修改）
custom_colnames <- c("variables", "Total", "Survivor", "Non-survivor", "P_value")  # 修改点

# 导出和清洗数据
export2csv(table1, file = "Table2_raw.csv")
table_df <- read.csv("Table2_raw.csv", stringsAsFactors = FALSE, 
                     encoding = "UTF-8", col.names = custom_colnames) %>%
  mutate(
    variables = gsub(":.*$", "", variables),  # 删除冒号
    variables = case_when(
      trimws(variables) == "0" ~ "    No",
      trimws(variables) == "1" ~ "    Yes",
      trimws(variables) == "F" ~ "    Female",
      trimws(variables) == "M" ~ "    Male",
      TRUE ~ variables
    )
  )

# 创建flextable
ft <- table_df %>%
  flextable() %>%
  theme_booktabs() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left") %>%
  set_table_properties(layout = "autofit") %>% 
  width(j = 1, width = 2.5) %>%
  hrule(rule = "exact")

# 保存为Word文档
save_as_docx(ft, 
             path = "Table 2.docx", 
             pr_section = prop_section(page_size = page_size(orient = "landscape")))