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
  filter(tableone == "Y") %>%
  arrange(order) %>%
  mutate(
    label = ifelse(is.na(label) | label == "", variable_name, label)
  )

# 准备数据
# 将分类变量转为因子
for (var in metadata_tableone$variable_name[metadata_tableone$flag == "Categorical"]) {
  imputed_data[[var]] <- factor(imputed_data[[var]])
}

# 设置变量标签
for (i in 1:nrow(metadata_tableone)) {
  var_name <- metadata_tableone$variable_name[i]
  var_label <- metadata_tableone$label[i]
  attr(imputed_data[[var_name]], "label") <- var_label
}

# 定义分组变量（如果需要按组比较）
group_var <- "level" 

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

# 创建表格对象 (保持您原有设置)
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

# 1. 使用compareGroups导出到CSV (兼容中文)
export2csv(table1, file = "Table1_raw.csv")

# 2. 读取CSV并进行数据清洗
custom_colnames <- c("variables", "Total", "Q1", "Q2", "Q3","Q4","P_value")
table_df <- read.csv("Table1_raw.csv", stringsAsFactors = FALSE, 
                     encoding = "UTF-8", col.names = custom_colnames) %>%
  mutate(
    variables = gsub(":.*$", "", variables),  # 删除冒号
    variables = case_when(
      trimws(variables) == "0" ~ "    No",
      trimws(variables) == "1" ~ "    Yes",
      trimws(variables) == "F" ~ "    Female",
      trimws(variables) == "M" ~ "    Male",
      TRUE ~ variables  # 保留其他不变
    )
  )


ft <- table_df %>%
  flextable() %>%
  
  # ===== 基础格式设置 =====
theme_booktabs() %>%                      # 专业三线表样式
  font(fontname = "Times New Roman", part = "all") %>%  # 学术字体
  fontsize(size = 10, part = "all") %>%      # 统一字号
  align(align = "center", part = "header") %>% # 表头居中
  align(j = 1, align = "left") %>%           # 首列左对齐
  
  # ===== 列宽优化 =====
set_table_properties(layout = "autofit") %>% 
  width(j = 1, width = 2.5) %>%             # 变量名列加宽
  hrule(rule = "exact")               # 固定行高


# 保存为Word文档
save_as_docx(ft, 
             path = "Table 1.docx", 
             pr_section = prop_section(page_size = page_size(orient = "landscape")))





