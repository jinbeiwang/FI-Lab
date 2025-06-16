# 加载必要的包
if(!require(mice)) install.packages("mice")
if(!require(VIM)) install.packages("VIM")
if(!require(dplyr)) install.packages("dplyr")
if(!require(flextable)) install.packages("flextable")
if(!require(officer)) install.packages("officer")
if(!require(tidyr)) install.packages("tidyr")

library(mice)
library(VIM)
library(dplyr)
library(flextable)
library(officer)
library(tidyr)

# 查看数据结构
str(fi_lab)

# 读取数据（保持您原有的数据处理）
check_data <- fi_lab %>%
  select(-all_of(c("Q1","Q2","Q3","subject_id","stay_id","hadm_id","total_number","total_deficit", "flab","keep")))

# 检查缺失数据模式
summary(check_data)
md.pattern(check_data)

# 安全的可视化缺失数据（避免图形参数错误）
plot_missing_data <- function(data) {
  tryCatch({
    # 清理图形环境
    while (!is.null(dev.list())) dev.off()
    
    # 可视化缺失数据
    aggr_plot <- aggr(data, 
                      col=c('navyblue','red'), 
                      numbers=TRUE, 
                      sortVars=TRUE,
                      labels=names(data), 
                      cex.axis=0.7,
                      gap=3, 
                      ylab=c("Missing data pattern","Pattern"))
    
    return(aggr_plot)
  }, error = function(e) {
    cat("图形显示出现问题，跳过可视化步骤\n")
    cat("错误信息:", e$message, "\n")
    return(NULL)
  })
}

# 尝试生成缺失数据图
aggr_result <- plot_missing_data(check_data)

# 修复版：创建缺失率统计表函数
create_missing_table <- function(data) {
  # 计算总样本量
  total_n <- nrow(data)
  
  # 使用更简单的方法计算缺失情况
  missing_stats <- data.frame(
    Variable = names(data),
    Number_of_missing = sapply(data, function(x) sum(is.na(x))),
    stringsAsFactors = FALSE
  )
  
  # 计算缺失百分比
  missing_stats$Percent_of_missing <- round((missing_stats$Number_of_missing / total_n) * 100, 2)
  
  # 按缺失数量降序排列
  missing_stats <- missing_stats[order(missing_stats$Number_of_missing, decreasing = TRUE), ]
  
  # 重置行名
  rownames(missing_stats) <- NULL
  
  return(missing_stats)
}

# 生成缺失率表
missing_table <- create_missing_table(check_data)

# 显示前几行数据
cat("缺失率统计结果（前10行）：\n")
print(head(missing_table, 10))

# 创建包含变量类型的详细表格
create_detailed_missing_table <- function(data) {
  # 获取变量类型
  var_types <- sapply(data, function(x) {
    if(is.numeric(x)) {
      return("Continuous")
    } else if(is.factor(x) || is.character(x)) {
      return("Categorical")
    } else {
      return("Other")
    }
  })
  
  # 计算总样本量
  total_n <- nrow(data)
  
  # 创建详细统计表
  detailed_stats <- data.frame(
    Variable = names(data),
    Variable_Type = var_types,
    Number_of_missing = sapply(data, function(x) sum(is.na(x))),
    stringsAsFactors = FALSE
  )
  
  # 计算缺失百分比
  detailed_stats$Percent_of_missing <- round((detailed_stats$Number_of_missing / total_n) * 100, 2)
  
  # 按缺失数量降序排列
  detailed_stats <- detailed_stats[order(detailed_stats$Number_of_missing, decreasing = TRUE), ]
  
  # 重置行名
  rownames(detailed_stats) <- NULL
  
  return(detailed_stats)
}

# 创建三线表格式函数
create_three_line_table <- function(missing_data, include_type = FALSE) {
  
  if(include_type) {
    # 包含变量类型的版本
    ft <- flextable(missing_data) %>%
      set_header_labels(
        Variable = "Variable",
        Variable_Type = "Type", 
        Number_of_missing = "Number of missing",
        Percent_of_missing = "Percent of missing (%)"
      ) %>%
      width(j = 1, width = 2.0) %>%
      width(j = 2, width = 1.2) %>%
      width(j = 3, width = 1.5) %>%
      width(j = 4, width = 1.8) %>%
      align(align = "left", j = 1:2, part = "body") %>%
      align(align = "center", j = 3:4, part = "body")
  } else {
    # 基础版本
    ft <- flextable(missing_data) %>%
      set_header_labels(
        Variable = "Variable",
        Number_of_missing = "Number of missing", 
        Percent_of_missing = "Percent of missing (%)"
      ) %>%
      width(j = 1, width = 2.5) %>%
      width(j = 2, width = 1.8) %>%
      width(j = 3, width = 2.0) %>%
      align(align = "left", j = 1, part = "body") %>%
      align(align = "center", j = 2:3, part = "body")
  }
  
  # 通用格式设置
  ft <- ft %>%
    # 设置字体和大小
    font(fontname = "Times New Roman", part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    # 设置标题对齐
    align(align = "center", part = "header") %>%
    # 添加三线表边框
    border_remove() %>%
    hline_top(border = fp_border(color = "black", width = 2), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 1), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 2), part = "body") %>%
    # 设置行高
    height_all(height = 0.3)
  
  return(ft)
}

# 保存为Word文档的函数
save_to_word <- function(table, filename = "missing_rate_table.docx", table_title = "Table S3: Missing rate for demographics and clinical variables extracted from the database during the observation period.") {
  tryCatch({
    # 创建Word文档
    doc <- read_docx()
    
    # 添加表格标题
    doc <- doc %>%
      body_add_par(table_title, style = "Normal") %>%
      body_add_par("") %>%
      body_add_flextable(table) %>%
      body_add_par("") %>%
      body_add_par("Note: This table shows the missing data pattern for all variables included in the analysis.", 
                   style = "Normal")
    
    # 保存文档
    print(doc, target = filename)
    cat("✓ 表格已成功保存为:", filename, "\n")
    
  }, error = function(e) {
    cat("❌ 保存Word文档时出错:", e$message, "\n")
    cat("请检查是否已正确安装officer包\n")
    
    # 尝试保存为HTML格式作为备选
    tryCatch({
      html_filename <- gsub("\\.docx$", ".html", filename)
      save_as_html(table, html_filename)
    }, error = function(e2) {
      cat("HTML保存也失败了\n")
    })
  })
}

# HTML备选保存函数
save_as_html <- function(table, filename) {
  html_content <- htmltools_value(table)
  writeLines(html_content, filename)
  cat("✓ 已保存HTML版本:", filename, "\n")
}

# 生成基础版本表格
cat("\n=== 正在生成基础版缺失率表格 ===\n")
basic_table <- create_three_line_table(missing_table, include_type = FALSE)
print(basic_table)

# 生成详细版本表格
cat("\n=== 正在生成详细版缺失率表格 ===\n")
detailed_missing_table <- create_detailed_missing_table(check_data)
detailed_table <- create_three_line_table(detailed_missing_table, include_type = TRUE)
print(detailed_table)

# 保存为Word格式
cat("\n=== 正在保存Word文档 ===\n")
save_to_word(basic_table, "missing_rate_table_basic.docx")
save_to_word(detailed_table, "missing_rate_table_detailed.docx")

# 分析结果总结
cat("\n", rep("=", 50), "\n", sep = "")
cat("         缺失数据分析结果总结\n")
cat(rep("=", 50), "\n", sep = "")

total_vars <- ncol(check_data)
total_samples <- nrow(check_data)
high_missing_vars <- missing_table[missing_table$Percent_of_missing > 20, ]
zero_missing_vars <- missing_table[missing_table$Number_of_missing == 0, ]

cat("📊 数据基本信息:\n")
cat("   总样本量:", total_samples, "\n")
cat("   总变量数:", total_vars, "\n")
cat("   完全无缺失变量:", nrow(zero_missing_vars), "个\n")

if(nrow(high_missing_vars) > 0) {
  cat("\n⚠️  缺失率超过20%的变量 (", nrow(high_missing_vars), "个):\n")
  for(i in 1:nrow(high_missing_vars)) {
    cat("   ", i, ".", high_missing_vars$Variable[i], 
        ": ", high_missing_vars$Percent_of_missing[i], "%\n")
  }
  cat("\n💡 建议: 考虑是否需要移除这些变量或进行插补处理\n")
} else {
  cat("\n✅ 所有变量的缺失率都在20%以下\n")
}

# 显示缺失率分布
cat("\n📈 缺失率分布:\n")
missing_ranges <- cut(missing_table$Percent_of_missing, 
                      breaks = c(0, 5, 10, 15, 20, 100),
                      labels = c("0-5%", "5-10%", "10-15%", "15-20%", ">20%"),
                      include.lowest = TRUE)
missing_dist <- table(missing_ranges)
for(i in 1:length(missing_dist)) {
  cat("   ", names(missing_dist)[i], ":", missing_dist[i], "个变量\n")
}

cat("\n📁 生成的文件:\n")
cat("   1. missing_rate_table_basic.docx - 基础三线表\n")
cat("   2. missing_rate_table_detailed.docx - 包含变量类型的详细表\n")

cat("\n✨ 任务完成！\n")