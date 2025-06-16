# 安装并加载必要的包
if(!require(survival)) install.packages("survival")
if(!require(survminer)) install.packages("survminer")
if(!require(dplyr)) install.packages("dplyr")
if(!require(flextable)) install.packages("flextable")
library(survival)
library(survminer)
library(dplyr)
library(flextable)
library(officer)
# 读取数据（确保已经处理了共线性问题）
# 读取数据testcd==ICU28D, HOSPXXD
data <-tte %>% filter(testcd=="HOSPXXD") %>%
  select(-all_of(c("Q1","Q2","Q3","subject_id","stay_id","hadm_id","total_number","total_deficit", "flab")))

# 准备进行Cox模型的变量, Model 2
predictor_vars <- metadata$variable_name[metadata$covars_m2 == 1]

# 创建Cox回归的公式
cox_formula <- as.formula(paste("Surv(aval, cnsr) ~", paste(predictor_vars, collapse = " + ")))

# 拟合Cox模型
cox_model <- coxph(cox_formula, data = data)

# --------------------------------
#  检查比例风险假定
# --------------------------------

ph_test <- cox.zph(cox_model)
# print(ph_test)

# 自动化生成检验报告
valid_vars <- rownames(ph_test$table)[rownames(ph_test$table) != "GLOBAL"]
ph_report <- data.frame(
  Variable = c(valid_vars, "GLOBAL"),
  Chisq = ph_test$table[, "chisq"],
  df = ph_test$table[, "df"],
  p_value = ph_test$table[, "p"],
  Significance = ifelse(ph_test$table[, "p"] < 0.05, "Violation", "Pass")
)

print(ph_report)

ft <- flextable(ph_report) %>%
  set_header_labels(
    chisq = "χ²", 
    df = "df",
    p = "p-value"
  ) %>%
  bg(part = "header", bg = "#1F4E79") %>%
  set_table_properties(layout = "autofit") %>%
  color(part = "header", color = "white")

# 保存为Word/HTML格式
save_as_docx(ft, path = "PH_Test_Results.docx")

# # 原始可视化Schoenfeld残差
# par(mfrow = c(2, 2))  # 设置2x2网格以显示多个图
# for (i in 1:length(ph_test$table[, 1])) {
#   plot(ph_test, var = i, main = paste("Schoenfeld Residuals for", names(ph_test$table[, 1])[i]))
#   abline(h = 0, col = "red", lty = 2)
# }
# par(mfrow = c(1, 1))  # 重置为单个图
# 
# # 使用survminer包提供的更美观的图形
# 但是得到警告信息:
#   In regularize.values(x, y, ties, missing(ties), na.rm = na.rm) :
#   折拢'x'成相互不同的值
# ggcoxzph(ph_test,ties = "ordered")

# --------------------------------
#  改用ggplot绘图并保存为矢量图形
# --------------------------------
valid_vars <- setdiff(rownames(ph_test$table), "GLOBAL")

# 创建图形列表（非ggcoxzph对象）
plot_list <- lapply(valid_vars, function(var) {
  
  # 提取残差数据
  resid_data <- data.frame(
    time = ph_test$time,
    resid = ph_test$y[, var],
    smooth = lowess(ph_test$time, ph_test$y[, var])$y
  )
  
  # 构建自定义ggplot对象
  ggplot(resid_data, aes(x = time)) +
    geom_point(aes(y = resid), alpha = 0.6, size = 2) +
    geom_line(aes(y = smooth), color = "red", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Schoenfeld Residuals:", var),
      subtitle = sprintf("χ² = %.2f (df=%d, p=%s)", 
                         ph_test$table[var, "chisq"],
                         ph_test$table[var, "df"],
                         ifelse(ph_test$table[var, "p"] < 0.001, 
                                "<0.001", 
                                sprintf("%.3f", ph_test$table[var, "p"]))),
      x = "Time", 
      y = "Scaled Schoenfeld Residuals"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
})

# 方法A：使用ggpubr（推荐）
ph_grid <- ggpubr::ggarrange(plotlist = plot_list, 
                             ncol = 2, nrow = ceiling(length(valid_vars)/2))

# 方法B：使用gridExtra（替代方案）
# ph_grid <- gridExtra::grid.arrange(grobs = plot_list, ncol = 2)

# --------------------------------
#  安全保存图形
# --------------------------------
ggsave("PH_Validation_Grid.svg", ph_grid,
       width = 25, 
       height = 10 * ceiling(length(valid_vars)/2),
       units = "cm", dpi = 600)

print(ph_grid)

# --------------------------------
#  检查全局测试结果
# --------------------------------

if (ph_test$table["GLOBAL", "p"] < 0.05) {
  cat("\n全局测试显示比例风险假定不成立 (p =", ph_test$table["GLOBAL", "p"], ")\n")
  
  # 识别违反PH假定的变量
  non_ph_vars <- names(ph_test$table[, "p"])[ph_test$table[, "p"] < 0.05]
  non_ph_vars <- non_ph_vars[non_ph_vars != "GLOBAL"]
  
  if (length(non_ph_vars) > 0) {
    cat("以下变量违反了比例风险假定：\n")
    print(non_ph_vars)
    
    cat("\n处理违反PH假定的方法：\n")
    cat("1. 分层Cox模型\n")
    cat("2. 使用时间依赖型协变量\n")
    cat("3. 使用扩展Cox模型\n")
    cat("4. 考虑使用其他生存分析模型，如AFT模型\n\n")
    
    # 示例：分层Cox模型
    if (length(non_ph_vars) == 1 && non_ph_vars %in% categorical_vars) {
      cat("示例方法1：对变量", non_ph_vars, "进行分层\n")
      
      # 创建分层公式
      strata_var <- non_ph_vars
      other_vars <- predictor_vars[!predictor_vars %in% strata_var]
      
      # 使用strata()函数创建分层Cox模型
      strata_formula <- as.formula(
        paste("Surv(time, event) ~", 
              paste(other_vars, collapse = " + "), 
              "+ strata(", strata_var, ")")
      )
      
      # 拟合分层Cox模型
      stratified_cox_model <- coxph(strata_formula, data = data)
      print(summary(stratified_cox_model))
      
      # 检查分层后的PH假定
      ph_test_stratified <- cox.zph(stratified_cox_model)
      print(ph_test_stratified)
      
      # 可视化残差
      ggcoxzph(ph_test_stratified)
      
      # 更新Cox模型为分层模型
      cox_model <- stratified_cox_model
    } 
    
    # 示例：时间依赖型协变量
    else {
      cat("示例方法2：将", non_ph_vars[1], "作为时间依赖型协变量\n")
      
      # 创建带有时间交互项的公式
      tt_var <- non_ph_vars[1]
      other_vars <- predictor_vars[!predictor_vars %in% tt_var]
      
      tt_formula <- as.formula(
        paste("Surv(time, event) ~", 
              paste(other_vars, collapse = " + "), 
              "+", tt_var,
              "+ tt(", tt_var, ")")
      )
      
      # 拟合带时间依赖型协变量的Cox模型
      tt_cox_model <- coxph(tt_formula, data = data)
      print(summary(tt_cox_model))
      
      # 更新Cox模型
      cox_model <- tt_cox_model
    }
  }
} else {
  cat("\n全局测试显示比例风险假定成立 (p =", ph_test$table["GLOBAL", "p"], ")\n")
  
  # 仍然检查是否有单个变量不满足PH假定
  non_ph_vars <- names(ph_test$table[, "p"])[ph_test$table[, "p"] < 0.05]
  non_ph_vars <- non_ph_vars[non_ph_vars != "GLOBAL"]
  
  if (length(non_ph_vars) > 0) {
    cat("但以下个别变量可能违反了比例风险假定：\n")
    print(non_ph_vars)
    cat("建议检查这些变量的Schoenfeld残差图并考虑处理方法\n")
  }
}