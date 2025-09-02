# 安装和加载必要的包
if(!require(survival)) install.packages("survival")
if(!require(survminer)) install.packages("survminer")
if(!require(flextable)) install.packages("flextable")
if(!require(officer)) install.packages("officer")
if(!require(broom)) install.packages("broom")
if(!require(dplyr)) install.packages("dplyr")
library(survival)
library(survminer)
library(flextable)
library(officer)
library(broom)
library(dplyr)

# 全局P值格式化函数
format_p_value <- function(p) {
  if(is.na(p)) return(NA_character_)
  if(p < 0.001) "< 0.001" else sprintf("%.3f", p)
}

# 重新定义提取函数
extract_cox_results <- function(cox_model, is_continuous = FALSE) {
  if(is.null(cox_model)) {
    if(is_continuous) {
      return(data.frame(
        Exposure = "Continuous variable per 0.01 unit",
        HR_CI = NA_character_,
        P_Value = NA_character_
      ))
    } else {
      return(data.frame(
        Exposure = c("Q1", "Q2", "Q3", "Q4"),
        HR_CI = c("Ref", NA_character_, NA_character_, NA_character_),
        P_Value = c(NA_character_, NA_character_, NA_character_, NA_character_)
      ))
    }
  }
  
  # 提取系数和置信区间
  coefs <- exp(coef(cox_model))
  conf_int <- exp(confint(cox_model))
  
  if(is_continuous) {
    hr_ci <- sprintf("%.2f (%.2f, %.2f)", 
                     coefs[1], conf_int[1, 1], conf_int[1, 2])
    p_value <- summary(cox_model)$coefficients[1, "Pr(>|z|)"]
    
    data.frame(
      Exposure = "Continuous variable per 0.01 unit",
      HR_CI = hr_ci,
      P_Value = format_p_value(p_value)
    )
  } else {
    hr_ci <- rep(NA_character_, 4)
    p_values <- rep(NA_character_, 4)
    
    hr_ci[1] <- "Ref"
    
    n_coefs <- length(coefs)
    if(n_coefs > 0) {
      for(i in 1:min(n_coefs, 3)) {
        hr_ci[i+1] <- sprintf("%.2f (%.2f, %.2f)", 
                              coefs[i], conf_int[i, 1], conf_int[i, 2])
        p_values[i+1] <- format_p_value(summary(cox_model)$coefficients[i, "Pr(>|z|)"])
      }
    }
    
    data.frame(
      Exposure = c("Q1", "Q2", "Q3", "Q4"),
      HR_CI = hr_ci,
      P_Value = p_values
    )
  }
}

# 主分析流程
analyze_cox_models <- function() {
  
  # 定义需要分析的多个时间点
  testcd_list <- c("HOSPXXD", "ICU28D")
  
  # 创建空结果表
  results_table <- data.frame(
    Exposure = character(),
    Crude_HR_CI = character(),
    Crude_P = character(),
    Model1_HR_CI = character(),
    Model1_P = character(),
    Model2_HR_CI = character(),
    Model2_P = character(),
    stringsAsFactors = FALSE
  )
  
  # 遍历每个时间点数据集
  for (testcd in testcd_list) {
    # 选择数据并合并 - 修复关键错误
    tte_selected <- tte %>%
      mutate(leveln=as.numeric(leveln))%>% #注意这个变量需要为数值连续型，可能前面因子化影响到了
      filter(testcd == !!testcd)  # 使用!!取消引用
    
    # 如果数据集为空，跳过当前时间点
    if(nrow(tte_selected) == 0) {
      warning(paste("No data available for testcd:", testcd))
      next
    }
  
    # 设置协变量
    covariates_m1 <- metadata$variable_name[metadata$covars_m1 == 1]
    covariates_m2 <- metadata$variable_name[metadata$covars_m2 == 1]
    
    
    # 1. 连续变量模型 ----
    cox_crude_cont <- tryCatch({
      coxph(Surv(aval, cnsr) ~ flab100, data = tte_selected)
    }, error = function(e) NULL)
    
    cox_m1_cont <- tryCatch({
      if(length(covariates_m1) > 0) {
        cox_formula <- as.formula(paste("Surv(aval, cnsr) ~ flab100 +", 
                                        paste(covariates_m1, collapse = "+")))
      } else {
        cox_formula <- as.formula("Surv(aval, cnsr) ~ flab100")
      }
      coxph(cox_formula, data = tte_selected)
    }, error = function(e) NULL)
    
    cox_m2_cont <- tryCatch({
      if(length(covariates_m2) > 0) {
        cox_formula <- as.formula(paste("Surv(aval, cnsr) ~ flab100 +", 
                                        paste(covariates_m2, collapse = "+")))
      } else {
        cox_formula <- as.formula("Surv(aval, cnsr) ~ flab100")
      }
      coxph(cox_formula, data = tte_selected)
    }, error = function(e) NULL)
    
    # 2. 分类变量模型 ----
    cox_crude_cat <- tryCatch({
      coxph(Surv(aval, cnsr) ~ level, data = tte_selected)
    }, error = function(e) NULL)
    
    cox_m1_cat <- tryCatch({
      if(length(covariates_m1) > 0) {
        cox_formula <- as.formula(paste("Surv(aval, cnsr) ~ level +", 
                                        paste(covariates_m1, collapse = "+")))
      } else {
        cox_formula <- as.formula("Surv(aval, cnsr) ~ level")
      }
      coxph(cox_formula, data = tte_selected)
    }, error = function(e) NULL)
    
    cox_m2_cat <- tryCatch({
      if(length(covariates_m2) > 0) {
        cox_formula <- as.formula(paste("Surv(aval, cnsr) ~ level +", 
                                        paste(covariates_m2, collapse = "+")))
      } else {
        cox_formula <- as.formula("Surv(aval, cnsr) ~ level")
      }
      coxph(cox_formula, data = tte_selected)
    }, error = function(e) NULL)
    
    # 3. 趋势检验 ----
    p_trend0 <- tryCatch({
      cox_trend <- coxph(Surv(aval, cnsr) ~ leveln, data = tte_selected)
      summary(cox_trend)$coefficients["leveln", "Pr(>|z|)"]
    }, error = function(e) NA)
    
    p_trend1 <- tryCatch({
      if(length(covariates_m1) > 0) {
        trend_formula <- as.formula(paste("Surv(aval, cnsr) ~ leveln +", 
                                          paste(covariates_m1, collapse = "+")))
      } else {
        trend_formula <- as.formula("Surv(aval, cnsr) ~ leveln")
      }
      cox_trend <- coxph(trend_formula, data = tte_selected)
      summary(cox_trend)$coefficients["leveln", "Pr(>|z|)"]
    }, error = function(e) NA)
    
    p_trend2 <- tryCatch({
      if(length(covariates_m2) > 0) {
        trend_formula <- as.formula(paste("Surv(aval, cnsr) ~ leveln +", 
                                          paste(covariates_m2, collapse = "+")))
      } else {
        trend_formula <- as.formula("Surv(aval, cnsr) ~ leveln")
      }
      cox_trend <- coxph(trend_formula, data = tte_selected)
      summary(cox_trend)$coefficients["leveln", "Pr(>|z|)"]
    }, error = function(e) NA)
    
    # 提取所有结果
    cont_crude <- extract_cox_results(cox_crude_cont, TRUE)
    cont_m1 <- extract_cox_results(cox_m1_cont, TRUE)
    cont_m2 <- extract_cox_results(cox_m2_cont, TRUE)
    
    cat_crude <- extract_cox_results(cox_crude_cat)
    cat_m1 <- extract_cox_results(cox_m1_cat)
    cat_m2 <- extract_cox_results(cox_m2_cat)
    
    # 创建当前时间点的结果表
    current_results <- data.frame(
      Exposure = c(cont_crude$Exposure, cat_crude$Exposure),
      Crude_HR_CI = c(cont_crude$HR_CI, cat_crude$HR_CI),
      Crude_P = c(cont_crude$P_Value, cat_crude$P_Value),
      Model1_HR_CI = c(cont_m1$HR_CI, cat_m1$HR_CI),
      Model1_P = c(cont_m1$P_Value, cat_m1$P_Value),
      Model2_HR_CI = c(cont_m2$HR_CI, cat_m2$HR_CI),
      Model2_P = c(cont_m2$P_Value, cat_m2$P_Value)
    )
    
    # 添加趋势检验行
    trend_results <- data.frame(
      Exposure = "P for trend",
      Crude_HR_CI = NA_character_,
      Crude_P = format_p_value(p_trend0),
      Model1_HR_CI = NA_character_,
      Model1_P = format_p_value(p_trend1),
      Model2_HR_CI = NA_character_,
      Model2_P = format_p_value(p_trend2)
    )
    
    current_results <- rbind(current_results, trend_results)
    
    # 添加自定义标题行
    title_text <- ifelse(grepl("^HOSP", testcd),
                         "In-hospital mortality",
                         paste0(gsub("ICU([0-9]+)D", "\\1", testcd), 
                                "-day all-cause mortality"))
    
    title_row <- data.frame(
      Exposure = title_text,
      Crude_HR_CI = "Crude model",
      Crude_P = "",
      Model1_HR_CI = "Model 1",
      Model1_P = "",
      Model2_HR_CI = "Model 2",
      Model2_P = ""
    )
    
    # 添加到总结果表
    results_table <- rbind(
      results_table,
      title_row,
      current_results,
      data.frame(Exposure = "", 
                 Crude_HR_CI = "",
                 Crude_P = "",
                 Model1_HR_CI = "",
                 Model1_P = "",
                 Model2_HR_CI = "",
                 Model2_P = "")
    )
  }
  
  return(results_table)
}

# 执行分析
results_table <- analyze_cox_models()

# 创建并格式化表格
create_cox_table <- function(results) {
  # 移除最后的空行
  results <- results[results$Exposure != "", ]
  
  ft <- flextable(results) %>%
    # 设置表格标题
    set_caption(caption = "Table 3 Cox proportional hazard models for in-hospital and 28-Day ICU mortality",
                style = "Table Caption") %>% 
    #2025/06/14 之前可以运行，这次运行失败
    # font(fontname = "Times New Roman", part = "all") %>%  # 学术字体
    # 基本样式
    theme_booktabs() %>%
    # #2025/06/14 之前可以运行，这次运行失败
    # font(fontname = "Times New Roman", part = "all") %>%  # 学术字体
    fontsize(size = 10, part = "all") %>%      # 统一字号
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left") %>%
    set_table_properties(layout = "autofit") %>%
    # 设置列名
    set_header_labels(
      Exposure = "Exposure",
      Crude_HR_CI = "HR (95% CI)",
      Crude_P = "P-value",
      Model1_HR_CI = "HR (95% CI)",
      Model1_P = "P-value",
      Model2_HR_CI = "HR (95% CI)",
      Model2_P = "P-value"
    ) %>%
    # 添加模型名称行
    add_header_row(values = c("", "Crude Model", "Model 1", "Model 2"),
                   colwidths = c(1, 2, 2, 2)) %>%
    # 设置页眉格式
    bold(part = "header") %>%
    
    # 自动调整列宽
    autofit()
  
  # 标识标题行和趋势行
  title_rows <- which(grepl("mortality", results$Exposure))
  trend_rows <- which(results$Exposure == "P for trend")
  
  # 逐个处理标题行合并
  for (row in title_rows) {
    ft <- merge_at(ft, i = row, j = 1:7)
  }
  
  # 设置格式
  ft <- ft %>%
    
    bold(i = title_rows) %>%
    italic(i = trend_rows) %>%
    bold(i = trend_rows)
  
  return(ft)
}

# 创建表格
cox_table <- create_cox_table(results_table)

# 保存为Word文档
save_as_docx(cox_table, 
             path = "Table 3.docx", 
             pr_section = prop_section(page_size = page_size(orient = "landscape")))

# 显示表格
cox_table