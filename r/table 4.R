# 包管理：自动安装缺失的包
required_packages <- c("rms", "Hmisc", "survIDINRI", "purrr", "stringr","officer", "flextable", "dplyr")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if(length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# 设置全局种子确保可复现性
set.seed(123)

# # 配置参数
# time_point <- 28  # ICU28D时间点
# scores <- c("apsiii", "sapsii", "oasis", "sofa", "sirs", "gcs")  # 所有评分系统
# prediction_var <- "flab100"

# 主分析函数
analyze_models <- function(score, data,time_point,prediction_var) {
  # 1. 构建模型公式
  original_formula <- as.formula(paste("Surv(time, event) ~", score))
  enhanced_formula <- as.formula(paste("Surv(time, event) ~", score, "+", prediction_var))
  
  # 2. 配置数据环境
  ddist <- datadist(data)
  options(datadist = ddist)
  
  # 3. 拟合模型
  original_model <- cph(original_formula, data = data, x = TRUE, y = TRUE, surv = TRUE, time.inc = time_point)
  enhanced_model <- cph(enhanced_formula, data = data, x = TRUE, y = TRUE, surv = TRUE, time.inc = time_point)
  
  # 4. 修正C-index计算：使用0.5 + abs(dxy)/2
  calc_cindex <- function(model) {
    pred <- predict(model, type = "lp")
    dxy_obj <- rcorr.cens(pred, Surv(data$time, data$event))
    dxy <- dxy_obj["Dxy"]
    est <- 0.5 + abs(dxy)/2  # 修正后的C-index计算
    se <- dxy_obj["S.D."] / 2
    list(
      est = est,
      lower = max(0.5, est - 1.96 * se),  # 确保不低于0.5
      upper = min(1.0, est + 1.96 * se)   # 确保不高于1.0
    )
  }
  
  cindex_orig <- calc_cindex(original_model)
  cindex_enh <- calc_cindex(enhanced_model)
  
  # 5. 准备协变量矩阵
  covs0_matrix <- as.matrix(data[[score]])
  covs1_matrix <- as.matrix(data[, c(score, "flab100")])
  indat_matrix <- as.matrix(data[, c("time", "event")])
  
  # 6. 计算IDI和连续NRI
  IDI_NRI_results <- IDI.INF(
    indata = indat_matrix,
    covs0 = covs0_matrix,
    covs1 = covs1_matrix,
    t0 = time_point,
    npert = 200
  )
  
  # 7. 提取结果
  list(
    original = list(
      score = score,
      cindex = cindex_orig,
      label = paste0(toupper(score))
    ),
    enhanced = list(
      score = score,
      cindex = cindex_enh,
      IDI = c(
        est = IDI_NRI_results$m1[1] * 100,
        lower = IDI_NRI_results$m1[2] * 100,
        upper = IDI_NRI_results$m1[3] * 100
      ),
      NRI = c(
        est = IDI_NRI_results$m2[1] * 100,
        lower = IDI_NRI_results$m2[2] * 100,
        upper = IDI_NRI_results$m2[3] * 100
      ),
      IDI_pvalue = IDI_NRI_results$m1[4],
      NRI_pvalue = IDI_NRI_results$m2[4],
      label = paste0(toupper(score), " + FI-Lab")
    )
  )
}

# 数据处理和分析主函数
analyze_and_generate <- function(testcd, time_point) {
  # 配置参数
  scores <- c("apsiii", "sapsii", "oasis", "sofa", "sirs", "gcs")
  prediction_var <- "flab100"
  
  # 数据处理
  analysis_data <- tte %>%
    filter(testcd == !!testcd) %>%  # 动态过滤
    select(-any_of(c("Q1", "Q2", "Q3", "Q4"))) %>%
    mutate(flab100 = as.numeric(flab100)) %>% 
    filter(!is.na(flab100)) %>%
    mutate(time = as.numeric(aval),
           event = as.numeric(cnsr))
  
  # 执行分析
  results <- map(scores, ~ analyze_models(.x, analysis_data, time_point,prediction_var = prediction_var)) %>%
    map(~ list(original = .x$original, enhanced = .x$enhanced)) %>%
    flatten()
  
  # 生成结果表格
  results_df <- map_dfr(results, function(res) {
    if ("IDI" %in% names(res)) {
      data.frame(
        Model = res$label,
        C_index = sprintf("%.3f (%.3f, %.3f)", res$cindex$est, res$cindex$lower, res$cindex$upper),
        IDI = sprintf("%.2f%% (%.2f%%, %.2f%%)", res$IDI[1], res$IDI[2], res$IDI[3]),
        IDI_pvalue = ifelse(res$IDI_pvalue < 0.001, "< 0.001", sprintf("%.3f", res$IDI_pvalue)),
        NRI = sprintf("%.2f%% (%.2f%%, %.2f%%)", res$NRI[1], res$NRI[2], res$NRI[3]),
        NRI_pvalue = ifelse(res$NRI_pvalue < 0.001, "< 0.001", sprintf("%.3f", res$NRI_pvalue))
      )
    } else {
      data.frame(
        Model = res$label,
        C_index = sprintf("%.3f (%.3f, %.3f)", res$cindex$est, res$cindex$lower, res$cindex$upper),
        IDI = "Ref",
        IDI_pvalue = "Ref",
        NRI = "Ref",
        NRI_pvalue = "Ref"
      )
    }
  })
  
  # 生成表格并返回文件名
  generate_table(testcd, results_df)
}

# 生成表格函数
generate_table <- function(testcd, results) {
  # 智能标题生成
  title <- ifelse(str_detect(testcd, "HOSP"), "In-hospital mortality",
                  ifelse(str_detect(testcd, "ICU"), 
                         paste0(str_extract(testcd, "\\d+"), "-day all-cause mortality"),
                         paste(testcd, "Mortality")))
  
  # 创建专业三线表
  ft <- flextable(results) %>%
    # 设置表格标题
    set_caption(caption = paste0("Table 4 Improvement in discrimination and risk reclassification for ",title, " after addition of FI-Lab"),
                style = "Table Caption") %>% 
    # font(fontname = "Times New Roman", part = "all") %>%  # 学术字体
    set_header_labels(
      Model = "Model",
      C_index = "C-index (95% CI)",
      IDI = "IDI, % (95% CI)",
      IDI_pvalue = "IDI P-value",
      NRI = "NRI, % (95% CI)",
      NRI_pvalue = "NRI P-value"
    ) %>%
    theme_booktabs() %>%  # 三线表样式
    autofit() %>%         # 自动适应列宽
    align(align = "center", part = "all") %>%  # 内容居中
    fontsize(size = 10, part = "all") %>%      # 字体大小
    bold(part = "header") %>%                  # 表头加粗
    padding(padding = 3, part = "all")         # 单元格内边距
  
  
  # 保存为Word文档
  save_as_docx(ft, 
               path = paste0("Table 4-", testcd,".docx"), 
               pr_section = prop_section(page_size = page_size(orient = "landscape")))
  
}

# 执行分析并生成表格
# ICU28D分析（Table 4）
icu_file <- analyze_and_generate(
  testcd = "ICU28D", 
  time_point = 28
)

# HOSPXXD分析（Table 4）
hosp_file <- analyze_and_generate(
  testcd = "HOSPXXD", 
  time_point = 28  # 也设HOSPXXD时间点为28天
)

# 输出结果
message(sprintf("生成完成: \n1. %s\n2. %s", icu_file, hosp_file))
