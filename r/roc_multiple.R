# 安装并加载所需包
required_packages <- c("rms", "Hmisc", "survIDINRI", "timeROC", "ggplot2", "pROC", 
                       "purrr", "stringr", "dplyr", "gridExtra", "RColorBrewer", "grid","future")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if(length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# 设置全局种子确保可复现性
set.seed(123)

# 增强版ROC曲线绘制函数
generate_roc_plots <- function(testcd, time_point, scores, data) {
  # 根据testcd生成标题
  title_text <- ifelse(
    grepl("ICU", testcd),
    paste0(
      "ROC curve analysis of the incremental effect of FI-Lab on ",
      ifelse(
        testcd == "ICU28D",
        "28-day",
        paste0(gsub("ICU", "", testcd), "-day")
      ),
      " all-cause mortality"
    ),
    "ROC curve analysis of the incremental effect of FI-Lab on in-hospital mortality"
  )
  
  
  # 创建存储图形的列表
  plot_list <- list()
  
  # 设置SCI文献配色方案
  sci_colors <- brewer.pal(8, "Set1")[c(1, 2)]
  
  # 对每个评分系统进行处理
  for (i in seq_along(scores)) {
    score <- scores[i]
    
    # 构建模型公式
    original_formula <- as.formula(paste("Surv(time, event) ~", score))
    enhanced_formula <- as.formula(paste("Surv(time, event) ~", score, "+ flab100"))
    
    # 配置数据环境
    ddist <- datadist(data)
    options(datadist = ddist)
    
    # 拟合模型
    original_model <- cph(original_formula, data = data, x = TRUE, y = TRUE, surv = TRUE, time.inc = time_point)
    enhanced_model <- cph(enhanced_formula, data = data, x = TRUE, y = TRUE, surv = TRUE, time.inc = time_point)
    
    # 获取预测值
    data$pred_original <- predict(original_model, type = "lp")
    data$pred_enhanced <- predict(enhanced_model, type = "lp")
    
    # 计算ROC曲线和AUC
    roc_original <- timeROC(T = data$time,
                            delta = data$event,
                            marker = data$pred_original,
                            cause = 1,
                            weighting = "marginal",
                            iid = TRUE,
                            times = time_point)
    
    roc_enhanced <- timeROC(T = data$time,
                            delta = data$event,
                            marker = data$pred_enhanced,
                            cause = 1,
                            weighting = "marginal",
                            iid = TRUE,
                            times = time_point)
    
    # 计算AUC比较的P值
    auc_diff_test <- compare(roc_original, roc_enhanced)
    p_value <- auc_diff_test$p_values_AUC[2]
    
    # 正确计算置信区间并提取t=27的值
    roc_original_ci <- confint(roc_original)
    roc_enhanced_ci <- confint(roc_enhanced)
    
    # 修改后的get_ci函数（专门处理列表结构）
    get_ci <- function(ci_list) {
      # 1. 验证输入结构
      if (!is.list(ci_list) || !all(c("CI_AUC", "CB_AUC") %in% names(ci_list))) {
        stop("输入必须是包含CLAUC和CB_AUC元素的列表")
      }
      
      # 2. 提取CLAUC矩阵（根据您的结构）
      clauc_matrix <- ci_list$CI_AUC
      last_n<-nrow(clauc_matrix)
      # # 3. 验证矩阵结构 HOSPXXD is 1X2, only t=27
      # if (!is.matrix(clauc_matrix) || nrow(clauc_matrix) != 2 || ncol(clauc_matrix) != 2) {
      #   stop("CLAUC元素必须是2x2矩阵")
      # }
      
      # 4. 提取置信区间值（假设最后一行是t=27）
      # 第一行可能是t=0或其他时间点，第二行是t=27
      ci_row <- clauc_matrix[last_n, ]  # 获取第二行数据
      ci_lower <- ci_row[1] / 100  # 第一列是2.5%
      ci_upper <- ci_row[2] / 100  # 第二列是97.5%
      
      return(c(ci_lower, ci_upper))
    }
    # 使用示例
    ci_original <- get_ci(roc_original_ci)  # 输入是列表
    ci_enhanced <- get_ci(roc_enhanced_ci)
    
    # 创建ROC曲线数据框
    roc_points <- min(nrow(roc_original$TP), nrow(roc_enhanced$TP))
    roc_df <- data.frame(
      Sensitivity = c(
        roc_original$TP[1:roc_points, 2], 
        roc_enhanced$TP[1:roc_points, 2]
      ),
      Specificity = c(
        1 - roc_original$FP[1:roc_points, 2], 
        1 - roc_enhanced$FP[1:roc_points, 2]
      ),
      Model = rep(
        c(paste0(toupper(score)), 
          paste0(toupper(score), " + FI-Lab")), 
        each = roc_points
      )
    )
    #
    
    
    # 创建插值函数解决曲线点数不一致问题
    interpolate_roc <- function(roc_obj, n_points = 300) {
      fpr <- roc_obj$FP[, 2]
      tpr <- roc_obj$TP[, 2]
      
      # 按FPR排序
      ord <- order(fpr)
      fpr <- fpr[ord]
      tpr <- tpr[ord]
      
      # 生成统一的FPR序列
      unified_fpr <- seq(0, 1, length.out = n_points)
      
      # 线性插值获取TPR
      unified_tpr <- approx(fpr, tpr, xout = unified_fpr, method = "linear", rule = 2)$y
      
      data.frame(FPR = unified_fpr, TPR = unified_tpr)
    }
    
    # 应用插值
    roc_orig_interp <- interpolate_roc(roc_original)
    roc_enh_interp <- interpolate_roc(roc_enhanced)
    
    # 创建ROC数据框
    roc_df <- data.frame(
      Sensitivity = c(roc_orig_interp$TPR, roc_enh_interp$TPR),
      Specificity = 1 - c(roc_orig_interp$FPR, roc_enh_interp$FPR),
      Model = rep(c(paste0(toupper(score)), 
                    paste0(toupper(score), " + FI-Lab")), 
                  each = nrow(roc_orig_interp))
    )
    
    # 创建AUC标签
    is_significant <- p_value < 0.05
    significance_star <- ifelse(is_significant, "*", "")
    
    auc_label <- sprintf(
      "AUC 1: %.3f (%.3f-%.3f)%s\nAUC 2: %.3f (%.3f-%.3f)%s\n%s",
      roc_original$AUC[2], ci_original[1], ci_original[2], ifelse(is_significant, "", " "),
      roc_enhanced$AUC[2], ci_enhanced[1], ci_enhanced[2], ifelse(is_significant, "*", ""),
      ifelse(p_value < 0.001, 
             "P < 0.001", 
             sprintf("P = %.3f", p_value))
      
    )
    

    # 创建ggplot对象
    p <- ggplot(roc_df, aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
      geom_line(linewidth = 1.2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      scale_color_manual(values = sci_colors) +
      labs(title = paste0(toupper(score), " Score + FI-Lab"),  # 修改标题格式
           x = "1 - Specificity",
           y = "Sensitivity") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = c(0.75, 0.10),  # 将图例移到图内
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "black")
      ) +
      coord_equal() +
      # 添加AUC文本标签到图内
      annotate("text", x = 0.5, y = 0.5, label = auc_label, 
               size = 3.5, hjust = 0, vjust = 1,color = "black")
    # 添加半透明背景框确保文本可读性
    # annotate("rect", 
    #          xmin = 0.55, 
    #          xmax = 0.95,
    #          ymin = 0.15, 
    #          ymax = 0.35,
    #          alpha = 0.2, 
    #          fill = "white")
    
    plot_list[[i]] <- p
  }
  
  # 组合所有图形
  combined_plot <- grid.arrange(grobs = plot_list, ncol = 3, 
                                top = grid::textGrob(title_text, 
                                                     gp = grid::gpar(fontsize = 16, fontface = "bold")))
  
  # 保存为不同格式
  formats <- c("png", "jpg", "svg", "tiff")
  for (fmt in formats) {
    filename <- paste0("Roc_plots/ROC-", testcd, ".", fmt)
    ggsave(filename, combined_plot, width = 16, height = 10, dpi = 300)
  }
  
  return(paste0("ROC-", testcd, ".", formats))
}

# 主分析函数保持不变
analyze_and_generate <- function(testcd, time_point) {
  # 配置参数
  scores <- c("apsiii", "sapsii", "oasis", "sofa", "sirs", "gcs")
  
  # 数据处理
  analysis_data <- tte %>%
    filter(testcd == !!testcd) %>%
    select(-any_of(c("Q1", "Q2", "Q3", "Q4"))) %>%
    mutate(flab100 = as.numeric(flab100)) %>% 
    filter(!is.na(flab100)) %>%
    mutate(time = as.numeric(aval),
           event = as.numeric(cnsr))
  
  # 生成ROC曲线
  roc_files <- generate_roc_plots(testcd, time_point, scores, analysis_data)
  
  return(roc_files)
}

# 执行分析并生成图形
# ICU28D分析
icu_files <- analyze_and_generate(testcd = "ICU28D", time_point = 27)

# HOSPXXD分析
hosp_files <- analyze_and_generate(testcd = "HOSPXXD", time_point = 27)

# 输出结果
message("生成完成:")
message("ICU28D文件: ", paste(icu_files, collapse = ", "))
message("HOSPXXD文件: ", paste(hosp_files, collapse = ", "))
