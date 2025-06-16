# 加载必要包
library(survival)
library(rms)
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(survminer)
library(scales)  # 用于透明色处理

# RCS曲线生成函数（优化版）
generate_rcs_plots <- function(tte_data, testcd, covariates = NULL) {
  
  # 数据准备
  plot_data <- tte_data %>%
    filter(testcd == !!testcd) %>%
    select(-any_of(c("Q1","Q2","Q3","Q4")))%>%
    mutate(flab100 = as.numeric(flab100)) %>% 
    filter(!is.na(flab100))
  
  # 变异度检查
  if (length(unique(plot_data$flab100)) < 2 || 
      sd(plot_data$flab100, na.rm = TRUE) < .Machine$double.eps^0.5) {
    stop("变量flab100缺乏变异度（所有值相同或接近），无法进行RCS分析 [testcd: ", testcd, "]")
  }
  
  # 如果有协变量，检查并处理缺失值
  if (!is.null(covariates)) {
    # 确保协变量存在
    existing_covars <- covariates[covariates %in% names(plot_data)]
    missing_covars <- setdiff(covariates, existing_covars)
    
    if (length(missing_covars) > 0) {
      warning("缺少协变量: ", paste(missing_covars, collapse = ", "), 
              " [testcd: ", testcd, "]")
    }
    
    # 将字符型/逻辑型协变量转换为因子
    # for (covar in existing_covars) {
    #   if (is.character(plot_data[[covar]]) {
    #     plot_data[[covar]] <- factor(plot_data[[covar]])
    #   } else if (is.logical(plot_data[[covar]])) {
    #     plot_data[[covar]] <- factor(plot_data[[covar]], levels = c(TRUE, FALSE))
    #   }
    # }
    
    # 移除协变量缺失值
    plot_data <- plot_data %>%
      select(all_of(c("aval", "cnsr", "flab100", existing_covars))) %>%
      na.omit()
  }
  
  # 样本量检查
  if (nrow(plot_data) < 50) {
    warning("样本量较小(n=", nrow(plot_data), ")，结果可能不稳定 [testcd: ", testcd, "]")
  }
  
  if (sum(plot_data$cnsr == 1) < 10) {
    stop("事件数量不足(<10)，无法进行可靠分析 [testcd: ", testcd, "]")
  }
  
  # 智能标题生成
  title <- case_when(
    str_detect(testcd, "HOSP") ~ "In-hospital mortality",
    str_detect(testcd, "ICU") ~ {
      days <- str_extract(testcd, "\\d+")
      paste0(days, "-day all-cause mortality")
    },
    TRUE ~ paste(testcd, "Mortality")
  )
  
  # 设置参考点（中位数）
  ref_point <- median(plot_data$flab100, na.rm = TRUE)
  
  # 初始化rms环境
  dd <- datadist(plot_data)
  options(datadist = dd)#这里之前因为用了"dd"导致了报错，注意不带引号，是对象不是字符
  
  # 构建生存对象
  surv_obj <- Surv(time = plot_data$aval, event = plot_data$cnsr)
  
  # 构建模型公式 - 修复警告问题
  if (is.null(covariates)) {
    formula <- surv_obj ~ rcs(flab100, 4)
    model_type <- "Univariate"
  } else {
    # 过滤存在的协变量
    existing_covars <- covariates[covariates %in% names(plot_data)]
    
    if (length(existing_covars) == 0) {
      formula <- surv_obj ~ rcs(flab100, 4)
      model_type <- "Univariate (no valid covariates)"
    } else {
      # 修复公式构建方式，避免字符向量警告
      covar_formula <- paste(existing_covars, collapse = " + ")
      formula_str <- paste("surv_obj ~ rcs(flab100, 4) +", covar_formula)
      formula <- as.formula(formula_str)  # 显式转换为公式对象
      model_type <- paste("Adjusted for:", paste(existing_covars, collapse = ", "))
    }
  }
  
  # 拟合RCS模型
  rcs_fit <- tryCatch(
    {
      cph(formula, data = plot_data, x = TRUE, y = TRUE)
    },
    error = function(e) {
      stop("模型拟合失败: ", e$message, " [testcd: ", testcd, "]")
    }
  )
  
  # 提取P值 - 修复语法错误
  extract_p_value <- function(anova_obj) {
    results <- list(overall = NA, nonlinear = NA)
    rownames_anova <- rownames(anova_obj)
    
    # 尝试提取整体P值
    if ("flab100" %in% rownames_anova) {
      results$overall <- anova_obj["flab100", "P"]
    } else {
      # 尝试匹配包含"flab100"的行
      idx <- grep("flab100", rownames_anova, fixed = TRUE)
      if (length(idx) > 0) {
        results$overall <- anova_obj[idx[1], "P"]
      }
    }
    
    # 尝试提取非线性P值
    if ("Nonlinear" %in% rownames_anova) {
      results$nonlinear <- anova_obj["Nonlinear", "P"]
    } else {
      # 尝试匹配包含"Nonlinear"的行
      idx <- grep("Nonlinear", rownames_anova, fixed = TRUE)
      if (length(idx) > 0) {
        results$nonlinear <- anova_obj[idx[1], "P"]
      }
    }
    
    results
  }
  
  anova_test <- anova(rcs_fit)
  p_values <- extract_p_value(anova_test)
  
  # 格式化P值标签
  format_p <- function(p) {
    if (is.na(p)) return("p = NA")
    if (p < 0.001) {
      "p < 0.001"
    } else if (p < 0.01) {
      "p < 0.01"
    } else if (p < 0.05) {
      "p < 0.05"
    } else {
      paste("p =", round(p, 3))
    }
  }
  
  p_label <- paste0(
    "Overall: ", format_p(p_values$overall), 
    "\nNon-linear: ", format_p(p_values$nonlinear)
  )
  
  # 生成预测数据
  pred_range <- range(plot_data$flab100, na.rm = TRUE)
  pred_data <- data.frame(flab100 = seq(
    pred_range[1],
    pred_range[2],
    length.out = 200
  ))
  
  # 添加协变量中位数/众数（如果有多变量）
  if (!is.null(covariates) && length(existing_covars) > 0) {
    for (covar in existing_covars) {
      if (is.numeric(plot_data[[covar]])) {
        pred_data[[covar]] <- median(plot_data[[covar]], na.rm = TRUE)
      } else if (is.factor(plot_data[[covar]])) {
        # 因子变量取最常见水平并保持因子结构
        tab <- table(plot_data[[covar]])
        mode_value <- names(sort(tab, decreasing = TRUE))[1]
        pred_data[[covar]] <- factor(mode_value, levels = levels(plot_data[[covar]]))
      } else {
        # 其他类型取最常见值
        pred_data[[covar]] <- names(sort(table(plot_data[[covar]]), decreasing = TRUE))[1]
      }
    }
  }
  
  # 计算预测值
  pred <- Predict(
    rcs_fit, 
    flab100 = pred_data$flab100,
    ref.zero = TRUE, 
    fun = exp
  )
  
  # ====== 高级配色方案（Nature期刊风格） ======
  primary_color <- "#2E5A87"  # 深蓝色（主曲线）
  ribbon_color <- alpha("#6C9BCF", 0.2)  # 半透明浅蓝色（置信区间）
  ref_line_color <- "#D93B3B"  # 红色（参考线）
  histogram_color <- alpha("#4D7EA8", 0.6)  # 半透明蓝色（直方图）
  
  # ====== 整合直方图到主图 ======
  # 创建直方图数据（使用更可靠的layer_data函数）
  hist_plot <- ggplot(plot_data, aes(x = flab100)) + 
    geom_histogram(bins = 30, fill = histogram_color)
  
  hist_data <- layer_data(hist_plot)  # 更可靠的获取方式
  
  # 确保有x位置数据
  if (!"x" %in% names(hist_data)) {
    if (all(c("xmin", "xmax") %in% names(hist_data))) {
      hist_data$x <- (hist_data$xmin + hist_data$xmax)/2
    } else {
      stop("无法确定直方图中心位置")
    }
  }
  
  # 计算宽度
  if (all(c("xmin", "xmax") %in% names(hist_data))) {
    hist_data$width <- hist_data$xmax - hist_data$xmin
  } else {
    # 估算宽度作为备选
    unique_x <- unique(hist_data$x)
    avg_width <- if (length(unique_x) > 1) mean(diff(sort(unique_x))) else 1
    hist_data$width <- rep(avg_width, nrow(hist_data))
  }
  
  # 缩放直方图高度（适配HR坐标轴）
  y_max <- max(pred$upper, na.rm = TRUE)
  
  # 安全处理零计数情况
  if (max(hist_data$count) == 0) {
    hist_scale_factor <- 0
  } else {
    hist_scale_factor <- 0.2 * y_max / max(hist_data$count)
  }
  
  hist_data$scaled_count <- hist_data$count * hist_scale_factor
  
  # ====== 创建主图 ======
  rcs_plot <- ggplot() +
    # 1. 添加直方图（底部）
    geom_bar(
      data = hist_data,
      aes(x = x, y = scaled_count),
      stat = "identity",
      width = hist_data$width,  # 使用每列的宽度
      fill = histogram_color,
      color = NA
    ) +
    # 2. 添加HR参考线
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    # 3. 添加置信区间
    geom_ribbon(
      data = pred,
      aes(x = flab100, ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = primary_color
    ) +
    # 4. 添加主曲线
    geom_line(
      data = pred,
      aes(x = flab100, y = yhat),
      linewidth = 1.5,
      color = primary_color
    ) +
    # 5. 添加参考线
    geom_vline(
      xintercept = ref_point,
      linetype = "dashed",
      color = ref_line_color,
      linewidth = 0.5
    ) +
    # 6. 添加P值标签
    annotate(
      "text",
      x = Inf, y = Inf,
      label = p_label,
      hjust = 1.1, vjust = 1.5,
      size = 4.5,
      fontface = "bold"
    ) +
    # 7. 添加模型类型标签
    # annotate(
    #   "text",
    #   x = -Inf, y = Inf,
    #   label = model_type,
    #   hjust = -0.05, vjust = 1.5,
    #   size = 4.0,
    #   color = "gray30",
    #   fontface = "italic"
    # ) +
    # 8. 添加中位数标签
    # annotate(
    #   "text",
    #   x = ref_point,
    #   y = 0,
    #   label = paste0("Median: ", round(ref_point, 2)),
    #   vjust = -0.5,
    #   hjust = 0.5,
    #   size = 4.0,
    #   color = ref_line_color,
    #   fontface = "bold"
    # ) +
    # 坐标轴和标签
    labs(
      title = title,
      x = "FI-Lab",  # 修改为FI-Lab
      y = "Hazard Ratio (95% CI)"
    ) +
    # 扩展Y轴底部空间以显示直方图
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    # Nature期刊风格主题
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16,
        margin = margin(b = 15)
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.title = element_text(face = "bold", size = 13),
      axis.text = element_text(color = "black", size = 11),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  return(rcs_plot)
}

# 图形保存函数
save_rcs_plots <- function(plot_obj, testcd, output_dir = "results") {
  # 创建输出目录
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  filename_base <- file.path(output_dir, paste0("RCS-", testcd))
  
  # 保存多种格式
  ggsave(paste0(filename_base, ".png"), plot_obj, width = 9, height = 7, dpi = 300, bg = "white")
  ggsave(paste0(filename_base, ".jpg"), plot_obj, width = 9, height = 7, dpi = 300, bg = "white")
  ggsave(paste0(filename_base, ".svg"), plot_obj, width = 9, height = 7, bg = "white")
  ggsave(paste0(filename_base, ".tif"), plot_obj, width = 9, height = 7, dpi = 300, compression = "lzw")
  
  message("保存图形: ", filename_base, ".[png|jpg|tif|.svg]")
}

# 主执行函数
create_all_rcs_plots <- function(tte_data, covariates = NULL, output_dir = "results") {
  testcd_list <- unique(tte_data$testcd)
  
  results <- list()
  errors <- list()
  
  for (testcd in testcd_list) {
    tryCatch({
      message("\n===== 处理: ", testcd, " =====")
      
      start_time <- Sys.time()
      rcs_plot <- generate_rcs_plots(tte_data, testcd, covariates)
      save_rcs_plots(rcs_plot, testcd, output_dir)
      
      elapsed <- round(as.numeric(Sys.time() - start_time), 1)
      message(paste0("✅ 成功创建 (耗时: ", elapsed, "s)"))
      results[[testcd]] <- rcs_plot
    }, error = function(e) {
      msg <- paste("❌ 失败:", e$message)
      message(msg)
      errors[[testcd]] <<- msg
    })
  }
  
  # 生成报告
  message("\n===== 处理完成 =====")
  message("成功: ", length(results), "/", length(testcd_list))
  message("失败: ", length(errors))
  
  if (length(errors) > 0) {
    message("\n失败详情:")
    for (testcd in names(errors)) {
      message("- ", testcd, ": ", errors[[testcd]])
    }
  }
  
  invisible(list(success = results, errors = errors))
}

# 使用示例 ================================================================

# 步骤1: 准备协变量（根据metadata）
# 假设metadata是一个数据框，包含变量名和选择标志
covariates_m2 <- metadata$variable_name[metadata$covars_m2 == 1]

# 步骤2: 创建子集
tte_xxdays <- tte %>%
  filter(testcd %in% c("ICU28D", "HOSPXXD"))  # 示例多个testcd

# 步骤3: 执行函数，使用model 2构建
results <- create_all_rcs_plots(
  tte_data = tte_xxdays,
  covariates = covariates_m2,
  output_dir = "RCS_Results"
)