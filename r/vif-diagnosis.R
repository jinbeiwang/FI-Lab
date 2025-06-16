#安装并加载必要的包
required_packages <- c("car", "survival", "corrplot", "tidyverse", 
                       "GGally", "igraph", "factoextra", "ggrepel", "ggraph")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

library(car)
library(survival)
library(corrplot)
library(tidyverse)
library(GGally)      # 用于散点图矩阵
library(igraph)      # 用于网络图
library(factoextra)  # 用于PCA可视化
library(ggrepel)     # 用于避免标签重叠
library(ggraph)      # 用于高级网络图

if(!dir.exists("collinearity_plots")) dir.create("collinearity_plots")

# 读取数据testcd==ICU28D, HOSPXXD
data <-tte %>% filter(testcd=="HOSPXXD") %>%
  select(-all_of(c("Q1","Q2","Q3","subject_id","stay_id","hadm_id","total_number","total_deficit", "flab")))

# 首先检查自变量之间的相关性
# 注意：仅选择数值型变量进行相关性分析
numeric_vars <- names(data)[sapply(data, is.numeric)]
# 从numeric_vars中排除时间和事件变量
time_event_vars <- c("aval", "cnsr")  # 替换为您的时间和事件变量名
numeric_vars <- setdiff(numeric_vars, time_event_vars)

# 计算相关矩阵
if(length(numeric_vars) > 1) {
  cor_matrix <- cor(data[, numeric_vars], use = "pairwise.complete.obs")
  
  # 高质量热图 (SCI级别)
  tiff("collinearity_plots/correlation_heatmap.tiff", 
       width = 8, height = 7, units = "in", res = 300)
  corrplot(cor_matrix, method = "color", 
           type = "upper", 
           tl.col = "black", 
           tl.srt = 45,
           title = "Correlation Matrix of Predictors",
           mar = c(0,0,2,0),
           addCoef.col = "black", 
           number.cex = 0.7,
           diag = FALSE)
  dev.off()
  # 2. 相关网络图 (高度相关变量) --------------------------------------------
  # 创建网络图数据
  cor_edges <- which(abs(cor_matrix) > 0.7 & upper.tri(cor_matrix), arr.ind = TRUE)
  
  if(nrow(cor_edges) > 0) {
    net_data <- data.frame(
      from = numeric_vars[cor_edges[, 1]],
      to = numeric_vars[cor_edges[, 2]],
      weight = abs(cor_matrix[cor_edges])
    )
    
    # 创建网络图
    net <- graph_from_data_frame(net_data, directed = FALSE)
    E(net)$width <- E(net)$weight * 5  # 线宽反映相关强度
    
    # 高质量网络图
    tiff("collinearity_plots/correlation_network.tiff", 
         width = 8, height = 7, units = "in", res = 300)
    set.seed(123)
    plot(net, 
         vertex.color = "lightblue",
         vertex.size = 15,
         vertex.label.cex = 0.8,
         vertex.label.color = "black",
         edge.color = ifelse(cor_matrix[cor_edges] > 0, "red", "blue"),
         edge.curved = 0.2,
         main = "Correlation Network (|r| > 0.7)")
    dev.off()
  }
  
  # 找出高度相关的变量对 (|r| > 0.7)
  high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
  if(nrow(high_cor) > 0) {
    high_cor_pairs <- data.frame(
      Var1 = numeric_vars[high_cor[, 1]],
      Var2 = numeric_vars[high_cor[, 2]],
      Correlation = cor_matrix[high_cor]
    )
    print("高度相关的变量对 (|r| > 0.7):")
    print(high_cor_pairs)
  } else {
    print("未发现高度相关的变量对 (|r| > 0.7)")
  }
}

# 准备进行Cox模型的变量, Model 2
predictor_vars <- metadata$variable_name[metadata$covars_m2 == 1]

# 创建Cox回归的公式
cox_formula <- as.formula(paste("Surv(aval, cnsr) ~", paste(predictor_vars, collapse = " + ")))

# 定义一个函数，计算VIF并处理高VIF变量
check_and_handle_multicollinearity <- function(data, formula, threshold = 5) {
  # 提取预测变量
  formula_terms <- terms(formula)
  predictors <- attr(formula_terms, "term.labels")
  
  # 检查预测变量是否存在
  missing_vars <- setdiff(predictors, names(data))
  if (length(missing_vars) > 0) {
    stop("以下预测变量在数据中不存在: ", paste(missing_vars, collapse = ", "))
  }
  
  # 创建模型数据（仅包含预测变量）
  model_data <- data[, predictors, drop = FALSE]
  
  # 检查是否有有效数据
  if (nrow(model_data) == 0) {
    stop("模型数据为空，无法计算VIF")
  }
  
  # 将字符变量转换为因子
  cat_vars <- names(model_data)[sapply(model_data, function(x) is.character(x) | is.factor(x))]
  for (var in cat_vars) {
    model_data[[var]] <- as.factor(model_data[[var]])
  }
  
  # 创建伪因变量（VIF计算需要）
  model_data$pseudo_y <- rnorm(nrow(model_data))
  
  # 尝试拟合线性模型
  lm_full <- tryCatch({
    lm_formula <- as.formula(paste("pseudo_y ~", paste(predictors, collapse = " + ")))
    lm(lm_formula, data = model_data)
  }, error = function(e) {
    cat("创建完整线性模型时出错:", e$message, "\n")
    cat("使用的预测变量:", paste(predictors, collapse = ", "), "\n")
    cat("数据预览:\n")
    print(head(model_data))
    return(NULL)
  })
  
  if (is.null(lm_full)) {
    stop("无法创建完整线性模型进行VIF计算")
  }
  
  # 计算VIF
  vif_results <- tryCatch({
    vif_matrix <- car::vif(lm_full)
    
    # 处理不同类型的结果
    if (is.matrix(vif_matrix)) {
      # 有分类变量的情况
      vif_values <- sapply(rownames(vif_matrix), function(var) {
        if (var %in% cat_vars) {
          # 分类变量：GVIF^(1/(2*Df))
          vif_matrix[var, "GVIF"]^(1/(2*vif_matrix[var, "Df"]))
        } else {
          # 连续变量：直接使用GVIF
          vif_matrix[var, "GVIF"]
        }
      })
    } else {
      # 没有分类变量的情况
      vif_values <- vif_matrix
    }
    vif_values
  }, error = function(e) {
    cat("使用car::vif计算VIF时出错:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(vif_results)) {
    stop("VIF计算失败")
  }
  
  # 创建VIF数据框
  vif_df <- data.frame(
    Variable = names(vif_results),
    VIF = as.numeric(vif_results)
  )
  vif_df <- vif_df[order(-vif_df$VIF), ]
  
  # 输出VIF值
  cat("\n==== 变量膨胀因子 (VIF) 值 ====\n")
  print(vif_df)
  
  # 5. VIF条形图 (SCI级别) ------------------------------------------------
  if (nrow(vif_df) > 0) {
    vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
      geom_bar(stat = "identity", fill = ifelse(vif_df$VIF > threshold, "#e74c3c", "#3498db")) +
      geom_hline(yintercept = threshold, linetype = "dashed", color = "#e74c3c") +
      geom_text(aes(label = sprintf("%.2f", VIF)), 
                hjust = -0.2, size = 3.5, color = "black") +
      coord_flip() +
      labs(title = "Variance Inflation Factors (VIF)",
           x = "Predictors",
           y = "VIF Value",
           caption = paste("Threshold =", threshold)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major.y = element_blank())
    
    # 保存高质量VIF图
    ggsave("collinearity_plots/vif_plot.tiff", 
           plot = vif_plot, 
           device = "tiff", 
           width = 8, height = 6, dpi = 300)
    
    print(vif_plot)
  }
  # 找出高VIF变量
  high_vif_vars <- vif_df$Variable[which(vif_df$VIF > threshold)]
  
  if (length(high_vif_vars) > 0) {
    cat("\n以下变量的VIF值超过", threshold, ":\n")
    print(high_vif_vars)
    return(list(high_vif = high_vif_vars, all_vif = vif_df))
  } else {
    cat("\n所有变量的VIF值均低于", threshold, "，无共线性问题。\n")
    return(list(high_vif = character(0), all_vif = vif_df))
  }
}
# 执行共线性检查
multicollinearity_results <- check_and_handle_multicollinearity(data, cox_formula)

# 处理共线性问题
if (length(multicollinearity_results$high_vif) > 0) {
  cat("\n处理共线性问题的方法：\n")
  cat("1. 移除高度共线的变量\n")
  cat("2. 合并相关变量为新变量\n")
  cat("3. 使用正则化方法，如ridge回归或lasso回归\n")
  cat("4. 使用主成分分析(PCA)降维\n\n")
  
  # 示例：移除VIF最高的变量
  vars_to_remove <- multicollinearity_results$high_vif[1]  # 移除VIF最高的变量
  cat("示例方法1: 移除变量:", vars_to_remove, "\n")
  
  # 更新预测变量列表
  predictor_vars_updated <- predictor_vars[!predictor_vars %in% vars_to_remove]
  
  # 更新公式
  cox_formula_updated <- as.formula(paste("Surv(time, event) ~", paste(predictor_vars_updated, collapse = " + ")))
  
  # 重新检查共线性
  cat("\n移除", vars_to_remove, "后重新检查共线性：\n")
  multicollinearity_results_updated <- check_and_handle_multicollinearity(data, cox_formula_updated)
  
  # 更新Cox公式用于后续分析
  cox_formula <- cox_formula_updated
  predictor_vars <- predictor_vars_updated
}