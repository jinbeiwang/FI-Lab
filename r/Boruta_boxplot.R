# 加载必要的包
library(Boruta)       # 特征选择
library(randomForest) # 随机森林
library(haven)        # 读取SAS数据
library(readxl)       # 读取Excel文件
library(dplyr)        # 数据处理
library(tidyr)        # 数据整理
library(forcats)      # 因子处理

# ======================
# 数据验证函数
# ======================
validate_data <- function(tte_df, metadata) {
  if (!is.data.frame(tte_df)) stop("tte_df必须是数据框")
  required_tte_cols <- c('testcd', 'aval', 'cnsr', 'flab100')
  missing_tte_cols <- setdiff(required_tte_cols, colnames(tte_df))
  if (length(missing_tte_cols) > 0) {
    stop("tte_df缺少必要列: ", paste(missing_tte_cols, collapse = ", "))
  }
  
  if (!is.data.frame(metadata)) stop("metadata必须是数据框")
  required_meta_cols <- c("xvar", "variable_name")
  missing_meta_cols <- setdiff(required_meta_cols, colnames(metadata))
  if (length(missing_meta_cols) > 0) {
    stop("metadata缺少必要列: ", paste(missing_meta_cols, collapse = ", "))
  }
  
  xvar_names <- metadata %>%
    filter(xvar == 1) %>%
    pull(variable_name)
  
  if (length(xvar_names) == 0) {
    stop("metadata中没有标记为xvar=1的自变量")
  }
  
  cat("成功验证数据:\n")
  cat("  tte_df:", nrow(tte_df), "行", ncol(tte_df), "列\n")
  cat("  metadata:", nrow(metadata), "行\n")
  cat("  找到", length(xvar_names), "个自变量\n")
  
  return(xvar_names)
}

# ======================
# Boruta分析函数（使用原生plot方法）
# ======================
run_boruta_analysis <- function(testcd, tte_df, xvar_names) {
  cat("\n", rep("=", 50), "\n", sep="")
  cat("处理", testcd, "...\n")
  cat(rep("=", 50), "\n\n", sep="")
  
  # 提取特定testcd的数据
  test_data <- tte_df %>%
    filter(testcd == !!testcd) %>%
    select(-any_of(c('Q1','Q2','Q3','Q4')))
  
  cat("  ", testcd, "数据集大小:", nrow(test_data), "行\n")
  
  # 清理flab100
  test_data$flab100 <- as.numeric(test_data$flab100)
  test_data <- test_data[!is.na(test_data$flab100), ]
  cat("  清理后数据集大小:", nrow(test_data), "行\n")
  
  # 创建二分类目标
  median_time <- median(test_data$aval)
  binary_target <- ifelse(test_data$cnsr == 1 & test_data$aval <= median_time, 1, 0)
  cat("  二分类目标分布: \n")
  print(table(binary_target))
  
  # 提取自变量数据
  available_xvar <- intersect(xvar_names, colnames(test_data))
  X <- test_data[, available_xvar] %>%
    mutate(across(everything(), as.numeric))
  
  cat("  使用的自变量:", length(available_xvar), "/", length(xvar_names), "\n")
  
  # 运行Boruta
  cat("  运行Boruta特征选择...\n")
  set.seed(42)
  boruta_result <- Boruta(
    x = X,
    y = factor(binary_target),
    doTrace = 2,
    maxRuns = 200,
    getImp = getImpRfZ
  )
  
  # 创建输出目录
  output_dir <- "Feature_selection/boxplot"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 基础文件名
  base_path <- file.path(output_dir, paste0(testcd, "_Boruta"))
  
  # ======================
  # 使用Boruta原生plot方法绘图
  # ======================
  save_boruta_plot <- function(file_path, testcd, width = 14, height = 10) {
    # 1. 动态计算参数
    sci_colors <- c(Confirmed="#1F77B4", Tentative="#FF7F0E", 
                    Rejected="#D62728", Shadow="#D3D3D3")
    
    # 获取真实特征（排除影子特征）
    features <- rownames(attStats(boruta_result))
    real_features <- features[!grepl("^shadow", features)]
    n_features <- length(features)
    
    # 3. 动态边距计算（根据特征数量）
    bottom_margin <- max(20, 8 + n_features * 0.3)  # 每特征增加0.3单位边距
    right_margin <- max(10, 8 + n_features * 0.1)   # 为图例预留空间
    
    # 重构绘图函数
    plot_boruta <- function() {
      # 动态调整边距
      bottom_margin <- max(15, 8 + n_features * 0.3)
      right_margin <- max(10, 8 + n_features * 0.1)
      
      par(
        mar = c(bottom_margin, 8, 8, right_margin) + 0.1,
        xpd = TRUE,
        cex.axis = 1.1,
        cex.lab = 1.3,
        cex.main = 1.5
      )
      
      plot(boruta_result, main = ifelse(testcd == "ICU28D", 
                                        "Boruta Feature Importance - 28-day all-cause mortality",
                                        "Boruta Feature Importance - In-hospital mortality"),
           sort=T,
           colCode = sci_colors,
           whichShadow=c(T,T,T),
           cex.axis=0.8, 
           las=2, 
           xlab=" ", 
           ylab = "Z-Score Importance")
      
      mtext("Variables", side = 1, 
            line = 5,                   # 关键：提升标签位置
            cex = 1.2)                  # 适当放大
      
      # 完整图例
      legend("topright",
             inset = c(-0.18, 0),
             legend = c("Confirmed", "Tentative", "Rejected", "Shadow"),
             fill = sci_colors,
             title = "Decision",
             cex = 0.8)
    }
    
    # 5. 多格式保存
    img_formats <- c("png", "jpg", "tiff", "svg")
    for (fmt in img_formats) {
      output_file <- paste0(file_path, ".", fmt)
      
      if (fmt %in% c("png", "jpg", "tiff")) {
        # 动态尺寸：特征越多图像越高
        res <- 600
        height_adj <- max(10, n_features * 0.4)  # 每特征0.4英寸高度
        width_px <- width * res
        height_px <- height_adj * res
        
        if (fmt == "png") png(output_file, width = width_px, height = height_px, res = res)
        if (fmt == "jpg") jpeg(output_file, width = width_px, height = height_px, res = res)
        if (fmt == "tiff") tiff(output_file, width = width_px, height = height_px, res = res)
      } else {
        svg(output_file, width = width, height = max(10, n_features * 0.4))
      }
      
      plot_boruta()
      dev.off()
    }
    cat("优化图像已保存:", paste0(file_path, ".{", paste(img_formats, collapse = ","), "}\n"))
  }
  # Save plots
  save_boruta_plot(base_path,testcd)
  cat("  Plots saved in four formats: PNG, JPG, SVG, TIFF\n")
  # Save CSV results
  imp_df <- attStats(boruta_result) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("feature") %>%
    select(feature, boruta_decision = decision, boruta_ranking = medianImp)
  
  write.csv(imp_df, paste0(base_path, "_Results.csv"), row.names = FALSE)
  cat("  Boruta results saved to: ", base_path, "_Results.csv\n", sep = "")
  
  
  return(boruta_result)
}
# ======================
# 主执行流程
# ======================
tte_df <- tte  # 从环境获取数据

# 验证数据并获取自变量列表
xvar_names <- validate_data(tte_df, metadata)

# 目标testcd（只处理ICU28D和HOSPXXD）
target_testcds <- c('ICU28D',"HOSPXXD")
all_testcds <- unique(tte_df$testcd)

cat("\n开始处理指定测试指标:\n")
for (testcd in target_testcds) {
  if (testcd %in% all_testcds) {
    cat("\n", rep("=", 30), "\n", sep="")
    cat("处理", testcd, "\n")
    cat(rep("=", 30), "\n\n", sep="")
    boruta_result <- run_boruta_analysis(testcd, tte_df, xvar_names)
  } else {
    cat("跳过 '", testcd, "'，因其不在数据中\n", sep="")
  }
}

cat("\n分析完成! 所有结果保存至 Feature_selection/boxplot 目录\n")
