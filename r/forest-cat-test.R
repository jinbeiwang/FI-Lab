# 更新代码 - 替换为四分类level变量
if(!require(forestplot)) install.packages("forestplot")
if(!require(survival)) install.packages("survival")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
library(forestplot)
library(survival)
library(dplyr)
library(tidyr)

# 定义要分析的testcd列表 - 按需修改
testcd_list <- c("ICU28D", "HOSPXXD")  # 替换为您的三个testcd值

# 遍历每个testcd生成森林图
for (current_testcd in testcd_list) {
  # 读取数据
  # 筛选特定testcd
  data <- tte %>% 
    filter(testcd == current_testcd)  # 根据需求修改
  
  # 确保level是因子变量，设置参考水平（Q1作为参考）
  data$level <- factor(data$level, levels = c("Q1", "Q2", "Q3", "Q4"))
  
  # 确保使用正确的协变量和亚组变量 - 根据您的实际设置
  covariates_m2 <- metadata$variable_name[metadata$covars_m2 == 1]
  subgroup_vars <- metadata$variable_name[metadata$subgroup == 1]
  
  # 确保时间和事件变量是数值型
  data$time <- as.numeric(data$aval)
  data$event <- as.numeric(data$cnsr)
  
  # 存储亚组分析结果
  subgroup_results <- list()
  interaction_p_values <- list()
  
  # 对每个亚组变量进行分析
  for (var_name in subgroup_vars) {
    # 获取变量类别
    if (is.numeric(data[[var_name]])) {
      # 数值变量转换为二分变量
      median_val <- median(data[[var_name]], na.rm = TRUE)
      var_categories <- c(paste0("≤", median_val), paste0(">", median_val))
    } else {
      var_categories <- sort(unique(na.omit(data[[var_name]])))
    }
    
    results_for_var <- list()
    
    # 对该变量的每个类别进行亚组分析
    for (category in var_categories) {
      if (is.numeric(data[[var_name]])) {
        # 数值变量的特殊处理
        if (grepl("≤", category)) {
          cutoff <- as.numeric(gsub("≤", "", category))
          subset_data <- data[data[[var_name]] <= cutoff, ]
        } else {
          cutoff <- as.numeric(gsub(">", "", category))
          subset_data <- data[data[[var_name]] > cutoff, ]
        }
      } else {
        subset_data <- data[data[[var_name]] == category, ]
      }
      
      # 计算总人数和事件数（百分比）
      total_n <- nrow(subset_data)
      
      # 在子集上拟合Cox模型
      tryCatch({
        # 构建协变量公式
        cov_formula <- if (length(covariates_m2) > 0) {
          paste(covariates_m2, collapse = " + ")
        } else {
          "1"
        }
        
        model_formula <- as.formula(paste("Surv(time, event) ~ level +", cov_formula))
        model <- coxph(model_formula, data = subset_data)
        
        # 提取结果
        summary_result <- summary(model)
        coefs <- summary_result$coefficients
        
        # 为每个level创建结果行 (Q1-Q4)
        for (lev in levels(data$level)) {
          # 跳过参考水平Q1（作为参照）
          if (lev == "Q1") {
            # 为Q1创建参照行
            event_n <- sum(subset_data$event[subset_data$level == lev] == 1, na.rm = TRUE)
            event_percent <- round(event_n / sum(subset_data$level == lev, na.rm = TRUE) * 100, 1)
            event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
            
            results_for_var[[paste0(category, "_", lev)]] <- data.frame(
              Subgroup = var_name,
              Category = category,
              Level = lev,
              Total = sum(subset_data$level == lev, na.rm = TRUE),
              Event = event_str,
              HR = 1.0,  # 参考水平HR=1
              Lower_CI = NA,
              Upper_CI = NA,
              P_value = NA,
              stringsAsFactors = FALSE
            )
          } else {
            # 提取非参考水平的结果
            row_name <- paste0("level", lev)
            exposure_row <- which(rownames(coefs) == row_name)
            
            if (length(exposure_row) > 0) {
              event_n <- sum(subset_data$event[subset_data$level == lev] == 1, na.rm = TRUE)
              event_percent <- round(event_n / sum(subset_data$level == lev, na.rm = TRUE) * 100, 1)
              event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
              
              hr <- exp(coefs[exposure_row, "coef"])
              lower_ci <- exp(coefs[exposure_row, "coef"] - 1.96 * coefs[exposure_row, "se(coef)"])
              upper_ci <- exp(coefs[exposure_row, "coef"] + 1.96 * coefs[exposure_row, "se(coef)"])
              p_value <- coefs[exposure_row, "Pr(>|z|)"]  # 提取P值
              
              # 保存结果
              results_for_var[[paste0(category, "_", lev)]] <- data.frame(
                Subgroup = var_name,
                Category = category,
                Level = lev,
                Total = sum(subset_data$level == lev, na.rm = TRUE),
                Event = event_str,
                HR = hr,
                Lower_CI = lower_ci,
                Upper_CI = upper_ci,
                P_value = p_value,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }, error = function(e) {
        message(paste("Error in subgroup", var_name, "-", category, ":", e$message))
      })
    }
    
    # 合并该变量的所有类别结果
    if (length(results_for_var) > 0) {
      var_df <- do.call(rbind, results_for_var)
      subgroup_results[[var_name]] <- var_df
      
      # 计算交互P值（使用似然比检验）
      reduced_formula <- as.formula(
        paste("Surv(time, event) ~ level +", var_name, "+", 
              paste(covariates_m2, collapse = " + "))
      )
      
      full_formula <- as.formula(
        paste("Surv(time, event) ~ level *", var_name, "+", 
              paste(covariates_m2, collapse = " + "))
      )
      
      reduced_model <- tryCatch({
        coxph(reduced_formula, data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      full_model <- tryCatch({
        coxph(full_formula, data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(reduced_model) && !is.null(full_model)) {
        lrt <- anova(reduced_model, full_model)
        interaction_p <- lrt[2, "Pr(>|Chi|)"]
        
        interaction_p_values[[var_name]] <- data.frame(
          Subgroup = var_name,
          P_interaction = interaction_p
        )
      }
    }
  }
  
  # 合并所有亚组结果
  all_subgroups_results <- if (length(subgroup_results) > 0) {
    do.call(rbind, subgroup_results)
  } else {
    data.frame()
  }
  
  # 添加Overall行 - 总体效应
  tryCatch({
    # 构建协变量公式
    cov_formula <- if (length(covariates_m2) > 0) {
      paste(covariates_m2, collapse = " + ")
    } else {
      "1"
    }
    
    model_formula <- as.formula(paste("Surv(time, event) ~ level +", cov_formula))
    overall_model <- coxph(model_formula, data = data)
    
    # 提取结果
    summary_result <- summary(overall_model)
    coefs <- summary_result$coefficients
    
    # 为每个level创建结果行 (Q1-Q4)
    overall_results <- list()
    for (lev in levels(data$level)) {
      if (lev == "Q1") {
        # 为Q1创建参照行
        event_n <- sum(data$event[data$level == lev] == 1, na.rm = TRUE)
        event_percent <- round(event_n / sum(data$level == lev, na.rm = TRUE) * 100, 1)
        event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
        
        overall_results[[lev]] <- data.frame(
          Subgroup = "Overall",
          Category = "Overall",
          Level = lev,
          Total = sum(data$level == lev, na.rm = TRUE),
          Event = event_str,
          HR = 1.0,  # 参考水平HR=1
          Lower_CI = NA,
          Upper_CI = NA,
          P_value = NA,
          stringsAsFactors = FALSE
        )
      } else {
        # 提取非参考水平的结果
        row_name <- paste0("level", lev)
        exposure_row <- which(rownames(coefs) == row_name)
        
        if (length(exposure_row) > 0) {
          event_n <- sum(data$event[data$level == lev] == 1, na.rm = TRUE)
          event_percent <- round(event_n / sum(data$level == lev, na.rm = TRUE) * 100, 1)
          event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
          
          hr <- exp(coefs[exposure_row, "coef"])
          lower_ci <- exp(coefs[exposure_row, "coef"] - 1.96 * coefs[exposure_row, "se(coef)"])
          upper_ci <- exp(coefs[exposure_row, "coef"] + 1.96 * coefs[exposure_row, "se(coef)"])
          p_value <- coefs[exposure_row, "Pr(>|z|)"]  # 提取P值
          
          # 保存结果
          overall_results[[lev]] <- data.frame(
            Subgroup = "Overall",
            Category = "Overall",
            Level = lev,
            Total = sum(data$level == lev, na.rm = TRUE),
            Event = event_str,
            HR = hr,
            Lower_CI = lower_ci,
            Upper_CI = upper_ci,
            P_value = p_value,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    # 将Overall行添加到结果顶部
    if (length(overall_results) > 0) {
      overall_df <- do.call(rbind, overall_results)
      all_subgroups_results <- bind_rows(overall_df, all_subgroups_results)
    }
  }, error = function(e) {
    message(paste("Error in overall model:", e$message))
  })
  
  # 合并交互作用P值
  if (length(interaction_p_values) > 0) {
    interaction_df <- do.call(rbind, interaction_p_values)
    all_subgroups_results <- all_subgroups_results %>%
      left_join(interaction_df, by = "Subgroup")
  } else {
    all_subgroups_results$P_interaction <- NA
  }
  
  # 使用metadata中的标签替换变量名
  if (!is.null(metadata) && "label" %in% colnames(metadata)) {
    # 创建从变量名到标签的映射
    label_mapping <- setNames(metadata$label, metadata$variable_name)
    # 添加Overall的映射
    label_mapping["Overall"] <- "Overall"
    
    # 替换Subgroup列
    all_subgroups_results$Subgroup <- ifelse(
      all_subgroups_results$Subgroup %in% names(label_mapping),
      label_mapping[all_subgroups_results$Subgroup],
      all_subgroups_results$Subgroup
    )
  }
  
  # 转换Category显示
  all_subgroups_results <- all_subgroups_results %>%
    mutate(
      Category = case_when(
        Subgroup !="AKI stage" & Category == "0" ~ "No",
        Subgroup !="AKI stage" & Category == "1" ~ "Yes",
        Category == "F" ~ "Female",
        Category == "M" ~ "Male",
        TRUE ~ as.character(Category)
      )
    )
  
  # 格式化数据用于森林图
  if (nrow(all_subgroups_results) == 0) {
    stop("亚组分析未生成任何结果，请检查数据、亚组变量和模型公式")
  }
  
  # 创建专业医学期刊风格的森林图数据
  # 从metadata中提取排序顺序
  if (!is.null(metadata) && "order" %in% colnames(metadata)) {
    # 创建排序映射
    order_mapping <- setNames(metadata$order[metadata$subgroup==1], metadata$label[metadata$subgroup==1])
    # 添加Overall的排序（设为0）
    order_mapping <- c(order_mapping, setNames(0, "Overall"))
  } else {
    # 如果metadata中没有order列，创建默认排序
    unique_subgroups <- setdiff(unique(all_subgroups_results$Subgroup), "Overall")
    order_mapping <- setNames(1:length(unique_subgroups), unique_subgroups)
    order_mapping["Overall"] <- 0
  }
  
  # 将排序顺序应用到数据
  all_subgroups_results <- all_subgroups_results %>%
    mutate(
      Order = order_mapping[Subgroup],
      # 确保Overall排在最前面
      Order = ifelse(Subgroup == "Overall", 0, Order)
    ) %>%
    arrange(Order, Subgroup, Category, Level)
  
  # 提取Subgroup label,为接下来拼接做准备
  Group_Header <- all_subgroups_results %>%
    filter(Subgroup != "Overall") %>%
    distinct(Order, Subgroup, P_interaction) %>%
    select(Order, Subgroup, P_interaction)
  
  # 创建森林图数据
  
  forest_data <- all_subgroups_results %>% 
    bind_rows(Group_Header) %>%
    arrange(Order, Subgroup, desc(is.na(Category)), Category, Level) %>%
    mutate(
      HR_CI = ifelse(Level == "Q1", "1 (Ref)", 
                     sprintf("%.2f (%.2f-%.2f)", HR, Lower_CI, Upper_CI)),
      P_value_str = ifelse(is.na(P_value), "", 
                           ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))),
      P_interaction = ifelse(is.na(P_interaction), "", 
                             ifelse(P_interaction < 0.001, "<0.001", sprintf("%.3f", P_interaction))),
      P_interaction = ifelse(!duplicated(Subgroup), P_interaction, "")
    ) %>%
    
    group_by(Subgroup, Category) %>%
    mutate(
      Group_Header = case_when(
        Subgroup == "Overall" ~ ifelse(row_number() == 1, as.character(Subgroup), ""),
        !is.na(Category) & Category != "Overall" ~ ifelse(row_number() == 1,paste0("  ", Category),""),
        TRUE ~ Subgroup
      )
      
    ) %>%  ungroup() %>%
    select(Group_Header, Level, Total, Event, HR, Lower_CI, Upper_CI, P_value_str, HR_CI, P_interaction) %>%
    relocate(Group_Header, Level, Total, Event, HR_CI, P_value_str, P_interaction)
  
  # 修复类型不匹配问题
  forest_data <- forest_data %>%
    mutate(
      Total = ifelse(is.na(Total), "", as.character(Total)),
      Event = ifelse(is.na(Event), "", as.character(Event)),
      P_value_str = ifelse(is.na(P_value_str), "", P_value_str)
    )
  
  # 添加表头行 - 包含P值列
  header <- data.frame(
    Group_Header = "Subgroup",
    Level="Level",
    Total = "Total n",
    Event = "Event n (%)",
    HR_CI = "HR (95% CI)",
    P_value_str = "P value",
    P_interaction = "P for interaction",
    stringsAsFactors = FALSE
  )
  
  # 合并数据（表头在最前面）
  forest_data <- bind_rows(header, forest_data)
  
  # 创建标识汇总行（需要加粗的行）的逻辑向量
  summary_vector <- rep(FALSE, nrow(forest_data))
  
  # 表头行（第一行）需要加粗
  summary_vector[1] <- TRUE
  for (i in 2:nrow(forest_data)) {
    # 获取当前行的Group_Header内容
    current_header <- forest_data$Group_Header[i]
    
    # 识别Overall行
    if (current_header == "Overall") {
      summary_vector[i] <- TRUE
    }
    # 识别亚组标题行 - 通过检查是否是未缩进的标题
    else if (!startsWith(current_header, "    ") && current_header != "") {
      summary_vector[i] <- TRUE
    }
  }
  
  # 创建与forest_data行数匹配的HR数据向量
  mean_vals <- rep(NA, nrow(forest_data))
  lower_vals <- rep(NA, nrow(forest_data))
  upper_vals <- rep(NA, nrow(forest_data))
  
  # 找出实际有HR数据的行
  category_rows <- which(!is.na(forest_data$HR))
  
  # 填充HR数据
  if (length(category_rows) > 0 && nrow(forest_data) > 0) {
    mean_vals[category_rows] <- forest_data$HR[category_rows]
    lower_vals[category_rows] <- forest_data$Lower_CI[category_rows]
    upper_vals[category_rows] <- forest_data$Upper_CI[category_rows]
  }
  
  # 创建文本矩阵 - 包含P值列
  label_matrix <- as.matrix(forest_data[, c("Group_Header", "Level", "Total", "Event", "HR_CI", "P_value_str",  "P_interaction")])
  
  # 确定合适的x轴范围
  all_hr_vals <- na.omit(c(all_subgroups_results$Lower_CI, all_subgroups_results$Upper_CI))
  if (length(all_hr_vals) > 0) {
    x_min <- min(0.5, floor(min(all_hr_vals)*10)/10)
    x_max <- max(2.5, ceiling(max(all_hr_vals)*10)/10)
    x_ticks <- seq(x_min, x_max, length.out = 5)
  } else {
    x_min <- 0.5
    x_max <- 2.0
    x_ticks <- c(0.5, 1.0, 1.5, 2.0)
  }
  
  # 修正横线设置
  hrzl_lines <- list()
  hrzl_lines[["1"]] <- gpar(lty = 1, lwd = 2)
  hrzl_lines[["2"]] <- gpar(lty = 1, lwd = 2)
  hrzl_lines[[as.character(nrow(forest_data)+1)]] <- gpar(lty = 1, lwd = 2)
  
  # 定制标题 - 根据testcd
  title_text <- switch(current_testcd,
                       "ICU28D" = "28-day all-cause mortality",
                       "ICUXXD" = "ICU mortality",
                       "HOSPXXD" = "In-hospital mortality",
                       "OTHER" = "Other Outcome",
                       paste("Forest Plot for", current_testcd))
  
  # 绘制专业森林图
  p <- forestplot(
    labeltext = label_matrix,
    mean = mean_vals,
    lower = lower_vals,
    upper = upper_vals,
    is.summary = summary_vector,
    zero = 1,
    boxsize = 0.2,
    lineheight = unit(10, "mm"),
    colgap = unit(4, "mm"),
    graphwidth = unit(100, "mm"),
    line.margin = unit(2, "mm"),
    margin = unit(rep(10, 4), "mm"),
    col = fpColors(
      box = "#1F497D",
      lines = "#1F497D",
      zero = "black",
      summary = "#8064A2"
    ),
    xlab = "Hazard Ratio",
    txt_gp = fpTxtGp(
      label = gpar(cex = 0.9),
      ticks = gpar(cex = 0.8),
      xlab = gpar(cex = 1.0),
      title = gpar(cex = 1.2),
      summary = list(
        gpar(fontface = "bold"),
        gpar(fill = "gray", col = "gray", lwd = 10)
      )
    ),
    title = title_text,
    graph.pos = 7,
    hrzl_lines = hrzl_lines,
    clip = c(x_min, x_max),
    lwd.zero = 1.5,
    lwd.ci = 1.5,
    lwd.xaxis = 1.5,
    grid = TRUE,
    xticks = x_ticks,
    ci.vertices = TRUE,
    ci.vertices.height = 0.15
  ) |> fp_set_zebra_style("#EFEFEF", ignore_subheaders = TRUE)
  
  # 创建输出目录
  if (!dir.exists("forestplot_output")) {
    dir.create("forestplot_output")
  }
  
  # 设置图像尺寸
  width_inch <- 28
  height_inch <- 20
  
  # 保存为四种格式
  base_filename <- paste0("forestplot_output/Qlevel_", current_testcd, "_forestplot")
  
  # PNG格式
  png(paste0(base_filename, ".png"), width = 12, height = 20, units = "in", res = 300)
  print(p)
  dev.off()
  
  # SVG格式
  svg(paste0(base_filename, ".svg"), width = width_inch, height = height_inch)
  print(p)
  dev.off()
  
  # JPG格式
  jpeg(paste0(base_filename, ".jpg"), width = 12, height = 20, units = "in", res = 300, quality = 100)
  print(p)
  dev.off()
  
  # TIFF格式
  tiff(paste0(base_filename, ".tif"), width = width_inch, height = height_inch, units = "in", res = 300, compression = "lzw")
  print(p)
  dev.off()
  
  message(paste("森林图已保存:", base_filename))
}
