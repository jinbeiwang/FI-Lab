# 修复后的完整代码 - 添加P值列和多testcd输出
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
      event_n <- sum(subset_data$event == 1, na.rm = TRUE)
      event_percent <- round(event_n / total_n * 100, 1)
      event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
      
      # 在子集上拟合Cox模型
      tryCatch({
        # 构建协变量公式
        cov_formula <- if (length(covariates_m2) > 0) {
          paste(covariates_m2, collapse = " + ")
        } else {
          "1"
        }
        
        model_formula <- as.formula(paste("Surv(time, event) ~ flab100 +", cov_formula))
        model <- coxph(model_formula, data = subset_data)
        
        # 提取结果
        summary_result <- summary(model)
        coefs <- summary_result$coefficients
        
        # 提取关于暴露变量的系数
        exposure_row <- which(grepl("flab100", rownames(coefs)))
        
        if (length(exposure_row) > 0) {
          hr <- exp(coefs[exposure_row, "coef"])
          lower_ci <- exp(coefs[exposure_row, "coef"] - 1.96 * coefs[exposure_row, "se(coef)"])
          upper_ci <- exp(coefs[exposure_row, "coef"] + 1.96 * coefs[exposure_row, "se(coef)"])
          p_value <- coefs[exposure_row, "Pr(>|z|)"]  # 提取P值
          
          # 保存结果
          results_for_var[[as.character(category)]] <- data.frame(
            Subgroup = var_name,
            Category = category,
            Level = gsub("flab100", "", rownames(coefs)[exposure_row]),
            Total = total_n,
            Event = event_str,
            HR = hr,
            Lower_CI = lower_ci,
            Upper_CI = upper_ci,
            P_value = p_value,  # 添加P值列
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        message(paste("Error in subgroup", var_name, "-", category, ":", e$message))
      })
    }
    
    # 合并该变量的所有类别结果
    if (length(results_for_var) > 0) {
      var_df <- do.call(rbind, results_for_var)
      subgroup_results[[var_name]] <- var_df
      
      # 计算交互P值
      interaction_formula <- as.formula(
        paste("Surv(time, event) ~ flab100 *", var_name, "+", 
              paste(covariates_m2, collapse = " + "))
      )
      
      interaction_model <- tryCatch({
        coxph(interaction_formula, data = data)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(interaction_model)) {
        interaction_terms <- grep(paste0("flab100.*:", var_name), names(coef(interaction_model)), value = TRUE)
        if (length(interaction_terms) > 0) {
          interaction_p <- coef(summary(interaction_model))[interaction_terms, "Pr(>|z|)"]
          interaction_p_values[[var_name]] <- data.frame(
            Subgroup = var_name,
            Category = "Interaction",
            Level = "P for interaction",
            Total = NA,
            Event = NA,
            HR = NA,
            Lower_CI = NA,
            Upper_CI = NA,
            P_value = NA,
            P_interaction = min(interaction_p)  # 取最小p值
          )
        }
      }
    }
  }
  
  # 合并所有亚组结果
  all_subgroups_results <- if (length(subgroup_results) > 0) {
    do.call(rbind, subgroup_results)
  } else {
    data.frame()
  }
  if (length(interaction_p_values) > 0) {
    interaction_df <- do.call(rbind, interaction_p_values)
  } else {
    interaction_df <- data.frame(Subgroup="",P_interaction="NA")
  }
  
  # P_interaction <- interaction_df %>% select(Subgroup, P_interaction)
  
  # 合并数据集
  all_subgroups_results <- all_subgroups_results %>%
    left_join(interaction_df %>% select(Subgroup, P_interaction), by = "Subgroup")
  
  # 添加Overall行 - 总体效应
  tryCatch({
    # 构建协变量公式
    cov_formula <- if (length(covariates_m2) > 0) {
      paste(covariates_m2, collapse = " + ")
    } else {
      "1"
    }
    
    model_formula <- as.formula(paste("Surv(time, event) ~ flab100 +", cov_formula))
    overall_model <- coxph(model_formula, data = data)
    
    # 提取结果
    summary_result <- summary(overall_model)
    coefs <- summary_result$coefficients
    
    # 提取关于暴露变量的系数
    exposure_row <- which(grepl("flab100", rownames(coefs)))
    
    if (length(exposure_row) > 0) {
      hr <- exp(coefs[exposure_row, "coef"])
      lower_ci <- exp(coefs[exposure_row, "coef"] - 1.96 * coefs[exposure_row, "se(coef)"])
      upper_ci <- exp(coefs[exposure_row, "coef"] + 1.96 * coefs[exposure_row, "se(coef)"])
      p_value <- coefs[exposure_row, "Pr(>|z|)"]  # 提取P值
      
      # 计算总人数和事件数
      total_n <- nrow(data)
      event_n <- sum(data$event == 1, na.rm = TRUE)
      event_percent <- round(event_n / total_n * 100, 1)
      event_str <- sprintf("%d (%0.1f%%)", event_n, event_percent)
      
      # 创建Overall行
      overall_row <- data.frame(
        Subgroup = "Overall",
        Category = "Overall",
        Level = "",
        Total = total_n,
        Event = event_str,
        HR = hr,
        Lower_CI = lower_ci,
        Upper_CI = upper_ci,
        P_value = p_value,  # 添加P值
        P_interaction = NA,
        stringsAsFactors = FALSE
      )
      
      # 将Overall行添加到结果顶部
      all_subgroups_results <- bind_rows(overall_row, all_subgroups_results)
    }
  }, error = function(e) {
    message(paste("Error in overall model:", e$message))
  })
  
  # 使用metadata中的标签替换变量名
  # 假设metadata包含variable_name和label列
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
    arrange(Order, Subgroup, Category)
  
  # 提取Subgroup label,为接下来拼接做准备
  Group_Header<-all_subgroups_results %>%
    filter(Subgroup !="Overall")%>%
    distinct(Order,Subgroup,P_interaction) %>%
    select(Order,Subgroup,P_interaction)
  
  # 1. 拼接Subgroup label,并缩进 category内容
  # 2. 处理HR_CI，P值 format
  # 3. P for interaction 仅出现在subgroup header 那一行
  forest_data <- all_subgroups_results %>% 
    bind_rows(Group_Header) %>%
    mutate(
      HR_CI = ifelse(is.na(HR),"",sprintf("%.2f (%.2f-%.2f)", HR, Lower_CI, Upper_CI)),
      P_value_str = ifelse(is.na(P_value), "", 
                           ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))),  # 格式化P值
      P_interaction = ifelse(is.na(P_interaction), "", 
                             ifelse(P_interaction < 0.001, "<0.001", sprintf("%.3f", P_interaction)))
    ) %>%
    arrange(Order,Subgroup,desc(is.na(Category)),Category) %>%
    mutate(Group_Header = ifelse(!is.na(Category) & Category!="Overall", paste0("  ", as.character(Category)), Subgroup),
           P_interaction = ifelse(!duplicated(P_interaction), P_interaction, "")
    ) %>%
    select(Group_Header, Total, Event, HR_CI, P_value_str, HR, Lower_CI, Upper_CI,   P_interaction) %>%  # 添加P值列
    relocate(Group_Header, Total, Event, HR_CI, P_value_str,P_interaction)  # 调整列顺序
  
  # 修复类型不匹配问题
  forest_data <- forest_data %>%
    mutate(
      Total = ifelse(is.na(Total), "", as.character(Total)),
      Event = ifelse(is.na(Event), "", as.character(Event))
    )
  
  # 添加表头行 - 包含P值列
  header <- data.frame(
    Group_Header = "Subgroup",
    Total = "Total n",
    Event = "Event n (%)",
    HR_CI = "HR (95% CI)",
    P_value_str = "P-value",  # 添加P值列标题
    P_interaction = "P for interaction",
    stringsAsFactors = FALSE
  )
  
  # 合并数据（表头在最前面）
  forest_data <- bind_rows(header, forest_data)
  
  # 标识组标题行
  is_header <- forest_data$Group_Header != "" | 
    forest_data$Group_Header == "Subgroup"  # 包含表头行
  
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
    else if (!startsWith(current_header, "  ")) {
      summary_vector[i] <- TRUE
    }
  }
  # 创建与forest_data行数匹配的HR数据向量
  # 初始化所有行为NA
  mean_vals <- rep(NA, nrow(forest_data))
  lower_vals <- rep(NA, nrow(forest_data))
  upper_vals <- rep(NA, nrow(forest_data))
  
  # 找出实际有HR数据的行（类别行）
  category_rows <- which(!is.na(forest_data$HR))
  
  # 从all_subgroups_results中提取HR数据填充到正确位置
  if (length(category_rows) > 0 && nrow(forest_data) > 0) {
    mean_vals[category_rows] <- forest_data$HR[category_rows]
    lower_vals[category_rows] <- forest_data$Lower_CI[category_rows]
    upper_vals[category_rows] <- forest_data$Upper_CI[category_rows]
  }
  
  # 创建文本矩阵 - 包含P值列
  label_matrix <- as.matrix(forest_data[, c("Group_Header",  "Total", "Event",  "HR_CI", "P_value_str", "P_interaction")])
  
  # 确定合适的x轴范围
  all_hr_vals <- na.omit(c(all_subgroups_results$Lower_CI, all_subgroups_results$Upper_CI))
  if (length(all_hr_vals) > 0) {
    x_min <- max(0.5, floor(min(all_hr_vals)*10)/10)  # 确保最小值不低于0.5
    x_max <- min(2.5, ceiling(max(all_hr_vals)*10)/10) # 确保最大值不超过2.5
    x_ticks <- seq(x_min, x_max, length.out = 5)       # 创建5个刻度点
  } else {
    x_min <- 0.5
    x_max <- 2.0
    x_ticks <- c(0.5, 1.0, 1.5, 2.0)
  }
  
  # 修正横线设置 - 使用一致的语法
  hrzl_lines <- list()
  hrzl_lines[["1"]] <- gpar(lty = 1, lwd = 2)  # 顶部线（表头上方）
  hrzl_lines[["2"]] <- gpar(lty = 1, lwd = 2)  # 顶部线（表头下方）
  hrzl_lines[[as.character(nrow(forest_data)+1)]] <- gpar(lty = 1, lwd = 2)  # 底部线（最后一行下方）
  
  # 定制标题 - 根据testcd
  title_text <- switch(current_testcd,
                       "ICU28D" = "28-day all-cause mortality",
                       "ICUXXD" = "ICU mortality",
                       "HOSPXXD" = "In-hospital mortality",
                       "OTHER" = "Other Outcome",
                       paste("Forest Plot for", current_testcd))  # 默认值
  
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
    line.margin = unit(1, "mm"),
    col = fpColors(
      box = "#1F497D",    # 方框色：专业蓝
      lines = "#1F497D",  # 连接线色：森林绿
      zero = "black",   # 无效线色：醒目红
      summary = "#8064A2" # 汇总行色：典雅紫
    ),
    xlab = "Hazard Ratio",
    txt_gp = fpTxtGp(
      label = gpar(cex = 0.9),
      ticks = gpar(cex = 0.8),
      xlab = gpar(cex = 1.0),
      title = gpar(cex = 1.2),
      summary = list (gpar(fontface = "bold"),
                      gpar(fill = "gray", col = "gray", lwd = 10)
      )# 亚组标题加粗
    ),
    title = title_text,  # 使用定制标题
    graph.pos = 6,  # 将图形放在倒数第二列
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
  
  # 创建输出目录（如果不存在）
  if (!dir.exists("forestplot_output")) {
    dir.create("forestplot_output")
  }
  
  # 设置图像尺寸
  width_inch <- 15
  height_inch <- 11.25
  
  # 保存为四种格式
  base_filename <- paste0("forestplot_output/filab_", current_testcd, "_forestplot")
  
  # PNG格式
  png_filename <- paste0(base_filename, ".png")
  png(png_filename, width = width_inch, height = height_inch, units = "in", res = 300)
  print(p)
  dev.off()
  
  # SVG格式
  svg_filename <- paste0(base_filename, ".svg")
  svg(svg_filename, width = width_inch, height = height_inch)
  print(p)
  dev.off()
  
  # JPG格式
  jpg_filename <- paste0(base_filename, ".jpg")
  jpeg(jpg_filename, width = width_inch, height = height_inch, units = "in", res = 300, quality = 100)
  print(p)
  dev.off()
  
  # TIFF格式
  tif_filename <- paste0(base_filename, ".tif")
  tiff(tif_filename, width = width_inch, height = height_inch, units = "in", res = 300, compression = "lzw")
  print(p)
  dev.off()
  
  message(paste("森林图已保存:", png_filename, svg_filename, jpg_filename, tif_filename))
}
