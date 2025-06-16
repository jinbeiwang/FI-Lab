# 加载必要包
library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)

# 定义绘图函数（解决所有问题）
generate_km_plots <- function(tte_data, testcd) {
  local_data <- as.data.frame(tte_data)
  # 数据准备（使用小写level）
  plot_data <- local_data %>%
    filter(testcd == !!testcd) %>%
    mutate(level = factor(level, levels = c("Q1", "Q2", "Q3", "Q4")))  # 变量名改为小写
  # 检查数据
  if(nrow(plot_data) == 0) {
    stop(paste("没有找到", testcd, "的数据"))
  }
  # 创建生存对象
  surv_obj <- Surv(time = plot_data$aval, event = plot_data$cnsr)
  
  # 智能标题生成
  title <- ifelse(str_detect(testcd, "HOSP"), "In-hospital mortality",
                  ifelse(str_detect(testcd, "ICU"), 
                         paste0(str_extract(testcd, "\\d+"), "-day all-cause mortality"),
                         paste(testcd, "Mortality")))
  km_fit <- with(plot_data, survfit(Surv(aval, cnsr) ~ level))
  # 拟合KM模型
  # km_fit <- survfit(surv_obj ~ level, data = plot_data)  # 使用level分组
  
  # 计算log-rank检验p值
  # pval <- surv_pvalue(km_fit)$pval
  surv_diff <- survdiff(Surv(aval, cnsr) ~ level, data = plot_data)
  pval <- surv_diff$pval
  pval_label <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", round(pval, 4)))
  # SCI期刊标准配色
  sci_palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")  # 蓝、橙、绿、红
  
  # 自动计算时间间隔
  max_time <- max(plot_data$aval, na.rm = TRUE)
  time_breaks <- pretty(seq(0, max_time, length.out = 8))
  break_step <- if(length(time_breaks) > 1) time_breaks[2] - time_breaks[1] else max_time/5
  
  # 创建基础图形（解决所有警告）
  km_plot <- ggsurvplot(
    km_fit,
    data = plot_data,
    conf.int = TRUE,
    risk.table = TRUE,
    legend.title = "FI-Lab",
    legend.labs = c("Q1", "Q2", "Q3", "Q4"),
    palette = sci_palette,
    ggtheme = theme_bw(base_size = 12),
    title = title,
    xlab = "Time (days)",
    ylab = "Survival Probability",
    break.time.by = break_step,  # 直接设置间隔解决scale警告
    risk.table.height = 0.25,
    pval = pval_label,
    pval.coord = c(0, 0.1),
    censor = TRUE,
    censor.shape = 3,
    censor.size = 2.5,
    risk.table.col = "strata",
    risk.table.y.text = TRUE,  # 显示分组标签
    risk.table.y.text.col = TRUE,  # 文本颜色与组别一致
    tables.theme = theme_cleantable(),
    font.title = c(14, "bold", "black"),
    font.x = c(12, "plain", "black"),
    font.y = c(12, "plain", "black"),
    font.legend = c(11, "plain", "black"),
    font.tickslab = c(10, "plain", "black"),
    legend = "top"  # 图例置于顶部
  )
  
  # 高级优化
  km_plot$plot <- km_plot$plot +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box.spacing = unit(0.2, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    guides(color = guide_legend(nrow = 1))
  
  # 优化风险表（显示分组标签）
  km_plot$table <- km_plot$table +
    labs(y = NULL) +  # 添加标签
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 5, b = 5),
      axis.title.y = element_text(size = 10, face = "bold")  # 标签样式
    )
  
  # 组合图形
  combined_plot <- arrange_ggsurvplots(
    list(km_plot),
    print = FALSE,
    ncol = 1,
    nrow = 1,
    heights = c(0.70, 0.30)
  )
  
  return(combined_plot)
}

# 输出函数（增加JPG和SVG格式）
save_km_plots <- function(plot_obj, testcd) {
  filename_base <- paste0("KM_plots/KM-", testcd)
  
  # 多种格式输出
  ggsave(paste0(filename_base, ".png"), plot_obj, width = 8.5, height = 7, dpi = 300)
  ggsave(paste0(filename_base, ".tif"), plot_obj, width = 8.5, height = 7, dpi = 300, compression = "lzw")
  ggsave(paste0(filename_base, ".jpg"), plot_obj, width = 8.5, height = 7, dpi = 300)  # 新增JPG
  ggsave(paste0(filename_base, ".svg"), plot_obj, width = 8.5, height = 7, dpi = 300)  # 新增SVG
}

# 主执行函数
create_all_km_plots <- function(tte_data) {
  testcd_list <- unique(tte_data$testcd)
  
  for (testcd in testcd_list) {
    tryCatch({
      km_plot <- generate_km_plots(tte_data, testcd)
      save_km_plots(km_plot, testcd)
      message(paste("成功创建", testcd, "的SCI标准KM曲线图"))
    }, error = function(e) {
      message(paste("创建", testcd, "KM曲线时出错:", e$message))
    })
  }
}

# 执行函数
tte_xxdays<-tte %>%
  filter(testcd  %in% c("ICU28D" , "HOSPXXD"))
# 执行函数（使用您的数据集）
create_all_km_plots(tte_xxdays)

