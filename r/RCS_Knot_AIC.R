# Load required packages
library(survival)
library(rms)
library(dplyr)
library(officer)
library(flextable)
library(purrr)

# Function to calculate optimal knots using AIC
calculate_optimal_knots <- function(tte_data, testcd, covariates = NULL, knot_range = 3:7) {
  # Prepare data
  plot_data <- tte_data %>%
    dplyr::filter(testcd == !!testcd) %>%
    dplyr::select(-any_of(c("Q1", "Q2", "Q3", "Q4"))) %>%
    dplyr::mutate(flab100 = as.numeric(flab100)) %>% 
    dplyr::filter(!is.na(flab100))
  
  # Check variability
  if (length(unique(plot_data$flab100)) < 2 || 
      sd(plot_data$flab100, na.rm = TRUE) < .Machine$double.eps^0.5) {
    stop("Insufficient variability in flab100 [testcd: ", testcd, "]")
  }
  
  # Handle covariates
  if (!is.null(covariates)) {
    existing_covars <- covariates[covariates %in% names(plot_data)]
    plot_data <- plot_data %>%
      select(all_of(c("aval", "cnsr", "flab100", existing_covars))) %>%
      na.omit()
  }
  
  # Sample size checks
  if (nrow(plot_data) < 30) warning("Small sample size (n=", nrow(plot_data), ")")
  if (sum(plot_data$cnsr == 1) < 5) stop("Insufficient events (<5)")
  
  # Create datadist object BEFORE model fitting (fixes the error)
  dd <- datadist(plot_data)
  options(datadist = dd)#这里之前因为用了"dd"导致了报错，注意不带引号，是对象不是字符
  
  # Create survival object
  surv_obj <- Surv(time = plot_data$aval, event = plot_data$cnsr)
  
  # Initialize results storage
  results <- data.frame()
  models <- list()
  
  # Loop through knot range
  for (k in knot_range) {
    # Build formula
    if (is.null(covariates)) {
      formula_str <- "surv_obj ~ rcs(flab100, k)"
    } else {
      covar_formula <- paste(existing_covars, collapse = " + ")
      formula_str <- paste("surv_obj ~ rcs(flab100, k) +", covar_formula)
    }
    
    formula <- as.formula(formula_str)
    
    # Fit model
    fit <- tryCatch(
      {
        cph(formula, data = plot_data, x = TRUE, y = TRUE)
      },
      error = function(e) {
        warning("Fit failed for knots ", k, ": ", e$message)
        NULL
      }
    )
    
    # Calculate statistics
    if (!is.null(fit)) {
      loglik <- fit$loglik[2]
      df <- length(fit$coefficients)
      aic_val <- -2 * loglik + 2 * df
      bic_val <- -2 * loglik + log(nrow(plot_data)) * df
      
      # Extract ANOVA results
      anova_res <- anova(fit)
      overall_p <- ifelse("flab100" %in% rownames(anova_res), 
                          anova_res["flab100", "P"], NA)
      nonlinear_p <- ifelse("Nonlinear" %in% rownames(anova_res), 
                            anova_res["Nonlinear", "P"], NA)
      
      # Store results
      row <- data.frame(
        Knots = k,
        AIC = round(aic_val, 2),
        BIC = round(bic_val, 2),
        LogLikelihood = round(loglik, 2),
        DF = df,
        Overall_P = format_p(overall_p),
        Nonlinear_P = format_p(nonlinear_p),
        Converged = "Yes"
      )
      
      results <- rbind(results, row)
      models[[as.character(k)]] <- fit
    } else {
      row <- data.frame(
        Knots = k,
        AIC = NA,
        BIC = NA,
        LogLikelihood = NA,
        DF = NA,
        Overall_P = NA,
        Nonlinear_P = NA,
        Converged = "No"
      )
      results <- rbind(results, row)
    }
  }
  
  # Identify best knot
  best_row <- which.min(results$AIC)
  best_knot <- ifelse(length(best_row) > 0, results$Knots[best_row], NA)
  
  return(list(
    aic_table = results,
    best_knot = best_knot,
    models = models
  ))
}

# Format p-values
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "p < 0.001"
  else if (p < 0.01) "p < 0.01"
  else if (p < 0.05) "p < 0.05"
  else paste0("p = ", round(p, 3))
}

# Save AIC results to Word
save_aic_results <- function(aic_results, testcd, output_dir = "RCS_Results") {
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create Word document
  doc <- read_docx()
  
  # Add title
  title <- paste("AIC Knot Selection Report -", testcd)
  doc <- body_add_par(doc, title, style = "heading 1")
  doc <- body_add_par(doc, paste("Generated:", format(Sys.Date(), "%Y-%m-%d")), style = "Normal")
  doc <- body_add_par(doc, " ", style = "Normal")  # Blank line
  
  # Add summary
  doc <- body_add_par(doc, "Analysis Summary", style = "heading 2")
  
  summary_text <- paste0(
    "Test Code: ", testcd, "\n",
    "Sample Size: ", aic_results$sample_size, "\n",
    "Event Count: ", aic_results$event_count, "\n",
    "Optimal Knots: ", ifelse(is.na(aic_results$best_knot), "Not determined", aic_results$best_knot)
  )
  
  doc <- body_add_par(doc, summary_text, style = "Normal")
  doc <- body_add_par(doc, " ", style = "Normal")  # Blank line
  
  # Add statistics table
  doc <- body_add_par(doc, "Knot Comparison Statistics", style = "heading 2")
  
  # Format table
  ft <- flextable(aic_results$aic_table) %>%
    theme_zebra() %>%
    autofit() %>%
    set_caption(paste(testcd, "- Knot Comparison"))
  
  # Highlight best knot row
  if (!is.na(aic_results$best_knot)) {
    best_row <- which(aic_results$aic_table$Knots == aic_results$best_knot)
    ft <- bg(ft, i = best_row, bg = "#FFF2CC")
  }
  
  # Add column labels
  ft <- set_header_labels(ft,
                          Knots = "Knots",
                          AIC = "AIC",
                          BIC = "BIC",
                          LogLikelihood = "Log Likelihood",
                          DF = "Degrees of Freedom",
                          Overall_P = "Overall P-value",
                          Nonlinear_P = "Nonlinear P-value",
                          Converged = "Convergence Status")
  
  doc <- body_add_flextable(doc, value = ft)
  
  # Add explanations
  doc <- body_add_par(doc, "Metric Explanations", style = "heading 3")
  
  explanations <- c(
    "• AIC (Akaike Information Criterion): Lower values indicate better model fit",
    "• BIC (Bayesian Information Criterion): Similar to AIC with stronger penalty for complexity",
    "• Log Likelihood: Measure of model fit (higher absolute values indicate better fit)",
    "• Degrees of Freedom: Number of parameters in the model",
    "• Overall P-value: Significance test for the entire variable",
    "• Nonlinear P-value: Test for nonlinear relationship",
    "• Highlighted row indicates optimal knot selection based on AIC"
  )
  
  for (exp in explanations) {
    doc <- body_add_par(doc, exp, style = "Normal")
  }
  
  # Save Word document
  word_file <- file.path(output_dir, paste0("AIC_Selection_", testcd, ".docx"))
  print(doc, target = word_file)
  message("AIC results saved to: ", word_file)
}

# Main execution function
run_aic_analysis <- function(tte_data, covariates = NULL, output_dir = "RCS_Results") {
  testcd_list <- unique(tte_data$testcd)
  all_results <- list()
  
  for (testcd in testcd_list) {
    message("\n===== Analyzing: ", testcd, " =====")
    
    tryCatch({
      # Calculate sample size and event count
      plot_data <- tte_data %>%
        dplyr::filter(testcd == !!testcd) %>%
        dplyr::mutate(flab100 = as.numeric(flab100)) %>% 
        dplyr::filter(!is.na(flab100))
      
      sample_size <- nrow(plot_data)
      event_count <- sum(plot_data$cnsr == 1, na.rm = TRUE)
      
      # Perform AIC analysis
      aic_result <- calculate_optimal_knots(tte_data, testcd, covariates)
      
      # Add sample information
      aic_result$sample_size <- sample_size
      aic_result$event_count <- event_count
      
      # Save results
      save_aic_results(aic_result, testcd, output_dir)
      
      # Record results
      all_results[[testcd]] <- aic_result
      
      # Print summary
      message("Sample size: ", sample_size)
      message("Event count: ", event_count)
      message("Optimal knots: ", ifelse(is.na(aic_result$best_knot), "Not determined", aic_result$best_knot))
      message("AIC results saved")
      
    }, error = function(e) {
      message("❌ Analysis failed: ", e$message)
      all_results[[testcd]] <<- paste("Failed:", e$message)
    })
  }
  
  return(all_results)
}

# ====================== Usage Example ====================== #
tte_xxdays <- tte %>%
  filter(testcd %in% c("ICU28D", "HOSPXXD"))  # 示例多个testcd
covariates_m2 <- metadata$variable_name[metadata$covars_m2 == 1]

# 步骤2: 执行AIC分析
aic_results <- run_aic_analysis(
  tte_data = tte_xxdays,
  covariates = covariates_m2,
  output_dir = "RCS_Results"
)