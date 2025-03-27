
install.packages("Hmisc")
library(Hmisc)

install.packages("dplyr")
library(dplyr)

install.packages("https://cran.r-project.org/src/contrib/Archive/ggpubr/ggpubr_0.2.4.tar.gz", repo=NULL, type="source")
install.packages("ggpubr")
library(ggpubr)

install.packages("car")
library(car)

install.packages("dunn.test")
library(dunn.test)

install.packages("FSA")
library(FSA)

dane <- read.csv2("przykladoweDane-Projekt.csv", sep = ";")
dane


impute_mean_by_group <- function(df, group_col) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric) & colnames(df) != group_col]
  df <- df %>%
    group_by_at(group_col) %>%
    mutate(across(all_of(numeric_cols), ~ impute(.x, mean)))
  return(as.data.frame(df))
}

dane_imputed <- impute_mean_by_group(dane, "grupa")
dane_imputed

report_outliers <- function(df) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  outliers_report <- data.frame(parameter = character(), outlier_value = numeric(), stringsAsFactors = FALSE)
  
  for (param in numeric_cols) {
    boxplot_data <- boxplot(df[[param]], plot = FALSE)
    outliers <- boxplot_data$out
    
    if (length(outliers) > 0) {
      outliers_report <- rbind(outliers_report, data.frame(parameter = rep(param, length(outliers)), outlier_value = outliers))
    }
  }
  print("Raport wartości odstających:")
  print(outliers_report)
}

report_outliers(dane_imputed)

par(mfrow = c(1, 1)) 

numeric_cols <- colnames(dane_imputed)[sapply(dane_imputed, is.numeric)]
unique_groups <- unique(dane_imputed$grupa)

######boxplot######

for (group in unique_groups) {
  dane_group <- dane_imputed[dane_imputed$grupa == group, ]
  
  for (col in numeric_cols) {
    
    png(paste0("boxplot_", group, "_", col, ".png"))
    
    
    boxplot_data <- boxplot(dane_group[[col]], outline = TRUE, main = paste("Group:", group, "-", col))
    outliers <- boxplot_data$out
    if (length(outliers) > 0) {
      text(rep(1, length(outliers)), outliers, labels = outliers, pos = 4, col = "red")
    }
    
    
    dev.off()
  }
}


library(ggplot2)

for (col in numeric_cols) {

  boxplot_plot <- ggplot(dane_imputed, aes(x = grupa, y = .data[[col]], fill = grupa)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) + 
    labs(title = paste("Boxplot for", col), x = "Group", y = col) +
    theme_minimal() + 
    theme(legend.position = "none") 
  
  ggsave(filename = paste0("boxplot_", col, ".png"), plot = boxplot_plot, width = 8, height = 6)
}


##########################podsumowanie ogolne#################################
generate_summary <- function(df) {
  summary_list <- list()
  for (col in names(df)) {
    col_summary <- summary(df[[col]])
    if (is.numeric(df[[col]])) {
      col_summary <- as.data.frame(t(col_summary))
    } else {
      col_summary <- as.data.frame(table(df[[col]]))
      colnames(col_summary) <- c("Value", "Frequency")
    }
    col_summary$Variable <- col
    summary_list[[col]] <- col_summary
  }
  return(summary_list)
}

summary_results <- generate_summary(dane_imputed)
summary_combined <- data.frame()


for (col_name in names(summary_results)) {
  assign(paste0("summary_", col_name), summary_results[[col_name]])
}



##########################podsumowanie grupowe#############################

podsumowanie_kolumn <- function(df, group_col) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  
  for (col in numeric_cols) {
    summary_df <- df %>%
      group_by_at(group_col) %>%
      summarise(
        variable = col,
        count = n(),
        min = min(.data[[col]], na.rm = TRUE),
        median = median(.data[[col]], na.rm = TRUE),
        mean = mean(.data[[col]], na.rm = TRUE),
        max = max(.data[[col]], na.rm = TRUE),
        sd = sd(.data[[col]], na.rm = TRUE),
        IQR = IQR(.data[[col]], na.rm = TRUE),
        var = var(.data[[col]], na.rm = TRUE)
      )
    assign(paste0("podsumowanie_", col), summary_df, envir = .GlobalEnv)
  }
}
podsumowanie_kolumn(dane, "grupa")

################################wykres gestosci##################################################

install.packages("ggplot2")
install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

numeric_cols <- colnames(dane_imputed)[sapply(dane_imputed, is.numeric)]

for (col in numeric_cols) {
  
  plot_density <- ggdensity(dane_imputed, x = col,
                            color = "grupa", fill = "grupa",
                            palette = c("#99cc00", "#660099", "#0047b3"),
                            ylab = "gęstość", xlab = paste(col, "[mg/l]")) +
    facet_wrap(~ grupa, scales = "free")
  
  ggsave(filename = paste0("density_plot_", col, ".png"), plot = plot_density, width = 8, height = 6)
}


#################shapiro-wilk###################################

test_shapiro_all_vars <- function(df, group_col) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]  # Znalezienie wszystkich kolumn numerycznych
  
  shapiro_results <- lapply(numeric_cols, function(col) {
    df %>%
      group_by_at(group_col) %>%
      summarise(
        variable = col,
        statistic = shapiro.test(get(col))$statistic,
        p_value = shapiro.test(get(col))$p.value
      )
  })
  
  shapiro_results_combined <- do.call(rbind, shapiro_results)
  
  shapiro_results_combined <- shapiro_results_combined %>%
    mutate(normality = ifelse(p_value > 0.05, "zgodny z rozkładem normalnym", "niezgodny"))
  
  return(shapiro_results_combined)
}

shapiro_results <- test_shapiro_all_vars(dane_imputed, "grupa")
shapiro_results  # Wyświetlenie wyników

######################levene########################

test_levene_all_vars <- function(df, group_col) {
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  
  levene_results <- data.frame(variable = character(), p_value = numeric(), variance_homogeneity = character(), stringsAsFactors = FALSE)
  
  for (col in numeric_cols) {
    levene_test <- leveneTest(as.formula(paste(col, "~", group_col)), data = df)
    
    p_value_levene <- levene_test[["Pr(>F)"]][1]
    
    levene_results <- rbind(levene_results, 
                            data.frame(variable = col, 
                                       p_value = p_value_levene, 
                                       variance_homogeneity = ifelse(p_value_levene > 0.05, "jednorodne wariancje", "niejednorodne wariancje")))
  }
  
  return(levene_results)
}
levene_results <- test_levene_all_vars(dane_imputed, "grupa")

print(levene_results)


install.packages("multcomp")
install.packages("dunn.test")
library(multcomp)
library(dunn.test)

perform_tests <- function(df, group_col, shapiro_results, levene_results) {
  
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  
  test_results <- data.frame(variable = character(), test_type = character(), p_value = numeric(), stringsAsFactors = FALSE)
  
  posthoc_results <- list()
  
  for (col in numeric_cols) {
    
    shapiro_p_values <- shapiro_results$p_value[shapiro_results$variable == col]
    levene_p <- levene_results$p_value[levene_results$variable == col]
    
    is_normal <- all(shapiro_p_values > 0.05)
    
    if (is_normal && levene_p > 0.05) {
      anova_test <- aov(as.formula(paste(col, "~", group_col)), data = df)
      p_value <- summary(anova_test)[[1]][["Pr(>F)"]][1]
      test_results <- rbind(test_results, data.frame(variable = col, test_type = "ANOVA", p_value = p_value))
      
      if(p_value < 0.05) {
        tukey_test <- TukeyHSD(anova_test)
        tukey_results <- as.data.frame(tukey_test[[1]])
        tukey_results$Comparison <- rownames(tukey_results)
        tukey_results$Variable <- col
        posthoc_results[[paste0(col, "_Tukey")]] <- tukey_results
      }
      
    } else {
      kruskal_test <- kruskal.test(as.formula(paste(col, "~", group_col)), data = df)
      p_value <- kruskal_test$p.value
      test_results <- rbind(test_results, data.frame(variable = col, test_type = "Kruskal-Wallis", p_value = p_value))
      
      if(p_value < 0.05) {
        # Test post hoc Dunna
        dunn_test <- dunn.test(df[[col]], df[[group_col]], kw = TRUE)
        dunn_results <- as.data.frame(dunn_test$P.adjusted)
        dunn_results$Comparison <- rownames(dunn_results)
        dunn_results$Variable <- col
        posthoc_results[[paste0(col, "_Dunn")]] <- dunn_results
      }
    }
  }
  
  list(test_results = test_results, posthoc_results = posthoc_results)
}

final_test_results <- perform_tests(dane_imputed, "grupa", shapiro_results, levene_results)

print(final_test_results$test_results)

print("Wyniki testów post hoc:")
for (result_name in names(final_test_results$posthoc_results)) {
  print(paste("Test:", result_name))
  print(final_test_results$posthoc_results[[result_name]])
}

#test korelacji

interpret_correlation <- function(r) {
  if (r <= -0.7) {
    return("bardzo silna korelacja ujemna")
  } else if (r <= -0.5) {
    return("silna korelacja ujemna")
  } else if (r <= -0.3) {
    return("korelacja ujemna o średnim natężeniu")
  } else if (r <= -0.2) {
    return("słaba korelacja ujemna")
  } else if (r < 0.2) {
    return("brak korelacji")
  } else if (r < 0.3) {
    return("słaba korelacja dodatnia")
  } else if (r < 0.5) {
    return("korelacja dodatnia o średnim natężeniu")
  } else if (r < 0.7) {
    return("silna korelacja dodatnia")
  } else {
    return("bardzo silna korelacja dodatnia")
  }
}

test_correlation <- function(data, col1, col2, shapiro_results, group_col) {
  normality_col1 <- shapiro_results %>% filter(variable == col1) %>% pull(p_value)
  normality_col2 <- shapiro_results %>% filter(variable == col2) %>% pull(p_value)
  
  use_spearman <- any(normality_col1 <= 0.05) || any(normality_col2 <= 0.05)
  
  method <- if (use_spearman) "spearman" else "pearson"
  
  result <- cor.test(data[[col1]], data[[col2]], method = method)
  r_value <- result$estimate
  p_value <- result$p.value
  korelacja <- if (r_value > 0) "korelacja dodatnia" else if (r_value == 0) "brak korelacji" else "korelacja ujemna"
  sila_korelacji <- interpret_correlation(r_value)
  
  return(list(p_value = p_value, r_value = r_value, korelacja = korelacja, sila_korelacji = sila_korelacji, method = method))
}

cor_results_df <- data.frame(
  Group = character(),
  parametr1 = character(),
  parametr2 = character(),
  Test = character(),
  Est = numeric(),
  p_value = numeric(),
  interpretacja = character(),
  stringsAsFactors = FALSE
)

unique_groups <- unique(dane_imputed$grupa)

for (group in unique_groups) {
  dane_group <- dane_imputed %>% filter(grupa == group)
  
  numeric_cols <- colnames(dane_group)[sapply(dane_group, is.numeric)]
  
  for (i in 1:(length(numeric_cols) - 1)) {
    for (j in (i + 1):length(numeric_cols)) {
      col1 <- numeric_cols[i]
      col2 <- numeric_cols[j]
      
      korelacja_result <- test_correlation(dane_group, col1, col2, shapiro_results, "grupa")
      
      cor_results_df <- rbind(cor_results_df, data.frame(
        Group = group,
        parametr1 = col1,
        parametr2 = col2,
        Test = korelacja_result$method,
        Est = korelacja_result$r_value,
        p_value = korelacja_result$p_value,
        interpretacja = korelacja_result$sila_korelacji
      ))
    }
  }
}

print(cor_results_df)

write.table(cor_results_df, file = "korelacja_report.txt", sep = "\t", quote = FALSE, row.names = FALSE)
cat("Raport korelacji zapisany do: korelacja_report.txt\n")

########################################wykres - korelacja

unique_groups <- unique(dane_imputed$grupa)

for (group in unique_groups) {
  dane_group <- dane_imputed %>% filter(grupa == group)
  numeric_cols_group <- colnames(dane_group)[sapply(dane_group, is.numeric)]
  
  cor_matrix_group <- cor(dane_group[numeric_cols_group], use = "pairwise.complete.obs")
  
  cor_matrix_melted <- melt(cor_matrix_group)
  colnames(cor_matrix_melted) <- c("Variable1", "Variable2", "Correlation")
  
  p <- ggplot(cor_matrix_melted, aes(x = Variable1, y = Variable2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    labs(title = paste("Macierz Korelacji dla grupy", group), x = "parametr", y = "parametr") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("correlation_matrix_", group, ".png"), plot = p, width = 10, height = 8)
}






