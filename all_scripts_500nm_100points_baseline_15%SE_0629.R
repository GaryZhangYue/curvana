#可以实现的功能：将“数字.spm.txt”结尾的文件改成对应的“curve数字.txt”
#获取当前目录
getwd() 
setwd(getwd())
# Section 1 rename ---- 
# 获取当前工作目录下的所有文件名，筛选出符合特定模式的文件
files <- list.files(pattern = "\\d+\\.spm\\.txt$")

# 循环遍历符合条件的文件并重命名
for (file in files) {
  # 使用正则表达式匹配末尾的数字
  parts <- regmatches(file, regexec("(\\d+)\\.spm\\.txt$", file))
  index <- parts[[1]][2]  # 获取匹配的数字部分
  
  # 构造新文件名
  new_name <- paste0("curve", index, ".txt")
  
  # 重命名文件
  if (file.rename(file, new_name)) {
    cat("Renamed", file, "to", new_name, "\n")
  } else {
    cat("Failed to rename", file, "\n")
  }
}

# Section 2 multi preview ----
#可以实现的功能：将工作目录下所有的curve数字.txt都导入，且将所有txt中的retract数据绘制折线图。并且计算所有折线图的最低点，并且绘制箱型图于左上角。
# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.txt")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组


# First read all data to determine the axes range
for (file in files) {
  data <- read.table(file, header = TRUE)
  all_x <- c(all_x, data[, 2])  # Collect all x data
  all_y <- c(all_y, data[, 4])  # Collect all y data
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.table(files[i], header = TRUE)
  x_data <- data[, 2]
  y_data <- data[, 4]
  
  # Draw the line graph, specify color for each file
  lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  
  # Find the lowest point
  min_index <- which.min(y_data)
  min_value <- y_data[min_index]
  min_values[i] <- min_value  # Store minimum value
  min_x <- x_data[min_index]
  # Store min points for legend
  min_points[i, ] <- c(min_x, min_value)
  names[i] <- paste("Curve", sub("curve(\\d+)\\.txt", "\\1", files[i]))
  
  
}

# Add a legend to the plot
#legend("topright", legend = names, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)


# Open a new plot area for the boxplot in the top left corner
par(fig = c(0.1, 0.4, 0.5, 0.85), new = TRUE,mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")





# Section 3 reverse ----
#可以实现的功能：将y轴数据从后往前排列
# 加载必要的库
library(tidyverse)

# 读取目录下所有文件的名称
files <- list.files(pattern = "curve\\d+\\.txt")

# 遍历所有文件
for (file in files) {
  # 读取文件，假设是以制表符分隔
  data <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  # 检查是否有足够的列
  if (ncol(data) >= 4) {
    # 将第 4 列的数据从后往前排列
    reversed_column <- rev(data[[4]])
    
    # 覆盖第 4 列
    data[[4]] <- reversed_column
    
    # 生成新的文件名
    new_file_name <- gsub("curve(\\d+)\\.txt$", "reversed_curve\\1.txt", file)
    
    # 保存修改后的文件，使用相同的分隔符
    write.table(data, new_file_name, row.names = FALSE, sep = "\t", quote = FALSE)
    
    # 打印原始文件名和新文件名
    cat(sprintf("Processed: %s -> %s\n", file, new_file_name))
  } else {
    cat(sprintf("Skipping %s, not enough columns.\n", file))
  }
}

# Section remove curve.txt ----
file.remove(list.files(pattern = "^curve\\d+\\.txt$"))


# Section individual view of all plot before process
#可以实现的功能：单独对每个在总图中出现的未处理过的数据进行绘图，且保存。用于人工筛选想要的图和文件。
# 创建用于保存图形的文件夹，如果不存在就创建一个
plot_dir <- "Original_individual_plot_for_selection"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.txt")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# 遍历每个文件，为每个文件单独创建一个图形
for (i in seq_along(files)) {
  # 设置PDF输出文件路径
  pdf_file_path <- file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(files[i])), ".pdf"))
  
  # 开始PDF绘图设备，指定文件大小为10x12英寸
  tryCatch({
    pdf(pdf_file_path, width = 12, height = 10)
    
    # 绘制图形
    plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
         xlab = "X Axis", ylab = "Y Axis", main = paste("Curve Analysis for", basename(files[i])))
    
    # 从对应文件中读取数据
    data <- read.table(files[i], header = TRUE, sep = "", stringsAsFactors = FALSE)
    x_data <- data[, 2][1:512]
    y_data <- data[, 4][1:512]
    
    # 绘制线条
    lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
    
    # 找出最小值及其索引
    min_index <- which.min(y_data)
    min_value <- y_data[min_index]
    min_x_value <- x_data[min_index]
    
    # 在图上标注最小值
    points(min_x_value, min_value, col = "red", pch = 19, cex = 1.5)
    text(min_x_value, min_value, labels = sprintf("Min: %.2f", min_value), pos = 1, offset = 1, col = "red")
    
  }, finally = {
    # 确保每次都关闭PDF设备
    dev.off()
  })
}



# Section 4 find se... -------有5%分位数限制，即先算出所有的se。然后看谁的se小于5%的分位数，就会被替换为那个5%分位数的阈值。这样做的目的是，避免斜的这一段被过度矫正，导致最低点x值小于0
###################自由选择计算se
library(dplyr)
library(readr)
library(stringr)

# ---------- 用户可自定义的参数 ----------0.95就是前百分之5的最大数。
percentile_threshold <- 0.85  # 你可以改成 0.85, 0.99, 0.9 等
# ----------------------------------------

# 读取所有 curve 文件
files <- list.files(pattern = "curve\\d+\\.txt")

# sensitivity 拟合函数
calc_sensitivity_from_last_12 <- function(x, y) {
  if (length(x) < 12 || length(y) < 12) stop("Not enough data points.")
  
  x_range <- tail(x, 12)
  y_range <- tail(y, 12)
  
  best_sensitivity <- NA
  best_r_squared <- -1
  best_interval <- NULL
  best_intercept <- NA
  best_model <- NULL
  
  for (start in 1:(12 - 8 + 1)) {
    for (length in 8:min(12, 12 - start + 1)) {
      current_x <- x_range[start:(start + length - 1)]
      current_y <- y_range[start:(start + length - 1)]
      model <- lm(current_y ~ current_x)
      r_squared <- summary(model)$r.squared
      
      if (r_squared > best_r_squared) {
        best_r_squared <- r_squared
        best_model <- model
        best_sensitivity <- -coef(model)['current_x']
        best_intercept <- coef(model)[1]
        best_interval <- list(start = start, end = start + length - 1)
      }
    }
  }
  
  if (!is.null(best_model)) {
    global_start <- length(x) - 12 + best_interval$start
    global_end <- length(x) - 12 + best_interval$end
    return(list(
      sensitivity = best_sensitivity,
      r_squared = best_r_squared,
      interval = c(global_start, global_end),
      intercept = best_intercept
    ))
  } else {
    return(list(sensitivity = NA, r_squared = NA, interval = NA, intercept = NA))
  }
}

# 第一步：收集所有 sensitivity（不改动）
initial_results <- lapply(files, function(file) {
  data <- read_delim(file, delim = "\t", col_names = TRUE)
  if (ncol(data) < 4) return(NULL)
  
  x <- data[[2]]
  y <- data[[4]]
  result <- calc_sensitivity_from_last_12(x, y)
  if (is.na(result$sensitivity)) return(NULL)
  
  list(
    file = file,
    original_sensitivity = result$sensitivity,
    intercept = result$intercept,
    interval = result$interval
  )
})

initial_results <- Filter(Negate(is.null), initial_results)

# 计算指定分位数的阈值
sensitivity_all <- sapply(initial_results, function(r) r$original_sensitivity)
threshold <- quantile(sensitivity_all, probs = percentile_threshold, na.rm = TRUE)
cat(sprintf("Quantile threshold (%.0f%%): %.5f\n", percentile_threshold * 100, threshold))

# 第二步：替换机制 + 重命名
report_list <- list()
results <- lapply(initial_results, function(r) {
  original_se <- r$original_sensitivity
  used_se <- if (original_se < threshold) threshold else original_se
  is_replaced <- original_se < threshold
  
  intercept <- r$intercept
  interval <- r$interval
  file <- r$file
  
  # 格式化数值
  se_fmt <- format(round(used_se, 4), nsmall = 4)
  int_fmt <- format(round(intercept, 4), nsmall = 4)
  
  # 构造新文件名并重命名
  new_file_name <- paste0(se_fmt, "_", int_fmt, "_", file)
  file.rename(file, new_file_name)
  
  # 汇总报告
  report_list[[length(report_list) + 1]] <<- data.frame(
    file = file,
    original_sensitivity = round(original_se, 5),
    final_sensitivity = round(used_se, 5),
    is_replaced = is_replaced,
    threshold = round(threshold, 5),
    percentile = paste0(percentile_threshold * 100, "%")
  )
  
  return(list(
    original_name = file,
    new_name = new_file_name,
    sensitivity = se_fmt,
    intercept = int_fmt,
    interval_1 = interval[1],
    interval_2 = interval[2]
  ))
})

# 输出主汇总文件
results_df <- do.call(rbind.data.frame, results)
write_csv(results_df, "sensitivity_and_intercept_results.csv")

# 输出替换报告文件
report_df <- bind_rows(report_list)
write_csv(report_df, "sensitivity_correction_report.csv")

# 控制台输出汇总信息
cat("---- Summary ----\n")
cat("Total files processed:", length(files), "\n")
cat("Files with replaced sensitivity:", sum(report_df$is_replaced), "\n")
cat(sprintf("Replacement threshold (%.0f%% quantile): %.5f\n", percentile_threshold * 100, threshold))



# Section 5 find bl ---- 这个版本是100个点的，不是300个点来找基线
#可以实现的功能：寻找baseline。对所有“任意.curve1.txt”类似的文件寻找baseline，并且将计算的结果写在该文件的文件名前面。并且生成一个summary记录改名的过程。
#以第100个点和第400个点为范围，每200个点为一组，10个点为间隔，计算斜率和标准误，查看平不平。
library(dplyr)
library(readr)
library(stringr)
# 读取目录下所有文件的名称
files <- list.files(pattern = "curve\\d+\\.txt")

# 寻找baseline的函数
find_baseline <- function(least_length, sensitivity, x, y) {
  # 检查数据长度
  if (length(x) < 200 || length(y) < 200) {
    stop("Input vectors must have at least 200 elements.")
  }
  
  # 调整 y 值为单位归一化
  y_sens_crct <- y / sensitivity
  
  # 初始化结果
  base_x <- NULL
  base_y <- NULL
  baseline_start_position <- NULL
  
  # 用 100 点窗口进行滑动判断
  for (start in seq(100, 301, by = 10)) {
    window_x <- x[start:(start + 99)]
    window_y <- y_sens_crct[start:(start + 99)]
    
    model <- lm(window_y ~ window_x)
    slope <- coef(model)[["window_x"]]
    stderr <- summary(model)$coefficients["window_x", "Std. Error"]
    
    if (abs(slope) < 0.001 && stderr < 0.005) {
      base_x <- window_x
      base_y <- window_y * sensitivity  # 恢复原始单位
      baseline_start_position <- start
      break
    }
  }
  
  if (is.null(base_x) || is.null(base_y)) {
    return(list(baseline = NA, start_position = NA))
  } else {
    baseline <- mean(base_y)
    return(list(baseline = baseline, start_position = baseline_start_position))
  }
}


# 函数，处理单个文件
process_file <- function(file) {
  # 读取数据
  data <- read_delim(file, delim = "\t", col_names = TRUE)
  
  # 确保数据列足够
  if (ncol(data) < 4) {
    cat("File", file, "does not have enough columns.\n")
    return(NULL)
  }
  
  # 使用第2列作为 x，第4列作为 y
  x <- data[[2]]
  y <- data[[4]]
  
  # 计算灵敏度
  result <- find_baseline(200,1,x, y)
  
  # 检查计算结果
  if (is.na(result$baseline)) {
    cat("Baseline calculation failed for", file, "\n")
    return(NULL)
  }
  
  # 保留四位小数
  baseline_formatted <- format(round(result$baseline, 4), nsmall = 4)
  
  
  # 构造新文件名
  new_file_name <- paste0(baseline_formatted, "_", file)
  
  # 重命名文件
  file.rename(file, new_file_name)
  
  # 打印结果
  cat("File", file, "renamed to", new_file_name, "\n")
  
  return(list(original_name = file, new_name = new_file_name, baseline = baseline_formatted))
}

# 处理每个文件
results <- lapply(files, process_file)

# 保存结果到一个 CSV 文件
results_df <- do.call(rbind.data.frame, results)
write_csv(results_df, "baseline_results.csv")



# Section 6 final calculation and to csv ----
#可以实现的功能：可以对目录下所有以“_curve数字.txt”结尾的文件进行处理。处理是：将文件名从前往后第一组四位小数定义为bl,第二组四位小数定义为se，第三组四位小数定义为in。之后用文件内第2列数都减去(bl-in)/-se得到的数再加上de，de是第4列的数减去bl的差除以se得到的结果。最终得到的结果定义为seperation，对应填充到第五列。之后，再用de乘以sc,sc是这个函数一开始需要给定的数值，de与sc的积定义为force，对应填充到第6列。

process_files <- function(sc) {
  # 获取当前工作目录
  directory <- getwd()
  
  # 获取目录中所有以"_curve数字.txt"结尾的文件
  files <- list.files(path = directory, pattern = "_curve\\d+\\.txt$", full.names = TRUE)
  
  for (file in files) {
    # 从文件名中提取四位小数
    file_name <- basename(file)
    file_parts <- unlist(strsplit(file_name, "_")) # 拆分文件名
    bl <- as.numeric(file_parts[1])
    se <- as.numeric(file_parts[2])
    inter <- as.numeric(file_parts[3])
    
    # 读取文件内容
    data <- read.table(file, header = TRUE)
    
    # 将数据转换为数值型
    data <- as.data.frame(sapply(data, as.numeric))
    
    # 计算de
    de <- (data[, 4] - bl) / se
    
    # 计算seperation
    seperation <- data[, 2] - ((bl - inter) / -se) + de
    
    # 计算force
    force <- de * sc
    
    # 添加新列到数据框
    data[,5] <- seperation #这里是并上了，而不是覆盖了。找到问题了
    data[,6] <- force
    
    # 设置列名
    colnames(data) <- c("Calc_Ramp_Ex_nm", "Calc_Ramp_Rt_nm", "Defl_V_Ex", "Defl_V_Rt", "seperation", "force")
    
    # 写回文件，转换为CSV格式
    write.csv(data, gsub(".txt$", ".csv", file), row.names = FALSE)
  }
}

# 使用函数，假设sc为给定值，比如0.08。这个值指的是那个probe的sensitivity测量中得到的数据。
process_files(0.08)
# Section devoff ----
dev.off()


# Section 7 multi view ----
#可以实现的功能：将工作目录下所有的curve数字.csv都导入，且将所有csv中的retract数据绘制折线图。并且计算所有折线图的最低点，并且绘制箱型图于左上角。

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.csv")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组


# First read all data to determine the axes range
for (file in files) {
  data <- read.csv(file, header = TRUE)
  all_x <- c(all_x, data[, 5])  # Collect all x data
  all_y <- c(all_y, data[, 6])  # Collect all y data
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.csv(files[i], header = TRUE)
  x_data <- data[, 5]
  y_data <- data[, 6]
  
  # Draw the line graph, specify color for each file
  lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  
  # Find the lowest point
  min_index <- which.min(y_data)
  min_value <- y_data[min_index]
  min_values[i] <- min_value  # Store minimum value
  min_x <- x_data[min_index]
  # Store min points for legend
  min_points[i, ] <- c(min_x, min_value)
  names[i] <- paste("Curve", sub("curve(\\d+)\\.txt", "\\1", files[i]))
  
  
}

# Add a legend to the plot
#legend("topright", legend = names, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)


# Open a new plot area for the boxplot in the top left corner
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")


# Section devoff ----
dev.off()
# Section 8 multi view filtered ----
#可以实现的功能：将工作目录下所有的curve数字.csv都导入，且将所有csv中的retract数据绘制折线图。并且开始filter。filter的依据是：1.去掉所有曲线的500-512位置的数，即左边最顶端。2.曲线最小值绝对值的百分位数。比如去掉曲线最小值绝对值最大的百分之十的曲线，此时设置percentile_value为0.9就好。

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.csv")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# Initialize variables to determine the plotting range
all_x <- numeric(0)
all_y <- numeric(0)
min_values <- numeric(length(files))  # 初始化存储最小值的数组


# First read all data to determine the axes range
for (file in files) {
  data <- read.csv(file, header = TRUE)
  all_x <- c(all_x, data[, 5][1:500])  # Collect all x data except 500-512
  all_y <- c(all_y, data[, 6][1:500])  # Collect all y data except 500-512
}

# Create the plot frame using the determined range
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points")

# Loop again, draw data for each file and mark the lowest point
min_points <- matrix(nrow = length(files), ncol = 2)
names <- vector("character", length(files))

for (i in seq_along(files)) {
  data <- read.csv(files[i], header = TRUE)
  x_data <- data[, 5][1:500]
  y_data <- data[, 6][1:500]
  
  # Draw the line graph, specify color for each file
  lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
  
  # Find the lowest point
  min_index <- which.min(y_data)
  min_value <- y_data[min_index]
  min_values[i] <- min_value  # Store minimum value
  min_x <- x_data[min_index]
  # Store min points for legend
  min_points[i, ] <- c(min_x, min_value)
  names[i] <- paste("Curve", sub("curve(\\d+)\\.txt", "\\1", files[i]))
  
  
}

# Add a legend to the plot
#legend("topright", legend = names, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)


# Open a new plot area for the boxplot in the top left corner
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values, main = "Boxplot of Minimum Values", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median
median_val <- median(min_values, na.rm = TRUE)

# Add a point for median
points(1, median_val, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point
text(1, median_val, labels = sprintf("Median: %.2f", median_val), pos = 3, cex = 0.8, col = "red")

dev.off()

###start to filter
# Calculate the percentile of the absolute minimum values
abs_min_values <- abs(min_values)
percentile_value <- 0.9  #在这里改变想要filter的阈值
cutoff_value <- quantile(abs_min_values, probs = percentile_value)

# Find indices of curves with min values' absolute in the top percentile
excluded_indices <- which(abs_min_values >= cutoff_value)


# Redraw the plot excluding these curves
plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
     xlab = "X Axis", ylab = "Y Axis", main = "Multiple Curves with Min Points Excluding Top 10%")

# Loop to redraw only the included curves
for (i in seq_along(files)) {
  if (!(i %in% excluded_indices)) {
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:500]
    y_data <- data[, 6][1:500]
    lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
    
  }
}

# Redraw the boxplot excluding the top 10% of minimum values
min_values_excluded <- min_values[!(seq_along(min_values) %in% excluded_indices)]
par(fig = c(0.75, 1, 0.75, 1), new = TRUE, mar = c(1, 1, 1, 1))
boxplot(min_values_excluded, main = "Boxplot of Minimum Values Excluding Top 10%", ylab = "Min Values", col = "cyan", axes = TRUE)
stripchart(min_values_excluded, method = "jitter", add = TRUE, pch = 21, col = "red", vertical = TRUE)

# Calculate median of the excluded values
median_val_excluded <- median(min_values_excluded, na.rm = TRUE)

# Add a point for median in the new boxplot
points(1, median_val_excluded, col = "red", pch = 19, cex = 1.5)  # Red point for median

# Label the median point in the new boxplot
text(1, median_val_excluded, labels = sprintf("Median: %.2f", median_val_excluded), pos = 3, cex = 0.8, col = "red")




# Section 9 individually view ----
# 初始化存储未绘制和已绘制文件名的向量
excluded_files <- vector("character")
included_files <- vector("character")

# 遍历每个文件，为每个文件单独创建一个图形
for (i in seq_along(files)) {
  if (!(i %in% excluded_indices)) {
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:500]
    y_data <- data[, 6][1:500]
    included_files <- c(included_files, files[i])
  } else {
    # 添加未绘制的文件名到向量
    excluded_files <- c(excluded_files, files[i])
  }
}

# 创建报告文本内容
report <- paste("Plot Report\n",
                "Cutoff Value for Filtering: ", -cutoff_value, "\n",
                sprintf("Percentile: %d%%\n", percentile_value * 100),
                "Files plotted (below cutoff): \n", paste(included_files, collapse = "\n"), "\n",
                "Files NOT plotted (above or equal to cutoff): \n", paste(excluded_files, collapse = "\n"), "\n",
                "Files with minimum value absolute greater than or equal to cutoff value have been filtered.\n",
                sep="")

# 打印和保存报告
writeLines(report, "plot_filtering_report.txt")
cat(report)  # 输出报告到控制台




#可以实现的功能：单独对每个在总图中出现的处理过的数据进行绘图，且保存。用于人工筛选想要的图和文件。
# 创建用于保存图形的文件夹，如果不存在就创建一个
plot_dir <- "individual_plot_for_selection"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

# Get all files that match the pattern
files <- list.files(pattern = "curve\\d+\\.csv")

# Prepare an array of colors, enough to distinguish each file
colors <- rainbow(length(files))

# 遍历每个文件，为每个文件单独创建一个图形
for (i in seq_along(files)) {
  # 设置PDF输出文件路径
  pdf_file_path <- file.path(plot_dir, paste0(tools::file_path_sans_ext(basename(files[i])), ".pdf"))
  
  # 开始PDF绘图设备，指定文件大小为10x12英寸
  tryCatch({
    pdf(pdf_file_path, width = 12, height = 10)
    
    # 绘制图形
    plot(1, type = "n", xlim = range(all_x, na.rm = TRUE), ylim = range(all_y, na.rm = TRUE), 
         xlab = "X Axis", ylab = "Y Axis", main = paste("Curve Analysis for", basename(files[i])))
    
    # 从对应文件中读取数据
    data <- read.csv(files[i], header = TRUE)
    x_data <- data[, 5][1:512]
    y_data <- data[, 6][1:512]
    
    # 绘制线条
    lines(x_data, y_data, type = "l", col = colors[i], lwd = 2)
    
    # 找出最小值及其索引
    min_index <- which.min(y_data)
    min_value <- y_data[min_index]
    min_x_value <- x_data[min_index]
    
    # 在图上标注最小值
    points(min_x_value, min_value, col = "red", pch = 19, cex = 1.5)
    text(min_x_value, min_value, labels = sprintf("Min: %.2f", min_value), pos = 1, offset = 1, col = "red")
    
  }, finally = {
    # 确保每次都关闭PDF设备
    dev.off()
  })
}





