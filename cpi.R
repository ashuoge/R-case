# === 配置 ===
csv_path       <- "D:/1118.csv"
indicator_name <- NULL   # 例如 "CPI" 或 "居民消费价格指数"；留空则自动猜测包含 "CPI" 的那一行

# 依赖
need <- c("tidyr","dplyr","ggplot2")
to_install <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)
library(dplyr)
library(tidyr)
library(ggplot2)

# 读取
df <- tryCatch(read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE),
               error = function(e) read.csv(csv_path, fileEncoding = "UTF-8-BOM", check.names = FALSE, stringsAsFactors = FALSE))

# 定位“指标”列
ind_col <- names(df)[grepl("^(指标|项目|名称)$", names(df))]
if (length(ind_col) == 0) ind_col <- names(df)[1]   # 回退为首列

# 将宽表转成长表：指标-日期-值
long <- df %>%
  pivot_longer(cols = setdiff(names(df), ind_col),
               names_to = "日期",
               values_to = "值") %>%
  mutate(
    日期 = {
      v <- as.character(日期)
      # 处理 "2025年9月1日" / "2025年9月" / "2025-09-01" / "2025.09" 等
      v1 <- gsub("年|\\.", "-", gsub("月", "-01", gsub("日", "", v)))
      as.Date(v1)
    }
  )

# 选择一个指标
if (is.null(indicator_name)) {
  # 优先猜测包含 CPI/居民消费 的指标；否则选择第一个能转成数值的指标
  cand <- unique(long[[ind_col]])
  pick <- cand[grepl("(?i)CPI|居民消费", cand)]
  if (length(pick) == 0) {
    pick <- cand
  }
  # 选出第一个可转数值的
  ok_name <- NA_character_
  for (nm in pick) {
    x <- long %>% filter(.data[[ind_col]] == nm) %>% pull(值)
    x <- suppressWarnings(as.numeric(gsub("[,% ，，\\s]", "", gsub("%$", "", as.character(x)))))
    x <- x[is.finite(x)]
    if (length(x) >= 10 && length(unique(x)) >= 2) { ok_name <- nm; break }
  }
  indicator_name <- ok_name
}

if (is.na(indicator_name)) {
  stop("未识别到可用指标。请将 indicator_name 设为 CSV 中“指标”列的一行名称。当前可选：\n",
       paste(unique(long[[ind_col]]), collapse = "，"))
}

dat <- long %>% filter(.data[[ind_col]] == indicator_name) %>% arrange(日期)

# 数值清洗
x_num <- suppressWarnings(as.numeric(gsub("[,% ，，\\s]", "", gsub("%$", "", as.character(dat$值)))))
x_num <- x_num[is.finite(x_num)]

if (length(x_num) < 10 || length(unique(x_num)) < 2) {
  stop("该指标有效数值不足或几乎无波动：", indicator_name)
}

# 统计量与密度
mu  <- mean(x_num)
md  <- median(x_num)
sd1 <- sd(x_num)
xmin_sd <- mu - sd1
xmax_sd <- mu + sd1

dens <- density(x_num)
dens_df <- data.frame(x = dens$x, y = dens$y)

# 标题时间范围
if (any(!is.na(dat$日期))) {
  rng <- range(na.omit(dat$日期))
  date_range_str <- paste0(format(rng[1], "%Y年%m月"), " - ", format(rng[2], "%Y年%m月"))
} else {
  date_range_str <- paste0("样本量：", length(x_num), " 个观测")
}

# 输出路径
out_path <- file.path(dirname(csv_path),
                      paste0(tools::file_path_sans_ext(basename(csv_path)),
                             "_", indicator_name, "_density.png"))

# 绘图（类似你第二张图）
p <- ggplot(data.frame(val = x_num), aes(x = val)) +
  annotate("rect", xmin = xmin_sd, xmax = xmax_sd, ymin = 0, ymax = Inf,
           fill = "#2F5597", alpha = 0.12) +
  geom_area(data = dens_df, aes(x = x, y = y),
            fill = "#2F5597", alpha = 0.35, color = "#2F5597", linewidth = 1) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "#D62728", linewidth = 1) +
  geom_vline(xintercept = md, linetype = "dotted", color = "#FF9900", linewidth = 1) +
  labs(
    title = paste0(indicator_name, " 核密度分布图"),
    subtitle = paste0("数据时间范围：", date_range_str, "｜样本量：", length(x_num), " 个"),
    x = paste0(indicator_name, " 数值"),
    y = "概率密度"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 8)),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(expand = FALSE)

# 右侧说明与左上角统计框
p <- p +
  annotate("label",
           x = max(dens_df$x) - 0.1 * diff(range(dens_df$x)),
           y = max(dens_df$y) * 0.6,
           hjust = 1,
           label = "解读示例：\n< 100：下降\n= 100：稳定\n> 100：上升",
           size = 3.5, label.size = 0.3, alpha = 0.9) +
  annotate("label",
           x = min(dens_df$x) + 0.18 * diff(range(dens_df$x)),
           y = max(dens_df$y) * 0.88,
           hjust = 0,
           label = sprintf("均值：%.2f\n中位数：%.2f\n±1标准差：%.2f ~ %.2f", mu, md, xmin_sd, xmax_sd),
           size = 3.5, label.size = 0.3, alpha = 0.9)

ggsave(out_path, p, width = 10, height = 6.2, dpi = 150)
cat("图片已保存：", out_path, "\n",
    "选用指标：", indicator_name, "；有效样本：", length(x_num), "\n", sep = "")