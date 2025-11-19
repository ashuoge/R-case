# === 配置 ===
csv_path       <- "D:/1118.csv"
indicator_name <- NULL  # 例如 "CPI" 或 "居民消费价格指数"；留空自动猜测包含“CPI/居民消费”的一行

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
if (length(ind_col) == 0) ind_col <- names(df)[1]

# 宽转长
long <- df %>%
  pivot_longer(cols = setdiff(names(df), ind_col),
               names_to = "日期",
               values_to = "值") %>%
  mutate(
    日期 = {
      v <- as.character(日期)
      v1 <- gsub("年|\\.", "-", gsub("月", "-01", gsub("日", "", v)))
      as.Date(v1)
    }
  )

# 选择指标
if (is.null(indicator_name)) {
  cand <- unique(long[[ind_col]])
  guess <- cand[grepl("(?i)CPI|居民消费", cand)]
  indicator_name <- if (length(guess)) guess[1] else cand[1]
}

dat <- long %>% filter(.data[[ind_col]] == indicator_name) %>% arrange(日期)

# 数值清洗
x_num <- suppressWarnings(as.numeric(gsub("[,% ，，\\s]", "", gsub("%$", "", as.character(dat$值)))))
dat$值_num <- x_num

# 过滤有效值
dat <- dat %>% filter(is.finite(值_num))
if (nrow(dat) < 10 || length(unique(dat$值_num)) < 2) {
  stop("该指标有效样本不足或几乎无波动：", indicator_name)
}

# 统计量
mu <- mean(dat$值_num); md <- median(dat$值_num)
sd1 <- sd(dat$值_num)

# 输出路径
out_path <- file.path(dirname(csv_path),
                      paste0(tools::file_path_sans_ext(basename(csv_path)),
                             "_", indicator_name, "_violin.png"))

# 小提琴图（水平；叠加箱线与散点）
p <- ggplot(dat, aes(x = factor(1), y = 值_num)) +
  geom_violin(fill = "#2F5597", color = "#2F5597", alpha = 0.25, trim = FALSE, width = 0.9) +
  geom_boxplot(width = 0.12, outlier.shape = NA, fill = "white", color = "#2F5597") +
  geom_jitter(width = 0.07, height = 0, alpha = 0.35, size = 1.6, color = "#2F5597") +
  coord_flip() +
  labs(
    title = paste0(indicator_name, " 小提琴图（分布与离散程度）"),
    subtitle = if (any(!is.na(dat$日期))) {
      rng <- range(na.omit(dat$日期))
      paste0("数据时间范围：", format(rng[1], "%Y年%m月"), " - ", format(rng[2], "%Y年%m月"),
             "｜样本量：", nrow(dat), " 个")
    } else {
      paste0("样本量：", nrow(dat), " 个")
    },
    x = NULL,
    y = paste0(indicator_name, " 数值")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text()
  ) +
  # 在图上加统计量标注
  annotate("label", x = 1.2, y = md,
           label = sprintf("中位数：%.2f", md),
           size = 3.5, label.size = 0.3, hjust = 0, alpha = 0.9) +
  annotate("label", x = 1.2, y = mu,
           label = sprintf("均值：%.2f\n±1SD：%.2f~%.2f", mu, mu - sd1, mu + sd1),
           size = 3.5, label.size = 0.3, hjust = 0, vjust = 1.2, alpha = 0.9)

ggsave(out_path, p, width = 10, height = 6, dpi = 150)
cat("图片已保存：", out_path, "\n",
    "选用指标：", indicator_name, "；有效样本：", nrow(dat), "\n", sep = "")