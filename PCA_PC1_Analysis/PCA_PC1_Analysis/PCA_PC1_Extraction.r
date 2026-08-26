# ============================================================
# 微塑料与重金属PCA分析 - FT区域完整代码（修正版）
# 数据: 46个样本 (23个采样点 × 2个季节, 已删除FT12)
# ============================================================

# 1. 加载必要的包 ----------------------------------------------
required_packages <- c("tidyverse", "factoextra", "ggplot2", "ggpubr", "psych")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(required_packages, install_if_missing))

# 2. 直接构建合并后的数据框（46行，已删除FT12）--------------------

# March数据 (FT01-FT11, FT13-FT24) = 23个样本
march_data <- data.frame(
  sample_ID = c("FT01_Mar", "FT02_Mar", "FT03_Mar", "FT04_Mar", "FT05_Mar", 
                "FT06_Mar", "FT07_Mar", "FT08_Mar", "FT09_Mar", "FT10_Mar", 
                "FT11_Mar", "FT13_Mar", "FT14_Mar", "FT15_Mar", "FT16_Mar", 
                "FT17_Mar", "FT18_Mar", "FT19_Mar", "FT20_Mar", "FT21_Mar", 
                "FT22_Mar", "FT23_Mar", "FT24_Mar"),
  Season = rep("March", 23),
  Cr = c(0.010942,0.010590,0.010605,0.010541,0.010802,0.011045,0.011016,0.011451,
         0.010862,0.011703,0.010741,0.011162,0.011034,0.010952,0.010939,0.010878,
         0.011095,0.011703,0.011875,0.011109,0.011594,0.011363,0.011440,0.012220)[1:23],
  Ni = c(0.003202,0.002099,0.003000,0.002735,0.003414,0.003087,0.003175,0.003440,
         0.003058,0.002845,0.002770,0.002282,0.003256,0.003143,0.003136,0.003196,
         0.003055,0.003195,0.002582,0.005139,0.002592,0.002463,0.004077,0.003667)[1:23],
  Cu = c(0.002867,0.001908,0.002243,0.002704,0.003747,0.002801,0.002896,0.003692,
         0.003440,0.003694,0.002838,0.003254,0.003196,0.003473,0.003306,0.003901,
         0.003231,0.003707,0.003448,0.003370,0.003187,0.002571,0.003483,0.004511)[1:23],
  Zn = c(0.037069,0.073733,0.062757,0.026709,0.040097,0.044485,0.026686,0.036061,
         0.035872,0.037519,0.016696,0.028484,0.041264,0.034943,0.033731,0.046143,
         0.056212,0.031477,0.028342,0.040155,0.062897,0.028192,0.038385,0.046439)[1:23],
  As = c(0.000593,0.000283,0.000896,0.000484,0.000465,0.000667,0.000498,0.000391,
         0.000424,0.000773,0.000440,0.000345,0.001261,0.000906,0.001705,0.000721,
         0.000715,0.000926,0.000591,0.000743,0.000960,0.000460,0.001582,0.001655)[1:23],
  Cd = c(0.000041,0.000090,0.000031,0.000029,0.000217,0.000068,0.000076,0.000096,
         0.000040,0.000108,0.000093,0.000069,0.000091,0.000085,0.000096,0.000057,
         0.000047,0.000070,0.000082,0.000136,0.000112,0.000048,0.000135,0.000093)[1:23],
  Hg = c(0.000022,0.000027,0.000041,0.000059,0.000049,0.000237,0.000033,0.000040,
         0.000018,0.000015,0.000023,0.000021,0.000059,0.000025,0.000032,0.000127,
         0.000037,0.000100,0.000139,0.000006,0.000016,0.000000,0.000137,0.000032)[1:23],
  Pb = c(0.001484,0.001448,0.002376,0.002701,0.003943,0.003337,0.002701,0.006391,
         0.003699,0.004219,0.001608,0.002869,0.004356,0.002962,0.003821,0.004226,
         0.003077,0.002538,0.003195,0.004461,0.003130,0.003045,0.003261,0.006493)[1:23],
  MP_items_L = c(0.75,0.45,0.55,0.40,0.40,1.65,1.35,1.60,1.00,2.70,1.35,
                 0.55,0.75,4.85,0.90,0.35,1.35,1.40,1.30,1.40,1.35,2.45,2.25)
)

# June数据 (FT01-FT11, FT13-FT24) = 23个样本
june_data <- data.frame(
  sample_ID = c("FT01_Jun", "FT02_Jun", "FT03_Jun", "FT04_Jun", "FT05_Jun", 
                "FT06_Jun", "FT07_Jun", "FT08_Jun", "FT09_Jun", "FT10_Jun", 
                "FT11_Jun", "FT13_Jun", "FT14_Jun", "FT15_Jun", "FT16_Jun", 
                "FT17_Jun", "FT18_Jun", "FT19_Jun", "FT20_Jun", "FT21_Jun", 
                "FT22_Jun", "FT23_Jun", "FT24_Jun"),
  Season = rep("June", 23),
  Cr = c(0.021874,0.009876,0.009343,0.023931,0.043116,0.010564,0.010229,0.010439,
         0.010715,0.010365,0.017112,0.013427,0.010520,0.011059,0.010813,0.011306,
         0.011242,0.010641,0.010400,0.010696,0.010292,0.010516,0.010297,0.010974)[1:23],
  Ni = c(0.008055,0.006358,0.004851,0.005374,0.006095,0.005810,0.006307,0.006876,
         0.006053,0.006432,0.006513,0.008132,0.006227,0.006462,0.006616,0.006130,
         0.005713,0.005787,0.005000,0.005477,0.006023,0.005702,0.006019,0.006328)[1:23],
  Cu = c(0.012750,0.010062,0.010082,0.010088,0.008198,0.007816,0.009745,0.012672,
         0.011072,0.011119,0.010520,0.012883,0.009159,0.015436,0.012120,0.009997,
         0.008977,0.011267,0.013158,0.015215,0.013892,0.015470,0.016452,0.014772)[1:23],
  Zn = c(0.032923,0.025026,0.017975,0.021411,0.024306,0.023806,0.013858,0.115014,
         0.043497,0.033528,0.031998,0.039729,0.012666,0.018390,0.019341,0.014699,
         0.013298,0.012247,0.016502,0.027693,0.016945,0.013242,0.022972,0.034232)[1:23],
  As = c(0.002523,0.001331,0.001396,0.001405,0.001124,0.001214,0.009418,0.010298,
         0.008588,0.028469,0.015478,0.008417,0.006534,0.005973,0.005719,0.006161,
         0.006173,0.005254,0.003742,0.004129,0.004480,0.004826,0.004053,0.004087)[1:23],
  Cd = c(0.000113,0.000056,0.000049,0.000030,0.000031,0.000033,0.000033,0.000047,
         0.000100,0.000103,0.000046,0.000269,0.000038,0.000109,0.000046,0.000044,
         0.000022,0.000003,0.000041,0.000052,0.000055,0.000021,0.000068,0.000094)[1:23],
  Hg = c(0.000005,0.000003,0.000001,0.000000,0.000000,0.000000,0.000002,0.000003,
         0.000001,0.000006,0.000003,0.000005,0.000003,0.000000,0.000000,0.000000,
         0.000000,0.000000,0.000001,0.000000,0.000002,0.000004,0.000004,0.000004)[1:23],
  Pb = c(0.004073,0.003869,0.001435,0.001280,0.000942,0.001495,0.000906,0.002297,
         0.003205,0.007095,0.000729,0.005657,0.001309,0.001487,0.001536,0.001619,
         0.001169,0.000933,0.002165,0.002807,0.001560,0.001074,0.001980,0.005178)[1:23],
  MP_items_L = c(2.00,0.65,0.40,1.15,0.45,0.55,2.60,3.60,11.45,8.65,2.40,
                 3.05,1.45,2.20,4.25,2.10,1.40,1.10,1.25,1.45,1.40,1.00,1.60)
)

# 合并March和June数据
df <- rbind(march_data, june_data)

# 检查数据维度
cat("数据维度:", dim(df))
cat("\n列名:", colnames(df))
cat("\n季节分布:\n")
table(df$Season)

# 处理Hg的0值 (替换为最小正值的一半)
min_hg_positive <- min(df$Hg[df$Hg > 0], na.rm = TRUE)
df$Hg[df$Hg == 0] <- min_hg_positive / 2
cat("\nHg的0值已替换为:", min_hg_positive/2)

# 3. 描述性统计 ------------------------------------------------
desc_stats <- df %>%
  group_by(Season) %>%
  summarise(across(Cr:Pb, 
                   list(mean = ~mean(.), sd = ~sd(.), min = ~min(.), max = ~max(.)),
                   .names = "{col}_{fn}")) %>%
  pivot_longer(-Season, names_to = c("Metal", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = Stat, values_from = value) %>%
  arrange(Metal)

cat("\n\n========== 描述性统计 ==========\n")
print(desc_stats, n = 50)

# 保存描述统计表
write.csv(desc_stats, "描述统计_重金属.csv", row.names = FALSE)

# 4. PCA分析 (仅重金属) ----------------------------------------
# 提取重金属数据矩阵
metal_matrix <- df %>% select(Cr, Ni, Cu, Zn, As, Cd, Hg, Pb)

# KMO检验 (psych包)
kmo_result <- KMO(metal_matrix)
cat("\n\n========== KMO检验 ==========\n")
print(kmo_result$MSA)
cat("\n整体KMO =", round(kmo_result$MSA[length(kmo_result$MSA)], 3))

# Bartlett球形检验
bartlett_result <- cortest.bartlett(cor(metal_matrix), n = nrow(metal_matrix))
cat("\n\n========== Bartlett球形检验 ==========\n")
cat("Chi-square =", round(bartlett_result$chisq, 2), 
    ", df =", bartlett_result$df, 
    ", p-value =", format(bartlett_result$p.value, scientific = TRUE))

# 标准化并执行PCA
metal_scaled <- scale(metal_matrix)
pca_result <- prcomp(metal_scaled, center = FALSE, scale. = FALSE)
summary_pca <- summary(pca_result)

cat("\n\n========== PCA方差解释率 ==========\n")
print(summary_pca$importance)

# 特征值
eigenvalues <- pca_result$sdev^2
cat("\n特征值:\n"); print(round(eigenvalues, 3))

# 5. 可视化 - 碎石图 -------------------------------------------
scree_plot <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60),
                       barfill = "steelblue", barcolor = "steelblue",
                       linecolor = "red") +
  ggtitle("碎石图 (Scree Plot)") +
  theme_minimal()

ggsave("碎石图.png", scree_plot, width = 6, height = 4, dpi = 300)
cat("\n碎石图已保存: 碎石图.png\n")

# 6. 载荷矩阵 -------------------------------------------------
loadings_matrix <- pca_result$rotation[, 1:3]
colnames(loadings_matrix) <- paste0("PC", 1:3)
loadings_df <- as.data.frame(loadings_matrix) %>%
  rownames_to_column("Metal")

cat("\n\n========== 载荷矩阵 (PC1-PC3) ==========\n")
print(loadings_df)

write.csv(loadings_df, "载荷矩阵.csv", row.names = FALSE)

# 7. Biplot双标图 ----------------------------------------------
biplot <- fviz_pca_biplot(pca_result, 
                          col.ind = df$Season,
                          palette = c("March" = "#2E8B57", "June" = "#DAA520"),
                          addEllipses = TRUE, ellipse.level = 0.68,
                          label = "var", repel = TRUE,
                          title = "PCA双标图 (按季节着色)") +
  theme_minimal()

ggsave("PCA_Biplot.png", biplot, width = 8, height = 6, dpi = 300)
cat("PCA双标图已保存: PCA_Biplot.png\n")

# 8. 计算PC1得分并添加到数据框 ------------------------------------
df$PC1 <- pca_result$x[, 1]

# 输出PC1得分
pc1_scores <- df %>% select(sample_ID, Season, PC1)
cat("\n\n========== PC1得分 (前12行) ==========\n")
print(head(pc1_scores, 12))

write.csv(pc1_scores, "PC1得分.csv", row.names = FALSE)

# 9. 季节差异检验 (March vs June PC1) ---------------------------
# 正态性检验
shapiro_march <- shapiro.test(df$PC1[df$Season == "March"])
shapiro_june <- shapiro.test(df$PC1[df$Season == "June"])
cat("\n\n========== 正态性检验 ==========\n")
cat("March PC1: W =", round(shapiro_march$statistic, 3), 
    ", p =", round(shapiro_march$p.value, 4))
cat("\nJune PC1: W =", round(shapiro_june$statistic, 3), 
    ", p =", round(shapiro_june$p.value, 4))

# t检验 (如果数据近似正态)
season_test <- t.test(PC1 ~ Season, data = df)
wilcox_test <- wilcox.test(PC1 ~ Season, data = df)

cat("\n\n========== 季节差异检验 ==========\n")
cat("t检验: t =", round(season_test$statistic, 3), 
    ", df =", round(season_test$parameter, 1),
    ", p =", format(season_test$p.value, scientific = TRUE))
cat("\nWilcoxon检验: W =", round(wilcox_test$statistic, 1),
    ", p =", format(wilcox_test$p.value, scientific = TRUE))

# 箱线图
boxplot_pc1 <- ggplot(df, aes(x = Season, y = PC1, fill = Season)) +
  geom_boxplot(width = 0.6, alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.6) +
  scale_fill_manual(values = c("March" = "#2E8B57", "June" = "#DAA520")) +
  labs(title = "PC1得分的季节差异", y = "PC1得分", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none") +
  stat_compare_means(method = "t.test", label = "p.format")

ggsave("PC1季节差异_箱线图.png", boxplot_pc1, width = 5, height = 4, dpi = 300)

# 10. 相关性分析: PC1 vs 微塑料 ---------------------------------

# 整体相关性 (n=46)
cor_overall <- cor.test(df$PC1, df$MP_items_L, method = "spearman")
cat("\n\n========== 整体相关性 (Spearman) ==========\n")
cat("rho =", round(cor_overall$estimate, 3), 
    ", p =", format(cor_overall$p.value, scientific = TRUE))

# 分季节相关性
cor_march <- cor.test(df$PC1[df$Season == "March"], 
                      df$MP_items_L[df$Season == "March"], 
                      method = "spearman")
cor_june <- cor.test(df$PC1[df$Season == "June"], 
                     df$MP_items_L[df$Season == "June"], 
                     method = "spearman")

cat("\n\n========== 分季节相关性 (Spearman) ==========\n")
cat("March: rho =", round(cor_march$estimate, 3), 
    ", p =", format(cor_march$p.value, scientific = TRUE))
cat("\nJune: rho =", round(cor_june$estimate, 3), 
    ", p =", format(cor_june$p.value, scientific = TRUE))

# 敏感性分析: 剔除FT09 June高值 (11.45 items/L)
df_no_outlier <- df[df$MP_items_L < 11, ]  # 剔除最高值

cor_overall_no_outlier <- cor.test(df_no_outlier$PC1, df_no_outlier$MP_items_L, 
                                   method = "spearman")
cat("\n\n========== 敏感性分析 (剔除FT09 June高值后) ==========\n")
cat("样本量 n =", nrow(df_no_outlier))
cat("\n整体 rho =", round(cor_overall_no_outlier$estimate, 3), 
    ", p =", format(cor_overall_no_outlier$p.value, scientific = TRUE))

# 11. 散点图 --------------------------------------------------
# 整体散点图
scatter_all <- ggplot(df, aes(x = PC1, y = MP_items_L)) +
  geom_point(aes(color = Season), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("March" = "#2E8B57", "June" = "#DAA520")) +
  annotate("text", x = min(df$PC1), y = max(df$MP_items_L), 
           label = paste0("rho = ", round(cor_overall$estimate, 3), 
                          "\np = ", format(cor_overall$p.value, scientific = TRUE, digits = 2)),
           hjust = 0, vjust = 1, size = 4) +
  labs(title = "PC1 vs 微塑料丰度 (整体)", 
       x = "PC1得分 (综合重金属指标)", y = "微塑料丰度 (items/L)") +
  theme_minimal()

# 分面散点图 (按季节)
scatter_facet <- ggplot(df, aes(x = PC1, y = MP_items_L)) +
  geom_point(size = 3, alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  facet_wrap(~Season, scales = "free") +
  stat_cor(method = "spearman", label.x.npc = "left", label.y.npc = "top") +
  labs(title = "PC1 vs 微塑料丰度 (分季节)", 
       x = "PC1得分", y = "微塑料丰度 (items/L)") +
  theme_minimal()

ggsave("PC1_vs_微塑料_整体.png", scatter_all, width = 6, height = 5, dpi = 300)
ggsave("PC1_vs_微塑料_分季节.png", scatter_facet, width = 8, height = 4, dpi = 300)

# 12. 相关性结果汇总表 ------------------------------------------
cor_summary <- data.frame(
  分析 = c("整体 (n=46)", "March (n=23)", "June (n=23)", "整体_剔除FT09高值"),
  Spearman_rho = c(cor_overall$estimate, cor_march$estimate, 
                   cor_june$estimate, cor_overall_no_outlier$estimate),
  p_value = c(cor_overall$p.value, cor_march$p.value, 
              cor_june$p.value, cor_overall_no_outlier$p.value),
  显著性 = c(ifelse(cor_overall$p.value < 0.05, "*", "ns"),
            ifelse(cor_march$p.value < 0.05, "*", "ns"),
            ifelse(cor_june$p.value < 0.05, "*", "ns"),
            ifelse(cor_overall_no_outlier$p.value < 0.05, "*", "ns"))
)

cat("\n\n========== 相关性分析结果汇总 ==========\n")
print(cor_summary)

write.csv(cor_summary, "相关性结果汇总.csv", row.names = FALSE)

# 13. 输出所有生成的文件列表 ------------------------------------
cat("\n\n========== 已生成的文件 ==========\n")
cat("1. 描述统计_重金属.csv\n")
cat("2. 载荷矩阵.csv\n")
cat("3. PC1得分.csv\n")
cat("4. 相关性结果汇总.csv\n")
cat("5. 碎石图.png\n")
cat("6. PCA_Biplot.png\n")
cat("7. PC1季节差异_箱线图.png\n")
cat("8. PC1_vs_微塑料_整体.png\n")
cat("9. PC1_vs_微塑料_分季节.png\n")

cat("\n\n========== 分析完成 ==========\n")