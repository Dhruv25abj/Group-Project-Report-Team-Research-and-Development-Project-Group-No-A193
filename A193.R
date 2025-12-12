df <- read.csv("C:/Users/Dhruv Mistry/Desktop/HRDataset_v14.csv")

df_filtered <- df[df$MaritalDesc %in% c("Single", "Married"), ]

score_order <- c("PIP", "Needs Improvement", "Fully Meets", "Exceeds")

df_filtered$PerformanceScore <- factor(df_filtered$PerformanceScore, levels = score_order)

contingency_counts <- table(df_filtered$PerformanceScore, df_filtered$MaritalDesc)

contingency_prop <- prop.table(contingency_counts, margin = 2)

print(t(contingency_counts)) 

png(filename = "stackedbarchart.png", width = 1000, height = 600)

bar_colors <- c("PIP" = "#F8766D", "Needs Improvement" = "#E377C2", 
                "Fully Meets" = "#00BFC4", "Exceeds" = "#00BA38")

par(mar = c(5, 4, 4, 12) + 0.1, xpd=TRUE)

barplot(contingency_prop, 
        beside = FALSE, 
        main = "Proportion of Performance Scores by Marital Status",
        xlab = "Marital Status (Married vs. Single)",
        ylab = "",
        yaxt = "n",
        ylim = c(0, 1.0),
        col = bar_colors)

legend("topright",
       legend = rownames(contingency_prop),
       fill = bar_colors,
       title = "Performance Score",
       inset = c(-0.20, 0),
       bty = "n")

dev.off()

chi_square_result <- chisq.test(contingency_counts)
print(contingency_counts)
print("chi-square-result: ")
print(chi_square_result)
