# |------------------------------------------------------------------------------------|
# | Project: PKPD of Anthocyanins After Oral Cherry Juice Concentrate in Gout Patients |
# | Script: Power and sample size calculations                                         |
# | Authors: Davit Sargsyan                                                            |   
# | Created: 03/31/2018                                                                |
# | Modified: 04/03/2018, DS:reran for larger delta (up to 100%, or 2-fold difference) |
# |------------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
# sink(file = "tmp/log_luigi_power_v3.txt")
date()

require(data.table)
require(pwr)
# require(bit64)
require(ggplot2)

delta <- seq(0, 100, 20)
std <- seq(20, 100, 20)
n <- c(5, 10, 15, 20)
# h <- delta/std

res <- list()
for (j in n) {
  out <- matrix(NA, 
                nrow = length(delta),
                ncol = length(std))
  for (i in std) {
    out[, which(std == i)] <- pwr.t.test(n = j,
                                         d = delta/i,
                                         sig.level = 0.05,
                                         type = "two.sample",
                                         alternative = "greater")$power
  }
  res[[which(n == j)]] <- data.table(n = j,
                                     delta,
                                     out)
}
res <- rbindlist(res)
colnames(res) <- c("N per Group",
                   "Delta(%)",
                   paste("SD=",
                         std,
                         "%",
                         sep = ""))
res

write.csv(res,
          file = "tmp/res.csv",
          row.names = FALSE)

dt1 <- melt.data.table(res,
                       id.vars = 1:2,
                       measure.vars = 3:ncol(res),
                       variable.name = "SD",
                       value.name = "Power")
dt1

p1 <- ggplot(dt1,
             aes(x = `Delta(%)`,
                 y = Power,
                 group = SD,
                 colour = SD)) +
  facet_wrap(~`N per Group`) + 
  geom_line() +
  # geom_smooth(se = FALSE,
  #             method = "loess",
  #             span = 0.8) + 
  geom_hline(yintercept = 0.8,
             linetype = "dashed") +
  scale_x_continuous(breaks = delta) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.1)) +
  ggtitle("Power vs. Mean Difference by Sample Size") + 
  theme(plot.title = element_text(hjust = 0.5))
p1

# Save the plot as a TIFF file
tiff(filename = "tmp/power_curves.tiff",
     height = 5,
     width = 5,
     units = 'in',
     res = 300,
     compression = "lzw+p")
print(p1)
graphics.off()

# sessionInfo()
# sink()
