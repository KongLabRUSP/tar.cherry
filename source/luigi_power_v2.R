require(data.table)
require(pwr)
# require(bit64)
require(ggplot2)

delta <- seq(5, 60, 5)
std <- se1(10, 50, 10)
h <- delta/std

dt1 <- data.table(`Difference(%)` = delta,
                  `SD = 10%` = pwr.2p.test(h = delta/10,
                                           n = 5,
                                           sig.level = 0.05,
                                           alternative = "g")$power,
                  `SD = 20%` = pwr.2p.test(h = delta/20,
                                           n = 5,
                                           sig.level = 0.05,
                                           alternative = "g")$power,
                  `SD = 30%` = pwr.2p.test(h = delta/30,
                                           n = 5,
                                           sig.level = 0.05,
                                           alternative = "g")$power,
                  `SD = 40%` = pwr.2p.test(h = delta/40,
                                           n = 5,
                                           sig.level = 0.05,
                                           alternative = "g")$power,
                  `SD = 50%` = pwr.2p.test(h = delta/50,
                                           n = 5,
                                           sig.level = 0.05,
                                           alternative = "g")$power)

dt1
write.csv(dt1,
            file = "dt1.csv")

dt2 <- melt.data.table(dt1,
                       id.vars = 1,
                       measure.vars = 2:6,
                       variable.name = "SD",
                       value.name = "Power")
dt2
ggplot(dt2,
       aes(x = `Difference(%)`,
           y = Power,
           group = SD,
           colour = SD)) +
  geom_line(size = 1.1) + 
  geom_hline(yintercept = 0.8,
              linetype = "dashed") +
  scale_x_continuous(breaks = delta) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  ggtitle("Power vs. differences, 5 subject per group")
