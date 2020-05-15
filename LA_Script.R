library (lmPerm)
library (ggplot2)

la_dwf = read.csv (file.choose (),
                   header = T,
                   na.strings = "")
attach (la_dwf)
names (la_dwf)
summary (la_dwf)

fit = aovp (Avg..DFW.RATE ~ LA, perm = "Prob")
summary (fit)

mytheme_present = theme (plot.title = element_text (face = "bold.italic", size = 28, hjust = 0.5), 
                         axis.title = element_text(face = "bold", size = 20), 
                         axis.text = element_text(size = 18), 
                         legend.title=element_text(size=18), 
                         legend.text=element_text(size=16))

LADWF_g = ggplot (aes(y = Avg..DFW.RATE, 
                      x = LA, 
                      fill = LA, 
                      alpha = 0.8), 
                  data = la_dwf)
LADWF_g + 
  geom_boxplot(outlier.size = -1) + 
  geom_point (pch = 21, 
              position = position_jitterdodge (jitter.height= 0.05)) + 
  scale_alpha (guide = "none") + 
  theme(legend.position = "none") +
  mytheme_present + 
  #scale_fill_grey(start = 0.6, end = 0.2) +
  #coord_cartesian(ylim = c(0,10)) +
  #theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14))
  labs (y = "Average DFW", x = "Learning Assistant")

fit = aovp (Avg..DFW.RATE ~ LA * DFW.Cate, 
            perm = "Prob")
summary (fit)

LADWF_g = ggplot (aes(y = Avg..DFW.RATE, 
                      x = LA, 
                      fill = DFW.Cate, 
                      alpha = 0.8), 
                  data = la_dwf)
LADWF_g + 
  geom_boxplot(outlier.size = -1) + 
  geom_point (pch = 21, 
              position = position_jitterdodge (jitter.height= 0.05)) + 
  scale_alpha (guide = "none") + 
  #theme(legend.position = "none") +
  mytheme_present + 
  #scale_fill_grey(start = 0.6, end = 0.2) +
  #coord_cartesian(ylim = c(0,10)) +
  #theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14))
  labs (y = "Average DFW", x = "Learning Assistant")

fit = aovp (Avg..DFW.RATE ~ LA * Course, 
            perm = "Prob")
summary (fit)

LADWF_g = ggplot (aes(y = Avg..DFW.RATE, 
                      x = LA, 
                      fill = DFW.Cate, 
                      alpha = 0.8), 
                  data = la_dwf)
LADWF_g + 
  geom_boxplot(outlier.size = -1) + 
  geom_point (pch = 21, 
              position = position_jitterdodge (jitter.height= 0.05)) + 
  scale_alpha (guide = "none") + 
  #theme(legend.position = "none") +
  mytheme_present + 
  #scale_fill_grey(start = 0.6, end = 0.2) +
  #coord_cartesian(ylim = c(0,10)) +
  #theme (axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14))
  labs (y = "Average DFW", x = "Learning Assistant")