#Figure 1
library(tidyverse)
library(gridExtra)

#Data ####

Observed=c(38,51,46,52,53, #1966-1969 (end 8, 10, 15)
           28,             #(end 9, 11, 16)
           34,             #(end 10, 12, 17)
           36,             #(end 11, 13, 18)
           19,             #(end 12, 14, 19) - 0.8
           23,             #(end 13, 15, 20)
           29,             #(end 14, 16, 21)
           21,             #(end 15, 17, 22)
           13,             #(end 16, 18, 23) Decline pop
           13,18,12);

#Arbitrary, the half of initial population
N.critical <- round(1/2*Observed[1],0)

P.quasi.ext.5 <- c(rep(NA,4), #t1:4
                   0,    #t5
                   0.6666667,  #t6
                   0.3333333, #t7
                   0.3333333,  #t8
                   rep(1,8)) #t19:16

P.quasi.ext.3 <- c(rep(NA,4), #t1:4
                   0,    #t5
                   0.7,  #t6
                   0.2, #t7
                   0.2,  #t8
                   0.8,  #t9
                   0.7, #t10
                   0.4, #t11
                   0.9, #t12
                   rep(1,4)) #t13:16

P.quasi.ext.10 <- c(rep(NA,4), #t1:4
                    0,    #t5
                    0.25,  #t6
                    0.1, #t7
                    0.1,  #t8
                    0.6,  #t9
                    0.35, #t10
                    0.3, #t11
                    0.8, #t12
                    0.85, #t13
                    rep(1,3)) #t14:16

#Predicted
Predicted.5.3 <- c(55,65,70)
Predicted.5.5 <- c(54,56,55,58,60)
Predicted.5.10 <- c(53,55,60,54,56,57,55,60,62,65)

Predicted.6.3 <- c(17,14,25)
Predicted.6.5 <- c(21,19,18,17,14)
Predicted.6.10 <- c(29,25,20,19,24,17,32,18,38,35)

Predicted.7.3 <- c(30,14,35)
Predicted.7.5 <- c(38,25,18,29,34)
Predicted.7.10 <- c(36,25,27,18,22,29,33,41,38,39)

Predicted.8.3 <- c(42,25,17)
Predicted.8.5 <- c(36,38,35,30,16)
Predicted.8.10 <- c(39,41,35,33,34,29,30,25,18,21)

Predicted.9.3 <- c(17,6,7)
Predicted.9.5 <- c(14,11,20,18,12)
Predicted.9.10 <- c(11,21,12,14,20,14,24,22,11,18)

Predicted.10.3 <- c(12,11,10)
Predicted.10.5 <- c(19,22,17,14,11)
Predicted.10.10 <- c(25,26,24,19,20,18,21,17,20,18)

Predicted.11.3 <- c(19,17,18)
Predicted.11.5 <- c(21,22,18,20,17)
Predicted.11.10 <- c(30,18,25,31,32,16,17,21,20,25)

Predicted.12.3 <- c(18,14,10)
Predicted.12.5 <- c(19,18,17,18,16)
Predicted.12.10 <- c(16,18,21,17,24,18,17,16,14,12)

Predicted.13.3 <- c(12,10,11)
Predicted.13.5 <- c(16,15,11,13,12)
Predicted.13.10 <- c(14,16,20,19,18,12,15,13,12,10)

Predicted.14.3 <- c(10,11,12)
Predicted.14.5 <- c(12,13,11,15,16)
Predicted.14.10 <- c(11,10,12,11,10,11,9,8,8,6)

Predicted.15.3 <- c(17,15,13)
Predicted.15.5 <- c(18,18,17,17,15)
Predicted.15.10 <- c(18,16,15,16,14,14,10,10,11,10)

#Data frames
df_fig <- data_frame(time = c(1:16),
                     Observed = Observed,
                     `S_{3 years}` = 1-P.quasi.ext.3,
                     `S_{5 years}` = 1-P.quasi.ext.5,
                     `S_{10 years}` = 1-P.quasi.ext.10)

#Figures
#Figure 1ab ####
Fig1a <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:5)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 6, xmax = 15,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 6, xmax = 10,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 6, xmax = 8,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:5)),
              aes(x = time, y = Observed),
              method = "lm", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 5,
             linetype = "dotted")+
  geom_line(aes(x = c(6:8),
                y = Predicted.5.3),
            color = "#747474")+
  geom_point(aes(x = c(6:8),
                 y = Predicted.5.3),
             color = "#747474",
             shape = 21)+
  geom_line(aes(x = c(6:10),
                y = Predicted.5.5),
            color = "#747474")+
  geom_point(aes(x = c(6:10),
                 y = Predicted.5.5),
             color = "#747474",
             shape = 22)+
  geom_line(aes(x = c(6:15),
                y = Predicted.5.10),
            color = "#747474")+
  geom_point(aes(x = c(6:15),
                 y = Predicted.5.10),
             color = "#747474",
             shape = 23)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:5,6,8,10,15),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(a)")),
       title = expression(italic("i")*" = 1; "*italic("t")*" = 5"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1a

Fig1b <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:5)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                   "S_{5 years}",
                                   "S_{10 years}")))) +
  geom_line(color = "#747474") +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(5:10),
                     breaks = c(5:5),
                     expand = c(0.02,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(b)")),
       title = expression(italic("i")*" = 1; "*italic("t")*" = 5"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")

Fig1b

#Figure 1cd ####
#i = 6
Fig1c <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:6)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 7, xmax = 16,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 7, xmax = 11,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 7, xmax = 9,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_segment(aes(x = 1, xend = 6,
                   y = 40, yend = 51),
               color = "black", size = 1)+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 6,
             linetype = "dotted")+
  geom_line(aes(x = c(7:9),
                y = Predicted.6.3),
            color = "#747474")+
   geom_point(aes(x = c(7:9),
                 y = Predicted.6.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(7:8),
                 y = Predicted.6.3[Predicted.6.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(7:11),
                y = Predicted.6.5),
            color = "#747474")+
  geom_point(aes(x = c(7:11),
                 y = Predicted.6.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(9:11),
                 y = Predicted.6.5[Predicted.6.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(7:16),
                y = Predicted.6.10),
            color = "#747474")+
  geom_point(aes(x = c(7:16),
                 y = Predicted.6.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(6+6,6+8),
                 y = Predicted.6.10[Predicted.6.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:6,7,9,11,16),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(c)")),
       title = expression(italic("i")*" = 2; "*italic("t")*" = 6"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1c

Fig1d <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:6)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(5:10),
                     breaks = c(5:6),
                     expand = c(0.02,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(d)")),
       title = expression(italic("i")*" = 2; "*italic("t")*" = 6"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1d


#Figure 1ef ####
#i = 7
Fig1e <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:7)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 8, xmax = 17,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 8, xmax = 12,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 8, xmax = 10,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_segment(aes(x = 1, xend = 7,
                   y = 42, yend = 48),
               color = "black", size = 1)+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 7,
             linetype = "dotted")+
  geom_line(aes(x = c(8:10),
                y = Predicted.7.3),
            color = "#747474")+
  geom_point(aes(x = c(8:10),
                 y = Predicted.7.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(9),
                 y = Predicted.7.3[Predicted.7.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(8:12),
                y = Predicted.7.5),
            color = "#747474")+
  geom_point(aes(x = c(8:12),
                 y = Predicted.7.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(10),
                 y = Predicted.7.5[Predicted.7.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(8:17),
                y = Predicted.7.10),
            color = "#747474")+
  geom_point(aes(x = c(8:17),
                 y = Predicted.7.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(11),
                 y = Predicted.7.10[Predicted.7.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:7,8,10,12,17),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(e)")),
       title = expression(italic("i")*" = 3; "*italic("t")*" = 7"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1e

Fig1f <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:7)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(5:10),
                     breaks = c(1:7),
                     expand = c(0.02,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(f)")),
       title = expression(italic("i")*" = 3; "*italic("t")*" = 7"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1f

#Figure 1gh ####
#i = 8
Fig1g <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:8)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 9, xmax = 18,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 9, xmax = 13,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 9, xmax = 11,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_segment(aes(x = 1, xend = 8,
                   y = 43, yend = 44),
               color = "black", size = 1)+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 8,
             linetype = "dotted")+
  geom_line(aes(x = c(9:11),
                y = Predicted.8.3),
            color = "#747474")+
  geom_point(aes(x = c(9:11),
                 y = Predicted.8.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(11),
                 y = Predicted.8.3[Predicted.8.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(9:13),
                y = Predicted.8.5),
            color = "#747474")+
  geom_point(aes(x = c(9:13),
                 y = Predicted.8.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(13),
                 y = Predicted.8.5[Predicted.8.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(9:18),
                y = Predicted.8.10),
            color = "#747474")+
  geom_point(aes(x = c(9:18),
                 y = Predicted.8.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(17),
                 y = Predicted.8.10[Predicted.8.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:8,9,11,13,18),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(g)")),
       title = expression(italic("i")*" = 4; "*italic("t")*" = 8"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1g

Fig1h <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:8)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(5:10),
                     breaks = c(1:8),
                     expand = c(0.02,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(h)")),
       title = expression(italic("i")*" = 4; "*italic("t")*" = 8"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1h

#Figure 1ij ####
#i = 9
Fig1i <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:9)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 10, xmax = 19,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 10, xmax = 14,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 10, xmax = 12,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_segment(aes(x = 1, xend = 9,
                   y = 50, yend = 30),
               color = "black", size = 1)+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 9,
             linetype = "dotted")+
  geom_line(aes(x = c(10:12),
                y = Predicted.9.3),
            color = "#747474")+
  geom_point(aes(x = c(10:12),
                 y = Predicted.9.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(10:12),
                 y = Predicted.9.3[Predicted.9.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(10:14),
                y = Predicted.9.5),
            color = "#747474")+
  geom_point(aes(x = c(10:14),
                 y = Predicted.9.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(10,11,13,14),
                 y = Predicted.9.5[Predicted.9.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(10:19),
                y = Predicted.9.10),
            color = "#747474")+
  geom_point(aes(x = c(10:19),
                 y = Predicted.9.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(10,12,13,15,18,19),
                 y = Predicted.9.10[Predicted.9.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:9,10,12,14,19),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(i)")),
       title = expression(italic("i")*" = 5; "*italic("t")*" = 9"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1i

Fig1j <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:9)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(1:10),
                     breaks = c(1:9),
                     expand = c(0.05,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(j)")),
       title = expression(italic("i")*" = 5; "*italic("t")*" = 9"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1j

#Figure 1kl - stop here ####
#i = 10
Fig1k <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:10)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 11, xmax = 20,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 11, xmax = 15,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 11, xmax = 13,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_segment(aes(x = 1, xend = 10,
                   y = 51, yend = 26),
               color = "black", size = 1)+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 10,
             linetype = "dotted")+
  geom_line(aes(x = c(11:13),
                y = Predicted.10.3),
            color = "#747474")+
  geom_point(aes(x = c(11:13),
                 y = Predicted.10.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(11:13),
                 y = Predicted.10.3[Predicted.10.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(11:15),
                y = Predicted.10.5),
            color = "#747474")+
  geom_point(aes(x = c(11:15),
                 y = Predicted.10.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(13:15),
                 y = Predicted.10.5[Predicted.10.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(11:20),
                y = Predicted.10.10),
            color = "#747474")+
  geom_point(aes(x = c(11:20),
                 y = Predicted.10.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(14,16,20),
                 y = Predicted.10.10[Predicted.10.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:10,11,13,15,20),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,20))+
  labs(x = "Time (years)",
       y = " ",
       tag = expression(bold("(k)")),
       title = expression(italic("i")*" = 6; "*italic("t")*" = 10"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1k

Fig1l <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:10)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(5,10),
                     minor_breaks = c(5:10),
                     breaks = c(1:10),
                     expand = c(0.02,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = "Time (years)",
       y = " ",
       tag = expression(bold("(l)")),
       title = expression(italic("i")*" = 6; "*italic("t")*" = 10"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1l


#Figure 1mn ####
#i = 11
Fig1m <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:11)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 12, xmax = 21,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 12, xmax = 16,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 12, xmax = 14,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:11)),
              aes(x = time, y = Observed),
              method = "loess", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 11.5,
             linetype = "dotted")+
  geom_line(aes(x = c(12:14),
                y = Predicted.11.3),
            color = "#747474")+
  geom_point(aes(x = c(12:14),
                 y = Predicted.11.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(13:14),
                 y = Predicted.11.3[Predicted.11.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(12:16),
                y = Predicted.11.5),
            color = "#747474")+
  geom_point(aes(x = c(12:16),
                 y = Predicted.11.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(14,16),
                 y = Predicted.11.5[Predicted.11.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(12:21),
                y = Predicted.11.10),
            color = "#747474")+
  geom_point(aes(x = c(12:21),
                 y = Predicted.11.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(13,17,18),
                 y = Predicted.11.10[Predicted.11.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:11,12,14,16,21),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,10))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(m)")),
       title = expression(italic("i")*" = 11"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1m

Fig1n <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:11)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(1,15),
                     minor_breaks = c(1:15),
                     breaks = c(1:11),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = "",
       y = "",
       tag = expression(bold("(n)")),
       title = expression(italic("i")*" = 11"))+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1n

#Figure 1op ####
#i = 12
Fig1o <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:12)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 13, xmax = 22,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 13, xmax = 17,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 13, xmax = 15,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:12)),
              aes(x = time, y = Observed),
              method = "loess", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 12.5,
             linetype = "dotted")+
  geom_line(aes(x = c(13:15),
                y = Predicted.12.3),
            color = "#747474")+
  geom_point(aes(x = c(13:15),
                 y = Predicted.12.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(13:15),
                 y = Predicted.12.3[Predicted.12.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(13:17),
                y = Predicted.12.5),
            color = "#747474")+
  geom_point(aes(x = c(13:17),
                 y = Predicted.12.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(14:17),
                 y = Predicted.12.5[Predicted.12.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(13:22),
                y = Predicted.12.10),
            color = "#747474")+
  geom_point(aes(x = c(13:22),
                 y = Predicted.12.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(13,14,16,18:22),
                 y = Predicted.12.10[Predicted.12.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:12,13,15,17,22),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,10))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(o)")),
       title = expression(italic("i")*" = 12"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1o

Fig1p <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:12)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(1,15),
                     minor_breaks = c(1:15),
                     breaks = c(1:12),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(p)")),
       title = expression(italic("i")*" = 12"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1p

#Figure 1qr ####
#i = 13
Fig1q <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:13)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 14, xmax = 23,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 14, xmax = 18,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 14, xmax = 16,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:13)),
              aes(x = time, y = Observed),
              method = "loess", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 13.5,
             linetype = "dotted")+
  geom_line(aes(x = c(14:16),
                y = Predicted.13.3),
            color = "#747474")+
  geom_point(aes(x = c(14:16),
                 y = Predicted.13.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(14:16),
                 y = Predicted.13.3[Predicted.13.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(14:18),
                y = Predicted.13.5),
            color = "#747474")+
  geom_point(aes(x = c(14:18),
                 y = Predicted.13.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(14:18),
                 y = Predicted.13.5[Predicted.13.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(14:23),
                y = Predicted.13.10),
            color = "#747474")+
  geom_point(aes(x = c(14:23),
                 y = Predicted.13.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(14,15,18:23),
                 y = Predicted.13.10[Predicted.13.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:13,14,16,18,23),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,10))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(q)")),
       title = expression(italic("i")*" = 13"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1q

Fig1r <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:13)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(1,15),
                     minor_breaks = c(1:15),
                     breaks = c(1:13),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(r)")),
       title = expression(italic("i")*" = 13"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1r

#Figure 1st ####
#i = 14
Fig1s <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:14)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 15, xmax = 24,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 15, xmax = 19,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 15, xmax = 17,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:14)),
              aes(x = time, y = Observed),
              method = "loess", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 14.5,
             linetype = "dotted")+
  geom_line(aes(x = c(15:17),
                y = Predicted.14.3),
            color = "#747474")+
  geom_point(aes(x = c(15:17),
                 y = Predicted.14.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(15:17),
                 y = Predicted.14.3[Predicted.14.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(15:19),
                y = Predicted.14.5),
            color = "#747474")+
  geom_point(aes(x = c(15:19),
                 y = Predicted.14.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(15:19),
                 y = Predicted.14.5[Predicted.14.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(15:24),
                y = Predicted.14.10),
            color = "#747474")+
  geom_point(aes(x = c(15:24),
                 y = Predicted.14.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(15:24),
                 y = Predicted.14.10[Predicted.14.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,20),
                     minor_breaks = c(1:20),
                     breaks = c(1:14,15,17,19,24),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,10))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(s)")),
       title = expression(italic("i")*" = 14"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1s

Fig1t <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:14)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(1,15),
                     minor_breaks = c(1:15),
                     breaks = c(1:14),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = " ",
       y = " ",
       tag = expression(bold("(t)")),
       title = expression(italic("i")*" = 14"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1t

#Figure 1uv ####
#i = 15
Fig1u <- ggplot()+
  geom_point(data = df_fig |>
               filter(time %in% c(1:15)),
             aes(x = time, y = Observed),
             color = "blue") +
  geom_rect(data = df_fig,
            xmin = 16, xmax = 25,
            ymin = -Inf, ymax = Inf,
            fill = "#f9f9f9",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 16, xmax = 20,
            ymin = -Inf, ymax = Inf,
            fill = "#ececec",
            alpha = 0.5)+
  geom_rect(data = df_fig,
            xmin = 16, xmax = 18,
            ymin = -Inf, ymax = Inf,
            fill = "#dfdfdf",
            alpha = 0.5)+
  geom_smooth(data = df_fig |>
                filter(time %in% c(1:15)),
              aes(x = time, y = Observed),
              method = "loess", se = F, color = "black")+
  geom_hline(yintercept = N.critical,
             linetype = "dashed")+
  geom_vline(xintercept = 15.5,
             linetype = "dotted")+
  geom_line(aes(x = c(16:18),
                y = Predicted.15.3),
            color = "#747474")+
  geom_point(aes(x = c(16:18),
                 y = Predicted.15.3),
             color = "#747474",
             shape = 21)+
  geom_point(aes(x = c(16:18),
                 y = Predicted.15.3[Predicted.15.3<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(16:20),
                y = Predicted.15.5),
            color = "#747474")+
  geom_point(aes(x = c(16:20),
                 y = Predicted.15.5),
             color = "#747474",
             shape = 22)+
  geom_point(aes(x = c(16:20),
                 y = Predicted.15.5[Predicted.15.5<N.critical]),
             color = "red",
             shape = 4)+
  geom_line(aes(x = c(16:25),
                y = Predicted.15.10),
            color = "#747474")+
  geom_point(aes(x = c(16:25),
                 y = Predicted.15.10),
             color = "#747474",
             shape = 23)+
  geom_point(aes(x = c(16:25),
                 y = Predicted.15.10[Predicted.15.10<N.critical]),
             color = "red",
             shape = 4)+
  scale_x_continuous(limits = c(1,25),
                     minor_breaks = c(1:25),
                     breaks = c(1:15,16,18,20,25),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(5,70),
                     minor_breaks = c(5:70),
                     breaks = seq(10,70,10))+
  labs(x = "Time (years)",
       y = " ",
       tag = expression(bold("(u)")),
       title = expression(italic("i")*" = 15"))+
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme_classic()
Fig1u

Fig1v <- df_fig |>
  pivot_longer(!c(time, Observed),
               names_to = "SimulatedWindow",
               values_to = "Persistence") |>
  filter(time %in% c(1:15)) |>
  ggplot(aes(x = time,
             y = Persistence,
             shape = factor(SimulatedWindow,
                            levels = c("S_{3 years}",
                                       "S_{5 years}",
                                       "S_{10 years}")))) +
  geom_line(color = "#747474", alpha = 0.75) +
  geom_point(color = "#747474") +
  scale_x_continuous(limits = c(1,15),
                     minor_breaks = c(1:15),
                     breaks = c(1:15),
                     expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,1))+
  scale_shape_manual(values = c(21,22,23))+
  labs(x = "Time (years)",
       y = " ",
       tag = expression(bold("(v)")),
       title = expression(italic("i")*" = 15"),
       shape = "Simulated window")+
  theme_classic() +
  guides(x = guide_axis(minor.ticks = T),
         y = guide_axis(minor.ticks = T))+
  theme(legend.position = "none")
Fig1v

#Arrange figure ####
#and combine in the figure

LabAbundance <- text_grob(label = '\n Abundance', face = "bold")
LabPersistence <- text_grob(label = paste("\n ","Persistence"),
                            face = "bold")

Fig1 <- grid.arrange(Fig1a, Fig1b,
                     Fig1c, Fig1d,
                     Fig1e, Fig1f,
                     Fig1g, Fig1h,
                     Fig1i, Fig1j,
                     Fig1k, Fig1l,
                     ncol = 2, widths = c(2.5,1),
                     top = grid.arrange(LabAbundance, LabPersistence,
                                        ncol = 2, widths = c(2.5,1)))

#Portrait 7x10




#Previous attempts ####

library(Hmisc)
#already have the functions of vpm

#Subset of California Condor 1965-1980

Observed.t=c(38,51,46,52,53, #1966-1969 (end 8, 10, 15)
             28,             #(end 9, 11, 16)
             33,             #(end 10, 12, 17)
             35,             #(end 11, 13, 18)
             17,             #(end 12, 14, 19)
             23,             #(end 13, 15, 20)
             27,             #(end 14, 16, 21)
             14,             #(end 15, 17, 22)
             13,             #(end 16, 18, 23)
             12,16,11);
Time.t=c(1:length(Observed.t));

yt <- log(Observed.t)
tt <- Time.t

#Arbitrary, the half of initial population
N.critical <- round(1/2*exp(yt[1]),0)

#initial model
yt1 <- yt[1:9]
tt1 <- tt[1:9]

#1 fit a population model (EGSS)
egss.parms.1 <- egss_remle(yt = yt1,
                              tt = tt1,
                              fguess = guess_egss(yt = yt1,
                                                  tt = tt1))
#2 See population parameters to simulate
egss.parms.1

#1 Model fitted
egss.predict.1 <- egss_predict(yt = yt1,
                               tt = tt1,
                               parms = egss.parms.1$remles,
                               plot.it = F)

#Conditions to simulations
last.tt <- last(tt1)
ntraj = 1
l = last(tt[1:last.tt])
len.sim <- c(3, 5, 10) #3+1, 5+1, 10+1

#3 Simulate dynamics

# Set up composite figure
par(mfrow = c(1, 3), # 1 row, 3 columns,
    mar = c(2, 2, 2, 2), #adjust margins (bottom, left, top, right)
    oma = c(3, 3, 0, 0))

for (i in seq_along(len.sim)){
  plot(tt[1:last.tt],
       exp(yt[1:last.tt]),
       type = "p", lwd = 1, cex.lab = 1.25, col = "blue",pch = 16,
       xlim = c(min(tt), 20),
       xlab = "",
       ylab = "",
       ylim = c(0,80),
       main = paste("Simulation window = ", len.sim[i], "years"))

  minor.tick(nx = 5, ny = 5, tick.ratio = 0.5)

  points(x = tt[1:last.tt],
       y = egss.predict.1[[1]][,2],
       type = "l", col = "black", pch = 1, lwd = 1.2)

  points(x = tt[1:last.tt],
         y = egss.predict.1[[1]][,2],
         type = "p", col = "black", pch = 1, lwd = 1.2)

  thres.times <- as.numeric(0:(len.sim[i]))
  len <- max(thres.times) + 1

  #ribbon
  polygon(x = c(tt[last.tt]+1, tt[last.tt]+1,
                tt[last.tt]+len-1, tt[last.tt]+len-1),
          y = c(0,110,110,0),
          col = "#ECECEC60",
          border = F)

  #simulation
  sim.mat <- egss_sim(ntraj,
                      tt = thres.times,
                      parms = egss.parms.1$remles)

  phi <- rep(0, ntraj)
  last.points <- rep(0, ntraj)

  for(n in 1:ntraj){
    Pop.sim <- round(exp(sim.mat),0);
    last.points[n] <- Pop.sim[len]

    Pop.sim <- Pop.sim[-1] #Removing first simulation, last obs

    simu.len <- thres.times[-1]

    points(l + simu.len,
          Pop.sim,
          col = "#676767",
          type = "l",
          lty = "solid")

    points(l + simu.len,
           Pop.sim,
           col = "#676767",
           type = "p",
           lty = "solid")

    #Points below.threshold
    below.threshold <- (Pop.sim < N.critical)
    points(l + simu.len[below.threshold],
           Pop.sim[below.threshold],
           col = "red",
           pch = 4)
  }

  abline(h = N.critical, lty=2, lwd=1);
}

#only for the third panel (13)
#mtext(side = 2, outer = T, line = 1, "Abundance", cex = 0.8)

#only for the fifth panel
mtext(side = 1, outer = T, line = 0.5, "Time (years)", cex = 0.8)

dev.off()

plot.new()
legend("center",
         legend = c("Observed", "Predicted", "Simulated", "Below threshold"),
         col = c("blue", "black", "#676767", "red"),
         lty = c(NA, 1, 1, NA),
       pch = c(16, 1, 1, 4),
       lwd = c(1, 1.2, 1, NA),
 #        cex = 0.3,
         bty = "n",
         horiz = F)
# Reset plotting layout

