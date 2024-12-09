#Figure 1
library(Hmisc)
#already have the functions of vpm

#fake some values
monitoring <- readRDS("data_tmp/snailkitesPP.rds") |>
  group_by(year) |>
  summarise(meanSM = round(min(abundance.monitored),0),
            meanCS = round(max(Observed.y),0)) |>
  drop_na() |>
  pivot_longer(!year, names_to = "mean", values_to = "ObsY")

ObsY <- c(monitoring$ObsY)
yt <- log(ObsY)
tt <- c(1:14)
N.critical <- round(1/2*mean(exp(yt)),0)

#initial model 1:10, 1:11, 1:12, 1:13, 1:14
yt1 <- yt[1:14]
tt1 <- tt[1:14]

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
       xlim = c(min(tt), 25),
       xlab = "",
       ylab = "",
       ylim = c(0,65),
       main = paste("Simulation window = ", len.sim[i], "years"))

  minor.tick(nx = 5, ny = 5, tick.ratio = 0.5)

  points(x = tt[1:last.tt],
       y = egss.predict.1[[1]][,2],
       type = "b", col = "black", pch = 1, lwd = 1.2)

  thres.times <- as.numeric(0:(len.sim[i]))
  len <- max(thres.times) + 1

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
          col = "darkgray",
          type = "b",
          lty = "solid")

    #Points below.threshold
    below.threshold <- (Pop.sim < N.critical)
    points(l + simu.len[below.threshold],
           Pop.sim[below.threshold],
           col = "red",
           pch = 4)
  }

  abline(h = N.critical, lty=2, lwd=1);

  polygon(x = c(tt[last.tt]+1, tt[last.tt]+1,
                tt[last.tt]+len-1, tt[last.tt]+len-1),
          y = c(0,65,65,0),
          col = "#ECECEC60",
          border = F)
}

#only for the third panel (13)
#mtext(side = 2, outer = T, line = 1, "Abundance", cex = 0.8)

#only for the fifth panel
mtext(side = 1, outer = T, line = 0.5, "Time (years)", cex = 0.8)

dev.off()

plot.new()
legend("center",
         legend = c("Observed", "Predicted", "Simulated", "Below threshold"),
         col = c("blue", "black", "darkgray", "red"),
         lty = c(NA, 1, 1, NA),
       pch = c(16, 1, 1, 4),
       lwd = c(1, 1.2, 1, NA),
#         cex = 0.54,
         bty = "n",
         horiz = F)
# Reset plotting layout

