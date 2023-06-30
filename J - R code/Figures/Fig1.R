# Efron & Moris figure 
# Model 
# Load output BayesFactor package: 
samplesindat5 <- readRDS("K - R objects/BF model to create figure 1B/BFmodelfit15.rds")
meanbfindat <- colMeans(samplesindat5)  # now BF model output with seperate effect mu's and different r scale (all set to 1)

# From data, from proposal r code 
# Observations 
indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
colnames(indat)=c('sub','block','trial','stim','resp','rt','error')

clean=function()
{
  indat=read.table(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-dist5/ld5.all'))
  colnames(indat)=c('sub','block','trial','stim','resp','rt','error')
  
  bad1=indat$sub%in%c(34,43)
  bad2=indat$rt<250 | indat$rt>2000
  bad3=indat$err==1
  bad4=indat$block==0 & indat$trial<20
  bad5=indat$trial==0
  
  bad=bad1 | bad2 | bad3 |bad4 |bad5
  dat=indat[!bad,]
  return(dat)
}


# x axis digit 
# y axis: mean RT pp 
indat1 <- clean()
dat <- indat1  # Save the dataframe in new object 

sub <- as.integer(as.factor(dat$sub))
I <- max(sub)

deltabf2 <- meanbfindat[(3*(I+1) + 1)] + meanbfindat[(3*(I+1) + 2):(4*(I+1))]
deltabf2  # digit two estimates 


# Mean observations 
meanrtindat <- aggregate(indat1$rt, list(indat1$stim, indat1$sub), mean)  # mean response time per condition per participant 
meanrt <- aggregate(indat1$rt, list(indat1$stim), mean)
for (i in 1:nrow(meanrtindat)){
  if (meanrtindat$Group.1[i] == 0) {meanrtindat$Group.1[i] <- 2 
  } else if(meanrtindat$Group.1[i] == 1) {meanrtindat$Group.1[i] <- 3 
  } else if(meanrtindat$Group.1[i] == 2) {meanrtindat$Group.1[i] <- 4
  } else if(meanrtindat$Group.1[i] == 3) {meanrtindat$Group.1[i] <- 6 
  } else if(meanrtindat$Group.1[i] == 4) {meanrtindat$Group.1[i] <- 7 
  } else {meanrtindat$Group.1[i] <- 8} 
}

meanrtindat6 <- meanrtindat[which(meanrtindat$Group.1==6),] 
meanrtindat7 <- meanrtindat[which(meanrtindat$Group.1==7),] 
realdatdif67 <- (meanrtindat6$x - meanrtindat7$x)/1000
realdatdif67 <- (meanrtindat6$x - meanrtindat7$x)

# Figure 1 
for (i in 1:nrow(meanrt)){
  if (meanrt$Group.1[i] == 0) {meanrt$Group.1[i] <- 2 
  } else if(meanrt$Group.1[i] == 1) {meanrt$Group.1[i] <- 3 
  } else if(meanrt$Group.1[i] == 2) {meanrt$Group.1[i] <- 4
  } else if(meanrt$Group.1[i] == 3) {meanrt$Group.1[i] <- 6 
  } else if(meanrt$Group.1[i] == 4) {meanrt$Group.1[i] <- 7 
  } else {meanrt$Group.1[i] <- 8} 
}
theme_set(theme_apa(base_size = 9))
meanrtplot <- ggplot(meanrtindat, aes(y = x, x = as.factor(Group.1))) + geom_line(aes(group = Group.2), alpha = .15) + 
  geom_line(data = meanrt, aes(group = 1), color='blue') + geom_point(data = meanrt, aes(group = 1), color='blue') + 
  labs(x = "Digit", y = "Response Time (Seconds)") + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank())

# Difference 3 and 4 
dif34indat <- meanrtindat[which(meanrtindat$Group.1 == 3 | meanrtindat$Group.1 == 4),]
meandif34 <- meanrt[which(meanrt$Group.1 == 3 | meanrt$Group.1 == 4),]

dif34indat$x <- dif34indat$x/1000
meandif34$x <- meandif34$x/1000
meanrtplot2 <- ggplot(dif34indat, aes(y = x, x = as.factor(Group.1))) + 
  geom_line(aes(group = Group.2), alpha = .15) + 
  geom_point(color = "white") +
  geom_point(shape = 21, fill = "white", alpha = .15) + 
  geom_line(data = meandif34, aes(group = 1), color='blue', size = 1) + 
  geom_point(data = meandif34, aes(group = 1), color='blue', shape = 21, size = 1.5, stroke = 1, fill = "white") + 
  labs(x = "Digit", y = "Response Time (Seconds)") + 
  theme_bw() + 
  theme(axis.line = element_line(colour = "black"), panel.border = element_blank(), panel.background = element_blank())+
  labs(subtitle = "A")


# Figure 2 
Combinedestreal67 <- data.frame(sub = rep(1:52, 2), type = rep(c("Model", "Sample"), each = 52), combined = c(deltabf2, realdatdif67))

hierplotefmor <- ggplot(Combinedestreal67, aes(x = combined, y = type, color = type, group = sub)) + 
  geom_line(color = "black", alpha = 0.15) + 
  geom_point(shape = 21, size = 1.5, stroke = 1, fill = "white") + 
  ylab("") + 
  xlab("Effect (in Seconds)") + 
  scale_color_manual(values = c("darkgreen", "darkorange")) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  labs(subtitle = "B")

# Save figure as png 
ggsave(file = "hierplotefmor.png", hierplotefmor)

# Combine figures 
library(gridExtra)
grid.arrange(meanrtplot2, hierplotefmor, nrow = 1, ncol = 2)

meanrtandhierplot_v2 <- arrangeGrob(meanrtplot2, hierplotefmor, nrow = 1, ncol = 2)
ggsave(file = "Fig1_meanrtandhierplot_v2.png", meanrtandhierplot_v2, width = 11, height = 4)
