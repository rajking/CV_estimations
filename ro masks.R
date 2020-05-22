library(ggplot2)

# b       # infections per contact (transmission risk) between 0 and 1. 
# k       # contacts per time
# d       # time per infectin (duration of infectivity)
# R = b*k*d

# b      (probability of infection given contact between a susceptible and infected individual)

# meff       # effectiveness of mask, probability between 0 and 1
# mask benefit = b*(1-m) # how much transmission risk is reduced.
# R_benefit = b*(1-m)*k*d = (1-m)*R

# mhazz       # hazardousness of mask, probability between 0 and 1
# mask harm = b*(1-m)+m = b+m*(1-b) # how much transmission risk is increased.
# R_harm =  [b+m*(1-b)]*k*d = [1+m*(1/b-1)]*R




# calculate benefit

Ro <- 3 # set R

Meff <- c(0.10,0.3,1) # filter efficiency of the mask
Mcov <- c(rep(0,length(Meff)),rep(1,length(Meff)))  # proportion of contacts that are taking place with proper use of a certain mask

Reff <- c(Meff*0+Ro,(1-Meff)*Ro)

MaskEff<-rep(Meff,2)

benefit<-data.frame(Mcov,Reff,MaskEff)

ggplot(benefit, aes(x=Mcov,y=Reff,group=MaskEff, color=factor(MaskEff))) +
  geom_line()+
  ylim(0,4) + 
  annotate("segment", linetype = "dashed", x = 0, xend = 1, y = 1, yend = 1) + 
  ggtitle("Effect of mask benefit and mask coverage on the reproduction number R") +
  ylab("Reproduction Number R") + 
  xlab("Mask Coverage Mcov") +
  labs(color = "Mask Benefit")


# calculate harm

Ro <- 3  # Set R0
b <- 0.1   # set transmission risk

Mhaz <- c(0.10,0.3,1) # hazardousness of the mask
Mcov <- c(rep(0,length(Mhaz)),rep(1,length(Mhaz)))  # proportion of contacts that are using mask

Rhaz <- c(Mhaz*0+Ro,(1+Mhaz*(1/b-1))*Ro)

MaskHaz <- rep(Mhaz,2)

harm<-data.frame(Mcov,Rhaz,MaskHaz)

ggplot(harm, aes(x=Mcov,y=Rhaz,group=MaskHaz,color=factor(MaskEff))) +
  geom_line()+ylim(0,30) +
  annotate("segment", linetype = "dashed", x = 0, xend = 1, y = 1, yend = 1) +  
  ggtitle("Effect of mask hazard and mask coverage on the reproduction number R") +
  ylab("Reproduction Number R") + 
  xlab("Mask Coverage Mcov") +
  labs(color = "Mask Hazard")



# how does mask hazard effect attack rate b?
b+Mhaz*(1-b)


