## Bell sound simulation based on vibrational modes supplied by FEA analysis

library(signal)
library(fftw)
library(seewave)
library(tuneR)
library(audio)
library(reshape2)

HUM <- 587.89
PRIME <- 1111.31
TIERCE <- 1466.49
NOM <- 1723.07
SUP_QUINT <- 2505
OCT_NOM <- 3446
SUP_TIERCE <- 5010
WEIRD <- 6012

peaks <- c(HUM,PRIME,TIERCE,NOM,SUP_QUINT,OCT_NOM,SUP_TIERCE,WEIRD)

env.1 <- rep(NA,441000)
env.2 <- rep(NA,441000)
env.3 <- rep(NA,441000)
env.4 <- rep(NA,441000)
env.5 <- rep(NA,441000)
env.6 <- rep(NA,441000)
env.7 <- rep(NA,441000)
env.8 <- rep(NA,441000)


## List of amp envelopes
env.total <- list(env.1,env.2,env.3,env.4,env.5,env.6,env.7,env.8)

##Space allocation
for(k in 1:length(env.total)){
  env.total[[k]]<- rep(NA,441000)
}

## Vector with y-intercepts of each exponential decay function
# intercepts <- c(213004,613403,433923,1000000,168337,53084,28636,16255)
intercepts <- c(213004,513403,433923,1000000,1168337,553084,228636,316255)

##Vector with exponents (first term) of decay functions
first_ex <- c(4,2,2,4,2,2,2,2)

##Vector with exponents (second term) of decay functions
# second_ex <- c(6,5,5,5,5,5,5,5)
second_ex <- c(6,5.4,5,5,5,4.9,5,5)

## Set of exponential decay functions defining the amplitude envelopes for each partial
for(j in 1:length(peaks)){
  
  for(i in 0:441000){
    buffer <- intercepts[j]*exp(-(first_ex[j])*(10^-(second_ex[j]))*i)
    env.total[[j]][i] <- buffer
  }
  
}


## Synthesis
for(i in 1:length(peaks)){
  partial <-synth2(env = env.total[[i]], ifreq=rep(c(as.numeric(peaks[i])), each=441000), f=44100, listen=FALSE)
  if(i==1){
    s <- partial
  }
  if(i!=1){
    s <- s+partial
  }
  
}

spectro(s, f=44100, ovlp=0, zp=16, collevels=seq(-40,0,0.5),flim=c(0,3),osc=TRUE)
listen(s, f=44100)

writeWave(normalize(Wave(s), unit = "16", center=TRUE), "C:/Users/Keiran/Desktop/Major Tierce Simulation_tuned.wav")

