library(signal)
library(fftw)
library(seewave)
library(tuneR)
library(audio)
library(reshape2)

lseq <- function(from, to, b, length.out) {
  exp(seq(log(from, base=b), log(to, base=b), length.out = length.out))
}

data <- readWave("C:/Users/Keiran/Desktop/C1.wav")

spectro(data, f=44100, ovlp=0, zp=16, collevels=seq(-40,0,0.5),flim=c(0,3),osc=TRUE)



time <- 1
spec<- spec(data,f=44100,at=time,flim=c(0,3),wl = 10000)
peaks <- fpeaks(spec, nmax=8,plot = TRUE,title=TRUE)
peaks[,1] <- peaks[,1]*1000


for (j in 0:9){
  spectrum <- spec(data, f=44100, at=j+0.1, plot = FALSE, norm = FALSE)
  peaks_temp <- as.data.frame(fpeaks(spectrum, f=44100, nmax=8, plot = FALSE))
  if(j==0){
    amplitudes <- as.data.frame(peaks_temp$amp)
  }
  if(j!=0){
    amplitudes <- as.data.frame(cbind.data.frame(amplitudes, peaks_temp$amp))
  }
  
}

colnames(amplitudes) <- c("0","44100","88200","132300","176400","220500","264600","308700","352800","396900")



##Here is the exponential function fitting to the decay of the partials
# timestamp <- 1:10
# plot(log(timestamp),log(as.numeric(amplitudes[4,])))
# 
# fit = lm(log(as.numeric(amplitudes[4,])) ~ log(timestamp))
# exponent <- fit$coefficients[2]
env.1 <- rep(NA,441000)
env.2 <- rep(NA,441000)
env.3 <- rep(NA,441000)
env.4 <- rep(NA,441000)
env.5 <- rep(NA,441000)
env.6 <- rep(NA,441000)
env.7 <- rep(NA,441000)
env.8 <- rep(NA,441000)
# s <- rep(NA,441000)

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
for(j in 1:length(peaks[,1])){
  
  for(i in 0:441000){
    buffer <- intercepts[j]*exp(-(first_ex[j])*(10^-(second_ex[j]))*i)
    env.total[[j]][i] <- buffer
  }
  
}


## Synthesis
for(i in 1:length(peaks[,1])){
    partial <-synth2(env = env.total[[i]], ifreq=rep(c(as.numeric(peaks[i,1])), each=441000), f=44100, listen=FALSE)
    if(i==1){
      s <- partial
    }
    if(i!=1){
      s <- s+partial
    }

}

wash <- noise(kind = c("red"), duration = 4410, samp.rate=44100, bit = 1, stereo = FALSE, xunit = "samples")
env.wash <- env(wash, f=44100, plot=FALSE)
ifreq.wash <- ifreq(wash, f=44100,plot=FALSE)$f[,2]
wash<- synth2(env=env.wash*10, ifreq=ifreq.wash, f=44100)
wash_withsilence <- c(wash,rep(0,each=436590))

spectro(data, f=44100, ovlp=0, zp=16, collevels=seq(-40,0,0.5),flim=c(0,3),osc=TRUE)
spectro(s, f=44100, ovlp=0, zp=16, collevels=seq(-40,0,0.5),flim=c(0,3),osc=TRUE)
listen(s, f=44100)
writeWave(normalize(Wave(s), unit = "16", center=TRUE), "C:/Users/Keiran/Desktop/C1_artificial.wav")
