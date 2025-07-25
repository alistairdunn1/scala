####################
# create scaled LF for ling using D'Arcy's strata
# strata used are from length model with optimised alpha
####################

require(tidyverse)
require(sf)

# load the data
load(make.filename("LF.LIN.rdata",DIR.LIN$RData))
load(make.filename("age.LIN.rdata",DIR.LIN$RData))
source(make.filename("(01) nzsf.function.faster.r",DIR$R))
my.plot<- plot.nzsf(plot.spp="LIN",my_proj=proj_flat,plot.stat.areas = F,plot.qma = T,plot.lin.stock.lines = T,plot.stat.area.names = F,plot.qma.range=c("LIN5","LIN6"),plot.suba=T)


####################
# get the strata
####################

# Load the function and data ----
#source(make.filename("Spatial\\functions.R",DIR$R))
load(make.filename("Spatial\\strata_length_0.19.rda",DIR.LIN$R))

# get the lf strata ---
temp.lf <- LF.LIN %>% filter(stock %in% "LIN5&6",!is.na(long),!is.na(lat))
temp.lf$lgth.stratum <- assign.stratum.generic(long=temp.lf$long,lat=temp.lf$lat,stratum.layer=length_strata,stratum.name="cluster")

#--------
# get the effort strata for scaling up

temp.effort <- EFFORT %>% filter(LIN.stock %in% "LIN5&6",LIN.catch>0)
temp.effort$lgth.stratum <- assign.stratum.generic(long = temp.effort$StartLongitude, lat = temp.effort$StartLatitude,stratum.layer=length_strata,stratum.name="cluster")


#--------------
# try some plots - tbc

my.plot[[1]] +
  geom_sf(data = length_strata, aes(fill = cluster)) +my.plot[[2]]

my.plot[[1]] +
  geom_sf(data = temp.lf %>% st_transform(crs = st_crs(length_strata)), aes(colour = cluster)) +
  plot_coast(proj = st_crs(length_strata), resolution = "low", fill = "black", colour = "black", size = 0.3) +
  plot_clip(x = length_strata %>% st_transform(crs = st_crs(length_strata)))

new.graph(0.6)
my.plot[[1]] +
  geom_sf(data = length_strata,aes(fill=cluster)) +
  my.plot[[2]] +scale_color_viridis_d()
  
new.graph(0.6)
my.map[[1]] +
  geom_sf(data = length_strata,aes(fill=cluster,col=cluster)) +
geom_sf(data = temp.lf %>% st_transform(crs = st_crs(length_strata)), aes(colour = cluster)) +
  plot_coast(proj = st_crs(length_strata), resolution = "low", fill = "black", colour = "black", size = 0.3)  + my.map[[2]] + scale_colour_viridis_d()



######################
# prep for the LFs
######################

# add the stratum weight
nrow(temp.lf)
temp.lf <- temp.effort %>% 
  group_by(myear,lgth.stratum) %>% 
  summarise(stratum.weight=Sum(LIN.catch)) %>%
  right_join(temp.lf) %>%
  rename(stratum=lgth.stratum)
nrow(temp.lf)


#--------------
# now add weight of each fish (sample.weight is not right: sometimes twice or three times)

temp.lf <- mutate(temp.lf, fish.weight = case_when(
  sex %in% "M" ~ 2.13e-6*length^3.179,
  sex %in% "F" ~ 1.32e-6*length^3.293,
  sex %in% "U" ~ 1.725e-6*length^3.236,
  TRUE ~ 0
))

#-------------
# fix where catch.weight = 0 with the sum of the fish weights

temp.lf <- temp.lf %>% group_by(fishing.event.key) %>%
  summarise(sum.fish.weight=Sum(fish.weight),n.fish=n())  %>%
  right_join(temp.lf)
nrow(temp.lf)

# and select where there have been at least 5 fish measured
temp.lf <- temp.lf %>% filter(n.fish>4)
nrow(temp.lf)

temp.lf <- temp.lf %>% mutate(catch.weight = case_when(
  is.na(catch.weight) | catch.weight == 0 | catch.weight < sum.fish.weight ~ sum.fish.weight,
  TRUE ~ catch.weight
))

#--------------
# calculate the stratum weighting (sample weighting is redone within the bootstrap)
# this is stratum weight / sum of the catch weight in that stratum
# needs to be done after catch.weight is fixed

temp.lf <- temp.lf %>% distinct(fishing.event.key,.keep_all=T) %>% 
  group_by(myear, stratum) %>% 
  summarise(stratum.sum.catch.weight=Sum(catch.weight)) %>%
  right_join(temp.lf)

temp.lf <- temp.lf %>% mutate(stratum.weighting = stratum.weight/stratum.sum.catch.weight*1000)
temp.lf$stratum.weighting[is.na(temp.lf$stratum.weighting) | temp.lf$stratum.weighting<1] <- 1
nrow(temp.lf)


#-------------
# select the variables and run by year and area
temp <- dplyr::select(temp.lf, c(myear,stratum,fishing.event.key,length,sex,stratum.weighting,catch.weight,fish.weight)) %>%
  group_by(myear,stratum)          # do it by year and stratum

summary(temp)
nrow(temp)


###############
# do the scaled LF
###############

# values
n.boot = 300
min.length=10
max.length=150
bin.length=1



#-----------
# resample set and fish with replacement but no weighting
# then apply the weighting 

res<-list()

for (i in 1:n.boot) { # loop for the bootstrap
  
  temp2 <- temp %>% dplyr::select(myear,stratum,fishing.event.key) %>%   # get the unique sets
    distinct() %>%
    slice_sample(prop=1,replace=T) %>%        # sample the unique sets
    pluck("fishing.event.key")
  
  # this is not pretty and not fast
  dat <- data.frame()
  for (j in 1:length(temp2)) {
    dat <- temp %>% filter(fishing.event.key %in% temp2[j]) %>%
      slice_sample(prop=1,replace=T) %>%
      mutate(sample.no = j) %>% # to be able to calculate weighting by sample number
      bind_rows(dat)
  }
  
  # calculate sample weighting
  dat <- dat %>% group_by(sample.no) %>%
    summarise(sum.fish.weight=Sum(fish.weight))  %>%
    right_join(dat) %>%
    mutate(sample.weighting = catch.weight/sum.fish.weight)
  dat$sample.weighting[dat$sample.weighting < 1] <- 1
  
  res[[i]] <- dat
}

lf.boot.lin.DW <- res
save(res,file=make.filename("LIN LF\\lf.boot.lin.DW.lgth.rdata",DIR.LIN$RData))
source(make.filename("LIN LF\\lf.boot.lin.DW.lgth.rdata",DIR.LIN$RData))  
