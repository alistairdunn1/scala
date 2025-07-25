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
temp <- LF.LIN %>% filter(stock %in% "LIN5&6",!is.na(long),!is.na(lat))
temp$stratum <- assign.stratum.generic(long=temp$long,lat=temp$lat,stratum.layer=length_strata,stratum.name="cluster")

#--------
# get the effort strata for scaling up

temp.effort <- EFFORT %>% filter(LIN.stock %in% "LIN5&6",LIN.catch>0)
temp.effort$stratum <- assign.stratum.generic(long = temp.effort$StartLongitude, lat = temp.effort$StartLatitude,stratum.layer=length_strata,stratum.name="cluster")


#--------------
# try some plots - tbc

my.plot[[1]] +
  geom_sf(data = length_strata, aes(fill = cluster)) +my.plot[[2]]

my.plot[[1]] +
  geom_sf(data = temp %>% st_transform(crs = st_crs(length_strata)), aes(colour = cluster)) +
  plot_coast(proj = st_crs(length_strata), resolution = "low", fill = "black", colour = "black", size = 0.3) +
  plot_clip(x = length_strata %>% st_transform(crs = st_crs(length_strata)))

new.graph(0.6)
my.plot[[1]] +
  geom_sf(data = length_strata,aes(fill=cluster)) +
  my.plot[[2]] +scale_color_viridis_d()
  
new.graph(0.6)
my.map[[1]] +
  geom_sf(data = length_strata,aes(fill=cluster,col=cluster)) +
geom_sf(data = temp %>% st_transform(crs = st_crs(length_strata)), aes(colour = cluster)) +
  plot_coast(proj = st_crs(length_strata), resolution = "low", fill = "black", colour = "black", size = 0.3)  + my.map[[2]] + scale_colour_viridis_d()



######################
# prep for the LFs
######################

# add the stratum weight
nrow(temp)
temp <- temp.effort %>% 
  group_by(myear,stratum) %>% 
  summarise(stratum.weight=Sum(LIN.catch)) %>%
  right_join(temp) %>%
  rename(stratum=stratum)
nrow(temp)


#--------------
# now add weight of each fish (sample.weight is not right: sometimes twice or three times)

temp <- mutate(temp, fish.weight = case_when(
  sex %in% "M" ~ 2.13e-6*length^3.179,
  sex %in% "F" ~ 1.32e-6*length^3.293,
  sex %in% "U" ~ 1.725e-6*length^3.236,
  TRUE ~ 0
))

#-------------
# fix where catch.weight = 0 with the sum of the fish weights

temp <- temp %>% group_by(fishing.event.key) %>%
  summarise(sum.fish.weight=Sum(fish.weight),n.fish=n())  %>%
  right_join(temp)
nrow(temp)

# and select where there have been at least 5 fish measured
temp <- temp %>% filter(n.fish>4)
nrow(temp)

temp <- temp %>% mutate(catch.weight = case_when(
  is.na(catch.weight) | catch.weight == 0 | catch.weight < sum.fish.weight ~ sum.fish.weight,
  TRUE ~ catch.weight
))

#--------------
# calculate the stratum weighting (sample weighting is redone within the bootstrap)
# this is stratum weight / sum of the catch weight in that stratum
# needs to be done after catch.weight is fixed

temp <- temp %>% distinct(fishing.event.key,.keep_all=T) %>% 
  group_by(myear, stratum) %>% 
  summarise(stratum.sum.catch.weight=Sum(catch.weight)) %>%
  right_join(temp)

temp <- temp %>% mutate(stratum.weighting = stratum.weight/stratum.sum.catch.weight*1000)
temp$stratum.weighting[is.na(temp$stratum.weighting) | temp$stratum.weighting<1] <- 1
nrow(temp)


###############
# do the scaled LF
###############

# values
n.boot = 100
min.length=10
max.length=150
bin.length=1


#-------------
# select the variables and run by year and area
temp <- dplyr::select(temp, c(myear,stratum,fishing.event.key,length,sex,stratum.weighting,catch.weight,fish.weight,n.fish)) %>%
  group_by(myear,stratum)          # do it by year and stratum

summary(temp)
nrow(temp)




#-----------
# resample set and fish with replacement but no weighting
# then apply the weighting 


#-----------------------
# original - 7 minutes per bootstrap (!) (sample weighting not the issue)

res<-list()

for (i in 1:n.boot) { # loop for the bootstrap
  print(paste("Length frequency bootstrap",i,"of",n.boot,sep=" "))
  
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

#-------------------------
# slower with all the loops (>25 min)

res<-list()
temp <- ungroup(temp)

for (i in 1:n.boot) { # loop for the bootstrap
  print(paste("Length frequency bootstrap",i,"of",n.boot,sep=" "))
  dat <- data.frame()
  for (i.year in unique(temp$myear)) {
    temp2 <- temp %>% filter(myear %in% i.year)
    for (i.stratum in unique(temp2$stratum)) {
      temp3 <- temp2 %>% filter(stratum %in% i.stratum)
      my.trips <- sample(unique(temp3$fishing.event.key))
      for (i.trip in my.trips) {
        dat <- temp3 %>% filter(fishing.event.key %in% i.trip) %>%
          slice_sample(prop=1,replace=T) %>%
          mutate(sample.no = j) %>% # to be able to calculate weighting by sample number
          bind_rows(dat)
      }
    }
  }
  # calculate sample weighting
  # dat <- dat %>% group_by(sample.no) %>%
  #   summarise(sum.fish.weight=Sum(fish.weight))  %>%
  #   right_join(dat) %>%
  #   mutate(sample.weighting = catch.weight/sum.fish.weight)
  # dat$sample.weighting[dat$sample.weighting < 1] <- 1
  
  res[[i]] <- dat
}  
  


#-------------------
# try with rbind rather than bind_rows, no change
# try lapply (from online) ~ 2 min

temp <- temp %>% group_by(myear,stratum)          # do it by year and stratum

res<-list()

for (i in 1:n.boot) { # loop for the bootstrap
  print(paste("Length frequency bootstrap",i,"of",n.boot,sep=" "))
  
  temp2 <- temp %>% dplyr::select(myear,stratum,fishing.event.key) %>%   # get the unique sets
    distinct() %>%
    slice_sample(prop=1,replace=T) %>%        # sample the unique sets
    pluck("fishing.event.key")

  samps <- do.call(rbind, lapply(temp2, function(x) {slice_sample(temp[temp$fishing.event.key %in% x,], prop=1, replace=TRUE)}))
  
  samps <- samps %>% group_by(fishing.event.key) %>%
    summarise(sum.fish.weight=Sum(fish.weight), n.boot.fish = n()) %>%
    right_join (samps)
  
  # calculate sample weighting including multiple tows
  samps <- samps %>% mutate(sample.weighting = catch.weight/sum.fish.weight*n.boot.fish/n.fish)
  samps$sample.weighting[samps$sample.weighting < 1] <- 1
  
  res[[i]] <- samps
}



#-------------------
# try using map ~ 2.4 min per bootstrap


temp <- temp %>% group_by(myear,stratum)          # do it by year and stratum

res<-list()

for (i in 1:n.boot) { # loop for the bootstrap
  print(paste("Length frequency bootstrap",i,"of",n.boot,sep=" "))
  
  samps <- temp %>% dplyr::select(myear,stratum,fishing.event.key) %>%   # get the unique sets
    distinct() %>%
    slice_sample(prop=1,replace=T) %>%        # sample the unique sets
    pluck("fishing.event.key") %>%
    map_dfr(function(x) {
      slice_sample(filter(temp,fishing.event.key %in% x), prop=1, replace=TRUE)  
    })
  
  samps <- samps %>% group_by(fishing.event.key) %>%
    summarise(sum.fish.weight=Sum(fish.weight), n.boot.fish = n()) %>%
    right_join (samps)
  
  # calculate sample weighting including multiple tows
  samps <- samps %>% mutate(sample.weighting = catch.weight/sum.fish.weight*n.boot.fish/n.fish)
  samps$sample.weighting[samps$sample.weighting < 1] <- 1
  
  res[[i]] <- samps
}




lf.boot.lin.DW <- res
save(res,file=make.filename("LIN LF\\lf.boot.lin.DW.lgth.rdata",DIR.LIN$RData))
load(make.filename("LIN LF\\lf.boot.lin.DW.lgth.rdata",DIR.LIN$RData))  



####################
# plot etc
####################


#####################
# now create LFs and cv by year and area
# sexed here
# need to remove the unsexed as might be not present sometimes
#####################

# Do that for all of the replicates
dat <- res %>% map(function(.x) {
  .x %>% 
    mutate(lgth.bin =  case_when(
      length < min.length ~ min.length,
      length > max.length ~ max.length,
      TRUE ~ floor(length/bin.length)*bin.length
    )) %>% 
    mutate(lgth.bin = factor(lgth.bin,levels=seq(from=min.length,to=max.length,by=bin.length))) %>%
    dplyr::filter(!sex %in% "U") %>%
    nest(data=!c(myear,stratum,sex)) %>% 
    mutate(LF = map(data,function(x) {
      x %>% group_by(lgth.bin,.drop=F) %>% summarise(n = Sum(stratum.weighting*sample.weighting)) %>%
        mutate(freq = n / sum(n)) 
    }
    ))
})

# create standardised LFs and sd - not by stratum here
# add stratum in the nest if needed

aa <- map(dat,function(x) {x %>% dplyr::select(!data)}) %>%  # remove data tibble
  map(function(x) {x %>% unnest(cols=c(LF))}) %>%           # unnest the LFs
  bind_rows(.id="column_label")  %>%
  nest(data=!c(myear,sex)) %>%                            # and them unbuild them again
  mutate(stdLF = map(data,function(x) {
    x %>% group_by(lgth.bin,.drop=F) %>% 
      summarise(n=sum(n),lf.mean=mean(freq),lf.sd=sd(freq)) 
  }))

aa <- aa %>% dplyr::select(!data)

#lf.std.hak<- aa



##################
# now plot them
##################

#aa<- lf.std.hak
aa <- aa %>% bind_rows(.id="column_label") %>% unnest(cols=c(stdLF))
aa <- ungroup(aa) 
aa <- aa %>% mutate(se = lf.sd / sqrt(n))
#labels <- c(chat = "Chathams", suba = "Subantarctic",wcsi = "WCSI")


# group by 5cm and plot differently

bb <- aa %>% mutate(lgth.bin5 = floor(as.numeric(as.character(lgth.bin))/5)*5) %>%
  group_by(myear,sex,lgth.bin5) %>% 
  summarise(lf.mean=Sum(lf.mean))

new.graph(0.8)
ggplot(data=filter(bb,myear %in% 2011:2020),
       aes(x=as.numeric(lgth.bin5),y=lf.mean,fill=sex,col=sex)) +
  geom_ribbon(aes(ymin=0,ymax=lf.mean),alpha=0.2)  +
  facet_grid(rows=vars(myear)) +
  scale_y_continuous(name="Proportion of Fish",expand=expansion(mult=c(0,0.05)),n.breaks=3) +
  scale_x_continuous(name="Total Length (cm)") +
  theme_gray() + scale_colour_manual(values=c("brown","blue"))  


####################
#check with the catch at age software
####################

PATH<-make.filename("Data",DIR.LIN$CAA1,T)

LIN.len<-list()
for(i in 1991:2020) {
  if(file.exists(make.filename(paste0("LIN",i,"_DWlgth.lfs"),PATH))) {
    LIN.len[[as.character(i)]]<-input.length(paste0("LIN",i,"_DWlgth.lfs"),1,paste0("LIN LFs: ",i),i,3,"DW length strata alpha 0.19")
  }
}
names(LIN.len)

temp <- LIN.len
temp2 <- data.frame()
for (i in 1:length(temp)) {
  temp[[i]]$length$fyear <- temp[[i]]$fyear
  temp2 <- bind_rows(temp2,temp[[i]]$length)
}

temp <- temp2 %>% relocate(fyear) %>%
  dplyr::select(!ends_with(c("cv")))

temp <- temp %>%
  pivot_longer(cols=male.1:total.4, names_to = c("sex","stratum"), 
               names_sep="\\.", values_to = "LF")
temp <- temp %>% mutate(LF=coalesce(LF,0))

# and make in 5cm bins
temp3 <- temp %>% mutate(lgth.bin5 = floor(length/5)*5) %>%
  group_by(fyear,stratum,sex,lgth.bin5) %>% 
  summarise(LF=Sum(LF))

temp3 <- temp3 %>% ungroup() %>%
  group_by(fyear,stratum) %>%
  mutate(LF.prop=LF/Sum(LF))

new.graph()
ggplot(filter(temp3,fyear>2009,sex != "total",stratum %in% "pooled"),aes(x=lgth.bin5,y=LF.prop,fill=sex,col=sex)) +
  geom_ribbon(aes(ymin=0,ymax=LF.prop),alpha=0.2)  +
  facet_grid(rows=vars(fyear)) +
  scale_y_continuous(name="Proportion of Fish",expand=expansion(mult=c(0,0.05)),n.breaks=3) +
  scale_x_continuous(name="Total Length (cm)",limits=c(40,115)) +  scale_colour_manual(values=c("brown","blue"))  + scale_fill_manual(values=c("brown","blue"))  + theme_gray()


bb <- bb %>% mutate(model="SM")
bb <- bb %>% ungroup() %>%
  group_by(myear) %>%
  mutate(LF.prop=lf.mean/Sum(lf.mean))

temp <- temp3 %>% ungroup() %>% filter(stratum %in% "pooled", !sex %in% "total") %>%
  rename(myear = fyear,lf.mean=LF) %>% 
  mutate(sex = case_when(
    sex %in% "female" ~ "F",
    sex %in% "male" ~ "M"
  ), model="CaA") %>%
  dplyr::select(myear,sex,lgth.bin5,lf.mean,LF.prop,model) %>%
  bind_rows(bb)
  
new.graph()
ggplot(data=filter(temp,myear %in% 2010:2020),
       aes(x=as.numeric(lgth.bin5),y=LF.prop,fill=model,col=model)) +
  geom_ribbon(aes(ymin=0,ymax=LF.prop),alpha=0.2)  +
  facet_grid(rows=vars(myear),cols=vars(sex)) +
  scale_y_continuous(name="Proportion of Fish",expand=expansion(mult=c(0,0.05)),n.breaks=3) +
  scale_x_continuous(name="Total Length (cm)") +
  theme_gray() + scale_colour_manual(values=c("brown","blue"))  
SavePlot("Catch at age (DW strata)\\NIWA vs 100 boot sm",DIR.LIN$Figures)


