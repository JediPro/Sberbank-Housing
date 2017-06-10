# Set environs ####
train.raw = read.csv("train.csv", header = T, sep = ",")
macro.raw = read.csv("macro.csv", header = T, sep = ",")
test.raw = read.csv("test.csv", header = T, sep = ",")

library(caret)
library(car)
library(mice)
library(VIM)
library(corrplot)
library(ggmap)

Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

dt.all2 = dt.all

dtrain = train.raw
dtest = test.raw
dt.all.describe = as.data.frame.matrix(summary(dt.all))
dt.all.describe = t(dt.all.describe)
train_describe = read.csv('Train_Summary.csv')

bplot = ggplot(data = dtrain)
file.edit('.Rprofile')

# Clean data ####
NA.pct = apply(dtrain, 2, function(x) {round((sum(is.na(x)) / length(x)) * 100, 1)})
NA.pct = NA.pct[NA.pct > 0]
NA.pct = data.frame(NAcount = NA.pct, var = names(NA.pct), row.names = NULL)
ggplot(NA.pct, aes(x = reorder(var, -NAcount), y = NAcount)) +
  geom_bar(stat = 'identity', fill = 'green') + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Impute using location ####
prop_features = c("id", "timestamp", "full_sq", "life_sq", "floor",                                
                  "max_floor", "material", "build_year", "num_room", "kitch_sq",                             
                  "state", "product_type", "sub_area")
dtrain2 = dtrain
dtrain = train.raw
dtrain[, 'train'] = 1

dtest[, "train"] = 0
dtest[, "price_doc"] = 0

dloc.train = read.csv('train_lat_lon.csv')
dloc.test = read.csv('test_lat_lon.csv')

dt.all = rbind(dtrain, dtest)
dloc.all = rbind(dloc.train, dloc.test)

# Impute Property features ####
# Impute max_floor using location
# check histogram
summary(dt.all$max_floor)
ggplot(dt.all) + geom_histogram(aes(x = max_floor), binwidth = 1)

# Put NA in all instances where max floor is LT floor
dt.all$max_floor[dt.all$max_floor < dt.all$floor] = NA
# Put NA where max floor GT 60
dt.all$max_floor[dt.all$max_floor > 60] = NA

for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$max_floor[i])){
    mod = max(Mode(dt.all$max_floor[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$max_floor[i] = mod
  }
}

# Impute material
dt.all$material[dt.all$material == 3] = 1
#use location data
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$material[i])){
    mod = max(Mode(dt.all$material[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$material[i] = mod
  }
}

# build_year
table(dt.all$build_year)
# recode illogical values
dt.all$build_year = recode(dt.all$build_year, "0:100 = NA; 215 = 2015; 1691 = 1961; 
                           4965 = 1965; 20052009 = 2009")

for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$build_year[i])){
    mod = max(Mode(dt.all$build_year[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$build_year[i] = mod
  }
}

# num_room
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$num_room[i])){
    mod = max(Mode(dt.all$num_room[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$num_room[i] = mod
  }
}

# kitch_sq
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$kitch_sq[i])){
    mod = max(Mode(dt.all$kitch_sq[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$kitch_sq[i] = mod
  }
}

# state
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$state[i])){
    mod = max(Mode(dt.all$state[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$state[i] = mod
  }
}


# life_sq ####
# create frame of living space date
LivingSpace = dt.all[, names(dt.all) %in% c('full_sq', 'life_sq')]
# Replace low values of full_sq by existing values of life sq
LivingSpace$full2 = LivingSpace$full_sq
LivingSpace$full2[LivingSpace$full_sq < 10 & !is.na(LivingSpace$life_sq)] = 
  LivingSpace$life_sq[LivingSpace$full_sq < 10 & !is.na(LivingSpace$life_sq)]
# find ratios
LivingSpace$ratio = LivingSpace$life_sq/LivingSpace$full2
# Replace values for which ratio is very high by NA
LivingSpace$life2 = LivingSpace$life_sq
LivingSpace$life2[!(LivingSpace$ratio > 0 & LivingSpace$ratio <= 1)] = NA
# recalculate ratios
LivingSpace$ratio = LivingSpace$life2/LivingSpace$full2
# plot ratio distribution
ggplot(LivingSpace, aes(x= ratio))+ geom_density()
# fiind mean of space ratio
SpaceRatio_mean = mean(LivingSpace$ratio, na.rm = T)
# replace in life2
LivingSpace$life2[is.na(LivingSpace$life2)]=LivingSpace$full2[is.na(LivingSpace$life2)]*SpaceRatio_mean
# replace in original
dt.all$life_sq = LivingSpace$life2
dt.all$full_sq = LivingSpace$full2

# floor ####
# replace using locations
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$floor[i])){
    mod = max(Mode(dt.all$floor[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$floor[i] = mod
  }
}

# num_room ####
# Num_room based on ratio of full_sq
Rooms = dt.all[,names(dt.all) %in% c('full_sq', 'num_room')]
Rooms$ratio = Rooms$num_room/Rooms$full_sq
#replace 0's with NA
Rooms$num_room[Rooms$num_room == 0] = NA
# recalculate ratio
Rooms$ratio = Rooms$num_room/Rooms$full_sq
# recode Num-room
Rooms$room = as.factor(recode(Rooms$num_room, "1=1; 2=2; 3=3; NA=NA; else='GT3'"))
# Plot charts for density of num rooms vs full_sq
ggplot(Rooms, aes(x = log(full_sq), col = as.factor(room))) + geom_density(size = 1.5) + 
  scale_color_brewer(palette = "Set1")
# find mean and sd to decide on boundaries
aggregate(Rooms$full_sq, by = list(Rooms$room), FUN = mean)
aggregate(Rooms$full_sq, by = list(Rooms$room), FUN = sd)
# recode values based on full_sq
Rooms$room2 = as.factor(ifelse(!is.na(Rooms$room), Rooms$room, 
                     ifelse(Rooms$full_sq < 47, 1, 
                            ifelse(Rooms$full_sq < 65, 2,
                                   ifelse(Rooms$full_sq < 90, 3, 'GT3')))))
Rooms$room2 = recode(Rooms$room2, "4 = 'GT3'")
# add back to original
dt.all$num_room = Rooms$room2

# Kitch_sq ####
Kitch = dt.all[,names(dt.all) %in% c('full_sq', 'kitch_sq')]
Kitch$ratio = Kitch$kitch_sq/Kitch$full_sq
ggplot(Kitch, aes((ratio))) + geom_density()
# replace value with ratio greater than 75 by NA
Kitch$kitch2 = Kitch$kitch_sq
Kitch$kitch2[Kitch$ratio > 0.75] = NA
Kitch$kitch2[Kitch$kitch_sq == 1] = NA
# recalculate ratio
Kitch$ratio = Kitch$kitch2/Kitch$full_sq
# find mean of ratio
KitchRatio_mean = mean(Kitch$ratio, na.rm = T)
# fill NAs using mean
LivingSpace$life2[is.na(LivingSpace$life2)]=LivingSpace$full2[is.na(LivingSpace$life2)]*SpaceRatio_mean
Kitch$kitch2[is.na(Kitch$kitch2)] = Kitch$full_sq[is.na(Kitch$kitch2)] * KitchRatio_mean
# load back to df
dt.all$kitch_sq = Kitch$kitch2

# material ####
# plot use of material over time
ggplot(dt.all, aes(x = build_year, colour = as.factor(material))) + geom_density(size = 1.5) + 
  scale_color_brewer(palette = 'Set1')
# plot size wrt material
ggplot(dt.all, aes(x = log(full_sq), colour = as.factor(material))) + geom_density(size = 1.5) + 
  scale_color_brewer(palette = 'Set1')

# plot price over years
price_year = aggregate(dt.all$price_doc, by = list(dt.all$build_year), FUN = mean)
ggplot(dt.all, aes(x = build_year, y = price_doc)) + geom_point() + geom_smooth()
ggplot(dt.all, aes(x = build_year, y = price_doc)) + geom_point() + geom_smooth()

ggplot(dt.all, aes(x = as.factor(material), y = price_doc)) + geom_boxplot() + geom_smooth()

# state ####
dt.all$state[dt.all$state == 33] = 3
ggplot(dt.all, aes(x = build_year, colour = as.factor(state))) + geom_density(size = 1.5) + 
  scale_color_brewer(palette = 'Set1')
# replace state based on year
dt.all$state = ifelse(!is.na(dt.all$state), dt.all$state, 
                      ifelse(dt.all$build_year < 1982, 2,
                             ifelse(dt.all$build_year < 2008, 4, 1)))

# Type ####
dt.all$product_type[is.na(dt.all$product_type)] = 'Investment'

# Build_year
ggplot(dt.all, aes(x = build_year, colour = sub_area)) + geom_density(size = 1.5) + 
  scale_color_brewer(palette = 'Set1')


###
# Impute Raion features ####
# pre_school
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$preschool_quota[i])){
    mod = max(Mode(dt.all$preschool_quota[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$preschool_quota[i] = mod
  }
}

# school quota  
for(i in 1:nrow(dt.all)){
  if(is.na(dt.all$school_quota[i])){
    mod = max(Mode(dt.all$school_quota[dloc.all$lat == dloc.all$lat[i] & dloc.all$lon == dloc.all$lon[i]]))
    dt.all$school_quota[i] = mod
  }
}

# Hospital beds
# create data frame 
hpb = as.data.frame.matrix(table(dt.all$sub_area, dt.all$hospital_beds_raion,useNA = "always"))
# find mean lat and lon of each raion
dloc.all$key = as.character(dloc.all$key)
# loop oveer rows and extract raion
for(i in 1:nrow(dloc.all)){
  dloc.all$raion[i] = substr(dloc.all$key[i], 
                             max(gregexpr(pattern = ':', text = dloc.all$key[i])[[1]]) + 1, 
                             nchar(dloc.all$key[i]))
}
# Aggregate data
dloc.agg = aggregate(dloc.all[,c(3,4,6)], by = list(dloc.all$raion), FUN = mean)

# Plot data on Google Map
gmap = get_map(location = 'Altufyevskoe', maptype = 'roadmap', source = 'google', zoom = 15)
raion_map = ggmap(gmap) + geom_point(aes(x = lon, y = lat), data = dloc.agg)
raion_map

# select rows with missing hospital beds values
hpb_missing_raion_names = row.names(hpb[which(hpb[80] > 0),])
hpb_missing_raion = dloc.all[dloc.all$raion %in% hpb_missing_raion_names,-c(1,5)]

gmap = get_map(location = c(lon = dloc.agg$lon[dloc.agg$Group.1 ==hpb_missing_raion_names[1]],
                            lat = dloc.agg$lat[dloc.agg$Group.1 ==hpb_missing_raion_names[1]]), 
               maptype = 'roadmap', source = 'google', zoom = 14)

ggmap(gmap) + geom_point(data=hpb_missing_raion[hpb_missing_raion$raion == hpb_missing_raion_names[1],],
                         aes(x = lon, y = lat))

Plot_Map = function(x){
     gmap = get_map(location = c(lon = dloc.agg$lon[dloc.agg$Group.1 ==hpb_missing_raion_names[x]],
                                lat = dloc.agg$lat[dloc.agg$Group.1 ==hpb_missing_raion_names[x]]), 
                   maptype = 'roadmap', source = 'google', zoom = 14)
    ggmap(gmap) + geom_point(data=hpb_missing_raion[hpb_missing_raion$raion == 
                                                      hpb_missing_raion_names[x],],
                             aes(x = lon, y = lat)) +
      labs(title = hpb_missing_raion_names[x])
    plotname = paste(hpb_missing_raion_names[x], '.jpg', sep = "")
    ggsave(plotname, device = "jpeg", scale = 2)
}

for(i in 1:length(hpb_missing_raion_names)){
  Plot_Map(i)
}

raion_hpb = aggregate(dt.all$hospital_beds_raion, by = list(dt.all$sub_area),FUN = mean)
raion_hpb2 = aggregate(dt.all$hospital_beds_raion, by = list(dt.all$sub_area),FUN = length)
raion_hpb2 = aggregate(dt.all$raion_popul, by = list(dt.all$sub_area),FUN = mean)

raion_hpb$popul = raion_hpb2$x
write.csv(raion_hpb, 'raion_hpb.csv', row.names = F)

