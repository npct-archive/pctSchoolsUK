s11 = readr::read_tsv(file = "private_data/Spring_Census_2011.txt")
lookup01to11 = readr::read_csv("private_data/lower_layer_super_output_areas_(2001)_to_lower_layer_super_output_areas_(2011)_to_local_authority_districts_(2011)_e+w_lookup/LSOA01_LSOA11_LAD11_EW_LU.csv")

# Despite the '11' in the column name the school flows have 2001 LSOA codes. Relabel the column appropriately
names(s11)[names(s11) == "LLSOA_SPR11"] = "LSOA01CD"

nrow(s11)
s11new = dplyr::inner_join(s11, lookup01to11[, c("LSOA01CD","LSOA11CD","CHGIND")], by=c("LSOA01CD"="LSOA01CD"))
nrow(s11new)


table(s11new$CHGIND)
# U = Unchanged LSOA from 2001 --> 2011
# S = LSOA split into two or more LSOAs 2001 --> 2011
# M = LSOAs merged
# X = Complex change of LSOAs

# THE FLOWS IN COMPLEX LSOA CHANGES IS SO LOW WE CAN DROP THEM
# s11new_u = s11new[s11new$CHGIND=="U", ] #unchanged 2001 --> 2011 LSOAs
# s11new_s = s11new[s11new$CHGIND=="S", ] #split
# s11new_m = s11new[s11new$CHGIND=="M", ] #merged
# s11new_x = s11new[s11new$CHGIND=="X", ] #complex change
# 
# sum(s11new_u$TOTAL)/sum(s11new$TOTAL)
# sum(s11new_s$TOTAL)/sum(s11new$TOTAL)
# sum(s11new_m$TOTAL)/sum(s11new$TOTAL)
# sum(s11new_x$TOTAL)/sum(s11new$TOTAL)

library(dplyr)
s11new_m = s11new %>% filter(CHGIND=="M") %>%
  group_by_(.dots=c("LSOA11CD","URN_SPR11")) %>%
  summarise(TOTAL=sum(TOTAL), CAR=sum(CAR), CYCLE=sum(CYCLE), WALK=sum(WALK), OTHER=sum(OTHER), UNKNOWN=sum(UNKNOWN))

# The inner join duplicates rows where there are multiple matches between the tables
#  e.g. see duplicate rows below. So we can just take the first one
# Otherwise the same school gets assigned to multiple 2011 LSOAs when a single 2001 LSOA is split (these rows are duplicated in the join)
# Therefore we just take the first one
# s11new %>% filter(CHGIND=="S") %>% group_by_(.dots=c("LSOA11CD","URN_SPR11"))

# randomEntry = function(entries){
#   ind = runif(1,1,length(entries))
#   return(entries[ind])
# }

# s11new_s = s11new  %>% filter(CHGIND=="s") %>%
#   group_by_(.dots=c("LSOA11CD","URN_SPR11")) %>%
#   summarise(TOTAL=first(TOTAL), CAR=first(CAR), CYCLE=first(CYCLE), WALK=first(WALK), OTHER=first(OTHER), UNKNOWN=first(UNKNOWN))


integerSplit = function(intgr, parts){
  if(intgr==0){
    partition = rep(0,parts)
  }
  if(intgr==1){
    #print(intgr)
    #print(parts)
    partition = c(1, rep(0, parts-1))
    partition = sample(partition, replace = F, size=length(partition))
  }else if((intgr!=1) & (parts==1)){
    partition = intgr
  }else if((intgr !=1) & (parts>1)){
    partitions = partitions::restrictedparts(intgr, parts, decreasing=T)
    partition = partitions[,ncol(partitions)]
  }
  partition = as.integer(partition)
  #if(!is.integer(partition))
  #  print(paste0(intgr, parts, partition))
  if(length(partition) > parts)
    print(paste(partition, parts))
  #return(data.frame(partition))
  return(as.numeric(partition))
}

#integerSplit = Vectorize(integerSplit)

# http://stat545.com/block023_dplyr-do.html
s2 = s11new_s %>% group_by_(.dots=c("LSOA11CD","URN_SPR11")) %>% do(TOTAL=integerSplit(.$TOTAL,length(.$TOTAL)), CAR=integerSplit(.$CAR,length(.$CAR)), CYCLE=integerSplit(.$CYCLE,length(.$CYCLE)), WALK=integerSplit(.$WALK,length(.$WALK)), OTHER=integerSplit(.$OTHER,length(.$OTHER)), UNKNOWN=integerSplit(.$UNKNOWN,length(.$UNKNOWN)))
#s11new_s %>% group_by_(.dots=c("LSOA11CD","URN_SPR11")) %>% do(integerSplit(.$TOTAL,length(.$TOTAL)), integerSplit(.$CAR,length(.$CAR)), integerSplit(.$CYCLE,length(.$CYCLE)), integerSplit(.$WALK,length(.$WALK)), integerSplit(.$OTHER,length(.$OTHER)), integerSplit(.$UNKNOWN,length(.$UNKNOWN)))


s11new_s = s11new  %>% filter(CHGIND=="S") %>%
  group_by_(.dots=c("LSOA11CD","URN_SPR11")) %>%
  summarise(TOTAL=floor(first(TOTAL)/n()), CAR=floor(first(CAR)/n()), CYCLE=floor(first(CYCLE)/n()), WALK=floor(first(WALK)/n()), OTHER=floor(first(OTHER)/n()), UNKNOWN=floor(first(UNKNOWN)/n()))



s11new_u = s11new %>% filter(CHGIND=="U") %>% select_(.dots=names(s11new_s)) #keep same columns as other subsets for rbind

s11new2 = bind_rows(s11new_u, s11new_m, s11new_s)

# check for duplicates in result
isTRUE(nrow(s11new2) == nrow(s11new2[!duplicated(s11new2), ]))

cents_lsoa = readRDS("private_data/cents_lsoa_2011.Rds")

# cents_lsoa has proper 2011 LSOAs check we now match those after 2001 --> 2011 conversion
summary(s11new2$LSOA11CD %in% cents_lsoa$LSOA)
summary(s11$LSOA01CD %in% cents_lsoa$LSOA)

isTRUE(sum(s11$TOTAL) == sum(s11new2$TOTAL))
sum(s11[!is.na(s11$LSOA01CD),]$TOTAL)
sum(s11new2[!is.na(s11new2$LSOA11CD),]$TOTAL)
(sum(s11$TOTAL)-sum(s11new2$TOTAL))/sum(s11$TOTAL)


# are the LSOAs we dropped in the conversion responsible for the drop in flows? No
sum(s11[s11[!is.na(s11$LSOA01CD),]$LSOA01CD %in% lookup01to11[lookup01to11$CHGIND=="X", "LSOA01CD"], ]$TOTAL)

# the 2011 LSOAs in the centroids file and in the look-up table match up nicely
summary((cents_lsoa$LSOA %in% lookup01to11$LSOA11CD))

# here is where we lost a lot of flows: 2001 LSOAs from flows that are not in the lookup table
summary((s11$LSOA01CD %in% lookup01to11$LSOA01CD))


table(s11[!(s11$LSOA01CD %in% lookup01to11$LSOA01CD), ]$LSOA01CD)
sum(s11[!(s11$LSOA01CD %in% lookup01to11$LSOA01CD), ]$TOTAL)
sum(s11$TOTAL)-sum(s11new2$TOTAL)





# library(partitions)
# #sudo apt-get install libgmp3-dev
# install.packages(c("gmp","partitions"))
# 
# partitions = partitions::restrictedparts(5,3, decreasing=T)
# partitions[,ncol(partitions)]
# 
# s11new_s %>% group_by_(.dots=c("LSOA01CD","URN_SPR11")) %>% do(TOTAL=integerSplit(first(.$TOTAL),nrow(.$TOTAL)))
# s11new_s %>% group_by_(.dots=c("LSOA01CD","URN_SPR11")) %>% do(TOTAL=partitions::restrictedparts(first(.$TOTAL), tally(.$TOTAL), decreasing=T)[,1])
# s11new_s %>% group_by_(.dots=c("LSOA11CD","LSOA11CD","URN_SPR11")) %>% mutate(TOTAL=n())
# sum(duplicated(s11new_m$URN_SPR11))/nrow(s11new_m)
