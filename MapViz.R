library(ggmap)
library(zipcode)
apiKey = "AIzaSyDA6TG2dM03244Vyd0WChghpatJvhP00Wk"
register_google(apiKey)


map<-get_map(location = "USA", zoom = 4)
data(zipcode)


medianPrice.state<-  aggregate(data_noOutliers$price,
                              by = list(data_noOutliers$state),
                         FUN = median)
names(medianPrice.state) <- c('geo','MedianPrice')

countPrices.state <- aggregate(data_noOutliers$price,
                         by = list(data_noOutliers$state),
                         FUN = length)

names(countPrices.state) <- c('geo','Count')
priceAggregates.state <- merge(medianPrice.state,countPrices.state,by.x="geo", by.y="geo")



medianlat<- aggregate(zipcode$latitude,
                      by = list(zipcode$state),
                      FUN = median)
names(medianlat) <- c('geo','latitude')

medianlng<- aggregate(zipcode$longitude,
                      by = list(zipcode$state),
                      FUN = median)
names(medianlng) <- c('geo','longitude')

medianlatlng <- merge(medianlat,medianlng,by.x="geo", by.y="geo")
  

priceAggregates.state <- merge(priceAggregates.state,medianlatlng,by.x="geo", by.y="geo")
  
ggmap(map)+ geom_point(
  aes(x=longitude, y=latitude, colour=MedianPrice, size=Count), 
  data=priceAggregates.state, alpha=.75, na.rm = T)  +   
  scale_color_gradient(low="blue", high="red")+
  scale_size_continuous(range = c(6,10))+ggtitle("Price and Count of listings by State")



## by zipcode



medianPrice.Zip <-  aggregate(data_noOutliers$price,
                         by = list(data_noOutliers$zip),
                         FUN = median)
names(medianPrice.Zip) <- c('geo','MedianPrice')

countPrices.Zip <- aggregate(data_noOutliers$price,
                         by = list(data_noOutliers$zip),
                         FUN = length)

names(countPrices.Zip) <- c('geo','Count')
priceAggregates.Zip <- merge(medianPrice.Zip,countPrices.Zip,by.x="geo", by.y="geo")

priceAggregates.Zip <- merge(priceAggregates.Zip,zipcode,by.x="geo", by.y="zip")

ggmap(map)+ geom_point(
  aes(x=longitude, y=latitude, colour=MedianPrice, size=Count), 
  data=priceAggregates.Zip, alpha=.75, na.rm = T)  + 
  scale_color_gradient(low="blue", high="red")+
  scale_size_continuous(range = c(2,8))+ggtitle("Price and Count of listings by Zipcode")



