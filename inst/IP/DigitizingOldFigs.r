#capturing old digitized data

require(digitize) ##there is no angle correction on this if the file is rotated you get wrong values 

ca = ReadAndCal('~/Documents/Adam/Lobster/LFA38/Jan2020/CampbellandDugganTotalEffortTotalTraps.png') #click xlow, xhigh, ylow, yhigh and calibratin is now ca
dp = DigitData(col = 'red') #right click when done

#Landings first
dF = Calibrate(dp,ca,1877,1979, 0, 800)

names(dF) = c('Year','TLand')
dF$Year = round(dF$Year)

#Traps
ca1 = ReadAndCal('~/Documents/Adam/Lobster/LFA38/Jan2020/CampbellandDugganTotalEffortTotalTraps.png') #click xlow, xhigh, ylow, yhigh and calibratin is now ca
dp1 = DigitData(col = 'red') #right click when done

dF1 = Calibrate(dp1,ca1,1877,1979, 0, 50000)

names(dF1) = c('Year','Traps')
dF1$Year = round(dF1$Year)

#Licenses
ca2 = ReadAndCal('~/Documents/Adam/Lobster/LFA38/Jan2020/CampbellandDugganTotalEffortTotalTraps.png') #click xlow, xhigh, ylow, yhigh and calibratin is now ca
dp2 = DigitData(col = 'red') #right click when done

dF2 = Calibrate(dp2,ca2,1877,1979, 0, 300) #calibrating numbers, 
names(dF2) = c('Year','Licenses')

dF2$Year = round(dF2$Year)


LFA38Data = merge(merge(dF,dF1,all=T),dF2,all=T)

write.csv(LFA38Data,file='~/Documents/Adam/Lobster/LFA38/Jan2020/CampbellandDugganFigureData.csv')



ca = ReadAndCal('C:/users/COoka/Downloads/Palheimo1963.png') #click xlow, xhigh, ylow, yhigh and calibratin is now ca
dp = DigitData(col = 'red') #right click when done
Calibrate(dp,ca,0,12,0,5)


ca = ReadAndCal('C:/users/COoka/Downloads/walking.png') #click xlow, xhigh, ylow, yhigh and calibratin is now ca
dp = DigitData(col = 'red') #right click when done
Calibrate(dp,ca,0,25,0,25)
