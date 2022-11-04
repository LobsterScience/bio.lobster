#catchability with temp

#from McLeese and Wilder 1958 monthly from Oct to April St Croix
temp = c(3.6,2.8,4.0,6.5,9.8,11.5)
catchability = c(7,5.1,2.6,16.6,55.7,58.8) #n per 100 TH
cS1 = rescale0_1(catchability)

#mcleese and wilder walking rate ft/min
temp3 = c(1.94,5.01,7.97,10,15,20,22,25)
walk = c(7.1,9.5,13.75,16.4,17.7,16.45,21.9,23.5)

#palheimo 1963
temp2 = c(3.90,3.39,3.77,6.63,8.18,7.58,9.36,11.29,11.56)
ca2 = c(0.12,0.46,0.87,2.94,3.03,3.56,5.01,5.03,4.47)
cS2 = rescale0_1(ca2)
