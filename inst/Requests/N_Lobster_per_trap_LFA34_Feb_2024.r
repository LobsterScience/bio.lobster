lobster.db('atSea.redo')
a = atSea
b = subset(a,LFA=="34" & month(STARTDATE) %in% c(11,12) &  SPECIESCODE==2550) 
b$N = 1
i = which(is.na(b$STRINGNO))
b$STRINGNO[i]=1

i = which(is.na(b$PORT))
b$PORT[i]=1
ba = aggregate(N~TRIPNO+PORT+STRINGNO+TRAPNO,data=b,FUN=sum)
ba = subset(ba,N<50)
t = hist(ba$N,breaks=seq(0:50),xlab='N Lobster Per Trap',main="")
tt = data.frame(N_lobster=t$breaks[-1],freq=t$counts)

tt$prop=cumsum(tt$freq)/sum(tt$freq)


a$dy = yday(a$STARTDATE)

b = subset(a,LFA=="34" & dy %in% 325:340 & GRIDNO %in% c(55,
                                                         68,
                                                         69,
                                                         80,
                                                         81,
                                                         91,
                                                         92,
                                                         103,
                                                         114,
                                                         126,
                                                         127,
                                                         141,
                                                         158) & SPECIESCODE==2550)
b$N = 1
i = which(is.na(b$STRINGNO))
b$STRINGNO[i]=1

i = which(is.na(b$PORT))
b$PORT[i]=1
ba = aggregate(N~TRIPNO+PORT+STRINGNO+TRAPNO,data=b,FUN=sum)
ba = subset(ba,N<50)
t = hist(ba$N,breaks=seq(0:50),xlab='N Lobster Per Trap',main="")
tt = data.frame(N_lobster=t$breaks[-1],freq=t$counts)

tt$prop=cumsum(tt$freq)/sum(tt$freq)
