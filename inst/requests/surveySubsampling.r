       require(RODBC)
        con = odbcConnect(oracle.server , uid=oracle.personal.user, pwd=oracle.personal.password, believeNRows=F) # believeNRows=F required for oracle db's

bas = sqlQuery(con,paste('select * from ILTS_2015_baskets'))
bas = subset(bas,SPECCD_ID==2550)
s = aggregate(BASKET~SET_NO,data=bas,FUN= function(x) length(x))
s = subset(s,BASKET>200,select=SET_NO)[,1] #no point at looking at less than 200 lobsters

pdf('All_lobsters.pdf')
for(i in s) {
    j = subset(bas,SET_NO==i)
    p = hist(j$FISH_LENGTH,seq(0,205,1),plot=F)
    p$cum = cumsum(p$density)
    with(p,plot(mids,cum,type = 'l',lwd=3,main=paste('setno = ',i)))
    b = unique(j$BASKET)
    e = NA
    print(b)
    for(o in 2 : length(b)) {
            n = numeric()
            for(l in 1:5) {
                    m = sample(b,o)
                    z = subset(j, BASKET %in% m, select = FISH_LENGTH)[,1]
                    n[l] = length(z)
                    p = hist(z,seq(0,205,1),plot=F)
                    p$cum = cumsum(p$density)
                    with(p,lines(mids,cum,type = 'l',col=o))
                    if(l == 5) e[o] = mean(n)
            }
        }
        legend('topleft',col=c(2,3,4,5,6,7,8),lwd=rep(1,8),c(paste('2',e[2],sep='-'),paste('3',e[3],sep='-'),paste('4',e[4],sep='-'),paste('5',e[5],sep='-'),paste('6',e[6],sep='-'),paste('7',e[7],sep='-'),paste('8',e[8],sep='-')),title='N Baskets Sampled')
     }

dev.off()

pdf('males_only.pdf')
for(i in s) {
    j = subset(bas,SET_NO==i & SEX == 1)
    p = hist(j$FISH_LENGTH,seq(0,205,1),plot=F)
    p$cum = cumsum(p$density)
    with(p,plot(mids,cum,type = 'l',lwd=3,main=paste('setno = ',i,'Males')))
    b = unique(j$BASKET)
    e = NA
    print(b)
    for(o in 2 : length(b)) {
            n = numeric()
            for(l in 1:5) {
                    m = sample(b,o)
                    z = subset(j, BASKET %in% m, select = FISH_LENGTH)[,1]
                    n[l] = length(z)
                    p = hist(z,seq(0,205,1),plot=F)
                    p$cum = cumsum(p$density)
                    with(p,lines(mids,cum,type = 'l',col=o))
                    if(l == 5) e[o] = mean(n)
            }
        }
        legend('topleft',col=c(2,3,4,5,6,7,8),lwd=rep(1,8),c(paste('2',e[2],sep='-'),paste('3',e[3],sep='-'),paste('4',e[4],sep='-'),paste('5',e[5],sep='-'),paste('6',e[6],sep='-'),paste('7',e[7],sep='-'),paste('8',e[8],sep='-')),title='N Baskets Sampled')

    }
dev.off()



pdf('females_only.pdf')
for(i in s) {
    j = subset(bas,SET_NO==i & SEX == 2)
    p = hist(j$FISH_LENGTH,seq(0,205,1),plot=F)
    p$cum = cumsum(p$density)
    with(p,plot(mids,cum,type = 'l',lwd=3,main=paste('setno = ',i,'Males')))
    b = unique(j$BASKET)
    e = NA
    print(b)
    for(o in 2 : length(b)) {
            n = numeric()
            for(l in 1:5) {
                    m = sample(b,o)
                    z = subset(j, BASKET %in% m, select = FISH_LENGTH)[,1]
                    n[l] = length(z)
                    p = hist(z,seq(0,205,1),plot=F)
                    p$cum = cumsum(p$density)
                    with(p,lines(mids,cum,type = 'l',col=o))
                    if(l == 5) e[o] = mean(n)
            }
        }
        legend('topleft',col=c(2,3,4,5,6,7,8),lwd=rep(1,8),c(paste('2',e[2],sep='-'),paste('3',e[3],sep='-'),paste('4',e[4],sep='-'),paste('5',e[5],sep='-'),paste('6',e[6],sep='-'),paste('7',e[7],sep='-'),paste('8',e[8],sep='-')),title='N Baskets Sampled')

    }
dev.off()
