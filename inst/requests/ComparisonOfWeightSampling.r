con = odbcConnect(oracle.server , uid=oracle.username, pwd=oracle.password, believeNRows=F) # believeNRows=F required for oracle db's

a = sqlQuery(con,"select * from groundfish.gsdet where spec=2550 and fwt is not null and substr(mission,4,4) in ('2014');")

sex = c(1,2,3) 
for(j in sex) {
				g = subset(a,FSEX==j)
				g = g[order(g$FLEN),]
				plot(g$FLEN,g$FWT)

				b = nls(FWT~a*FLEN^b,data=g,start=list(a=0.001,b=3))
				summary(b)

				pw = predict(b)

				g = cbind(g,pw)

				lines(g$FLEN,g$pw,col='red',lwd=1)

				g$rfl = floor(g$FLEN/3)*3+1


				g$ID = paste(g$MISSION,g$SETNO,g$rfl,sep="-")

				gI = unique(g$ID)

				out = NULL
				for(i in gI){
					u = subset(g,ID==i)
					out = rbind(out,u[sample(1:nrow(u),1),])

				}
				out = out[order(out$FLEN),]
				h = nls(FWT~a*FLEN^b,data=out,start=list(a=0.001,b=3))
				summary(h)

				pw = predict(h)
				print(length(predict(h))-length(predict(b)))
				out = cbind(out,pw)
				points(out$FLEN,out$FWT, col='blue')
				lines(out$FLEN,out$pw,col='blue',lwd=1)
				title(j)


				g$rfl = floor(g$FLEN/5)*5+2


				g$ID = paste(g$MISSION,g$SETNO,g$rfl,sep="-")

				gI = unique(g$ID)

				out = NULL
				for(i in gI){
					u = subset(g,ID==i)
					out = rbind(out,u[sample(1:nrow(u),1),])

				}
				out = out[order(out$FLEN),]
				w = nls(FWT~a*FLEN^b,data=out,start=list(a=0.001,b=3))
				summary(w)

				pw = predict(w)
				print(paste('sex = ',j,'sample one per 5 diff',length(predict(w)) - length(predict(b))),sep=" ")
				out = cbind(out,pw)
				points(out$FLEN,out$FWT, col='green')
				lines(out$FLEN,out$pw,col='green',lwd=1)
				title(j)
		legend('topleft',lty=c(1,1,1),col=c('red','blue','green'),c('Full Data Set','1 per 3','1 per 5'))
				x11()



			}