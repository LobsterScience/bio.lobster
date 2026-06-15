#at sea by grid groups
gr = readRDS('C:/Users/cooka/Documents/git/bio.lobster.data/mapping_data/GridGroupings_DepthPruned_37Split.rds')
gr$Label = paste(gr$LFA,gr$GridGroup,sep="-")
lobster.db('atSea')

a = subset(atSea,LFA %in% c(33,34,35) & CARLENGTH>= 82 & SEX !=3 & is.na(VNOTCH) & DESCRIPTION %ni% c('EA-MINAS','Maturity Sampling','Data not collected by trap','Petitcodiac Sampling','Tagging','Out of Season',"Black Point Ocean Disposal exp",'Industry Sample')
           & SPECIESCODE==2550 & STARTDATE>as.Date('2017-1-1') ,           select=c(TRIPNO,STARTDATE,LFA,LICENCE_ID,TRAPNO,STRINGNO,DEPTH, SOAKDAYS, LONGITUDE,LATITUDE, SPECIESCODE, SPECIES, SEX, SHELL, CARLENGTH, CONDITION,CALWT, VNOTCH)) 
a$YR=year(a$STARTDATE)
a$mn = lubridate::month(a$STARTDATE)
a$SYEAR = ifelse(a$mn %in% c(10,11,12),a$YR+1, a$YR)
a = st_as_sf(subset(a,!is.na(LONGITUDE)),coords=c('LONGITUDE','LATITUDE'),crs=4326)

a = st_join(a,st_make_valid(gr), join=st_within)

a$Legal = ifelse(a$CARLENGTH>82,1,0)
a$Legal = ifelse(a$CARLENGTH==82,.5,a$Legal)
a = subset(a,!is.na(Legal))
a$LegalWt = a$Legal*a$CALWT


a34 = aggregate(cbind(Legal,LegalWt)~LFA.y+GridGroup,subset(a,LFA.y==34 & !is.na(GridGroup) & Legal>0 & SYEAR>2017 ),FUN=sum)
a34_j = aggregate(cbind(Legal,LegalWt)~LFA.y+GridGroup,subset(a,CARLENGTH>=140&LFA.y==34 & !is.na(GridGroup) & Legal>0 & YR>2017),FUN=sum)
names(a34_j)[3:4] = paste(names(a34_j)[3:4],'j',sep="_")
a34 = merge(a34,a34_j)
a34$propN = round(a34$Legal_j/a34$Legal*100,1)
a34$propW = round(a34$LegalWt_j/a34$LegalWt*100,1)
a34$n_Legal = round(a34$Legal)
Final34 = subset(a34,select=c(LFA.y, GridGroup, propN, propW,n_Legal))
g34 = ggplot(subset(gr,LFA==34))+geom_sf()+ geom_sf_text(data=st_point_on_surface(subset(gr,LFA==34)),aes(label=Label),size=3)+theme_test_adam()

a34 = aggregate(cbind(Legal,LegalWt)~LFA.y+GridGroup,subset(a,LFA.x==33 & !is.na(GridGroup) & Legal>0 & YR>2017),FUN=sum)
a34_j = aggregate(cbind(Legal,LegalWt)~LFA.y+GridGroup,subset(a,CARLENGTH>=140&LFA.y==33 & !is.na(GridGroup) & Legal>0 & YR>2017),FUN=sum)
names(a34_j)[3:4] = paste(names(a34_j)[3:4],'j',sep="_")
a34 = merge(a34,a34_j)
a34$propN = round(a34$Legal_j/a34$Legal*100,1)
a34$propW = round(a34$LegalWt_j/a34$LegalWt*100,1)
a34$n_Legal = round(a34$Legal)
Final33 = subset(a34,select=c(LFA.y, GridGroup, propN, propW,n_Legal))
g33 = ggplot(subset(gr,LFA==33))+geom_sf()+ geom_sf_text(data=st_point_on_surface(subset(gr,LFA==33)),aes(label=Label),size=3)+theme_test_adam()


a34 = aggregate(cbind(Legal,LegalWt)~LFA.y,subset(a,LFA.y==35 & !is.na(GridGroup) & Legal>0 & YR>2017),FUN=sum)
a34_j = aggregate(cbind(Legal,LegalWt)~LFA.y,subset(a,CARLENGTH>=140&LFA.y==35 & !is.na(GridGroup) & Legal>0 & YR>2017),FUN=sum)
names(a34_j)[2:3] = paste(names(a34_j)[2:3],'j',sep="_")
a34 = merge(a34,a34_j)
a34$propN = round(a34$Legal_j/a34$Legal*100,2)
a34$propW = round(a34$LegalWt_j/a34$LegalWt*100,2)
a34$n_Legal = round(a34$Legal)
Final35 = subset(a34,select=c(LFA.y, propN, propW,n_Legal))

g35 = ggLobsterMap('35',addLFALabels = T)


library(flextable)
library(officer)

###table 
make_ft <- function(df) {
  flextable(df) |> 
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 9, part = "all") |>
  bold(part = "header") |>  
  align(align = "center", part = "all") |>
  autofit()
}

##LFA setionc
add_section <- function(doc, df, plot, lfa, tablenum, fignum) {
  doc <- body_add_fpar(doc,
                       fpar(
                         ftext(paste0("Table ", tablenum, ": "), prop = fp_text(bold = TRUE,font.family = "Calibri", font.size = 9)),
                         ftext(paste0("LFA ", lfa, "; Proportion of Catch by Number or Weight for Legal Lobsters >140mm CL; 2018-2025"),prop = fp_text(font.family = "Calibri", font.size = 9))
                       ))
  
  doc <- body_add_flextable(doc, make_ft(df))
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_gg(doc, value = plot, width = 4, height = 4, res = 300, style = "centered")
  
  doc <- body_add_fpar(doc,
                       fpar(
                         ftext(paste0("Figure ", fignum, ": "), prop = fp_text(bold = TRUE,font.family = "Calibri", font.size = 9)),
                         ftext(paste0("Grid Groupings for LFA ", lfa), prop = fp_text(font.family = "Calibri", font.size = 9))
                       ))
  
  body_add_break(doc)
}




names(Final33) = names(Final34) =  c('LFA','Grid Grouping','Prop. Num','Prop. Wgt','n Legal')
names(Final35) = c('LFA','Prop Num','Prop Wgt','n Legal')

doc <- read_docx()
doc <- body_add_par(doc,
                    "Spatial Patterns in Jumbo (>140mm) Lobster Catch (2018–2025)",
                    style = "heading 1"
        )
doc <- body_add_par(doc,
                    "Adam Cook, PESD",
                    style = "Normal"
          )

doc <- body_add_fpar(
  doc,
  fpar(
    ftext("", prop = fp_text()),
    fp_p = fp_par(line_spacing = 2)  # adds vertical space
  )
)

doc <- body_add_par(doc,
                  "This report summarizes spatial variation in the proportion of legal lobster catch (>140 mm CL) across LFAs 33–35. Data are aggregated by grid groupings to assess regional differences in catch composition. All data are derived from at-sea sampling conducted between 2018 and 2025. Results from LFAs 36 and 38 are not presented, as comprehensive at-sea sampling programs have not been conducted in recent years, and available data may not accurately reflect current patterns.
                  ",
                    style = "Normal"
)



doc <- add_section(doc, df=Final33,lfa=33, plot=g33, tablenum = 1, fignum = 1)
doc <- add_section(doc, df=Final34,lfa=34, plot=g34, tablenum = 2, fignum = 2)
doc <- add_section(doc, df=Final35,lfa=35, plot=g35, tablenum = 3, fignum = 3)
print(doc,target = 'ff3.docx')
