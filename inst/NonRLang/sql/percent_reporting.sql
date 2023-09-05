select yearmth, sum(LFA27miss) L33miss, sum(lfa27recd) L27recd, sum(lfa27percent) L27percent,
sum(LFA28miss) L28miss, sum(lfa28recd) L28recd, sum(lfa28percent) L28percent,
sum(LFA29miss) L29miss, sum(lfa29recd) L29recd, sum(lfa29percent) L29percent,
sum(LFA30miss) L30miss, sum(lfa30recd) L30recd, sum(lfa30percent) L30percent,
sum(LFA31Amiss) L31Amiss, sum(lfa31Arecd) L31Arecd, sum(lfa31Apercent) L31Apercent,
sum(LFA31Bmiss) L31Bmiss, sum(lfa31Brecd) L31Brecd, sum(lfa31Bpercent) L31Bpercent,
sum(LFA32miss) L32miss, sum(lfa32recd) L32recd, sum(lfa32percent) L32percent,
sum(LFA33miss) L33miss, sum(lfa33recd) L33recd, sum(lfa33percent) L33percent,
sum(LFA34miss) L34miss, sum(lfa34recd) L34recd, sum(lfa34percent) L34percent,
sum(LFA35miss) L35miss, sum(lfa35recd) L35recd, sum(lfa35percent) L35percent,
sum(LFA36miss) L36miss, sum(lfa36recd) L36recd, sum(lfa36percent) L36percent,
sum(LFA38miss) L38miss, sum(lfa38recd) L38recd, sum(lfa38percent) L38percent
from (
select yearmth, 
min(decode(lfa, '27', miss)) LFA27miss,
min(decode(lfa, '27', recd)) LFA27recd,
min(decode(lfa, '27', percent)) LFA27percent,
min(decode(lfa, '28', miss)) LFA28miss,
min(decode(lfa, '28', recd)) LFA28recd,
min(decode(lfa, '28', percent)) LFA28percent,
min(decode(lfa, '29', miss)) LFA29miss,
min(decode(lfa, '29', recd)) LFA29recd,
min(decode(lfa, '29', percent)) LFA29percent,
min(decode(lfa, '30', miss)) LFA30miss,
min(decode(lfa, '30', recd)) LFA30recd,
min(decode(lfa, '30', percent)) LFA30percent,
min(decode(lfa, '31A', miss)) LFA31Amiss,
min(decode(lfa, '31A', recd)) LFA31Arecd,
min(decode(lfa, '31A', percent)) LFA31Apercent,
min(decode(lfa, '31B', miss)) LFA31Bmiss,
min(decode(lfa, '31B', recd)) LFA31Brecd,
min(decode(lfa, '31B', percent)) LFA31Bpercent,
min(decode(lfa, '32', miss)) LFA32Amiss,
min(decode(lfa, '32', recd)) LFA32Arecd,
min(decode(lfa, '32', percent)) LFA32percent,
min(decode(lfa, '33', miss)) LFA33miss,
min(decode(lfa, '33', recd)) LFA33recd,
min(decode(lfa, '33', percent)) LFA33percent,
min(decode(lfa, '34', miss)) LFA34miss,
min(decode(lfa, '34', recd)) LFA34recd,
min(decode(lfa, '34', percent)) LFA34percent,
min(decode(lfa, '35', miss)) LFA35miss,
min(decode(lfa, '35', recd)) LFA35recd,
min(decode(lfa, '35', percent)) LFA35percent,
min(decode(lfa, '36', miss)) LFA36miss,
min(decode(lfa, '36', recd)) LFA36recd,
min(decode(lfa, '36', percent)) LFA36percent,
min(decode(lfa, '38', miss)) LFA38miss,
min(decode(lfa, '38', recd)) LFA38recd,
min(decode(lfa, '38', percent)) LFA38percent

from (


select missing.yearmth, missing.lfa, missing.miss, recd.recd, round(missing.miss/sum(missing.miss+recd.recd)*100,0) percent
from 
(select 
case 
when area_id = 278 then 27
when area_id = 279 then 28
when area_id = 280 then 29
when area_id = 281 then 30
when area_id = 293 then '31A'
when area_id = 294 then '31B'
when area_id = 282 then 32   
when area_id = 283 then 33
when area_id = 284 then 34
when area_id = 285 then 35
when area_id = 286 then 36
when area_id = 288 then 38
else null
end LFA,
area_id, yearmth, count(distinct licence_id) recd from (
SELECT LA.AREA_ID, TO_CHAR (SD.END_DATE, 'YYYYMM') YEARMTH, L.LICENCE_ID
  FROM MARFISSCI.SUM_DOCS SD, MARFISSCI.LICENCES L, MARFISSCI.LICENCE_AREAS LA
WHERE     L.LICENCE_ID = SD.LICENCE_ID
       AND L.SPECIES_CODE = 700
       AND L.LICENCE_ID = LA.LICENCE_ID
       AND SD.START_DATE BETWEEN LA.START_DATE AND LA.END_DATE
) where yearmth > 202112
and area_id in (283,284,285,286,288)
group by area_id, yearmth) recd,

(select  
case 
when area_id = 278 then 27
when area_id = 279 then 28
when area_id = 280 then 29
when area_id = 281 then 30
when area_id = 293 then '31A'
when area_id = 294 then '31B'
when area_id = 282 then 32   
when area_id = 283 then 33
when area_id = 284 then 34
when area_id = 285 then 35
when area_id = 286 then 36
when area_id = 288 then 38
else null
end LFA, yearmth, count(distinct licence_id) miss from (
SELECT V.YEARMTH,
       lp.fin,
       L.LICENCE_ID,
       A.AREA_ID
  FROM marfissci.LICENCES              L,
       marfissci.LICENCE_PARTICIPANTS  LP,
       marfissci.LICENCE_VESSELS       LV,
       marfissci.LICENCE_AREAS         LA,
       marfissci.AREAS                 A,
       marfis.LOBSTER_AREA_SD_MTHS  V,
       marfis.MARFIS_VARIABLES      MV
WHERE     L.SECTOR_ID = 7
       AND L.SPECIES_CODE = 700
       AND V.AREA_ID = A.AREA_ID
       AND L.LICENCE_ID = LP.LICENCE_ID
       AND (   (V.YEARMTHDD BETWEEN TRUNC (LP.START_DATE)
                                AND TRUNC (LP.END_DATE))
            OR V.YEARMTHDD_EOMS BETWEEN TRUNC (LP.START_DATE)
                                    AND TRUNC (LP.END_DATE))
       AND L.LICENCE_ID = LV.LICENCE_ID
       AND (   (V.YEARMTHDD BETWEEN TRUNC (LV.START_DATE)
                                AND TRUNC (LV.END_DATE))
            OR V.YEARMTHDD_EOMS BETWEEN TRUNC (LV.START_DATE)
                                    AND TRUNC (LV.END_DATE))
       AND L.LICENCE_ID = LA.LICENCE_ID
       AND V.YEARMTHDD BETWEEN LA.START_DATE AND LA.END_DATE
       AND LA.AREA_ID = A.AREA_ID
       AND SYSDATE < l.EXPIRY_DATE
       --forget the licences that are no longer owned
       --as per mike eagles aug 12, 2009
       AND LP.FIN <> '700000000'
       AND MV.MARFIS_VKEY = 1
       AND SUBSTR (V.YEARMTH, 1, 4) >= '2014'
       AND l.LICENCE_TYPE_ID <> 9
--and lp.LICENCE_ID = 110320 test
--and v.LICENCE_YEAR = 2009  test
--AND SUBSTR(V.YEARMTH,1,4) > TO_CHAR(MV.CE_LAST_YEAR_ARCHIVED)
MINUS
/*
THE SILENT PARTNERS
*/
SELECT V.YEARMTH,
       lp.fin,
       L.LICENCE_ID,
       a.AREA_ID
  FROM marfissci.LICENCES              L,
       marfissci.LICENCE_PARTICIPANTS  LP,
       marfissci.LICENCE_AREAS         LA,
       marfissci.AREAS                 A,
       marfis.LOBSTER_AREA_SD_MTHS  V,
       marfis.MARFIS_VARIABLES      MV,
       marfis.PARTNERSHIP_LICENCES  PL
WHERE     L.SECTOR_ID = 7
       AND L.SPECIES_CODE = 700
       AND V.AREA_ID = A.AREA_ID
       AND L.LICENCE_ID = LP.LICENCE_ID
       AND (   V.YEARMTHDD BETWEEN TRUNC (LP.START_DATE)
                               AND TRUNC (LP.END_DATE)
            OR V.YEARMTHDD_EOMS BETWEEN TRUNC (LP.START_DATE)
                                    AND TRUNC (LP.END_DATE))
       AND (   V.YEARMTHDD BETWEEN TRUNC (PL.START_DATE)
                               AND TRUNC (PL.END_DATE)
            OR V.YEARMTHDD_EOMS BETWEEN TRUNC (PL.START_DATE)
                                    AND TRUNC (PL.END_DATE))
       AND L.LICENCE_ID = PL.LICENCE_ID
       AND PL.PARTNER_TYPE = 'S'
       AND L.LICENCE_ID = LA.LICENCE_ID
       AND V.YEARMTHDD BETWEEN LA.START_DATE AND LA.END_DATE
       AND LA.AREA_ID = A.AREA_ID
       AND MV.MARFIS_VKEY = 1
       AND SUBSTR (V.YEARMTH, 1, 4) >= '2014'
       --AND SUBSTR(V.YEARMTH,1,4) > TO_CHAR(MV.CE_LAST_YEAR_ARCHIVED)
       AND l.LICENCE_TYPE_ID IN (1, 11)
MINUS
/*
THE DOCS REC'D
*/
SELECT TO_CHAR (SD.END_DATE, 'YYYYMM'),
       lp.fin,
       L.LICENCE_ID,
       LA.AREA_ID
  FROM marfissci.SUM_DOCS              SD,
       marfissci.LICENCES              L,
       marfissci.LICENCE_AREAS         LA,
       marfissci.LICENCE_PARTICIPANTS  LP
WHERE     L.LICENCE_ID = SD.LICENCE_ID
       AND L.SPECIES_CODE = 700
       AND L.LICENCE_TYPE_ID IN (1, 11)
       AND L.LICENCE_ID = LA.LICENCE_ID
       AND SD.END_DATE BETWEEN LA.START_DATE AND LA.END_DATE
       AND L.LICENCE_ID = LP.LICENCE_ID
       AND SD.END_DATE BETWEEN LP.START_DATE AND LP.END_DATE
MINUS
/*
THE DOCS from out of region - aug 2009
currently 0 from sf but maybe someday...
*/
SELECT TO_CHAR (SD.END_DATE, 'YYYYMM'),
       sd.fin,
       L.LICENCE_ID,
       sd.AREA_ID
  FROM marfissci.SUM_DOCS_out_of_region SD, marfissci.LICENCES L
WHERE     L.LICENCE_ID = SD.LICENCE_ID
       AND L.SPECIES_CODE = 700
       AND l.SECTOR_ID = 7
       AND L.LICENCE_TYPE_ID IN (1, 11)
       ) where yearmth > 202112
       and area_id in (278,279,280,281,293,294,282,283,284,285,286,288)
       group by area_id, yearmth
       order by area_id, yearmth) missing
where missing.yearmth = recd.yearmth
and missing.lfa = recd.lfa
group by missing.yearmth, missing.lfa, missing.miss, recd.recd
order by missing.lfa, missing.yearmth
)
group by yearmth)
group by yearmth
order by yearmth