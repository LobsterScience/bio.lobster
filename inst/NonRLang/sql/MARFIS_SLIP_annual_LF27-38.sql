select yr, 
round(SUM(decode(lfa, '27', wt_lbs/2.2046/1000)),0) LFA27,
round(SUM(decode(lfa, '28', wt_lbs/2.2046/1000)),0) LFA28,
round(SUM(decode(lfa, '29', wt_lbs/2.2046/1000)),0) LFA29,
round(SUM(decode(lfa, '30', wt_lbs/2.2046/1000)),0) LFA30,
round(SUM(decode(lfa, '31A', wt_lbs/2.2046/1000)),0) LFA31A,
round(SUM(decode(lfa, '31B', wt_lbs/2.2046/1000)),0) LFA31B,
round(SUM(decode(lfa, '32', wt_lbs/2.2046/1000)),0) LFA32,
round(SUM(decode(lfa, '33', wt_lbs/2.2046/1000)),0) LFA33,
round(SUM(decode(lfa, '34', wt_lbs/2.2046/1000)),0) LFA34,
round(SUM(decode(lfa, '35', wt_lbs/2.2046/1000)),0) LFA35,
round(SUM(decode(lfa, '36', wt_lbs/2.2046/1000)),0) LFA36,
round(SUM(decode(lfa, '38', wt_lbs/2.2046/1000)),0) LFA38,
round(SUM(decode(lfa, '41', wt_lbs/2.2046/1000)),0) LFA41
from (
select to_char(date_landed, 'YYYY') yr, lfa, sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_SD_SLIP
where lfa in ('27','28','29','30','31A','31B','32', '33', '34', '35', '36', '38')
and species_code = 700
group by to_char(date_landed, 'YYYY'), lfa
union all
select to_char(yr), to_char(lfa), mt*2.2046*1000 wt_lbs
from frailc.gulf_land
where yr > 2001
union all
select to_char(landing_date_time, 'YYYY') yr, '41' lfa, sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_MD_SLIP
where licence_id in (141926,141929,141930)
group by to_char(landing_date_time, 'YYYY')
) GROUP BY yr
ORDER BY yr