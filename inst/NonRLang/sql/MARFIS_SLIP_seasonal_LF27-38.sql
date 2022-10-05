select season, 
round(SUM(decode(lfa, '33', wt_lbs/2.2046/1000)),0) LFA33,
round(SUM(decode(lfa, '34', wt_lbs/2.2046/1000)),0) LFA34,
round(SUM(decode(lfa, '35', wt_lbs/2.2046/1000)),0) LFA35,
round(SUM(decode(lfa, '36', wt_lbs/2.2046/1000)),0) LFA36,
round(SUM(decode(lfa, '38', wt_lbs/2.2046/1000)),0) LFA38
from (
select lfa, 
case 
    when date_landed between '2002-01-01' and '2002-10-31'
    then '2001/2002M'
    when date_landed between '2002-11-01' and '2003-10-31'
    then '2002/2003'
    when date_landed between '2003-11-01' and '2004-10-31'
    then '2003/2004'
    when date_landed between '2004-11-01' and '2005-10-31'
    then '2004/2005'
    when date_landed between '2005-11-01' and '2006-10-31'
    then '2005/2006'
    when date_landed between '2006-11-01' and '2007-10-31'
    then '2006/2007'
    when date_landed between '2007-11-01' and '2008-10-31'
    then '2007/2008'
    when date_landed between '2008-11-01' and '2009-10-31'
    then '2008/2009'
    when date_landed between '2009-11-01' and '2010-10-31'
    then '2009/2010'
    when date_landed between '2010-11-01' and '2011-10-31'
    then '2010/2011'
    when date_landed between '2011-11-01' and '2012-10-31'
    then '2011/2012'
    when date_landed between '2012-11-01' and '2013-10-31'
    then '2012/2013'
    when date_landed between '2013-11-01' and '2014-10-31'
    then '2013/2014'
    when date_landed between '2014-11-01' and '2015-10-31'
    then '2014/2015'
    when date_landed between '2015-11-01' and '2016-10-31'
    then '2015/2016'
    when date_landed between '2016-11-01' and '2017-10-31'
    then '2016/2017'
    when date_landed between '2017-11-01' and '2018-10-31'
    then '2017/2018'
    when date_landed between '2018-11-01' and '2019-10-31'
    then '2018/2019'
    when date_landed between '2019-11-01' and '2020-10-31'
    then '2019/2020'
    when date_landed between '2020-11-01' and '2021-10-31'
    then '2020/2021'
    when date_landed between '2021-11-01' and '2022-10-31'
    then '2021/2022'    
    else 'premarfis'
    end SEASON,
sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_SD_SLIP
where lfa in ('33', '34', '36', '38')
and species_code = 700
group by date_landed, lfa
union all
select lfa, 
case 
    when date_landed between '2002-01-01' and '2002-09-30'
    then '2001/2002M'
    when date_landed between '2002-10-01' and '2003-09-30'
    then '2002/2003'
    when date_landed between '2003-10-01' and '2004-09-30'
    then '2003/2004'
    when date_landed between '2004-10-01' and '2005-09-30'
    then '2004/2005'
    when date_landed between '2005-10-01' and '2006-09-30'
    then '2005/2006'
    when date_landed between '2006-10-01' and '2007-09-30'
    then '2006/2007'
    when date_landed between '2007-10-01' and '2008-09-30'
    then '2007/2008'
    when date_landed between '2008-10-01' and '2009-09-30'
    then '2008/2009'
    when date_landed between '2009-10-01' and '2010-09-30'
    then '2009/2010'
    when date_landed between '2010-10-01' and '2011-09-30'
    then '2010/2011'
    when date_landed between '2011-10-01' and '2012-09-30'
    then '2011/2012'
    when date_landed between '2012-10-01' and '2013-09-30'
    then '2012/2013'
    when date_landed between '2013-10-01' and '2014-09-30'
    then '2013/2014'
    when date_landed between '2014-10-01' and '2015-09-30'
    then '2014/2015'
    when date_landed between '2015-10-01' and '2016-09-30'
    then '2015/2016'
    when date_landed between '2016-10-01' and '2017-09-30'
    then '2016/2017'
    when date_landed between '2017-10-01' and '2018-09-30'
    then '2017/2018'
    when date_landed between '2018-10-01' and '2019-09-30'
    then '2018/2019'
    when date_landed between '2019-10-01' and '2020-09-30'
    then '2019/2020'
    when date_landed between '2020-10-01' and '2021-09-30'
    then '2020/2021'
    when date_landed between '2021-10-01' and '2022-09-30'
    then '2021/2022'
    
    else 'premarfis'
    end SEASON,
sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_SD_SLIP
where lfa in ('35')
and species_code = 700
group by date_landed, lfa
)
group by season
order by season