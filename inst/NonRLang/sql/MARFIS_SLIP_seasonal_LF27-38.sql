select season, 
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
round(SUM(decode(lfa, '38', wt_lbs/2.2046/1000)),0) LFA38
from (
select lfa, 
case 
    when date_landed between '2002-01-01' and '2002-04-30'
    then '2001/2002M'
    when date_landed between '2002-05-01' and '2003-04-30'
    then '2002/2003'
    when date_landed between '2003-05-01' and '2004-04-30'
    then '2003/2004'
    when date_landed between '2004-05-01' and '2005-04-30'
    then '2004/2005'
    when date_landed between '2005-05-01' and '2006-04-30'
    then '2005/2006'
    when date_landed between '2006-05-01' and '2007-04-30'
    then '2006/2007'
    when date_landed between '2007-05-01' and '2008-04-30'
    then '2007/2008'
    when date_landed between '2008-05-01' and '2009-04-30'
    then '2008/2009'
    when date_landed between '2009-05-01' and '2010-04-30'
    then '2009/2010'
    when date_landed between '2010-05-01' and '2011-04-30'
    then '2010/2011'
    when date_landed between '2011-05-01' and '2012-04-30'
    then '2011/2012'
    when date_landed between '2012-05-01' and '2013-04-30'
    then '2012/2013'
    when date_landed between '2013-05-01' and '2014-04-30'
    then '2013/2014'
    when date_landed between '2014-05-01' and '2015-04-30'
    then '2014/2015'
    when date_landed between '2015-05-01' and '2016-04-30'
    then '2015/2016'
    when date_landed between '2016-05-01' and '2017-04-30'
    then '2016/2017'
    when date_landed between '2017-05-01' and '2018-04-30'
    then '2017/2018'
    when date_landed between '2018-05-01' and '2019-04-30'
    then '2018/2019'
    when date_landed between '2019-05-01' and '2020-04-30'
    then '2019/2020'
    else 'premarfis'
    end SEASON,
  sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_SD_SLIP
where lfa in ('27','28','29','30')
and species_code = 700
group by date_landed, lfa
union all
select to_char(lfa),
case when to_char(yr) = 2002 then '2002/2003'
     when to_char(yr) = 2003 then '2003/2004'
     when to_char(yr) = 2004 then '2004/2005'
     when to_char(yr) = 2005 then '2005/2006'
     when to_char(yr) = 2006 then '2006/2007'
     when to_char(yr) = 2007 then '2007/2008'
     when to_char(yr) = 2008 then '2008/2009'
     when to_char(yr) = 2009 then '2009/2010'
     when to_char(yr) = 2010 then '2010/2011'
     when to_char(yr) = 2011 then '2011/2012'
     when to_char(yr) = 2012 then '2012/2013'
     when to_char(yr) = 2013 then '2013/2014'
     when to_char(yr) = 2014 then '2014/2015'
     when to_char(yr) = 2015 then '2015/2016'
     when to_char(yr) = 2016 then '2016/2017'
     when to_char(yr) = 2017 then '2017/2018'
     when to_char(yr) = 2018 then '2018/2019'
     when to_char(yr) = 2019 then '2019/2020'
end SEASON,
mt*2.2046*1000 wt_lbs
from FRAILC.GULF_LAND
where yr > 2001
union all
select lfa, 
case 
    when date_landed between '2002-01-01' and '2002-03-31'
    then '2001/2002M'
    when date_landed between '2002-04-01' and '2003-03-31'
    then '2002/2003'
    when date_landed between '2003-04-01' and '2004-03-31'
    then '2003/2004'
    when date_landed between '2004-04-01' and '2005-03-31'
    then '2004/2005'
    when date_landed between '2005-04-01' and '2006-03-31'
    then '2005/2006'
    when date_landed between '2006-04-01' and '2007-03-31'
    then '2006/2007'
    when date_landed between '2007-04-01' and '2008-03-31'
    then '2007/2008'
    when date_landed between '2008-04-01' and '2009-03-31'
    then '2008/2009'
    when date_landed between '2009-04-01' and '2010-03-31'
    then '2009/2010'
    when date_landed between '2010-04-01' and '2011-03-31'
    then '2010/2011'
    when date_landed between '2011-04-01' and '2012-03-31'
    then '2011/2012'
    when date_landed between '2012-04-01' and '2013-03-31'
    then '2012/2013'
    when date_landed between '2013-04-01' and '2014-03-31'
    then '2013/2014'
    when date_landed between '2014-04-01' and '2015-03-31'
    then '2014/2015'
    when date_landed between '2015-04-01' and '2016-03-31'
    then '2015/2016'
    when date_landed between '2016-04-01' and '2017-03-31'
    then '2016/2017'	
    when date_landed between '2017-04-01' and '2018-03-31'
    then '2017/2018'
    when date_landed between '2018-04-01' and '2019-03-31'
    then '2018/2019'	
    when date_landed between '2019-04-01' and '2020-03-31'
    then '2019/2020'
    else 'premarfis'
    end SEASON,
sum(slip_weight_lbs) wt_lbs
from MARFISSCI.LOBSTER_SD_SLIP
where lfa in ('31A', '31B', '32')
and species_code = 700
group by date_landed, lfa
union all
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