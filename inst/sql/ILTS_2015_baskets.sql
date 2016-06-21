create table ILTS_2015_baskets as (
select tripno, 
tripcd_id,
case when tripcd_id = 7051 then 'ITQ'
     when tripcd_id = 7065 then 'ILTS'
     else null
     end survey_type,	
board_date, 
landing_date, 
vessel, 
fishset_id,
set_no, 
station, 
fish_id, 
speccd_id,
common, 
fish_length, 
sex, 
round(calc_wt_g,3) calc_wt_g,
sum(set_lat) set_lat, 
sum(set_lon) set_lon, 
sum(set_depth) set_depth, 
sum(set_time) set_time, 
min(set_date) set_date,
sum(haul_lat) haul_lat, 
sum(haul_lon) haul_lon, 
sum(haul_depth) haul_depth, 
sum(haul_time) haul_time, 
min(haul_date) haul_date,
abdominal_width, 
mating_status, 
egg, 
clutch, 
vnotch, 
shell, 
damage, 
shell_disease, 
cull,
basket
from (
select  a.trip_id tripno, 
        a.tripcd_id,
        e.fish_id,
        a.board_date, 
        a.landing_date, 
        g.vessel_name vessel, 
        b.fishset_id,
        b.set_no,
        b.station,
        c.speccd_id, 
        h.common,
        d.fish_length,
        d.sexcd_id sex, 
case when d.sexcd_id = 1 and c.speccd_id = 2550
     then power(d.fish_length,3.0583)*0.000608
     when d.sexcd_id = 2 and c.speccd_id = 2550
     then power(d.fish_length,2.8746)*0.001413
     when d.sexcd_id = 3 and c.speccd_id = 2550
     then power(d.fish_length,2.638)*0.00482
     else null
     end calc_wt_g,
case when f.pntcd_id = 2 then f.latitude
end set_lat,
case when f.pntcd_id = 2 then f.longitude
end set_lon,
case when f.pntcd_id = 2 then f.depth
end set_depth,
case when f.pntcd_id = 2 then f.settime
end set_time,
case when f.pntcd_id = 2 then f.setdate
end set_date,
case when f.pntcd_id = 3 then f.latitude
end haul_lat,
case when f.pntcd_id = 3 then f.longitude
end haul_lon,
case when f.pntcd_id = 3 then f.depth
end haul_depth,
case when f.pntcd_id = 3 then f.settime
end haul_time,
case when f.pntcd_id = 3 then f.setdate
end haul_date,
        min(DECODE(e.mrphcd_id,54, e.QUANT_VALUE)) abdominal_width,
        min(DECODE(e.mrphcd_id,136, e.mrphvcd_id)) mating_status,
        min(DECODE(e.mrphcd_id,78, e.mrphvcd_id)) egg,
        min(DECODE(e.mrphcd_id,138, e.mrphvcd_id)) clutch,
        min(DECODE(e.mrphcd_id,76, e.mrphvcd_id)) vnotch,
        min(DECODE(e.mrphcd_id,77, e.mrphvcd_id)) shell,
        min(DECODE(e.mrphcd_id,137, e.mrphvcd_id)) damage,
        min(DECODE(e.mrphcd_id,135, e.mrphvcd_id)) shell_disease,
        min(DECODE(e.mrphcd_id,79, e.mrphvcd_id)) cull,
        min(DECODE(e.mrphcd_id,115, e.QUANT_VALUE)) Basket
from    observer.istrips a, 
        observer.isfishsets b, 
        observer.iscatches c, 
        observer.isfish d, 
        observer.isfishmorphs e, 
        observer.issetprofile f,
        observer.isvessels g, 
        observer.isspeciescodes h
       where   a.trip_id = b.trip_id
        and b.fishset_id = c.fishset_id
        and c.catch_id = d.catch_id
        and d.fish_id = e.fish_id
        and b.fishset_id = f.fishset_id(+)
        and a.vess_id = g.vess_id
        and c.speccd_id = h.speccd_id
        and a.tripcd_id in (7051, 7065)
        and d.fish_length is not null
group by a.trip_id, 
        a.tripcd_id,
        e.fish_id,
        a.board_date, 
        a.landing_date, 
        b.comarea_id, 
        g.vessel_name,
        b.fishset_id,
        b.set_no,
        c.speccd_id, 
        h.common,
        d.fish_length, 
        d.sexcd_id,
        f.latitude,
        f.longitude,
        b.station,
        f.depth,
        f.settime,
        f.setdate,
        f.pntcd_id
)
where basket is not null
group  by tripno, tripcd_id, fish_id, board_date, landing_date, vessel, fishset_id,set_no, station, speccd_id,
common, fish_length, sex, calc_wt_g,
abdominal_width, mating_status, egg, clutch, vnotch, shell, damage, shell_disease, cull,basket);
