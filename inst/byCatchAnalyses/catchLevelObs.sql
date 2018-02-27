
#string aggregate catches 
select tripno, startdate, lfa, -99 port, 
'unknown' portname, 'unknown' captain, licence_id, 'ISDB'samcode, 
'ISDB' description,  -99 traptype, stringno, 
case    when start_haul_depth is not null
    then start_haul_depth
    when start_haul_depth is null
    then end_haul_depth
    else end_set_depth
    end depth,
(start_haul_trapdate-end_set_trapdate)+1 soakdays, 
case    when start_haul_lat is not null
    then start_haul_lat
    when start_haul_lat is null
    then end_haul_lat
    else end_set_lat
    end latitude,
case    when start_haul_long is not null
    then start_haul_long*-1
    when start_haul_long is null
    then end_haul_long*-1
    else end_set_long*-1
    end longitude,
-99 gridno,  speciescode, species,  EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT,num_hook_haul     ,comments          
from (
select  a.trip_id tripno, 
        a.board_date startdate, 
        a.landing_date enddate, 
        substr(b.comarea_id,2,3) LFA, 
        a.MARFIS_LICENSE_NO licence_id,
        g.vessel_name vessel, 
        b.set_no stringno,
        min(decode(f.pntcd_id, 2, f.latitude,null)) end_set_lat,
        min(decode(f.pntcd_id, 2, f.longitude,null)) end_set_long,
        min(decode(f.pntcd_id, 3, f.latitude,null)) start_haul_lat,
        min(decode(f.pntcd_id, 3, f.longitude,null)) start_haul_long,
        min(decode(f.pntcd_id, 4, f.latitude,null)) end_haul_lat,
        min(decode(f.pntcd_id, 4, f.longitude,null)) end_haul_long,
        min(decode(f.pntcd_id, 2, f.setdate,null)) end_set_trapdate,
        min(decode(f.pntcd_id, 3, f.setdate,null)) start_haul_trapdate,
        min(decode(f.pntcd_id, 4, f.setdate,null)) end_haul_trapdate,
        min(decode(f.pntcd_id, 2, f.depth,null)) end_set_depth,
        min(decode(f.pntcd_id, 3, f.depth,null)) start_haul_depth,
        min(decode(f.pntcd_id, 4, f.depth,null)) end_haul_depth,
        c.speccd_id speciescode, 
        h.common species,
       EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT,num_hook_haul  , b.comments    
        from    observer.istrips a, 
        observer.isfishsets b, 
        observer.iscatches c, 
        observer.isvessels g, 
        observer.issetprofile f, 
        observer.isspeciescodes h
where   a.trip_id = b.trip_id
        and b.fishset_id = c.fishset_id
        and b.fishset_id = f.fishset_id(+)
        and a.vess_id = g.vess_id
        and c.speccd_id = h.speccd_id
        and a.tripcd_id = 2550
        and b.source=0 
      
group by a.trip_id, 
        a.board_date, 
        a.landing_date, 
        b.comarea_id, 
        a.MARFIS_LICENSE_NO,
        g.vessel_name, 
        b.set_no,
        c.speccd_id, 
        h.common,   EST_NUM_CAUGHT, EST_KEPT_WT, EST_DISCARD_WT,num_hook_haul  ,b.comments               
    
   order by a.trip_id, b.set_no
);
