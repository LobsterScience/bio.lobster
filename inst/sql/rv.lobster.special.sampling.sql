CREATE OR REPLACE FORCE table 
  GSDET_SPECIAL_Lobster_table AS 
  select G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN, 
  max(case when key= 'Spermatophore Presence' then value else NULL END) Sperm_Plug,
   max(case when key= 'Abdominal Width' then value else NULL END) Ab_width,
   max(case when key= 'Egg Stage' then value else NULL END) Egg_St,
      max(case when key= 'Clutch Fullness Rate' then value else NULL END) Clutch_Full
from
  (select mission, setno, spec, size_class, specimen_id, flen, fwt, fsex, fmat, fshno, agmat, remarks, age, clen from groundfish.gsdet) G,
    (select mission, spec, specimen_id, lv1_observation key, data_value value  from groundfish.gs_lv1_observations
   where spec=2550) FC
where 
G.mission = FC.mission (+) and
G.spec = FC.spec (+) and
G.specimen_id = FC.specimen_id (+)
group by G.MISSION,G.SETNO,G.SPEC,G.SIZE_CLASS,G.SPECIMEN_ID,G.FLEN,G.FWT, G.FSEX,  G.CLEN;


select a.mission, a.setno, b.strat, a.spec, a.size_class, a.specimen_id, a.flen, a.fwt, a.fsex, sperm_plug,ab_width,egg_st,clutch_full
from gsdet_special_lobster_table a, groundfish.gsinf b
where a.mission=b.mission and a.setno=b.setno 
group by a.mission, a.setno, b.strat, a.spec, a.size_class, a.specimen_id, a.flen, a.fwt, a.fsex, sperm_plug,ab_width,egg_st,clutch_full
order by a.setno;
