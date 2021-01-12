SELECT a.lfa,
  a.yr,
  a.species_code,
  a.common,
  a.kg,
  ROUND(a.kg*b.ratio/1000,2) mt
FROM
  (select lfa, yr, species_code, common, sum(KG) kg from (
 SELECT lfa,
    TO_CHAR(startdate,'yyyy') yr,
    CASE
      WHEN species_code IN (300,301,309,312)
      THEN 309
      ELSE species_code
    END species_code,
    CASE
      WHEN species_code IN (300,301,309,312)
      THEN 'SCULPIN (NS)'
      ELSE common
    END common,
    ROUND(SUM(calwt_g/1000),2) kg
  FROM
    (SELECT TO_CHAR(a.tripno) tripno,
      a.lfa,
      b.startdate,
      a.species_code,
      a.common,
      a.count,
      a.calwt_g
    FROM lobster.crcatches a,
      lobster.crtrips b
    WHERE a.tripno     = b.tripno
    AND b.samcode NOT IN ('05I','09I','12I','14I','15I','17I','22I')
    AND to_char(b.startdate,'yyyy-mm-dd')    > '2011-12-31'
    AND a.LFA         IN ('27','29','30','31A','31B','32','33')
    UNION ALL
    SELECT TO_CHAR(tripno),
      lfa,
      startdate,
      speciescode,
      species common,
      COUNT(carlength) COUNT,
      SUM(calwt) calwt_g
    FROM lobster.lobster_atsea_vw
    WHERE samcode NOT IN ('05I','09I','12I','14I','15I','17I','22I')
    AND to_char(startdate,'yyyy-mm-dd')      > '2011-12-31'
    AND LFA           IN ('27','29','30','31A','31B','32','33')
    GROUP BY tripno,
      lfa,
      startdate,
      speciescode,
      species
    UNION ALL
    SELECT a.trip tripno,
      SUBSTR(a.comarea_id,2,3) lfa,
      a.board_date startdate,
      a.speccd_id species_code,
      b.common,
      COUNT(a.fish_length) COUNT,
      SUM(calwt_g) calwt_g
    FROM cooka.LOBSTER_BYCATCH_ASSOC a,
      isdb.isspeciescodes b
    WHERE a.speccd_id      = b.speccd_id
    AND upper(comarea_id) IN ('L27','L29','L30','L31A','L31B','L32','L33')
    GROUP BY a.trip,
      a.comarea_id,
      a.board_date,
      a.speccd_id,
      b.common
    )
  WHERE species_code IN (10,122,320,300,301,309,312,2511,2513)
  GROUP BY lfa,
    TO_CHAR(startdate,'yyyy'),
    species_code,
    common
  ORDER BY lfa,
    TO_CHAR(startdate,'yyyy'),
    species_code,
    common
    )
    group by lfa, yr, species_code, common
  )a,
  (SELECT lfa,
    yr,
    traps_sampled,
    traps_hauled,
    traps_hauled/traps_sampled ratio
  FROM
    (SELECT lfa,
      yr,
      SUM(traps_sampled) traps_sampled,
      SUM(traps_hauled) traps_hauled
    FROM
      (SELECT lfa,
        yr,
        SUM(traps_sampled) traps_sampled,
        0 traps_hauled
      FROM
        (SELECT lfa,
          yr,
          COUNT(trap_id) traps_sampled
        FROM
          ( SELECT DISTINCT SUBSTR(comarea_id,2,3) lfa,
            TO_CHAR(board_date,'yyyy')yr,
            trip,
            trap_id
          FROM cooka.LOBSTER_BYCATCH_ASSOC
          )
        WHERE LFA IN ('27','29','30','31A','31B','32')
        GROUP BY lfa,
          yr
        UNION ALL
        SELECT lfa,
          yr,
          COUNT(trapno) traps_sampled
        FROM
          ( SELECT DISTINCT lfa,
            TO_CHAR(startdate,'yyyy') yr,
            tripno,
            trapno
          FROM lobster.lobster_atsea_vw
          WHERE samcode NOT IN ('05I','09I','12I','14I','15I','17I','22I')
          AND to_char(startdate,'yyyy-mm-dd')      > '2011-12-31'
          AND LFA           IN ('27','29','30','31A','31B','32')
          )
        GROUP BY lfa,
          yr
        )
      GROUP BY lfa,
        yr
      UNION ALL
      SELECT lfa,
        TO_CHAR(date_fished, 'yyyy') yr,
        0 traps_sampled,
        SUM(NVL(num_of_traps,0)+NVL(num_of_traps_b,0)+NVL(num_of_traps_c,0)) trap_hauled
      FROM marfissci.lobster_sd_log
      WHERE to_char(date_fished,'yyyy-mm-dd') > '2011-12-31'
      AND LFA          IN ('27','29','30','31A','31B','32')
      GROUP BY lfa,
        TO_CHAR(date_fished, 'yyyy'),
        'traps_sampled'
      )
    GROUP BY lfa,
      yr
    )
  WHERE traps_sampled !=0
  ORDER BY lfa,
    yr
  ) b
WHERE a.lfa = b.lfa
AND a.yr    = b.yr
AND a.lfa   = '31B'
order by lfa, yr, species_code
