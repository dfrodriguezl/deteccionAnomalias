--------------------------------------------------------
-- Archivo creado  - martes-febrero-23-2021   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package TESIS_MCIC_GEO
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE "HERMES"."TESIS_MCIC_GEO" AS 

  /* TODO enter package declarations (types, exceptions, methods etc) here */ 

  FUNCTION volumeMBB
  (
    minim IN spt_pos,
    maxim IN spt_pos
  )
    RETURN NUMBER;

  FUNCTION OVvolumeMBB
  (
    minim1 IN spt_pos,
    maxim1 IN spt_pos,
    minim2 IN spt_pos,
    maxim2 IN spt_pos
  )
    RETURN NUMBER;

  FUNCTION SIM
  (
    OV NUMBER,
    V NUMBER
  )
    RETURN NUMBER;

  FUNCTION SDIST
  (
    SIML NUMBER,
    minim1 IN spt_pos,
    maxim1 IN spt_pos,
    minim2 IN spt_pos,
    maxim2 IN spt_pos
  )
    RETURN NUMBER;
    
    FUNCTION Construct_Segments 
  (
    CP spt_pos_nt
  )
    RETURN line_segment_nt;

  FUNCTION Segments2spt_pos
  (
    lines line_segment_nt
  )
    RETURN spt_pos_nt;

  FUNCTION Calculate_Neps
  (
    D IN OUT NOCOPY line_segment_nt,
    pos NUMBER,
    eps NUMBER
  )
    RETURN INTEGER_NT;

  FUNCTION IsCore
  (
    Neps INTEGER_NT,
    MinTrs NUMBER
  )
    RETURN BOOLEAN;

  FUNCTION AssignCluster
  (
    Neps INTEGER_NT,
    D IN OUT NOCOPY line_segment_nt,
    cluster_id INTEGER
  )
    RETURN line_segment_nt;

  FUNCTION Main_
  (
    D IN OUT NOCOPY line_segment_nt,
    Eps NUMBER,
    MinTrs INTEGER
  )
    RETURN internal_cluster_nt;

  FUNCTION lines_cp
  (
    D line_segment_nt
  )
    RETURN line_segment_nt;
    
  FUNCTION join_segments
  (
    table_name VARCHAR2
  )
    RETURN line_segment_nt;
    
   FUNCTION extract_points_clusters
  (
    O internal_cluster_nt
  )
    RETURN line_segment_nt;

  FUNCTION segments_containing_x
  (
    x IN NUMBER,
    segments line_segment_nt
  )
    RETURN line_segment_nt;

  FUNCTION EXTRACT_RTR
    (
      O INTERNAL_CLUSTER,
      MinTrs NUMBER
    )
    RETURN SPT_POS_NT;

  FUNCTION create_direction_vector
  (
    O INTERNAL_CLUSTER
  )
    RETURN sp_pos_nt;
    
    FUNCTION angle
    (
        s1 IN sp_pos,
    e1 IN sp_pos,
    s2 IN sp_pos,
    e2 IN sp_pos
    )
        RETURN NUMBER;
        
      FUNCTION angle_xx
    (
        s IN sp_pos,
    e IN sp_pos
    )
        RETURN NUMBER;
        
    FUNCTION sort_collection (
                   input_collection IN spt_pos_nt
                    ) RETURN spt_pos_nt;
                    
    FUNCTION compare_trajectories (a NUMBER) RETURN moving_point;                   

END TESIS_MCIC_GEO;

/
