--------------------------------------------------------
-- Archivo creado  - martes-febrero-23-2021   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for Package Body TESIS_MCIC_GEO
--------------------------------------------------------

  CREATE OR REPLACE PACKAGE BODY "HERMES"."TESIS_MCIC_GEO" 
AS
  -- -----------------------------------------------------
  -- Function Volume(MBB)
  -- -----------------------------------------------------
  FUNCTION volumeMBB(
      minim IN spt_pos,
      maxim IN spt_pos )
    RETURN NUMBER
  IS
    dx NUMBER;
    dy NUMBER;
    dt NUMBER;
  BEGIN
    dx := maxim.x                - minim.x;
    dy := maxim.y                - minim.y;
    dt := maxim.t.get_Abs_Date() - minim.t.get_Abs_Date();
    RETURN dx                    *dy*dt;
  END volumeMBB;
-- -----------------------------------------------------
-- Function OVolume(MBBi,MBBj)
-- -----------------------------------------------------
  FUNCTION OVvolumeMBB(
      minim1 IN spt_pos,
      maxim1 IN spt_pos,
      minim2 IN spt_pos,
      maxim2 IN spt_pos )
    RETURN NUMBER
  IS
    dx NUMBER;
    dy NUMBER;
    dt NUMBER;
  BEGIN
    dx := TBFUNCTIONS.TBMAX(TBFUNCTIONS.TBMin(maxim1.x,maxim2.x)                               - TBFUNCTIONS.TBmax(minim1.x,minim2.x),0);
    dy := TBFUNCTIONS.TBMAX(TBFUNCTIONS.TBMin(maxim1.y,maxim2.y)                               - TBFUNCTIONS.TBmax(minim1.y,minim2.y),0);
    dt := TBFUNCTIONS.TBMAX(TBFUNCTIONS.TBMin(maxim1.t.get_Abs_Date(),maxim2.t.get_Abs_Date()) - TBFUNCTIONS.TBmax(minim1.t.get_Abs_Date(),minim2.t.get_Abs_Date()),0);
    RETURN dx                                                                                  *dy*dt;
  END OVvolumeMBB;
-- -----------------------------------------------------
-- Function SIM(MBBi,MBBj)
-- -----------------------------------------------------
  FUNCTION SIM(
      OV NUMBER,
      V  NUMBER )
    RETURN NUMBER
  IS
    DIV NUMBER;
  BEGIN
    IF V   = 0 THEN
      DIV := 0;
    ELSE
      DIV := OV/V;
    END IF;
    RETURN DIV;
  END SIM;
-- -----------------------------------------------------
-- Function SDIST(sim,trj,tri)
-- -----------------------------------------------------
  FUNCTION SDIST(
      SIML NUMBER,
      minim1 IN spt_pos,
      maxim1 IN spt_pos,
      minim2 IN spt_pos,
      maxim2 IN spt_pos )
    RETURN NUMBER
  IS
    LENGTH1 NUMBER;
    LENGTH2 NUMBER;
    BETA    NUMBER;
    RES     NUMBER;
  BEGIN
    BETA    := SQRT(2)/2;
    LENGTH1 := UTILITIES.distance(minim1.x,minim1.y,maxim1.x,maxim1.y);
    LENGTH2 := UTILITIES.distance(minim2.x,minim2.y,maxim2.x,maxim2.y);
    RES     := (1-SIML)*BETA*(LENGTH1+LENGTH2);
    RETURN RES;
  END SDIST;
-- -----------------------------------------------------
-- Function Construct_Segments (spt_pos_nt)
-- -----------------------------------------------------
  FUNCTION Construct_Segments(
      CP spt_pos_nt )
    RETURN line_segment_nt
  IS
    pos1 NUMBER;
    pos2 NUMBER;
    line line_segment;
    line_set line_segment_nt := line_segment_nt();
    p1 spt_pos;
    p2 spt_pos;
  BEGIN
    pos1       := CP.FIRST;
    pos2       := CP.NEXT(pos1);
    WHILE pos1 IS NOT NULL AND pos2 IS NOT NULL
    LOOP
      p1   := CP(pos1);
      p2   := CP(pos2);
      line := line_segment(p1,p2);
      line_set.EXTEND;
      line_set(line_set.LAST) := line;
      pos1                    := pos2;
      pos2                    := CP.NEXT(pos1);
    END LOOP;
    RETURN line_set;
  END Construct_Segments;
-- -----------------------------------------------------
-- Function Segments2spt_pos (line_segment_nt)
-- -----------------------------------------------------
  FUNCTION Segments2spt_pos(
      lines line_segment_nt )
    RETURN spt_pos_nt
  IS
    start_point spt_pos;
    end_point spt_pos;
    points spt_pos_nt := spt_pos_nt();
    pos1 NUMBER;
  BEGIN
    pos1       := lines.FIRST;
    WHILE pos1 IS NOT NULL
    LOOP
      start_point := lines(pos1).s;
      end_point   := lines(pos1).e;
      --dbms_output.put_line(pos1);
      IF pos1 > 1 THEN
        --dbms_output.put_line(pos1);
        IF start_point IS NOT NULL AND points(points.LAST) IS NOT NULL THEN
          --dbms_output.put_line(start_point.x);
          --dbms_output.put_line(start_point.y);
          --dbms_output.put_line(start_point.t.get_Abs_Date());
          IF start_point.x = points(points.LAST).x AND start_point.y = points(points.LAST).y AND start_point.t.get_Abs_Date() = points(points.LAST).t.get_Abs_Date() THEN
            dbms_output.put_line('');
          ELSE
            points.EXTEND;
            points(points.LAST) := start_point;
          END IF;
        END IF;
      ELSE
        points.EXTEND;
        points(points.LAST) := start_point;
      END IF;
      points.EXTEND;
      points(points.LAST) := end_point;
      pos1                := lines.NEXT(pos1);
      --dbms_output.put_line(pos1);
    END LOOP;
    RETURN points;
  END Segments2spt_pos;
-- -----------------------------------------------------
-- Function Calculate_Neps (D line_segment_nt,pos number, eps number)
-- -----------------------------------------------------
  FUNCTION Calculate_Neps(
      D IN OUT NOCOPY line_segment_nt,
      pos NUMBER,
      eps NUMBER )
    RETURN INTEGER_NT
  IS
    vp NUMBER;
    L_T line_segment;
    L_T_A line_segment;
    neighbor INTEGER_NT := INTEGER_NT();
    pos1 NUMBER;
    OV   NUMBER;
    SIMI NUMBER;
    DIST NUMBER;
  BEGIN
    L_T        := D(pos);
    vp         := volumeMBB(L_T.s,L_T.e);
    pos1       := D.FIRST;
    FOR pos1 in 1..D.count 
    --WHILE pos1 IS NOT NULL
    LOOP
      IF pos1   <> pos THEN
        IF D(pos1).classified = 0 THEN
        L_T_A   := D(pos1);
        OV      := OVvolumeMBB(L_T.s,L_T.e,L_T_A.s,L_T_A.e);
        SIMI    := SIM(OV,vp);
        DIST    := SDIST(SIMI,L_T.s,L_T.e,L_T_A.s,L_T_A.e);
       -- DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
         --                          action_name => 'DIST: '|| DIST);
        IF DIST <= eps THEN
          neighbor.EXTEND;
          neighbor(neighbor.LAST) := pos1;
        END IF;
      END IF;
      
      END IF;
      --pos1 := D.NEXT(pos1);
      --DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
        --                           action_name => 'conteo ng: '|| neighbor.COUNT);
    END LOOP;
    neighbor.EXTEND;
    neighbor(neighbor.LAST) := pos;
    RETURN neighbor;
  END Calculate_Neps;
-- -----------------------------------------------------
-- Function IsCore (Neps INTEGER_NT, MinTrs NUMBER)
-- -----------------------------------------------------
  FUNCTION IsCore(
      Neps INTEGER_NT,
      MinTrs NUMBER )
    RETURN BOOLEAN
  IS
    CORE BOOLEAN;
  BEGIN
    IF Neps.COUNT >= MinTrs THEN
      CORE        := TRUE;
    ELSE
      CORE := FALSE;
    END IF;
    RETURN CORE;
  END IsCore;
-- -----------------------------------------------------
-- Function AssignCluster (Neps INTEGER_NT, D line_segment_nt, cluster_id INTEGER)
-- -----------------------------------------------------
  FUNCTION AssignCluster(
      Neps INTEGER_NT,
      D IN OUT NOCOPY line_segment_nt,
      cluster_id INTEGER )
    RETURN line_segment_nt
  IS
    pos1 NUMBER;
    pos2 NUMBER;
    RES LINE_SEGMENT_NT;
  BEGIN
    pos1       := Neps.FIRST;
    RES := LINE_SEGMENT_NT();
    WHILE pos1 IS NOT NULL
    LOOP
    
    --IF D(Neps(pos1)).classified = 0 THEN
      D(Neps(pos1)).cluster_id := cluster_id;
      D(Neps(pos1)).classified := 1;
      RES.EXTEND;
      RES(RES.LAST) := D(Neps(pos1));
    --END IF;
      pos1          := Neps.NEXT(pos1);
    END LOOP;
    RETURN RES;
  END AssignCluster;
-- -----------------------------------------------------
-- Function lines_cp(D line_segment_nt)
-- -----------------------------------------------------
  FUNCTION lines_cp(
      D line_segment_nt )
    RETURN line_segment_nt
  IS
    pointsflines spt_pos_nt;
    cp spt_pos_nt;
    linesfcp line_segment_nt;
  BEGIN
    --DBMS_OUTPUT.put_line('D: ' || D.COUNT);
    pointsflines := Segments2spt_pos(D);
    --DBMS_OUTPUT.put_line('pointsflines: ' || pointsflines.COUNT);
    cp := TRACLUS.APPROXIMATE_TRAJ_PARTITIONING(pointsflines);
    --DBMS_OUTPUT.put_line('cp: ' || cp.COUNT);
    linesfcp := Construct_Segments(cp);
    --DBMS_OUTPUT.put_line('linesfcp: ' || linesfcp.COUNT);
    RETURN linesfcp;
  END lines_cp;
-- -----------------------------------------------------
-- Function extract_points_clusters(O internal_cluster_nt)
-- -----------------------------------------------------
  FUNCTION extract_points_clusters(
      O internal_cluster_nt )
    RETURN line_segment_nt
  IS
    pos1 NUMBER;
    pos2 NUMBER;
    lines line_segment_nt;
    line line_segment;
    s spt_pos;
    e spt_pos;
  BEGIN
    
    pos1       := O.FIRST;
    WHILE pos1 IS NOT NULL
    LOOP
      lines      := O(pos1).segments;
      pos2       := lines.FIRST;
      
      WHILE pos2 IS NOT NULL
      LOOP
        line := lines(pos2);
        s    := line.s;
        e    := line.e;
        --DBMS_OUTPUT.PUT_LINE('cluster_id: '|| O(pos1).cluster_id|| ',s.x: ' ||s.x||',s.y: '||s.y||',e.x: '||e.x||',e.y: '||e.y);
        INSERT INTO LINES_POINTS VALUES
          (O(pos1).cluster_id,s.x,s.y,e.x,e.y);
        pos2 := lines.NEXT(pos2);
      END LOOP;
      pos1 := O.NEXT(pos1);
    END LOOP;
    RETURN lines;
  END extract_points_clusters;
-- -----------------------------------------------------
-- Function join_segments(table_name varchar)
-- -----------------------------------------------------
  FUNCTION join_segments
    (
      table_name VARCHAR2
    )
    RETURN line_segment_nt
  IS
    CURSOR LINES_C
    IS
      SELECT *
      FROM LINES L
      WHERE NOT EXISTS
        (SELECT *
        FROM
          (SELECT DISTINCT L.UNIDAD,L.LINEA,L.RUTA,L.VIAJE FROM LINES L
        MINUS
        SELECT L.UNIDAD,L.LINEA,L.RUTA,L.VIAJE AS NT FROM LINES L, TABLE(L.NT) T
          )
        WHERE UNIDAD =L.UNIDAD
        AND LINEA    = L.LINEA
        AND VIAJE    = L.VIAJE
        AND RUTA     = L.RUTA
        );
      LINES_REC LINES_C%ROWTYPE;
      RESULTADO LINE_SEGMENT_NT;
      PARTIAL_RESULT LINE_SEGMENT_NT;
      POS1 NUMBER;
      NPARTIAL_RESULT LINE_SEGMENT_NT;
      cont number;
    BEGIN
    DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'join segments');
      RESULTADO := LINE_SEGMENT_NT();
      cont := 0;
      OPEN LINES_C;
      LOOP
        cont := cont +1;
        FETCH LINES_C INTO LINES_REC;
        EXIT
      WHEN LINES_C%NOTFOUND;
        NPARTIAL_RESULT := LINES_REC.NT;
        PARTIAL_RESULT  := lines_cp(NPARTIAL_RESULT);
        DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'conteo join segments: '|| cont);
        --DBMS_OUTPUT.PUT_LINE('CONTEO LINEAS PARTIAL: ' || PARTIAL_RESULT.COUNT);
        POS1       := PARTIAL_RESULT.FIRST;
        WHILE POS1 IS NOT NULL
        LOOP
          RESULTADO.EXTEND;
          RESULTADO(RESULTADO.LAST) := PARTIAL_RESULT(POS1);
          POS1                      := PARTIAL_RESULT.NEXT(POS1);
        END LOOP;
      END LOOP;
      CLOSE LINES_C;
      --DBMS_OUTPUT.PUT_LINE('CONTEO:' || RESULTADO.COUNT);
      RETURN RESULTADO;
    END join_segments;
   
  -- -----------------------------------------------------
-- Function sort_collection(table_name varchar)
-- ----------------------------------------------------- 
  FUNCTION sort_collection (
                   input_collection IN spt_pos_nt
                    ) RETURN spt_pos_nt IS
  
       type sort_spt_pos_s is table of spt_pos index by PLS_INTEGER;
       sorted_collection sort_spt_pos_s;
       l_idx NUMBER;
       l_sort_idx NUMBER;
       new_collection spt_pos_nt := spt_pos_nt();
       
  
    BEGIN
       l_idx:= input_collection.first;
       loop
        sorted_collection(input_collection(l_idx).x) := input_collection(l_idx);
        l_idx := input_collection.next(l_idx);
        exit when l_idx is null;
       
       
       end loop;
       
       --input_collection.delete;
       l_sort_idx := sorted_collection.first;
       
       loop
        new_collection.extend;
        --dbms_output.put_line(l_sort_idx);
        new_collection(new_collection.last) := sorted_collection(l_sort_idx);
        l_sort_idx := sorted_collection.next(l_sort_idx);
        exit when l_sort_idx is null;
       
       end loop;
       
       return new_collection;
 
   END sort_collection;
    
    -- -----------------------------------------------------
-- Function EXTRACT_RTR(CLUSTER INTERNAL_CLUSTER_NT, MinTrs NUMBER)
-- -----------------------------------------------------
  FUNCTION EXTRACT_RTR
    (
      O INTERNAL_CLUSTER,
      MinTrs NUMBER
    )
    RETURN SPT_POS_NT
  IS
  POS1 NUMBER;
  POS2 NUMBER;
  POS3 NUMBER;
  start_point spt_pos;
  end_point spt_pos;
  seg_x line_segment_nt := line_segment_nt();
  --TYPE points_array IS RECORD (
  --  coord_x NUMBER,
  --  coord_y NUMBER
  --); 
  --TYPE t_p_a IS TABLE OF points_array;
  --p_a t_p_a := t_p_a();
  --p2 t_p_a := t_p_a();
  --rec points_array;   
  slope NUMBER;
  y_cross NUMBER;
  div NUMBER;
  TYPE xss IS TABLE OF NUMBER;
  --x xss := xss();
  x_u xss := xss();
  y_avg NUMBER;
  RTR spt_pos_nt := spt_pos_nt();
  tp tau_tll.d_timepoint_sec := tau_tll.d_timepoint_sec(1, 1, 1, 0, 0, 0);
  dist NUMBER;
  distm NUMBER;
  y NUMBER;
  p_t NUMBER;
  t_m NUMBER;
  p_m NUMBER;
  sustra NUMBER;
  direction_vector sp_pos_nt;
  ang NUMBER;
  points spt_pos_nt := spt_pos_nt();
  points_u spt_pos_nt := spt_pos_nt();
  x NUMBER;
  n_t NUMBER;
  x_r NUMBER;
  y_r NUMBER;
  t_avg DOUBLE PRECISION;
    BEGIN
    -- 1. RECORRER SEGMENTOS DE CLUSTER
    POS1 := O.segments.FIRST;
--    DBMS_OUTPUT.PUT_LINE('COORD_X               COORD_Y');
--    DBMS_OUTPUT.PUT_LINE('--------              --------');
    
    DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'RTR:ACTION');
                                   
   direction_vector := create_direction_vector(O);       

  ang := angle(sp_pos(0, 0), sp_pos(1, 0), sp_pos(direction_vector(direction_vector.FIRST).x, direction_vector(direction_vector.FIRST).y), sp_pos(direction_vector(direction_vector.LAST).x, direction_vector(direction_vector.LAST).y));
    
    pos1 := O.segments.FIRST;
    WHILE pos1 IS NOT NULL
    LOOP
      points.EXTEND;
      points(points.LAST) := O.segments(pos1).s;

      points.EXTEND;
      points(points.LAST) := O.segments(pos1).e;

      pos1 := O.segments.NEXT(pos1);
      
    END LOOP;
    
    --x_u := points.x multiset union distinct points.x;
    points := sort_collection(points);
    
    
    DBMS_OUTPUT.PUT_LINE('CONTEO X: '||x_u.COUNT);
    tp := tau_tll.d_timepoint_sec(1, 1, 1, 0, 0, 0);
    
    POS2 := points.FIRST;
    t_avg := 0;
    <<loop1>>
    WHILE POS2 IS NOT NULL
    LOOP
      dbms_application_info.set_client_info('X_U='||(POS2/points.COUNT)*100);
      x := O.rotated_x(points(POS2),ang);
      seg_x := O.segments_containing_x(x,ang);
      --DBMS_OUTPUT.PUT_LINE('CONTADOR X: '||POS2);
      --DBMS_OUTPUT.PUT_LINE(x(31));
      IF seg_x.COUNT >= MinTrs THEN
        --DBMS_OUTPUT.PUT_LINE('CONTEO SEGMENTOS: '||seg_x.COUNT);
        POS3 := seg_x.FIRST;
        y_cross := 0;
        t_m := 0;
        sustra := 0;
        y := 0;
        <<loop2>>
        WHILE POS3 IS NOT NULL 
        LOOP
          --div := seg_x(POS3).e.x-seg_x(POS3).s.x;
          --dist := UTILITIES.distance(seg_x(POS3).s.x,seg_x(POS3).s.y,seg_x(POS3).e.x,seg_x(POS3).e.y);
          dist := UTILITIES.distance(O.rotated_x(seg_x(POS3).s,ang),O.rotated_y(seg_x(POS3).s,ang),O.rotated_x(seg_x(POS3).e,ang),O.rotated_y(seg_x(POS3).e,ang));
          --DBMS_OUTPUT.PUT_LINE('dist: '||dist);
          p_t := seg_x(POS3).e.t.get_abs_date() - seg_x(POS3).s.t.get_abs_date();
          --DBMS_OUTPUT.PUT_LINE('P_T: '||p_t);
          --DBMS_OUTPUT.PUT_LINE('DIV=: '||div);
          --dbms_application_info.set_client_info('div='||div );
          --IF (div) = 0 THEN
            --sustra := sustra + 1;
            --POS3 := seg_x.NEXT(POS3);
             --CONTINUE;
          --ELSE            
            --slope := (seg_x(POS3).e.y-seg_x(POS3).s.y)/div;
           --DBMS_OUTPUT.PUT_LINE('slope=: '||slope);
            --dbms_application_info.set_client_info('slope='||slope );
          --END IF;

          --y := (slope*x - slope*seg_x(POS3).e.x + (seg_x(POS3).e.y));
          n_t := O.segments_cross_y(seg_x(POS3), x, ang);
          --DBMS_OUTPUT.PUT_LINE('x: '||O.rotated_x(seg_x(POS3).s,ang));
          --DBMS_OUTPUT.PUT_LINE('y: '||O.rotated_y(seg_x(POS3).s,ang));
          --DBMS_OUTPUT.PUT_LINE('x_1: '||x);
          --DBMS_OUTPUT.PUT_LINE('y_1: '||n_t);
          distm := UTILITIES.distance(O.rotated_x(seg_x(POS3).s,ang),O.rotated_y(seg_x(POS3).s,ang),x,n_t);
          --DBMS_OUTPUT.PUT_LINE('distm: '||distm);
          t_m := ((distm*p_t)/dist) + t_m;
          --DBMS_OUTPUT.PUT_LINE('t_m: '||t_m);
          
          
          IF POS3 = 1 THEN
            t_m := 0;
          END IF;
          --p_m := seg_x(POS3).e.t.get_abs_date() - p_t;
          --y_cross := y_cross + y;
          
           --DBMS_OUTPUT.PUT_LINE('Y_CROSS: '||y_cross);
           --POS3 := seg_x.NEXT(POS3);
           
           
          IF n_t IS NULL THEN
            POS2 := points.NEXT(POS2);
            CONTINUE loop1;
          END IF;

          y := y + n_t;

          POS3 := seg_x.NEXT(POS3);
        END LOOP;
        --y_avg := y_cross/(seg_x.COUNT - sustra);
        y := y / seg_x.COUNT;
        t_avg := (t_m/seg_x.COUNT);
        --DBMS_OUTPUT.PUT_LINE('t_avg: ' ||t_avg);
        --DBMS_OUTPUT.PUT_LINE('t_p: ' ||tp.get_abs_date());
        --tp.set_abs_date(0);
        --tp.set_abs_date(tp.get_abs_date() + t_avg);
        --DBMS_OUTPUT.PUT_LINE('year:'|| tp.year|| ' ,month: '||tp.month|| ' ,day: '||tp.day|| ' ,hour: '||tp.hour|| ' ,minute: '||tp.minute|| ' ,second: '||tp.second|| ' ,x: '||x_u(POS2)||tp.second|| ' ,y: '||y_avg);
        x_r := O.reverse_rotation_x(sp_pos(x, y), ang);
        y_r := O.reverse_rotation_y(sp_pos(x, y), ang);
        RTR.EXTEND;
        RTR(RTR.LAST) := spt_pos(x_r,y_r,tp);

      END IF;
      POS2 := points.NEXT(POS2);
    END LOOP;
   RETURN RTR;
   
    END EXTRACT_RTR;
    
    -- -----------------------------------------------------
-- Function segments_containing_x
-- -----------------------------------------------------
  FUNCTION segments_containing_x
  (
    x IN NUMBER,
    segments line_segment_nt
  )
    RETURN line_segment_nt
  IS
    ret line_segment_nt := line_segment_nt();
    pos1 NUMBER;
  BEGIN
    pos1 := segments.FIRST;
    --DBMS_OUTPUT.PUT_LINE('NEW SEGMENT');
    WHILE pos1 IS NOT NULL
    LOOP
      IF segments(pos1).s.x < segments(pos1).e.x THEN
        IF segments(pos1).s.x <= x AND segments(pos1).e.x >= x THEN
          ret.EXTEND;
          ret(ret.LAST) := segments(pos1);
        END IF;
      ELSE
        IF segments(pos1).s.x >= x AND segments(pos1).e.x <= x THEN
          ret.EXTEND;
          ret(ret.LAST) := segments(pos1);
        END IF;
      END IF;

      pos1 := segments.NEXT(pos1);
     -- DBMS_OUTPUT.PUT_LINE(ret(ret.LAST).s.x || ' ' || ret(ret.LAST).s.y);
    END LOOP;

    RETURN ret;
  END segments_containing_x;
  
      -- -----------------------------------------------------
-- Function create_direction_vector
-- -----------------------------------------------------
  FUNCTION create_direction_vector
  (
    O INTERNAL_CLUSTER
  )
    RETURN sp_pos_nt
  IS
    n NUMBER;
    pos1 NUMBER;
    x NUMBER;
    y NUMBER;
    xb NUMBER := 0;
    yb NUMBER := 0;
    ret sp_pos_nt := sp_pos_nt();
  BEGIN
    n := O.segments.COUNT;
    
     IF n = 0 THEN
      RETURN NULL;
    END IF;
    
    pos1 := O.segments.FIRST;
    --DBMS_OUTPUT.PUT_LINE('NEW SEGMENT');
    
    WHILE pos1 IS NOT NULL
    LOOP
      x := O.segments(pos1).e.x - O.segments(pos1).s.x;
      y := O.segments(pos1).e.y - O.segments(pos1).s.y;
      
      xb := xb + x;
      yb := yb + y;
      
      pos1 := O.segments.NEXT(pos1);
    END LOOP;
    
    xb := xb / n;
    yb := yb / n;

    ret.EXTEND;
    ret(ret.LAST) := sp_pos(0, 0);

    ret.EXTEND;
    ret(ret.LAST) := sp_pos(xb, yb);
    RETURN ret;
    
  END create_direction_vector;
  
    -- -----------------------------------------------------
-- Function angle_xx
-- -----------------------------------------------------
  FUNCTION angle_xx
    (
        s IN sp_pos,
    e IN sp_pos
    )
        RETURN NUMBER
    IS
  BEGIN
    RETURN atan2(e.y - s.y, e.x - s.x);
  END angle_xx;
  
  -- -----------------------------------------------------
-- Function angle
-- -----------------------------------------------------
  FUNCTION angle
    (
        s1 IN sp_pos,
    e1 IN sp_pos,
    s2 IN sp_pos,
    e2 IN sp_pos
    )
        RETURN NUMBER
    IS
  BEGIN
    RETURN abs(angle_xx(s1, e1) - angle_xx(s2, e2));
  END angle;
  


    -- -----------------------------------------------------
    -- Function Main_ (D line_segment_nt, Eps NUMBER, MinTrs INTEGER)
    -- -----------------------------------------------------
    FUNCTION Main_(
        D IN OUT NOCOPY line_segment_nt,
        Eps    NUMBER,
        MinTrs INTEGER )
      RETURN internal_cluster_nt
    IS
      linesfcp line_segment_nt;
      pos1 NUMBER;
      Ngh INTEGER_NT;
      is_core BOOLEAN;
      c_id    NUMBER        := 1;
      O internal_cluster_nt := internal_cluster_nt();
      smooth_factor INTEGER := 0;
      cl line_segment_nt;
      lin line_segment_nt;
      pos2 NUMBER;
      timestart NUMBER;
      rs_lines SPT_POS_NT;
      pos3 NUMBER;
    BEGIN
    
      timestart := dbms_utility.get_time();
       DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'INICIO: '|| timestart);
      linesfcp := join_segments('LINES');
      --DBMS_OUTPUT.put_line('LINEAS TOTALES: ' || linesfcp.COUNT);
      DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'conteo: '|| linesfcp.COUNT);
      cl := line_segment_nt();
      pos1 := linesfcp.FIRST;
      --pos1 := 1;
      WHILE pos1 IS NOT NULL
      LOOP
      --DBMS_OUTPUT.PUT_LINE('clasificado= '|| linesfcp(pos1).classified);
        IF linesfcp(pos1).classified = 0 THEN
          --DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   --action_name => 'Ngh');
          Ngh                       := Calculate_Neps(linesfcp,pos1,Eps);  
          DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'is_core');
          HERMES.ENTROPY_STORE(Ngh);
          is_core                   := IsCore(Ngh,MinTrs);
          --DBMS_OUTPUT.PUT_LINE('CORE ='||sys.diutil.bool_to_int(is_core));
          IF is_core THEN
            cl := AssignCluster(Ngh,linesfcp,c_id);
            O.EXTEND;
            O(c_id) := internal_cluster(c_id, MinTrs, smooth_factor);
            O(c_id).segments.EXTEND;        
            O(c_id).segments := cl;
            --O(c_id).post_process();
            c_id             := c_id + 1;
            cl.DELETE;
          END IF;
        END IF;
        DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'AVANCE '|| (pos1/linesfcp.COUNT)*100 || ' %');
        --DBMS_OUTPUT.PUT_LINE(linesfcp(pos1).classified);
        pos1 := linesfcp.NEXT(pos1);
      END LOOP;
      --rs_lines := EXTRACT_RTR(O(1),MinTrs);
      --DBMS_OUTPUT.PUT_LINE('conteo rtr ' ||rs_lines.COUNT);
      pos3 := O.FIRST;
      
      DBMS_APPLICATION_INFO.set_module(module_name => 'rtr',
                                   action_name => 'EXTRACT RTR');
      WHILE pos3 IS NOT NULL
      LOOP
          rs_lines := EXTRACT_RTR(O(pos3),MinTrs);
          HERMES.INSERT_RTR(rs_lines,O(pos3).cluster_id);
          DBMS_OUTPUT.PUT_LINE('conteo rtr cluster pos3 ' ||rs_lines.COUNT);
          --DBMS_OUTPUT.PUT_LINE('conteo rtr cluster pos3 ' ||O(1).segments(2).s.t.hour);
          pos3 := O.NEXT(pos3);
      END LOOP;
      
      --lin := extract_points_clusters(O);
      --EXTRACT_POINTS_CLUSTERS_SP(O);
      --EXTRACT_RTR_CLUSTERS_SP(O);
      --DBMS_OUTPUT.PUT_LINE(O(1).segments.COUNT);
      --DBMS_OUTPUT.PUT_LINE(O(2).segments.COUNT);
      --DBMS_OUTPUT.PUT_LINE(O(3).segments.COUNT);
      --DBMS_OUTPUT.PUT_LINE(O(4).segments.COUNT);
      --DBMS_OUTPUT.PUT_LINE(O(5).segments.COUNT);
      --DBMS_OUTPUT.PUT_LINE(O(6).segments.COUNT);
      DBMS_OUTPUT.PUT_LINE('clusters ' ||O.COUNT);
      dbms_output.put_line(dbms_utility.get_time() - timestart);
      RETURN O;
    END Main_;

-- FUNCIÓN PARA COMPARAR PATRÓN IDEAL Y REAL
  FUNCTION compare_trajectories (a NUMBER) RETURN moving_point AS
  velocidad_pattern NUMBER;
  velocidad_real NUMBER;
  initial_timepoint TAU_TLL.D_TIMEPOINT_SEC;
  mpoint_pattern hermes.ruta_ideal_mpoint.mpoint%TYPE;
  mpoint_real hermes.rtr_mpoint.mpoint%TYPE;
  final_timepoint TAU_TLL.D_TIMEPOINT_SEC;
  diff_timepoints NUMBER;
  iterate_timepoint TAU_TLL.D_TIMEPOINT_SEC;
  XP NUMBER;
  YP NUMBER;
  XR NUMBER;
  YR NUMBER;
  initial_timepoint_real TAU_TLL.D_TIMEPOINT_SEC;
  BEGIN
   SELECT mpoint INTO mpoint_pattern
  FROM hermes.ruta_ideal_mpoint;
  
  SELECT mpoint INTO mpoint_real
  FROM HERMES.RTR_MPOINT;
  
    initial_timepoint := mpoint_pattern.f_initial_timepoint();
    final_timepoint := mpoint_pattern.f_final_timepoint();
    diff_timepoints := final_timepoint.get_Abs_Date() - initial_timepoint.get_Abs_Date();
    initial_timepoint_real := mpoint_real.f_initial_timepoint();
   -- DBMS_OUTPUT.PUT_LINE('initial timepoint: '||initial_timepoint.get_Abs_Date());
    
    WHILE initial_timepoint.get_Abs_Date() <= final_timepoint.get_Abs_Date()
      LOOP
       velocidad_pattern := mpoint_pattern.f_speed(initial_timepoint);
       velocidad_real := mpoint_real.f_speed(initial_timepoint_real);
       initial_timepoint.set_Abs_Date(initial_timepoint.get_Abs_Date() + 1);
       initial_timepoint_real.set_Abs_Date(initial_timepoint_real.get_Abs_Date() + 1);
       /*DBMS_OUTPUT.PUT_LINE('vel_pattern: '||velocidad_pattern);
       DBMS_OUTPUT.PUT_LINE('vel_real: '||velocidad_real);
       DBMS_OUTPUT.PUT_LINE('vel_real: '||mpoint_pattern.at_instant(initial_timepoint).sdo_point.x);*/
       XP := mpoint_pattern.at_instant(initial_timepoint).sdo_point.x;
       YP := mpoint_pattern.at_instant(initial_timepoint).sdo_point.y;
       XR := mpoint_real.at_instant(initial_timepoint_real).sdo_point.x;
       YR := mpoint_real.at_instant(initial_timepoint_real).sdo_point.y;
       INSERT INTO HERMES.COMPARATION_TRAJECTORIES (XP,YP,VP,XR,YR,VR) VALUES (
         XP,
         YP,
         velocidad_pattern,
          XR,
         YR,
         velocidad_real        
       );

      END LOOP;
      
    --DBMS_OUTPUT.PUT_LINE('initial timepoint: ' || initial_timepoint);
    --DBMS_OUTPUT.PUT_LINE('diff: ' || diff_timepoints);
    --DBMS_OUTPUT.PUT_LINE('MOVING POINT: ' || mpoint_pattern.to_string());
    --DBMS_OUTPUT.PUT_LINE('Velocidad: '||velocidad);
    
    RETURN NULL;
  END compare_trajectories;

END TESIS_MCIC_GEO;

/
