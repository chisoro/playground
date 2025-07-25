/* Program.................dbmrpt1.p
   Notes:................. Meter reading exception report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsAvg     AS DEC FORM "zzzzz9.99-". 
DEF VAR wsvarian  AS DEC FORM "zzzzz9.99-".
DEF VAR wsconsum  AS DEC FORM "zzzzz9.99-". 
DEF VAR wsNo      AS INT. /* Count of months with meter readings */
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsStart   LIKE dbcmf.dbacc.
DEF VAR wsEnd     LIKE dbcmf.dbacc.
DEF VAR X AS INT.
DEF VAR t AS INT.
DEF  VAR wsTitle AS CHAR  FORM "x(60)".
DEF VAR wsdev AS DEC FORM "zz9.99-".
DEF VAR wsopt AS INT INITIAL 1 VIEW-AS RADIO-SET 
             RADIO-BUTTONS "METER READING LIST",0,
                           "HIGH CONSUMPTION REPORT", 1,
                           "LOW CONSUMPTION REPORT",2, 
                           "METER TURNS REPORT", 3.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEF BUFFER bfdbmmf FOR dbmmf.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsOpt COLON 30 LABEL "SELECT REPORT   " 
    SKIP(0.5)  
    wsDev COLON 30 LABEL "PERCENTAGE DEVIATION"
    SKIP(0.5)
    btn-ok AT ROW 12.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "METER EXCEPTION REPORTS" VIEW-AS DIALOG-BOX.

FORM
    bfdbmmf.dbacc       LABEL "ACCOUNT" 
    dbcmf.NAME          LABEL "NAME"
    bfdbmmf.Serial       LABEL "METER"
    bfdbmmf.READ[12]    LABEL "PRE-READ"
    bfdbmmf.READ[13]    LABEL "CUR-READ"
    wsConsum            LABEL "CONSUMPTION"
    wsAvg               LABEL "AVERAGE"
    wsvarian            LABEL "VARIANCE %"
    HEADER skip(4) wsTitle TODAY  "Page: " AT  72 PAGE-NUMBER(a)
     SKIP(3)
    WITH NO-BOX DOWN  STREAM-IO WIDTH 132 NO-LABELS FRAME frm-rpt1.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  wsOpt wsdev.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.

ON 'choose':U OF wsopt IN FRAME frm-main
DO:
    ASSIGN wsOpt.
    IF wsOpt = 0 OR wsOpt = 3 THEN
        DISABLE wsDev WITH FRAME frm-main.
    ELSE ENABLE wsDev WITH FRAME frm-main.
    RETURN.
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
wsOpt:SCREEN-VALUE = "0".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
CASE wsopt:
    WHEN 0 THEN DO:
        wsTitle = "METER READING LISTING REPORT: ".
        FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 NO-LOCK: /* Meter reading list */
            FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc NO-LOCK NO-ERROR.
            ASSIGN wsAvg = 0
                   X = 12
                   wsNo = 0.
            DO WHILE X >=  2:
               ASSIGN wsAvg = wsAvg + bfdbmmf.consum[X] WHEN bfdbmmf.consum[X] > 0 
                       wsNo = wsNo + 1 WHEN bfdbmmf.consum[X] > 0
                       X = X - 1.
            END.
            ASSIGN wsAvg    = wsAvg / wsNo
                   wsConsum = bfdbmmf.consum[13]
                   wsVarian = ROUND((( wsConsum - wsAvg  ) / wsAvg * 100),2). 
            DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME  bfdbmmf.Serial bfdbmmf.read[12] bfdbmmf.read[13] wsconsum  wsavg wsvarian
                              WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
    WHEN 1 THEN DO:
         wsTitle = string(wsdev) + "% DEVIATION - HIGH CONSUMPTION METER EXCEPTION REPORT: ".
        FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 NO-LOCK:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc NO-LOCK NO-ERROR.
            ASSIGN wsAvg = 0
                   X = 12
                   wsNo = 0.
            DO WHILE X >=  2:
               ASSIGN wsAvg = wsAvg + bfdbmmf.consum[X] WHEN bfdbmmf.consum[X] > 0 
                       wsNo = wsNo + 1 WHEN bfdbmmf.consum[X] > 0
                       X = X - 1.
            END.
            ASSIGN wsAvg    = wsAvg / wsNo
                   wsConsum = bfdbmmf.consum[13]
                   wsVarian = ROUND((( wsConsum - wsAvg  ) / wsAvg * 100),2). 
            IF wsAvg < wsConsum AND  wsVarian >=  wsdev THEN DO: 
                DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME bfdbmmf.Serial bfdbmmf.read[12] bfdbmmf.read[13] wsconsum  wsavg wsvarian
                          WITH FRAME frm-rpt1.
                DOWN STREAM a WITH FRAME frm-rpt1.
            END.
        END.
    END.
    WHEN 2 THEN DO:
        wsTitle = string(wsdev) + "% DEVIATION - LOW CONSUMPTION METER EXCEPTION REPORT: ".
        FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 NO-LOCK:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc NO-LOCK NO-ERROR.
            ASSIGN wsAvg = 0
                   X = 12
                   wsNo = 0.
            DO WHILE X >=  2:
               ASSIGN wsAvg = wsAvg + bfdbmmf.consum[X] WHEN bfdbmmf.consum[X] > 0 
                       wsNo = wsNo + 1 WHEN bfdbmmf.consum[X] > 0
                       X = X - 1.
            END.
            ASSIGN wsAvg    = wsAvg / wsNo
                   wsConsum = bfdbmmf.consum[13]
                   wsVarian = ROUND((( wsConsum - wsAvg  ) / wsAvg * 100),2). 
            IF wsAvg > wsConsum AND  wsVarian <=  wsdev THEN DO: 
                DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME bfdbmmf.Serial bfdbmmf.read[12] bfdbmmf.read[13] wsconsum  wsavg wsvarian
                          WITH FRAME frm-rpt1.
                DOWN STREAM a WITH FRAME frm-rpt1.
            END.
        END.
    END.
    WHEN 3 THEN DO:
        wsTitle = "METER TURN CONSUMPTION METER EXCEPTION REPORT: ".
        FOR EACH bfdbmmf WHERE bfdbmmf.mStats <> 3 AND bfdbmmf.comm[13] = 99 NO-LOCK BY bfdbmmf.consum[13] DESCENDING: /* Meter turns */
            FIND FIRST dbcmf WHERE dbcmf.dbacc = bfdbmmf.dbacc NO-LOCK NO-ERROR.
            ASSIGN wsAvg = 0
                   X = 12
                   wsNo = 0.
            DO WHILE X >=  2:
               ASSIGN wsAvg = wsAvg + bfdbmmf.consum[X] WHEN bfdbmmf.consum[X] > 0 
                       wsNo = wsNo + 1 WHEN bfdbmmf.consum[X] > 0
                       X = X - 1.
            END.
            ASSIGN wsAvg    = wsAvg / wsNo
                   wsConsum = bfdbmmf.consum[13]
                   wsVarian = ROUND((( wsConsum - wsAvg  ) / wsAvg * 100),2). 
            DISPLAY STREAM a bfdbmmf.dbacc dbcmf.NAME bfdbmmf.Serial bfdbmmf.read[12] bfdbmmf.read[13] wsconsum  wsavg wsvarian
                              WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
END CASE.
END PROCEDURE.
    
    
     
