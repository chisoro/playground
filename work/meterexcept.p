DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsQty     AS INT.
DEF VAR wsNo      AS INT. /* Count of months with meter readings */
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsStart   LIKE dbcmf.dbacc.
DEF VAR wsEnd     LIKE dbcmf.dbacc.
DEF VAR X AS INT.
DEF VAR t AS INT.
DEF VAR wsopt AS INT INITIAL 1 VIEW-AS RADIO-SET 
             RADIO-BUTTONS "HIGH CONSUMPTION REPORT", 1,
                           "LOW CONSUMPTION REPORT",2, 
                           "METER TURNS REPORT", 3.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEF BUFFER bfdbmmf FOR dbmmf.
DEF TEMP-TABLE bfmeter
    FIELD dbacc LIKE dbmmf.dbacc
    FIELD consum AS INT
    FIELD wsavg  AS INT
    FIELD varian AS DECIMAL.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsOpt COLON 20 LABEL "SELECT REPORT" 
    SKIP(0.5)  
    btn-ok AT ROW 12.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "METER EXCEPTION REPORTS" VIEW-AS DIALOG-BOX.

FORM
    bfmeter.dbacc  LABEL "ACCOUNT" 
    bfmeter.consum LABEL "CONSUMPTION"
    bfmeter.wsavg  LABEL "AVERAGE"
    bfmeter.varian LABEL "VARIANCE %"
     HEADER "50% PLUS HIGH CONSUMPTION METER EXCEPTION REPORT" SKIP(2)
    WITH NO-BOX DOWN  STREAM-IO no-labels
	     FRAME frm-rpt1.
FORM
    bfdbmmf.dbacc  LABEL "ACCOUNT" 
    bfdbmmf.consum[10] LABEL "LAST 3 CONSUMPTIONS"
    bfdbmmf.consum[11]
    bfdbmmf.consum[12]
    bfdbmmf.consum[13]  LABEL "CURRENT"
     HEADER "METER TURN CONSUMPTION METER EXCEPTION REPORT" SKIP(2)
    WITH NO-BOX DOWN  STREAM-IO no-labels
	     FRAME frm-rpt2.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  wsOpt.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
FOR EACH bfdbmmf:
        ASSIGN wsQty = 0.
               X = 12.
               wsNo = 0.
         DO WHILE X >=  2:
              ASSIGN wsQty = wsQty + bfdbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                              WHEN bfdbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                     wsNo = wsNo + 1 WHEN bfdbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                     X = X - 1.
         END.
         wsQty = wsQty / wsNo.
        
         IF wsQty < bfdbmmf.consum[13] AND ((( bfdbmmf.consum[13] - wsqty  ) / wsqty * 100) > 50
                                        OR (( bfdbmmf.consum[13] - wsqty  ) / wsqty * 100) < - 50 ) THEN
         DO: 
             CREATE bfmeter.
             ASSIGN bfmeter.dbacc = bfdbmmf.dbacc
                    bfmeter.consum = bfdbmmf.consum[13]
                    bfmeter.wsavg  = wsqty
                    bfmeter.varian = ROUND((( bfdbmmf.consum[13] - wsqty  ) / wsqty * 100),2).
         END.
END.
CASE wsopt:
    WHEN 1 THEN DO:
        FOR EACH bfmeter WHERE bfmeter.varian > 50 BY bfmeter.varian DESCENDING:  /* Too high consupmtion */
            DISPLAY STREAM a bfmeter.dbacc bfmeter.consum bfmeter.wsavg bfmeter.varian
            WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
    WHEN 2 THEN DO:
        FOR EACH bfmeter WHERE bfmeter.varian < -50 BY bfmeter.varian:  /* Too low consupmtion */
         DISPLAY STREAM a bfmeter.dbacc bfmeter.consum bfmeter.wsavg bfmeter.varian
            WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
    END.
    WHEN 3 THEN DO:
        FOR EACH bfdbmmf WHERE bfdbmmf.comm[13] = 96 BY bfdbmmf.consum[13] DESCENDING: /* Meter turns */
        DISPLAY STREAM a bfdbmmf.dbacc bfdbmmf.consum[10]bfdbmmf.consum[11] bfdbmmf.consum[12]
                         bfdbmmf.consum[13] WITH FRAME frm-rpt2.
        DOWN STREAM a WITH FRAME frm-rpt2.
        END.
    END.
END CASE.
END.
    
    
     
