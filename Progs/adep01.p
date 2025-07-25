/* Program.................adep01.p
   Notes:...... Asset Monthly Depreciation Run
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsDepr AS DEC FORM "zzzzzzzzz9.99-".
DEF VAR wsCum AS DEC FORM "zzzzzzzzz9.99-".
DEF VAR wsPer  LIKE SIMCTR.NDep.
DEF VAR wscurPer AS DEC FORM "999999".
DEF VAR wsDrLedger LIKE glmf.acct.
DEF VAR wsCrLedger LIKE glmf.acct.
DEF VAR wsDes LIKE dbgl.descrip.
DEF VAR wsSource LIKE dbgl.source.
DEF VAR wsDr      AS DEC FORM "zz,zzz,zz9.99-".
DEF VAR wsCr      AS DEC FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsDr.
DEF VAR wsTCr    LIKE wsDr.
DEF VAR wslog AS LOGICAL INITIAL NO.
DEF VAR wsDate AS DATE.
DEF VAR wsD    AS INT.
DEF VAR X AS INT.
DEF VAR yy AS INT.
DEF VAR mm AS INT.
DEF VAR cnt AS INT.
DEF VAR ter AS LOGICAL.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".

DEF BUTTON btnok       LABEL "RUN".
DEF BUTTON btnClose    LABEL "Cancel".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 10.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.
FORM
    assmf.acode       COLUMN-LABEL "CODE"
    assmf.PDate       COLUMN-LABEL "PURCHASE!   DATE"
    assmf.AValue      COLUMN-LABEL "VALUE"
    assmf.accumdep    COLUMN-LABEL "ACCUM DEPR"
    assmf.curDep      COLUMN-LABEL "CURR DEPR"
    assmf.nbv         COLUMN-LABEL "NBV"
    HEADER skip(1) "MONTHLY DEPRECIATION RUN REPORT - DATE: " wsDate  "PERIOD  :" wsPer
     "Page:" AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.
FORM 
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "PAYMENTS UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" wsPer
      "Page:" AT 110 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

DEFINE FRAME frm-Main
     SKIP(1.5)
     wsper      COLON 30 LABEL "Depreciation Period" VIEW-AS TEXT SKIP(0.5)
     wsDate     COLON 30 LABEL "Depreciation Date" AUTO-RETURN SKIP(0.5)
     wsLog      COLON 30 LABEL "Calculate Backlog Y/N)" AUTO-RETURN SKIP(0.5)
     btnok AT ROW 12.5 COL 10 SPACE(50) btnclose
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 12.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 15
     TITLE "ASSET MONTHLY DEPRECIATION RUN" VIEW-AS DIALOG-BOX.

ON 'TAB':U OF wsDate IN FRAME frm-main
   OR 'enter':U OF wsDate IN FRAME frm-main
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <> INT(wsper:SCREEN-VALUE)
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btnok IN FRAME frm-Main 
DO:
    ASSIGN wsDate wsLog wsper.
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="Proc.ip"
                     &paged} 
   APPLY 'entry' TO btnClose IN FRAME frm-main.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
CLEAR FRAME frm-main.
ASSIGN wsper:SCREEN-VALUE = STRING(SIMCTR.NDep) /*last depreciation period */
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsLog :SCREEN-VALUE = "NO"
       wsSource = "AS".
IF SIMCTR.AssetDepr = NO THEN DO:
    MESSAGE "System not set to run Depreciations" VIEW-AS ALERT-BOX.
    APPLY 'close' TO THIS-PROCEDURE .
    HIDE FRAME frm-main.
END.
ELSE
ENABLE ALL WITH FRAME frm-main.
APPLY 'entry' TO wsDate IN FRAME frm-Main.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Proc.ip:
    ASSIGN wsTime    = STRING(TIME,"HH:MM:SS")
           wsTransID = DEC (string(YEAR(TODAY),"9999")
                     + string(MONTH(TODAY),"99" )
                     + string(DAY(TODAY),"99")
                     + SUBSTR(STRING(wsTIME),1,2) 
                     + SUBSTR(STRING(wsTIME),4,2) 
                     + SUBSTR(STRING(wsTIME),7,2) ).
    FOR EACH asubcat:
        FOR EACH assmf WHERE assmf.subcat = asubcat.subcat AND assmf.RemPer > 0 
            AND Assmf.Astatus = "A" AND Assmf.DepDate <> wsDate:
            ASSIGN ter = NO
                   cnt = 0.
                   wsCurPer = assmf.DepPer.
          /* Arrear depreciation */
            /* IF wslog = yes and assmf.DepPer < wsPer THEN DO: 
                wsDes = "Backlog Depreciation".
                MESSAGE acode " This needs backlog calaculation" VIEW-AS ALERT-BOX.
                wsCurPer = assmf.DepPer.
                ASSIGN yy = INT(SUBSTR(STRING(wscurPer),1,4))
                       mm = INT(SUBSTR(STRING(wscurPer),5,2)).
                DO  WHILE ter = NO:     
                    DO WHILE mm <= 12 AND ter = NO:
                        IF mm <= 12 THEN
                           RUN depr.ip.
                        ASSIGN mm = mm + 1
                               wscurPer = DEC(STRING(yy) + STRING(mm,"99"))
                               cnt = cnt + 1.
                        IF cnt = assmf.Alife  THEN
                            ter = YES.
                        IF assmf.DepPer = wsPer THEN
                            ter = YES.
                    END.
                    IF assmf.DepPer < wsPer THEN
                    ASSIGN mm = 1
                           yy = yy + 1
                           wsCurPer = DEC(STRING(yy) + STRING(mm,"99")).
                END.
            END. /*  eof Arrear depreciation */
            ELSE */
                wsDes = "Monthly Depreciation" .
                RUN depr.ip.
            
            DISPLAY STREAM a assmf.acode PDate AValue accumdep curDep nbv WITH FRAME frm-rpt.
            DOWN STREAM a  WITH FRAME frm-rpt.
        END.
    END. /* eo-Asubcat */
    ASSIGN wsTCr = 0
              wsTDr = 0.
     ASSIGN yy = INT(SUBSTR(STRING(wsPer),1,4))
           mm = INT(SUBSTR(STRING(wsPer),5,2))
           wsCurPer = DEC(STRING(yy) + STRING(mm,"99")) + 1.
    IF SUBSTR(STRING(wsCurPer),5,2) = "13" THEN
                ASSIGN mm = 1
                       yy = yy + 1
                wsCurPer = DEC(STRING(yy) + STRING(mm,"99")).
    ELSE
        wsCurPer = DEC(STRING(yy) + STRING((mm + 1),"99")).
    FIND FIRST SIMCTR NO-ERROR.
    MESSAGE wsCurPer VIEW-AS ALERT-BOX.
    SIMCTR.NDep = wsCurPer. 
     {glcon.i}

     
     RETURN.
END.

PROCEDURE Depr.ip:
    ASSIGN yy = INT(SUBSTR(STRING(wscurPer),1,4))
           mm = INT(SUBSTR(STRING(wscurPer),5,2))
           wsCurPer = DEC(STRING(yy) + STRING(mm,"99")).
    IF SUBSTR(STRING(wsCurPer),5,2) = "13" THEN
                ASSIGN mm = 1
                       yy = yy + 1
                       wsCurPer = DEC(STRING(yy) + STRING(mm,"99")).
    CASE Asubcat.DMethod:
         WHEN 1  THEN /* Straight Line Method */
             IF Assmf.RemPer  <> 0 AND assmf.Nbv > assmf.RCost THEN
                ASSIGN wsDepr = ROUND(((assmf.AValue - assmf.RCost) / assmf.Alife),2).                
         WHEN 2 THEN  /* Reducing Method */
                ASSIGN wsDepr = ROUND(((AValue - RCost - AccumDep) * (assmf.DPer /  100)),2).
         WHEN 3  THEN
             IF assmf.Nbv <> assmf.RCost THEN DO: /* Production Units Method */
                DO X = 1 TO assmf.Alife:
                    wsD = wsD + X. 
                END.
                ASSIGN wsDepr = ROUND(((RemPer / wsD) * (AValue - RCost)),2).
         END.
    END CASE.
    IF wsDepr <> 0 THEN DO:
        ASSIGN Assmf.AccumDep = Assmf.AccumDep + wsDepr
               assmf.DepPer   = wsPer                
               Assmf.RemPer   = Assmf.RemPer - 1       
               Assmf.CurDep   = wsDepr                 
               Assmf.NBV      =  Assmf.AValue -  Assmf.AccumDep.
       FIND FIRST gldept WHERE gldept.dept = assmf.dept NO-LOCK NO-ERROR.
       ASSIGN wsDrLedger = dec(string(GlDept.FUND) + STRING(GlDept.Dept) + STRING(asubcat.Lsuffix[3]))
                                WHEN simctr.LForm = 1
              wsCrLedger = dec(string(GlDept.FUND) + STRING(asubcat.Lsuffix[2],"99999999")) WHEN simctr.LForm = 1
              wsDrLedger = asubcat.Lsuffix[3] WHEN simctr.LForm = 2
              wsCrLedger = asubcat.Lsuffix[2] WHEN simctr.LForm = 2.
       CREATE asstf.
       ASSIGN asstf.acct    = wsDrLedger
              asstf.ACode   = assmf.acode
              asstf.amt     = wsDepr
              asstf.Descrip = wsdes
              asstf.Per     = wsPer
              asstf.ref     = "Depr:" + STRING(wsPer)
              asstf.TrDate  = wsDate.
       /* create DR record for ledger */
       FIND FIRST dbgl WHERE dbgl.acct = wsDrLedger AND dbgl.descrip = wsDes no-error. 
       IF NOT AVAILABLE dbgl THEN DO:
            CREATE dbgl.
            ASSIGN  dbgl.CREDATE = TODAY
                    dbgl.DESCRIP  = wsDes 
                    dbgl.period   = wsPer
                    dbgl.REF      = "Depr:" + STRING(wsPer)
                    dbgl.TransID  = wsTransId
                    dbgl.trDATE   = wsDate
                    dbgl.UID      = varUser
                    dbgl.acct     = wsDrLedger
                    dbgl.Dept     = Assmf.Dept
                    dbgl.Proj     = 0
                    dbgl.SOURCE   = "AS"
                    dbgl.UID2    = varUser.
        END.
        ASSIGN dbgl.AMT     = dbgl.AMT + wsDepr.
         /* create CR record for ledger */
       FIND FIRST dbgl WHERE dbgl.acct = wsCrLedger AND dbgl.descrip = wsDes no-error.
       IF NOT AVAILABLE dbgl THEN DO:
            CREATE dbgl.
            ASSIGN  dbgl.CREDATE = TODAY
                    dbgl.DESCRIP  = wsDes 
                    dbgl.period   = wsPer
                    dbgl.REF      = "Depr:" + STRING(wsPer)
                    dbgl.TransID  = wsTransId
                    dbgl.trDATE   = wsDate
                    dbgl.UID      = varUser
                    dbgl.acct     = wsCrLedger
                    dbgl.Dept     = Assmf.Dept
                    dbgl.Proj     = 0
                    dbgl.SOURCE   = "AS"
                    dbgl.UID2    = varUser.
        END.
        ASSIGN dbgl.AMT     = dbgl.AMT + (wsDepr * -1).
    END.
END.
