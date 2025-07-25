/* Program.................gltb01.p
   Notes:................. Trial Balance Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE glmf.acct.
DEF VAR wsend LIKE glmf.acct.
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Consolidated Trial Balance", 1,
    "Trial Balance by Project", 2,
    "Trial Balance by Segment", 3,
    "Trial Balance by Departments", 4.
DEF VAR wsFile AS CHAR FORM "x(40)".    
DEF VAR curAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 2 FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC /*EXTENT 2 */FORM "ZZZZZZZZZZ9.99-".
DEF VAR DTAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR PTAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR st-proj LIKE glproj.proj.
DEF VAR end-Proj LIKE glproj.proj.
DEF VAR st-dept LIKE gldept.dept.
DEF VAR end-dept LIKE gldept.dept.
DEF VAR st-fund  LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR wsProg AS CHAR.
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR wsSource LIKE gltdf.SOURCE.
DEF VAR wsLedger LIKE gltdf.acct.
DEF VAR wsdes LIKE gltdf.descrip.
DEF VAR wsDr  LIKE wsAmt FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsCr  LIKE wsAmt FORM "ZZZZZZZZZZ9.99-".
DEF VAR st-per LIKE gltdf.period.
DEF VAR X AS INT.

DEF BUTTON btn-Detail LABEL "DETAILS".
DEF BUTTON btn-Src    LABEL "SOURCE".
DEF BUTTON btn-Prn    LABEL "PRINT" .  
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-exp    LABEL "Export".

DEF BUFFER bfrdbhtf FOR dbhtf.
DEF TEMP-TABLE tmptdf LIKE gltdf.

DEF TEMP-TABLE tmpsrc
    FIELDS acct    LIKE gltdf.acct
    FIELDS period  LIKE gltdf.period
    FIELDS trDATE  LIKE gltdf.trDate
    FIELDS REF     LIKE gltdf.ref
    FIELDS seq     LIKE cbtrans.seq 
    FIELDS DESCRIP LIKE gltdf.descrip
    FIELDS AMT     LIKE gltdf.Amt
    FIELDS vat     LIKE gltdf.amt
    FIELDS decRate LIKE cbTrans.decrate
    FIELDS TransID LIKE gltdf.TransId
    FIELDS SOURCE  LIKE gltdf.SOURCE.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 20.5.

DEFINE RECTANGLE rec-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rec-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF    QUERY qry-dbsrc FOR bfrdbhtf SCROLLING.
DEF BROWSE brw-dbsrc QUERY qry-dbsrc
    DISPLAY bfrdbhtf.Accper bfrdbhtf.dbAcc bfrdbhtf.trDate bfrdbhtf.DESCRIP bfrdbhtf.Sgrp bfrdbhtf.Ref bfrdbhtf.Vat bfrdbhtf.Amt bfrdbhtf.TransID  WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-src FOR tmpsrc SCROLLING.
DEF BROWSE brw-src QUERY qry-src
    DISPLAY tmpsrc.acct tmpsrc.period tmpsrc.trDATE tmpsrc.REF tmpsrc.seq COLUMN-LABEL "SEQ/SVGRP" tmpsrc.DESCRIP FORM "x(40)" WIDTH 40
     tmpsrc.AMT tmpsrc.decRate tmpsrc.TransID WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-trans FOR tmptdf SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY tmptdf.SOURCE tmptdf.period tmptdf.trDATE tmptdf.REF tmptdf.DESCRIP FORM "x(40)" WIDTH 40
     tmptdf.AMT tmptdf.CREDATE tmptdf.TransID WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-tb FOR glbal SCROLLING.
DEF BROWSE brw-tb QUERY qry-tb
    DISPLAY glbal.acct COLUMN-LABEL "ACCOUNT" WsDes COLUMN-LABEL "DESCRIPTION"
            wsDr COLUMN-LABEL "DR" WIDTH 17 WsCr COLUMN-LABEL "CR" WIDTH 17 WITH 23 DOWN SEPARATORS.

DEFINE FRAME frmPeriod
    st-per LABEL "Start Period" AT 5
    skip(1.5)
    btn-ok COLON 5
    btn-exit COLON 20
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Period Selection".

DEFINE FRAME frm-trans 
    brw-trans AT ROW 2 COL 5
    skip(0.5)
     btn-Src COLON 20 SPACE(40) btn-exit
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Listing".

DEFINE FRAME frm-dbsrc 
    brw-dbsrc AT ROW 2 COL 5
    skip(0.5)
     btn-exit COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Source Transaction Listing".

DEFINE FRAME frm-src 
    brw-src AT ROW 2 COL 5
    skip(0.5)
     btn-exit COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Source Transaction Listing".

DEFINE FRAME frm-tb 
    brw-tb AT ROW 2 COL 5
    skip(0.5)
    tAmt[1] AT ROW 20.5 COL 57.5 NO-LABEL
    tAmt[2] AT ROW 20.5 COL 76 NO-LABEL
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 22 COL 3  
    btn-Prn AT ROW 22.6 COL 15 SPACE(22)
    btn-Detail SPACE(22) 
    btn-exit
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SIZE 100 BY 25 SCROLLABLE CENTERED TITLE wsTitle.

DEF FRAME frm-main
    SKIP(2)
    wsYear    LABEL "Accounting Year" COLON 30 
    wsMonth   LABEL "Accounting Month"  
     SKIP(0.5)
    wsOpt    LABEL  "Selection Report Option" COLON 30 SKIP(0.5)
    st-Proj   LABEL "Start Project"   COLON 30 SKIP(0.5)
    end-Proj  LABEL  "End Project"    COLON 30
        SKIP(0.5)
    st-Fund   LABEL "Start Segment"   COLON 30 SKIP(0.5)
    end-Fund  LABEL  "End Segment"    COLON 30
        SKIP(0.5)
    st-Dept   LABEL "Start Department"   COLON 30 SKIP(0.5)
    end-Dept  LABEL  "End Department"    COLON 30
        SKIP(1)
    btn-ok AT ROW 20.7 COL 20 LABEL "VIEW"
    SPACE(50) btn-exit SKIP(1)
    rec-2 AT ROW 1.4 COL 3
    rec-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 23.4
    TITLE "TRIAL BALANCE REPORT" VIEW-AS DIALOG-BOX KEEP-TAB-ORDER.


ON CHOOSE OF btn-src IN FRAME frm-Trans 
  DO:
     GET CURRENT qry-Trans NO-WAIT.
    RUN creData-ip.
     /* VIEW FRAME frm-src.
     ENABLE ALL WITH FRAME frm-src.
     OPEN QUERY qry-src FOR EACH tmpsrc NO-LOCK.
     WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-src 
              OR CLOSE OF THIS-PROCEDURE IN FRAME frm-src.
     CLOSE QUERY qry-src.
     HIDE FRAME frm-src. */
     RETURN.
END.

ON CHOOSE OF btn-Detail IN FRAME frm-tb 
  DO:
    FOR EACH tmptdf.
        DELETE tmptdf.
    END.
     GET CURRENT qry-Tb NO-WAIT.
     CREATE tmptdf.
     ASSIGN tmptdf.acct = glbal.acct
     tmptdf.SOURCE = "GL"
     tmptdf.period = INT(string(wsYear) + "01")
     tmptdf.trDATE = DATE("01/01/" + string(wsYear))
     tmptdf.REF    = ""
     tmptdf.DESCRIP = "Balance B/F"
     tmptdf.AMT     = glbal.BFBAL
     tmptdf.CREDATE = TODAY.
     FOR EACH gltdf WHERE gltdf.acct = glbal.acct 
         AND gltdf.period <= st-per AND gltdf.period >= INT(STRING(wsYear) + "01")  NO-LOCK.
         CREATE tmptdf.
         ASSIGN tmptdf.acct    = gltdf.acct
                tmptdf.SOURCE  = gltdf.SOURCE
                tmptdf.period  = gltdf.period
                tmptdf.trDATE  = gltdf.trdate
                tmptdf.REF     = gltdf.ref
                tmptdf.DESCRIP = gltdf.descrip
                tmptdf.AMT     = gltdf.Amt
                tmptdf.CREDATE = gltdf.credate
                tmptdf.transId = gltdf.transId.
     END.
     VIEW FRAME frm-trans.
     ENABLE ALL WITH FRAME frm-trans.
     OPEN QUERY qry-trans FOR EACH tmptdf WHERE tmptdf.acct = glbal.acct 
         AND tmptdf.period <= st-per AND tmptdf.period >= INT(STRING(wsYear) + "01")  NO-LOCK
         BY  tmptdf.period BY  tmptdf.trDate.
     WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-trans 
              OR CLOSE OF THIS-PROCEDURE IN FRAME frm-trans.
     CLOSE QUERY qry-trans.
     HIDE FRAME frm-trans.
     RETURN.
END.

ON 'choose':U OF btn-ok  IN FRAME frm-main
DO:
    ASSIGN wsYear   = INT(wsYear:SCREEN-VALUE IN FRAME frm-main)
           wsMonth  = INT(wsMonth:SCREEN-VALUE IN FRAME frm-main)
           st-per   = DEC(STRING(wsYear) + STRING(wsMonth,"99")).
    wsTitle = "TRIAL BALANCE FOR THE PERIOD ENDED " + STRING(wsYear) + STRING(wsMonth,"99").
    FOR EACH glbal WHERE glbal.YEAR = wsYear:
            ASSIGN wsAmt = glbal.bfbal.
        DO X = 1 TO wsMonth:
            wsAmt = wsAmt + glbal.amt[X].
        END.
        IF wsAmt <= 0 THEN
            ASSIGN tAmt[2] = tAmt[2] + wsAmt.
        ELSE IF wsAmt > 0 THEN
            ASSIGN tAmt[1] = tAmt[1] + wsAmt.
    END.
    VIEW FRAME frm-tb.
    DISPLAY TAmt WITH FRAME frm-tb.
    OPEN QUERY qry-tb FOR EACH glbal WHERE glbal.YEAR = wsYear NO-LOCK.
    ENABLE ALL WITH FRAME frm-tb.
    WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-tb.
    HIDE FRAME frm-tb.
    RETURN.
END.

ON ROW-DISPLAY OF brw-tb
DO:
    FIND FIRST glmf WHERE glmf.acct = glbal.acct NO-LOCK NO-ERROR.
    ASSIGN wsAmt = glbal.bfbal
           wsDes = glmf.DESCRIPTION.
    DO X = 1 TO wsMonth:
        wsAmt = wsAmt + glbal.amt[X].
    END.
    IF wsAmt <= 0 THEN
        ASSIGN wsCr = wsAmt
               wsDr = 0.
    ELSE IF wsAmt > 0 THEN
        ASSIGN wsDr = wsAmt
               wsCr = 0.
   IF wsDr = 0 AND wsCr = 0 THEN NEXT.
END.


/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
FIND FIRST simctr.
ASSIGN wsYear:SCREEN-VALUE = STRING(YEAR(TODAY))
       wsMonth:SCREEN-VALUE = STRING(MONTH(TODAY))
       st-proj:SCREEN-VALUE = "00"
       end-Proj:SCREEN-VALUE = "99"
       st-Dept:SCREEN-VALUE = "0000"
       end-Dept:SCREEN-VALUE = "9999"
       st-Fund:SCREEN-VALUE = "0000"
       end-Fund:SCREEN-VALUE = "99"
       wsTitle = simctr.CONAME.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE CreData-ip:
    FOR EACH tmpsrc.
        DELETE tmpsrc.
    END.
    CASE tmptdf.SOURCE:
        WHEN "DB" THEN DO:
         VIEW FRAME frm-dbsrc.
         ENABLE ALL WITH FRAME frm-dbsrc.
         FIND FIRST dbsgr WHERE dbsgr.ctrledger = tmptdf.acct NO-LOCK NO-ERROR.
         OPEN QUERY qry-dbsrc FOR EACH bfrdbhtf WHERE bfrdbhtf.TransId = tmptdf.transId AND bfrdbhtf.Sgrp = dbsgr.sgrp
                                 AND bfrdbhtf.Accper = tmptdf.per USE-INDEX dbacc NO-LOCK.
         WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-dbsrc 
                  OR CLOSE OF THIS-PROCEDURE IN FRAME frm-dbsrc.
         CLOSE QUERY qry-dbsrc.
         HIDE FRAME frm-dbsrc.
            /*FOR EACH bfrdbhtf WHERE bfrdbhtf.TransId = tmptdf.transId /*AND bfrdbhtf.Sgrp = dbsgr.sgrp */ 
                                 AND bfrdbhtf.Accper = tmptdf.per NO-LOCK:
                     CREATE tmpsrc.
                     ASSIGN tmpsrc.acct    = bfrdbhtf.dbacc
                            tmpsrc.SOURCE  = tmptdf.SOURCE
                            tmpsrc.period  = bfrdbhtf.Accper
                            tmpsrc.trDATE  = bfrdbhtf.trdate
                            tmpsrc.REF     = bfrdbhtf.ref
                            tmpsrc.DESCRIP = bfrdbhtf.descrip
                            tmpsrc.AMT     = bfrdbhtf.amt
                            tmpsrc.vat     = bfrdbhtf.vat
                            tmpsrc.seq     = bfrdbhtf.sgrp
                            tmpsrc.decrate = bfrdbhtf.decRate
                            tmpsrc.transId = bfrdbhtf.transId.
            END. */
        END.
        WHEN "HS" THEN DO:
           FOR EACH hsehtf WHERE hsehtf.TransId = tmptdf.transId /*AND hsehtf.iLedger = glbal.acct */ 
                             AND hsehtf.Accper = tmptdf.per  NO-LOCK:
                  CREATE tmpsrc.
                  ASSIGN tmpsrc.acct    = hsehtf.dbacc
                         tmpsrc.SOURCE  = tmptdf.SOURCE
                         tmpsrc.period  = hsehtf.Accper
                         tmpsrc.trDATE  = hsehtf.trdate
                         tmpsrc.REF     = hsehtf.ref
                         tmpsrc.DESCRIP = hsehtf.descrip
                         tmpsrc.AMT     = hsehtf.amt
                         tmpsrc.seq     = hsehtf.Scheme
                        tmpsrc.decrate  = hsehtf.cRate
                       tmpsrc.transId   = hsehtf.transId.
           END.
        END.
        WHEN "CB" THEN DO:
              FOR EACH cbtrans WHERE cbtrans.TransId = tmptdf.transId /*AND cbTrans.Ledger = glbal.acct AND cbtrans.Ref = tmptdf.Ref*/ 
                                 AND cbTrans.Accper = tmptdf.per  NO-LOCK:
                       CREATE tmpsrc.
                       ASSIGN tmpsrc.acct    = cbtrans.bank
                              tmpsrc.SOURCE  = tmptdf.SOURCE
                              tmpsrc.period  = cbTrans.Accper
                              tmpsrc.trDATE  = cbtrans.trdate
                              tmpsrc.REF     = cbtrans.ref
                              tmpsrc.DESCRIP = cbtrans.descrip
                              tmpsrc.AMT     = cbtrans.amount
                              tmpsrc.seq     = cbtrans.seq
                              tmpsrc.decrate = cbtrans.decRate
                              tmpsrc.transId = cbtrans.transId.
              END.
        END.
        WHEN "RE" THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.ctrLedger = tmptdf.acct NO-LOCK NO-ERROR.
            IF AVAILABLE dbsgr THEN DO:
                  FOR EACH bfrdbhtf WHERE bfrdbhtf.TransId = tmptdf.transId AND bfrdbhtf.sgrp = dbsgr.sgrp
                                        AND bfrdbhtf.Accper = tmptdf.per NO-LOCK:
                          CREATE tmpsrc.
                          ASSIGN tmpsrc.acct    = bfrdbhtf.dbacc
                                 tmpsrc.SOURCE  = tmptdf.SOURCE
                                 tmpsrc.period  = bfrdbhtf.Accper
                                 tmpsrc.trDATE  = bfrdbhtf.trdate
                                 tmpsrc.REF     = bfrdbhtf.ref
                                 tmpsrc.DESCRIP = bfrdbhtf.descrip
                                 tmpsrc.AMT     = bfrdbhtf.amt
                                 tmpsrc.seq     = bfrdbhtf.sgrp
                                 tmpsrc.decrate = bfrdbhtf.decRate
                                 tmpsrc.transId = bfrdbhtf.transId.
                  END.
            END.
            ELSE DO:
                FIND FIRST hsesch WHERE hsesch.CtrLedger = tmptdf.acct NO-LOCK NO-ERROR.
                IF AVAILABLE hsesch THEN DO:
                      FOR EACH hsehtf WHERE hsehtf.TransId = tmptdf.transId /*AND hsehtf.iLedger = glbal.acct */ 
                                 AND hsehtf.Accper = tmptdf.per  NO-LOCK:
                      CREATE tmpsrc.
                      ASSIGN tmpsrc.acct    = hsehtf.dbacc
                             tmpsrc.SOURCE  = tmptdf.SOURCE
                             tmpsrc.period  = hsehtf.Accper
                             tmpsrc.trDATE  = hsehtf.trdate
                             tmpsrc.REF     = hsehtf.ref
                             tmpsrc.DESCRIP = hsehtf.descrip
                             tmpsrc.AMT     = hsehtf.amt
                             tmpsrc.seq     = hsehtf.Scheme
                            tmpsrc.decrate  = hsehtf.cRate.
                      END.
                END.
                ELSE DO:
                    FOR EACH cbtrans WHERE cbtrans.TransId = tmptdf.transId AND cbTrans.Ledger = tmptdf.acct /* AND cbtrans.Ref = tmptdf.Ref*/ 
                                 AND cbTrans.Accper = tmptdf.per  NO-LOCK:
                       CREATE tmpsrc.
                       ASSIGN tmpsrc.acct    = cbtrans.bank
                              tmpsrc.SOURCE  = tmptdf.SOURCE
                              tmpsrc.period  = cbTrans.Accper
                              tmpsrc.trDATE  = cbtrans.trdate
                              tmpsrc.REF     = cbtrans.ref
                              tmpsrc.DESCRIP = cbtrans.descrip
                              tmpsrc.AMT     = cbtrans.amount
                              tmpsrc.seq     = cbtrans.seq
                              tmpsrc.decrate = cbtrans.decRate
                              tmpsrc.transId = cbtrans.transId.
                    END.
                END.
            END.
        END.
        OTHERWISE DO:
            MESSAGE "Unidentified source " VIEW-AS ALERT-BOX.
        END.
    END CASE.
       /*
            FIND FIRST cbkmf WHERE cbkmf.Ledger = glbal.acct NO-LOCK NO-ERROR.
            IF NOT AVAILABLE cbkmf THEN DO:
                MESSAGE "I/E Control" VIEW-AS ALERT-BOX.
                IF (glbal.acct = SIMCTR.VAT[1] OR glbal.acct = SIMCTR.VAT[2]
                    OR glbal.acct = SIMCTR.VAT[3] OR glbal.acct = SIMCTR.VAT[4]) THEN DO:
               
                    FOR EACH cbtrans WHERE cbtrans.TransId = tmptdf.transId AND cbTrans.Ledger = glbal.acct /*AND cbtrans.Ref = tmptdf.Ref*/ 
                                        AND cbTrans.Accper <= st-per AND cbTrans.Accper >= INT(STRING(wsYear) + "01") NO-LOCK:
                           CREATE tmpsrc.
                           ASSIGN tmpsrc.acct    = cbtrans.bank
                                  tmpsrc.SOURCE  = tmptdf.SOURCE
                                  tmpsrc.period  = cbTrans.Accper
                                  tmpsrc.trDATE  = cbtrans.trdate
                                  tmpsrc.REF     = cbtrans.ref
                                  tmpsrc.DESCRIP = cbtrans.descrip
                                  tmpsrc.AMT     = cbtrans.amount
                                  tmpsrc.seq     = cbtrans.seq
                                  tmpsrc.decrate = cbtrans.decRate
                                  tmpsrc.transId = cbtrans.transId.
                       END.
                END. */
END PROCEDURE.

