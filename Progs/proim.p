/*....Program   :ProImp.p
      Notes     : Migrate Promun Data to the SimAcc system.
*************************************************************/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsAcc   LIKE dbcmf.dbacc.
DEF VAR wsstatus  LIKE dbcmf.dbacc.
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wstar LIKE dbtmf.Tarif.
DEF VAR wscnt AS DEC.
DEF VAR wsrate AS DEC FORM ">>>>>9.999999-".

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEFINE FRAME frm-main
    SKIP(1)
    wsAcc   COLON 30 LABEL "Enter Start Account" SKIP(.5)
    wsPer     COLON 30 LABEL "Enter Accounting Period" SKIP(.5)
    wsDate    COLON 30 LABEL "Enter Transaction Date" SKIP(.5)
    wsRate    COLON 30 LABEL "Enter Processing USD rate" SKIP(.5)
    wsStatus  COLON 5 NO-LABEL view-as text "Processing...." 
    skip(3.5)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "Promun Account Importation".


ON CHOOSE OF btn-oK IN FRAME frm-main DO:
    ASSIGN wsdate
           wsAcc = DEC(wsAcc:SCREEN-VALUE)
           wsPer = INT(wsPer:SCREEN-VALUE)
           wsRate.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = wsacc NO-ERROR.
    IF AVAILABLE dbcmf THEN DO:
        MESSAGE "Account has already been imported.." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
         session:set-wait-state("").
                RUN Imp-ip.
                MESSAGE "IMPORT OF DATA COMPLETED..." VIEW-AS ALERT-BOX.
        RETURN.
    END.
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsAcc:SCREEN-VALUE = "0".
       
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Imp-ip:
/* Ratepayer Account */
FIND LAST muntmf WHERE muntmf.TYPE = 3 NO-ERROR.
IF AVAILABLE muntmf THEN
    wstar = muntmf.tarif.
FOR EACH muncmf WHERE company = 0 AND acc = wsAcc NO-LOCK:
    ASSIGN wscnt = wscnt + 1.
    DISPLAY muncmf.acc WITH TITLE "Master file". PAUSE 0.
    CREATE dbcmf.
    ASSIGN dbcmf.AccBal     = muncmf.balance
           dbcmf.AccStat    = muncmf.active
           dbcmf.Add1       = muncmf.addr1
           dbcmf.add2       = muncmf.addr2
           dbcmf.Add3       = muncmf.addr3
           dbcmf.BalBf      = muncmf.bf-bal
           dbcmf.Cell       = DEC(muncmf.cell)
           dbcmf.Cons       = muncmf.cons-code           
           dbcmf.Credate    = muncmf.eff-date
           dbcmf.dbAcc      = muncmf.acc
           dbcmf.DeedNo     = muncmf.deed-no
           dbcmf.email      = muncmf.eMail-acc
           dbcmf.emailAdd   = muncmf.email
           dbcmf.Name       = muncmf.NAME    
           dbcmf.RegID      = muncmf.idno
           dbcmf.SiteValue  = muncmf.nr-site
           dbcmf.Size       = muncmf.area
           dbcmf.Sms        = muncmf.sms-acc
           dbcmf.SortName   = muncmf.sortname
           dbcmf.StandNo    = STRING(muncmf.erf-no)
           dbcmf.Suburb     = muncmf.suburb
           dbcmf.Usr        = "1"
           dbcmf.VatNo      = INT(muncmf.vat-reg-no)
           dbcmf.Ward       = INT(muncmf.ward)
           dbcmf.BldValue   = muncmf.nr-bldg
           dbcmf.Rate-tarif = muncmf.rates-tarif
           dbcmf.RNextP     = wsPer
           dbcmf.POwner     = YES WHEN muncmf.OWNER = "O"
           dbcmf.POwner     = NO WHEN muncmf.OWNER = "T".
    j = 0.
    DO X = 1 TO 6:
        IF muncmf.sv-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = muncmf.sv-tar[X]
                   dbcmf.Units[j] = muncmf.sv-pts[X]
                   dbcmf.NextP[X] = wsPer.
        END.
       IF muncmf.av-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = wstar + muncmf.av-tar[X]
                   dbcmf.Units[j] = muncmf.av-pts[X]
                   dbcmf.NextP[X] = wsPer.
        END.    
    END.
END.

FOR EACH munbmf WHERE  munbmf.company = 0 AND  munbmf.acc = wsacc NO-LOCK:
    wscnt = wscnt + 1.
    DISPLAY munbmf.acc WITH TITLE "Balance file". PAUSE 0.
    CREATE dbblf.
    ASSIGN dbblf.dbAcc = munbmf.acc
           dbblf.Sgrp = munbmf.TYPE.
    DO X = 1 TO 14:
        ASSIGN dbblf.amt[X] = munbmf.bal[X]
               dbblf.Int    = dbblf.Int  + munbmf.Interest[X]
               dbblf.dbamt[1] = dbblf.dbamt[1] + munbmf.bal[X] + munbmf.Interest[X].
    END.
    dbblf.dbamt[2] =ROUND((dbblf.dbamt[2] * wsRate),2).
END.

FOR EACH munrmf WHERE munrmf.company = 0 AND munrmf.acc = wsAcc AND ew = "w" NO-LOCK:
    wscnt = wscnt + 1.
    DISPLAY munrmf.acc WITH TITLE "Meter file". 
    PAUSE 0.
    CREATE dbmmf.
    ASSIGN dbmmf.dbAcc   = munrmf.acc
           dbmmf.Meter   = munrmf.meter-id /* INT(munrmf.meter-no) */
           dbmmf.mStats  = 1 WHEN meter-blocked = NO
           dbmmf.mStats  = 2 WHEN meter-blocked = YES
           dbmmf.Route   =  munrmf.route
           dbmmf.Serial  = STRING(munrmf.meter-id) 
           dbmmf.Tarif   = munrmf.tarif
           dbmmf.wNextP  = wsPer.
    DO X = 1 TO 13:
        ASSIGN dbmmf.READ[X] = munrmf.READ[X]
               dbmmf.rdate[X] = munrmf.read-date[X].
               
    END.
    ASSIGN dbmmf.consum[13] = dbmmf.READ[13] - dbmmf.READ[12] WHEN dbmmf.READ[13] <> 0
           dbmmf.consum[12] = dbmmf.READ[12] - dbmmf.READ[11] WHEN dbmmf.READ[12] <> 0
           dbmmf.consum[11] = dbmmf.READ[11] - dbmmf.READ[10] WHEN dbmmf.READ[11] <> 0
           dbmmf.consum[10] = dbmmf.READ[10] - dbmmf.READ[9] WHEN dbmmf.READ[10] <> 0
           dbmmf.consum[9] = dbmmf.READ[9] - dbmmf.READ[8] WHEN dbmmf.READ[9] <> 0
           dbmmf.consum[8] = dbmmf.READ[8] - dbmmf.READ[7] WHEN dbmmf.READ[8] <> 0
           dbmmf.consum[7] = dbmmf.READ[7] - dbmmf.READ[6] WHEN dbmmf.READ[7] <> 0
           dbmmf.consum[6] = dbmmf.READ[6] - dbmmf.READ[5] WHEN dbmmf.READ[6] <> 0
           dbmmf.consum[5] = dbmmf.READ[5] - dbmmf.READ[4] WHEN dbmmf.READ[5] <> 0
           dbmmf.consum[4] = dbmmf.READ[4] - dbmmf.READ[3] WHEN dbmmf.READ[4] <> 0
           dbmmf.consum[3] = dbmmf.READ[3] - dbmmf.READ[2] WHEN dbmmf.READ[3] <> 0
           dbmmf.consum[2] = dbmmf.READ[2] - dbmmf.READ[1] WHEN dbmmf.READ[2] <> 0.
END.

FOR EACH munthf WHERE munthf.company = 0 AND munthf.acc = wsAcc:
    DISPLAY munthf.acc period WITH TITLE "Historical Detors Data". 
    PAUSE 0.
    CREATE dbhtf.
    ASSIGN dbhtf.Accper     = munthf.period
               dbhtf.Amt        = munthf.amt
               dbhtf.dbAcc      = munthf.acc
               dbhtf.DESCRIP    = munthf.trans-desc
               dbhtf.Iledger    = DEC(munthf.gl-acct-no)
               dbhtf.PoDate     = munthf.eff-date
               dbhtf.prog       = munthf.prog
               dbhtf.Ref        = munthf.ref
               dbhtf.Seq        =  munthf.seq-no 
               dbhtf.Sgrp       = munthf.bmf-type
               dbhtf.Tarif      = munthf.tarif
               dbhtf.TransID    = munthf.link-no
               dbhtf.trDate     = munthf.tr-date
               dbhtf.UID        = STRING(munthf.signon)
               dbhtf.UID2       = "1"
               dbhtf.Vat        = munthf.vat-amt
               dbhtf.type       = 0.
END.
END PROCEDURE.
