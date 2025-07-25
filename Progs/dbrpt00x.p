/* Program.................dbrpt00.p
   Notes:................. Debtors Master File details
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsfac AS INTEGER.
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsStatus AS CHAR.
DEF VAR  wsSub LIKE dbsmf.descrip.
DEF VAR  wsWard LIKE dbsmf.descrip.
DEF VAR  wsTar LIKE dbsmf.descrip.
DEF VAR varStat     AS CHAR FORM "x(1)" INITIAL "A".
DEF VAR X AS INT.
DEF TEMP-TABLE tmptmf
    FIELD unit AS INT
    FIELD descrip LIKE dbtmf.descrip
    FIELD charge LIKE dbtmf.charge.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    varStat     COLON 30         LABEL "Account Status" AUTO-RETURN
               view-as combo-box size 20 by 2 LIST-ITEM-PAIRS
      "A - All","A","O - Active","O", "F - Finilisation","F" , "C - Closed","C" 
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "DEBTORS MASTERFILE STATIC DETAILS REPORT" VIEW-AS DIALOG-BOX.

FORM
    SKIP (0.5)
     "------------------ACCOUNT DETAILS ------------------" COLON 10 SKIP(2)
    "ACCOUNT:" COLON 5 dbcmf.dbacc
     "ACCOUNT NAME: "   dbcmf.NAME
    "SORT NAME: "     COLON 77.5 dbcmf.Sortname  SKIP
    "ID/REG NO.: "     COLON 5 dbcmf.RegID  
    "VAT REG. NO.: "    COLON 77.5 dbcmf.VatNo SKIP
    "ADDRESS: "         colon 5 dbcmf.Add1 
    dbcmf.Add2         colon 15
    dbcmf.Add3         colon 15 SKIP
    dbcmf.cell         COLON 15   
   dbcmf.emailAdd      COLON 15   SKIP(2)
    
    "-------------PROPERTY DETAILS ---------------" COLON 20 SKIP(1)
   "STAND NUMBER: "   COLON 5 dbcmf.standNo
   "PROPERTY OWNER: "   COLON 35 dbcmf.POwner
    "DEED NUMBER: "   COLON 65 dbcmf.DeedNo   SKIP
    "SURBUB: "        COLON 5 dbsmf.Descrip
    "WARD: "          COLON 65 dbwrd.Descrip SKIP
    "RATES DETAILS - TARIF     :" COLON 5 dbtmf.Descrip
    "                STAND SIZE:" COLON 5 dbcmf.SIZE      SKIP
    "                SITE VALUE:"  COLON 5 dbcmf.sitevalue SKIP
    "            BUILDING VALUE:" COLON 5 dbcmf.bldvalue    SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX NO-LABELS
    WIDTH 132 FRAME frm-rpt.

FORM 
      dbtmf.Descrip LABEL "ATTACHED TARIFS" COLON 5
      dbtmf.Charge  LABEL "CHARGE"          COLON 50
      wsFac         LABEL "AFCTOR"          COLON 63
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX NO-LABELS
    WIDTH 132 FRAME frm-detail.

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsStart wsEnd varStat.
   IF varStat = "A" THEN DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
   END.
   IF varStat = "O" THEN DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="report1.ip"
                    &paged} 
   END.
   IF varStat = "F" THEN DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="report2.ip"
                    &paged} 
   END.
   IF varStat = "C" THEN DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="report3.ip"
                    &paged} 
   END.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
    FOR EACH dbcmf WHERE dbacc >= wsStart AND dbAcc <= wsEnd:
        FIND FIRST dbsmf WHERE dbsmf.Suburb = dbcmf.Suburb NO-LOCK NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsSub = dbsmf.descrip.
            ELSE   wsSub = "N/A".
        FIND FIRST dbwrd WHERE dbwrd.Ward = dbcmf.Ward NO-LOCK NO-ERROR.
        IF AVAILABLE dbwrd THEN
            wsward = dbwrd.descrip.
            ELSE   wsWard = "N/A".
        FIND FIRST dbtmf WHERE dbcmf.Rate-tarif = dbtmf.Tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE dbtmf THEN
            wsTar = dbtmf.descrip.
            ELSE   wsTar = "N/A".
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.Name dbcmf.POwner dbcmf.RegID 
            dbcmf.SiteValue dbcmf.Size dbcmf.SortName dbcmf.StandNo dbcmf.VatNo 
            dbcmf.emailAdd dbcmf.DeedNo dbcmf.Cell dbcmf.BldValue dbcmf.Add3 
            dbcmf.add2 dbcmf.Add1 wsward @ dbwrd.descrip wsSub @ dbsmf.descrip 
            wsTar @ dbtmf.Descrip WITH FRAME frm-rpt.
        DO X = 1 TO 14:
            IF dbcmf.tarif[X] <> 0 AND unit[X] <> 0 THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
                IF AVAILABLE dbtmf THEN DO:
                    CREATE tmptmf.
                    ASSIGN tmptmf.descrip = dbtmf.Descrip
                           tmptmf.charge =  dbtmf.Charge
                           tmptmf.unit   =  dbcmf.unit[X].
                END.
            END.
        END.
        FOR EACH tmptmf:
             DISPLAY STREAM a tmptmf.descrip @ dbtmf.Descrip tmptmf.charge @ dbtmf.Charge 
                              tmptmf.unit @ wsFac WITH FRAME frm-detail.
             DOWN STREAM a WITH FRAME frm-detail.
             DELETE tmptmf.
        END.
        PAGE STREAM a.
    END.
END.

PROCEDURE report1.ip:
    FOR EACH dbcmf WHERE dbacc >= wsStart AND dbAcc <= wsEnd AND Accstat = 0:
        FIND FIRST dbsmf WHERE dbsmf.Suburb = dbcmf.Suburb NO-LOCK NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsSub = dbsmf.descrip.
            ELSE   wsSub = "N/A".
        FIND FIRST dbwrd WHERE dbwrd.Ward = dbcmf.Ward NO-LOCK NO-ERROR.
        IF AVAILABLE dbwrd THEN
            wsward = dbwrd.descrip.
            ELSE   wsWard = "N/A".
        FIND FIRST dbtmf WHERE dbcmf.Rate-tarif = dbtmf.Tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE dbtmf THEN
            wsTar = dbtmf.descrip.
            ELSE   wsTar = "N/A".
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.Name dbcmf.POwner dbcmf.RegID 
            dbcmf.SiteValue dbcmf.Size dbcmf.SortName dbcmf.StandNo dbcmf.VatNo 
            dbcmf.emailAdd dbcmf.DeedNo dbcmf.Cell dbcmf.BldValue dbcmf.Add3 
            dbcmf.add2 dbcmf.Add1 wsward @ dbwrd.descrip wsSub @ dbsmf.descrip 
            wsTar @ dbtmf.Descrip WITH FRAME frm-rpt.
        DO X = 1 TO 14:
            IF dbcmf.tarif[X] <> 0 AND unit[X] <> 0 THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
                IF AVAILABLE dbtmf THEN DO:
                    CREATE tmptmf.
                    ASSIGN tmptmf.descrip = dbtmf.Descrip
                           tmptmf.charge =  dbtmf.Charge
                           tmptmf.unit   =  dbcmf.unit[X].
                END.
            END.
        END.
        FOR EACH tmptmf:
             DISPLAY STREAM a tmptmf.descrip @ dbtmf.Descrip tmptmf.charge @ dbtmf.Charge 
                              tmptmf.unit @ wsFac WITH FRAME frm-detail.
             DOWN STREAM a WITH FRAME frm-detail.
             DELETE tmptmf.
        END.
        PAGE STREAM a.
    END.
END.

PROCEDURE report2.ip:
    FOR EACH dbcmf WHERE dbacc >= wsStart AND dbAcc <= wsEnd AND Accstat = 95:
        FIND FIRST dbsmf WHERE dbsmf.Suburb = dbcmf.Suburb NO-LOCK NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsSub = dbsmf.descrip.
            ELSE   wsSub = "N/A".
        FIND FIRST dbwrd WHERE dbwrd.Ward = dbcmf.Ward NO-LOCK NO-ERROR.
        IF AVAILABLE dbwrd THEN
            wsward = dbwrd.descrip.
            ELSE   wsWard = "N/A".
        FIND FIRST dbtmf WHERE dbcmf.Rate-tarif = dbtmf.Tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE dbtmf THEN
            wsTar = dbtmf.descrip.
            ELSE   wsTar = "N/A".
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.Name dbcmf.POwner dbcmf.RegID 
            dbcmf.SiteValue dbcmf.Size dbcmf.SortName dbcmf.StandNo dbcmf.VatNo 
            dbcmf.emailAdd dbcmf.DeedNo dbcmf.Cell dbcmf.BldValue dbcmf.Add3 
            dbcmf.add2 dbcmf.Add1 wsward @ dbwrd.descrip wsSub @ dbsmf.descrip 
            wsTar @ dbtmf.Descrip WITH FRAME frm-rpt.
        DO X = 1 TO 14:
            IF dbcmf.tarif[X] <> 0 AND unit[X] <> 0 THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
                IF AVAILABLE dbtmf THEN DO:
                    CREATE tmptmf.
                    ASSIGN tmptmf.descrip = dbtmf.Descrip
                           tmptmf.charge =  dbtmf.Charge
                           tmptmf.unit   =  dbcmf.unit[X].
                END.
            END.
        END.
        FOR EACH tmptmf:
             DISPLAY STREAM a tmptmf.descrip @ dbtmf.Descrip tmptmf.charge @ dbtmf.Charge 
                              tmptmf.unit @ wsFac WITH FRAME frm-detail.
             DOWN STREAM a WITH FRAME frm-detail.
             DELETE tmptmf.
        END.
        PAGE STREAM a.
    END.
END.

PROCEDURE report3.ip:
    FOR EACH dbcmf WHERE dbacc >= wsStart AND dbAcc <= wsEnd AND Accstat = 99:
        FIND FIRST dbsmf WHERE dbsmf.Suburb = dbcmf.Suburb NO-LOCK NO-ERROR.
        IF AVAILABLE dbsmf THEN
            wsSub = dbsmf.descrip.
            ELSE   wsSub = "N/A".
        FIND FIRST dbwrd WHERE dbwrd.Ward = dbcmf.Ward NO-LOCK NO-ERROR.
        IF AVAILABLE dbwrd THEN
            wsward = dbwrd.descrip.
            ELSE   wsWard = "N/A".
        FIND FIRST dbtmf WHERE dbcmf.Rate-tarif = dbtmf.Tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE dbtmf THEN
            wsTar = dbtmf.descrip.
            ELSE   wsTar = "N/A".
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.Name dbcmf.POwner dbcmf.RegID 
            dbcmf.SiteValue dbcmf.Size dbcmf.SortName dbcmf.StandNo dbcmf.VatNo 
            dbcmf.emailAdd dbcmf.DeedNo dbcmf.Cell dbcmf.BldValue dbcmf.Add3 
            dbcmf.add2 dbcmf.Add1 wsward @ dbwrd.descrip wsSub @ dbsmf.descrip 
            wsTar @ dbtmf.Descrip WITH FRAME frm-rpt.
        DO X = 1 TO 14:
            IF dbcmf.tarif[X] <> 0 AND unit[X] <> 0 THEN DO:
                FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.tarif[X] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
                IF AVAILABLE dbtmf THEN DO:
                    CREATE tmptmf.
                    ASSIGN tmptmf.descrip = dbtmf.Descrip
                           tmptmf.charge =  dbtmf.Charge
                           tmptmf.unit   =  dbcmf.unit[X].
                END.
            END.
        END.
        FOR EACH tmptmf:
             DISPLAY STREAM a tmptmf.descrip @ dbtmf.Descrip tmptmf.charge @ dbtmf.Charge 
                              tmptmf.unit @ wsFac WITH FRAME frm-detail.
             DOWN STREAM a WITH FRAME frm-detail.
             DELETE tmptmf.
        END.
        PAGE STREAM a.
    END.
END.
