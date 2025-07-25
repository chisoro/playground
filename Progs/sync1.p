DEF VAR lName AS CHAR INITIAL "dbAc".
DEF VAR lName1 AS CHAR INITIAL "db03".
DEF VAR wsa AS CHAR FORM "x(20)".
DEF VAR wst AS CHAR FORM "x(20)".
DEF VAR wsp AS CHAR FORM "x(20)".

DEF BUTTON btn-Ok LABEL "OK".
DEF BUTTON btn-exit LABEL "EXIT".

DEFINE RECTANGLE rect1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 2.3.
DEFINE RECTANGLE rect2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 6.5.


DEF  FRAME frm-Main
    "Online Data Exchange" AT ROW 2 COL 10 SKIP (2)
    wsp COLON 7 NO-LABEL VIEW-AS TEXT SKIP(.5)
    "Processing" COLON 7 SPACE(5) wsa NO-LABEL VIEW-AS TEXT SPACE(5) wst NO-LABEL VIEW-AS TEXT
    btn-ok AT ROW 8.7 COL 10
    space(30) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 8 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    /*BGCOLOR 8 FGCOLOR 1 */ SIZE 65 BY 11.3
    TITLE "SIMACC ONLINE DATA EXCHANGE"  VIEW-AS DIALOG-BOX.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
    IF (CONNECTED(LName)) AND (CONNECTED(LName1)) THEN DO:
         MESSAGE "Connected to Online Landing Database" VIEW-AS ALERT-BOX.
         RUN sysnc.ip.
         APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
       HIDE FRAME frm-main.
       RETURN.
    END.
    ELSE IF (NOT CONNECTED(LName)) AND (NOT CONNECTED(LName1)) THEN DO:
         CONNECT -db dbAc    -L 40000 -H 192.168.1.20 -S 19688 -U "Simacc" -P Sim6771 -N TCP NO-ERROR.
         CONNECT -db db03    -L 40000 -H 192.168.1.20 -S 19691 -U "Simacc" -P Sim6771 -N TCP NO-ERROR.
        
         IF (CONNECTED(LName)) AND (CONNECTED(LName1)) THEN DO:
             RUN sysnc.ip.
             APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
         HIDE FRAME frm-main.
         RETURN.
         END.
         ELSE IF (NOT CONNECTED(lName)) AND (NOT CONNECTED(lName1)) THEN DO:
           MESSAGE "Failed to connect to Online Landing Database.....Exiting" VIEW-AS ALERT-BOX.
           APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-main.
           HIDE FRAME frm-main.
        END.
        RETURN.
     END.
    
END.


/********** MAIN LOGIC **********/
VIEW FRAME frm-Main.
ENABLE ALL WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Main.
HIDE FRAME frm-Main.



PROCEDURE sysnc.ip.
    /*Forex exchange rates*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Forex Exchange".
     FOR EACH dbAcc.tblForex NO-LOCK:
        FIND FIRST dbAc.tblForex WHERE dbAc.tblForex.txtCur = dbAcc.tblForex.txtCur NO-ERROR.
        IF  NOT AVAILABLE dbAc.tblForex THEN DO:
            CREATE dbAc.tblForex.
            ASSIGN
                dbAc.tblForex.CreDate = dbAcc.tblForex.CreDate
                dbAc.tblForex.decRate = dbAcc.tblForex.decRate
                dbAc.tblForex.DeviceName = dbAcc.tblForex.DeviceName
                dbAc.tblForex.DtRate = dbAcc.tblForex.DtRate 
                dbAc.tblForex.txtCur = dbAcc.tblForex.txtCur
                dbAc.tblForex.UID = dbAcc.tblForex.UID.
        END.
        IF AVAILABLE dbAc.tblForex THEN DO:
            ASSIGN dbAc.tblForex.decRate = dbAcc.tblForex.decRate
                dbAc.tblForex.DtRate = dbAcc.tblForex.DtRate.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = dbAcc.tblForex.txtCur. 
        wst:SCREEN-VALUE IN FRAME frm-Main = string(dbAcc.tblForex.decRate).
    END.
 /*client accounts*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Rate Payers".
     FOR EACH db003.dbcmf NO-LOCK:
        FIND FIRST db03.dbcmf WHERE db03.dbcmf.dbAcc = db003.dbcmf.dbAcc NO-ERROR.
        IF  NOT AVAILABLE db03.dbcmf THEN DO:
            CREATE db03.dbcmf.
            ASSIGN
                db03.dbcmf.CreDate = db003.dbcmf.CreDate
                db03.dbcmf.DeviceName = db003.dbcmf.DeviceName
    	        db03.dbcmf.UID = db003.dbcmf.UID
                db03.dbcmf.AccBal  =  db003.dbcmf.AccBal 
                db03.dbcmf.AccStat  =             db003.dbcmf.AccStat 
                db03.dbcmf.Add1  =             db003.dbcmf.Add1 
                db03.dbcmf.add2  =             db003.dbcmf.add2 
                db03.dbcmf.Add3  =             db003.dbcmf.Add3 
                db03.dbcmf.Auth  =             db003.dbcmf.Auth 
                db03.dbcmf.Authdate  =             db003.dbcmf.Authdate 
                db03.dbcmf.BalBf  =             db003.dbcmf.BalBf 
                db03.dbcmf.BldValue  =             db003.dbcmf.BldValue 
                db03.dbcmf.Cell  =             db003.dbcmf.Cell 
                db03.dbcmf.Cons  =             db003.dbcmf.Cons 
                db03.dbcmf.dbAcc  =             db003.dbcmf.dbAcc 
                db03.dbcmf.DeedNo  =             db003.dbcmf.DeedNo 
                db03.dbcmf.DepAmt  =             db003.dbcmf.DepAmt 
                db03.dbcmf.email  =             db003.dbcmf.email 
                db03.dbcmf.emailAdd  =             db003.dbcmf.emailAdd 
                db03.dbcmf.intgrp  =             db003.dbcmf.intgrp 
                db03.dbcmf.LDepAmt  =             db003.dbcmf.LDepAmt 
                db03.dbcmf.lpDate  =             db003.dbcmf.lpDate 
                db03.dbcmf.Name  =             db003.dbcmf.Name 
                db03.dbcmf.NextP[1]  =             db003.dbcmf.NextP[1] 
                db03.dbcmf.NextP[2]  =             db003.dbcmf.NextP[2] 
                db03.dbcmf.NextP[3]  =             db003.dbcmf.NextP[3] 
                db03.dbcmf.NextP[4]  =             db003.dbcmf.NextP[4] 
                db03.dbcmf.NextP[5]  =             db003.dbcmf.NextP[5] 
                db03.dbcmf.NextP[6]  =             db003.dbcmf.NextP[6] 
                db03.dbcmf.NextP[7]  =             db003.dbcmf.NextP[7] 
                db03.dbcmf.NextP[8]  =             db003.dbcmf.NextP[8] 
                db03.dbcmf.NextP[9]  =             db003.dbcmf.NextP[9] 
                db03.dbcmf.NextP[10]  =             db003.dbcmf.NextP[10] 
                db03.dbcmf.NextP[11]  =             db003.dbcmf.NextP[11] 
                db03.dbcmf.NextP[12]  =             db003.dbcmf.NextP[12] 
                db03.dbcmf.NextP[13]  =             db003.dbcmf.NextP[13] 
                db03.dbcmf.NextP[14]  =             db003.dbcmf.NextP[14] 
                db03.dbcmf.POwner  =             db003.dbcmf.POwner 
                db03.dbcmf.printstat  =             db003.dbcmf.printstat 
                db03.dbcmf.Rate-tarif  =             db003.dbcmf.Rate-tarif 
                db03.dbcmf.RegID  =             db003.dbcmf.RegID 
                db03.dbcmf.RNextP  =             db003.dbcmf.RNextP 
                db03.dbcmf.SiteValue  =             db003.dbcmf.SiteValue 
                db03.dbcmf.Size  =             db003.dbcmf.Size 
                db03.dbcmf.Sms  =             db003.dbcmf.Sms 
                db03.dbcmf.SortName  =             db003.dbcmf.SortName 
                db03.dbcmf.StandNo  =             db003.dbcmf.StandNo 
                db03.dbcmf.stno  =             db003.dbcmf.stno 
                db03.dbcmf.street  =             db003.dbcmf.street 
                db03.dbcmf.Suburb  =             db003.dbcmf.Suburb 
                db03.dbcmf.tarif[1]  =             db003.dbcmf.tarif[1] 
                db03.dbcmf.tarif[2]  =             db003.dbcmf.tarif[2] 
                db03.dbcmf.tarif[3]  =             db003.dbcmf.tarif[3] 
                db03.dbcmf.tarif[4]  =             db003.dbcmf.tarif[4] 
                db03.dbcmf.tarif[5]  =             db003.dbcmf.tarif[5] 
                db03.dbcmf.tarif[6]  =             db003.dbcmf.tarif[6] 
                db03.dbcmf.tarif[7]  =             db003.dbcmf.tarif[7] 
                db03.dbcmf.tarif[8]  =             db003.dbcmf.tarif[8] 
                db03.dbcmf.tarif[9]  =             db003.dbcmf.tarif[9] 
                db03.dbcmf.tarif[10]  =             db003.dbcmf.tarif[10] 
                db03.dbcmf.tarif[11]  =             db003.dbcmf.tarif[11] 
                db03.dbcmf.tarif[12]  =             db003.dbcmf.tarif[12] 
                db03.dbcmf.tarif[13]  =             db003.dbcmf.tarif[13] 
                db03.dbcmf.tarif[14]  =             db003.dbcmf.tarif[14] 
                db03.dbcmf.TIN  =             db003.dbcmf.TIN 
                db03.dbcmf.Units[1]  =             db003.dbcmf.Units[1] 
                db03.dbcmf.Units[2]  =             db003.dbcmf.Units[2] 
                db03.dbcmf.Units[3]  =             db003.dbcmf.Units[3] 
                db03.dbcmf.Units[4]  =             db003.dbcmf.Units[4] 
                db03.dbcmf.Units[5]  =             db003.dbcmf.Units[5] 
                db03.dbcmf.Units[6]  =             db003.dbcmf.Units[6] 
                db03.dbcmf.Units[7]  =             db003.dbcmf.Units[7] 
                db03.dbcmf.Units[8]  =             db003.dbcmf.Units[8] 
                db03.dbcmf.Units[9]  =             db003.dbcmf.Units[9] 
                db03.dbcmf.Units[10]  =             db003.dbcmf.Units[10] 
                db03.dbcmf.Units[11]  =             db003.dbcmf.Units[11] 
                db03.dbcmf.Units[12]  =             db003.dbcmf.Units[12] 
                db03.dbcmf.Units[13]  =             db003.dbcmf.Units[13] 
                db03.dbcmf.Units[14]  =             db003.dbcmf.Units[14] 
                db03.dbcmf.VatNo  =             db003.dbcmf.VatNo 
                db03.dbcmf.Ward =             db003.dbcmf.Ward.
        END.
        IF AVAILABLE db03.dbcmf THEN DO:
            ASSIGN db03.dbcmf.NAME = db003.dbcmf.NAME
                db03.dbcmf.RegID = db003.dbcmf.RegId
                db03.dbcmf.AccBal = db003.dbcmf.AccBal.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = string(db003.dbcmf.dbAcc).
        wst:SCREEN-VALUE IN FRAME frm-Main = db003.dbcmf.name. 
    END.

    /*account balance details */
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Account Aging".
    FOR EACH db003.dbblf NO-LOCK:
        FIND FIRST db03.dbblf WHERE db03.dbblf.dbAcc = db003.dbblf.dbAcc AND db03.dbblf.sgrp = db003.dbblf.sgrp NO-ERROR.
        IF  NOT AVAILABLE db03.dbblf THEN DO:
            CREATE db03.dbblf.
            ASSIGN
                db03.dbblf.Amt[1]  = db003.dbblf.Amt[1] 
                db03.dbblf.Amt[2]  =             db003.dbblf.Amt[2] 
                db03.dbblf.Amt[3]  =             db003.dbblf.Amt[3] 
                db03.dbblf.Amt[4]  =             db003.dbblf.Amt[4] 
                db03.dbblf.Amt[5]  =             db003.dbblf.Amt[5] 
                db03.dbblf.Amt[6]  =             db003.dbblf.Amt[6] 
                db03.dbblf.Amt[7]  =             db003.dbblf.Amt[7] 
                db03.dbblf.Amt[8]  =             db003.dbblf.Amt[8] 
                db03.dbblf.Amt[9]  =             db003.dbblf.Amt[9] 
                db03.dbblf.Amt[10]  =             db003.dbblf.Amt[10] 
                db03.dbblf.Amt[11]  =             db003.dbblf.Amt[11] 
                db03.dbblf.Amt[12]  =             db003.dbblf.Amt[12] 
                db03.dbblf.Amt[13]  =             db003.dbblf.Amt[13] 
                db03.dbblf.Amt[14]  =             db003.dbblf.Amt[14] 
                db03.dbblf.Amt[15]  =             db003.dbblf.Amt[15] 
                db03.dbblf.dbAcc  =             db003.dbblf.dbAcc 
                db03.dbblf.dbAmt[1]  =             db003.dbblf.dbAmt[1] 
                db03.dbblf.dbAmt[2]  =             db003.dbblf.dbAmt[2] 
                db03.dbblf.Int  =             db003.dbblf.Int 
                db03.dbblf.lDate  =             db003.dbblf.lDate 
                db03.dbblf.LRate  =             db003.dbblf.LRate 
                db03.dbblf.Sgrp  =             db003.dbblf.Sgrp 
                db03.dbblf.Stat  =             db003.dbblf.Stat 
                db03.dbblf.vat =             db003.dbblf.vat.
        END.
        IF AVAILABLE db03.dbblf THEN DO:
            ASSIGN db03.dbblf.Amt[1]  = db003.dbblf.Amt[1] 
                db03.dbblf.Amt[2]  =             db003.dbblf.Amt[2] 
                db03.dbblf.Amt[3]  =             db003.dbblf.Amt[3] 
                db03.dbblf.Amt[4]  =             db003.dbblf.Amt[4] 
                db03.dbblf.Amt[5]  =             db003.dbblf.Amt[5] 
                db03.dbblf.Amt[6]  =             db003.dbblf.Amt[6] 
                db03.dbblf.Amt[7]  =             db003.dbblf.Amt[7] 
                db03.dbblf.Amt[8]  =             db003.dbblf.Amt[8] 
                db03.dbblf.Amt[9]  =             db003.dbblf.Amt[9] 
                db03.dbblf.Amt[10]  =             db003.dbblf.Amt[10] 
                db03.dbblf.Amt[11]  =             db003.dbblf.Amt[11] 
                db03.dbblf.Amt[12]  =             db003.dbblf.Amt[12] 
                db03.dbblf.Amt[13]  =             db003.dbblf.Amt[13] 
                db03.dbblf.Amt[14]  =             db003.dbblf.Amt[14] 
                db03.dbblf.Amt[15]  =             db003.dbblf.Amt[15] 
                db03.dbblf.dbAmt[1]  =             db003.dbblf.dbAmt[1] 
                db03.dbblf.dbAmt[2]  =             db003.dbblf.dbAmt[2] 
                db03.dbblf.Int  =             db003.dbblf.Int 
                db03.dbblf.lDate  =             db003.dbblf.lDate 
                db03.dbblf.LRate  =             db003.dbblf.LRate 
                db03.dbblf.Stat  =             db003.dbblf.Stat 
                db03.dbblf.vat =             db003.dbblf.vat.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = string(db003.dbblf.dbAcc).
        wst:SCREEN-VALUE IN FRAME frm-Main = STRING(db003.dbblf.sgrp). 
    END.
    /*receipting codes*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Receipting Codes".
    FOR EACH db003.dbRcod NO-LOCK:
        FIND FIRST db03.dbRcod WHERE db03.dbRcod.rCode = db003.dbRcod.rCode NO-ERROR.
        IF  NOT AVAILABLE db03.dbRcod THEN DO:
            CREATE db03.dbRcod.
            ASSIGN
                db03.dbRcod.CreDate = db003.dbRcod.CreDate
                db03.dbRcod.DeviceName = db003.dbRcod.DeviceName
    	        db03.dbRcod.UID = db003.dbRcod.UID
                db03.dbRCod.Acb   = db003.dbRCod.Acb  
                db03.dbRCod.Dept  =             db003.dbRCod.Dept 
                db03.dbRCod.Descrip  =             db003.dbRCod.Descrip 
                db03.dbRCod.Ledger  =             db003.dbRCod.Ledger 
                db03.dbRCod.Proj  =             db003.dbRCod.Proj 
                db03.dbRCod.rCode  =             db003.dbRCod.rCode 
                db03.dbRCod.Sgrp  =             db003.dbRCod.Sgrp 
                db03.dbRCod.target  =             db003.dbRCod.target 
                db03.dbRCod.track   =             db003.dbRCod.track  
                db03.dbRCod.vat% =             db003.dbRCod.vat%.
        END.
        IF AVAILABLE db03.dbRcod THEN DO:
            ASSIGN 
                db03.dbRCod.Acb   = db003.dbRCod.Acb  
                db03.dbRCod.Dept  =             db003.dbRCod.Dept 
                db03.dbRCod.Descrip  =             db003.dbRCod.Descrip 
                db03.dbRCod.Ledger  =             db003.dbRCod.Ledger 
                db03.dbRCod.Proj  =             db003.dbRCod.Proj 
                db03.dbRCod.Sgrp  =             db003.dbRCod.Sgrp 
                db03.dbRCod.target  =             db003.dbRCod.target 
                db03.dbRCod.track   =             db003.dbRCod.track  
                db03.dbRCod.vat% =             db003.dbRCod.vat%.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = STRING(db003.dbRcod.rCode). 
        wst:SCREEN-VALUE IN FRAME frm-Main = db003.dbRcod.Descrip. 
    END.
    
    /*receipt operator*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Operators".
    FOR EACH db003.dbRecop NO-LOCK:
        FIND FIRST db03.dbRecop WHERE db03.dbRecop.usercode = db003.dbRecop.usercode NO-ERROR.
        IF  NOT AVAILABLE db03.dbRecop THEN DO:
            CREATE db03.dbRecop.
            ASSIGN
                db03.dbRecop.CreDate = db003.dbRecop.CreDate
                db03.dbRecop.DeviceName = db003.dbRecop.DeviceName
    	        db03.dbRecop.UID = db003.dbRecop.UID
                db03.dbRecop.EOD  = db003.dbRecop.EOD 
                db03.dbRecop.password  =             db003.dbRecop.password 
                db03.dbRecop.prn  =             db003.dbRecop.prn 
                db03.dbRecop.RecDate  =             db003.dbRecop.RecDate 
                db03.dbRecop.RecNo  =             db003.dbRecop.RecNo 
                db03.dbRecop.txtPass  =             db003.dbRecop.txtPass 
                db03.dbRecop.usercode =             db003.dbRecop.usercode.
        END.
        IF AVAILABLE db03.dbRecop THEN DO:
            ASSIGN db03.dbRecop.EOD  = db003.dbRecop.EOD 
                db03.dbRecop.password  =             db003.dbRecop.password 
                db03.dbRecop.prn  =             db003.dbRecop.prn 
                db03.dbRecop.RecDate  =             db003.dbRecop.RecDate 
                db03.dbRecop.RecNo  =             db003.dbRecop.RecNo 
                db03.dbRecop.txtPass  =             db003.dbRecop.txtPass .
    
        END.
       wsa:SCREEN-VALUE IN FRAME frm-Main = db003.dbRecop.usercode.
       wst:SCREEN-VALUE IN FRAME frm-Main = string(db003.dbRecop.recno). 
    END.
    
    /*Online receipt operator*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Online operators".
    FOR EACH db003.dbRecopon NO-LOCK:
        FIND FIRST db03.dbRecopon WHERE db03.dbRecopon.ucode = db003.dbRecopon.ucode AND db03.dbRecopOn.Paytype  = db003.dbRecopOn.Paytype   NO-ERROR.
        IF  NOT AVAILABLE db03.dbRecopon THEN DO:
            CREATE db03.dbRecopon.
            ASSIGN
                db03.dbRecopon.CreDate = db003.dbRecopon.CreDate
                db03.dbRecopon.DeviceName = db003.dbRecopon.DeviceName
    	        db03.dbRecopon.UID = db003.dbRecopon.UID
                db03.dbRecopOn.Paytype  = db003.dbRecopOn.Paytype 
                db03.dbRecopOn.txtCur  =             db003.dbRecopOn.txtCur 
                db03.dbRecopOn.txtKey  =             db003.dbRecopOn.txtKey 
                db03.dbRecopOn.txtName  =             db003.dbRecopOn.txtName 
                db03.dbRecopOn.ucode =             db003.dbRecopOn.ucode.
        END.
        IF AVAILABLE db03.dbRecopon THEN DO:
            ASSIGN db03.dbRecopOn.txtCur  =             db003.dbRecopOn.txtCur 
                db03.dbRecopOn.txtKey  =             db003.dbRecopOn.txtKey 
                db03.dbRecopOn.txtName  =             db003.dbRecopOn.txtName.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = string(db003.dbRecopon.ucode). 
        wst:SCREEN-VALUE IN FRAME frm-Main = db003.dbRecopon.txtName.
    END.
    
    /*Service group*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Services".
    FOR EACH db003.dbsgr NO-LOCK:
        FIND FIRST db03.dbsgr WHERE db03.dbsgr.sgr = db003.dbsgr.sgr NO-ERROR.
        IF  NOT AVAILABLE db03.dbsgr THEN DO:
            CREATE db03.dbsgr.
            ASSIGN
                db03.dbsgr.CreDate = db003.dbsgr.CreDate
                db03.dbsgr.DeviceName = db003.dbsgr.DeviceName
    	        db03.dbsgr.UID = db003.dbsgr.UID
                db03.dbsgr.ctrLedger  =  db003.dbsgr.ctrLedger 
                db03.dbsgr.Dept  =             db003.dbsgr.Dept 
                db03.dbsgr.Descrip   =             db003.dbsgr.Descrip  
                db03.dbsgr.IntLedger  =             db003.dbsgr.IntLedger 
                db03.dbsgr.Proj  =             db003.dbsgr.Proj 
                db03.dbsgr.Sgrp  =             db003.dbsgr.Sgrp.
        END.
        IF AVAILABLE db03.dbsgr THEN DO:
            ASSIGN  db03.dbsgr.ctrLedger  =  db003.dbsgr.ctrLedger 
                db03.dbsgr.Dept  =             db003.dbsgr.Dept 
                db03.dbsgr.Descrip   =             db003.dbsgr.Descrip  
                db03.dbsgr.IntLedger  =             db003.dbsgr.IntLedger 
                db03.dbsgr.Proj  =             db003.dbsgr.Proj.
    
        END.
        wsa:SCREEN-VALUE IN FRAME frm-Main = string(db003.dbsgr.sgr). 
        wst:SCREEN-VALUE IN FRAME frm-Main = db003.dbsgr.descrip. 
    END.
    
    /*Sending back information to main server, receipts only
    Create a receipt entry in main server
    create receipt control if it doesn't exist
    send receipt entry to history in landing server for backup*/
    wsp:SCREEN-VALUE IN FRAME frm-Main = "Copying Receipts".
    FOR EACH db03.dbRec.
     CREATE db03.dbRech.
     ASSIGN
         db03.dbRecH.Account  = db03.dbRec.Account 
         db03.dbRecH.Amount  =      db03.dbRec.Amount 
         db03.dbRecH.contype  =      db03.dbRec.contype 
         db03.dbRecH.decRate  =      db03.dbRec.decRate 
         db03.dbRecH.Descrip  =      db03.dbRec.Descrip 
         db03.dbRecH.OpCode  =      db03.dbRec.OpCode 
         db03.dbRecH.Paytype  =      db03.dbRec.Paytype 
         db03.dbRecH.rCode  =      db03.dbRec.rCode 
         db03.dbRecH.RecDate  =      db03.dbRec.RecDate 
         db03.dbRecH.RecNo  =      db03.dbRec.RecNo 
         db03.dbRecH.RecStat  =      db03.dbRec.RecStat 
         db03.dbRecH.Ref  =      db03.dbRec.Ref 
         db03.dbRecH.SeqNo  =      db03.dbRec.SeqNo 
         db03.dbRecH.txtCur =      db03.dbRec.txtCur.
     CREATE db003.dbRec.
     ASSIGN
         db003.dbRec.Account  = db03.dbRec.Account 
         db003.dbRec.Amount  =      db03.dbRec.Amount 
         db003.dbRec.contype  =      db03.dbRec.contype 
         db003.dbRec.decRate  =      db03.dbRec.decRate 
         db003.dbRec.Descrip  =      db03.dbRec.Descrip 
         db003.dbRec.OpCode  =      db03.dbRec.OpCode 
         db003.dbRec.Paytype  =      db03.dbRec.Paytype 
         db003.dbRec.rCode  =      db03.dbRec.rCode 
         db003.dbRec.RecDate  =      db03.dbRec.RecDate 
         db003.dbRec.RecNo  =      db03.dbRec.RecNo 
         db003.dbRec.RecStat  =      db03.dbRec.RecStat 
         db003.dbRec.Ref  =      db03.dbRec.Ref 
         db003.dbRec.SeqNo  =      db03.dbRec.SeqNo 
         db003.dbRec.txtCur =      db03.dbRec.txtCur.
     wsa:SCREEN-VALUE IN FRAME frm-Main = string(db03.dbRec.recno). 
     wst:SCREEN-VALUE IN FRAME frm-Main = string(db03.dbRec.recno).
     DELETE db03.dbRec.
    END.
    FOR EACH db03.dbRecCtr NO-LOCK.
         FIND FIRST db003.dbRecCtr WHERE db003.dbRecCtr.Usercode = db03.dbRecCtr.usercode AND db003.dbRecCtr.recDate = db03.dbRecCtr.recDate NO-ERROR.
         IF NOT AVAILABLE db003.dbRecCtr THEN DO:
         CREATE db003.dbRecCtr.
         ASSIGN
             db003.dbRecCtr.EOD  =  db03.dbRecCtr.EOD 
             db003.dbRecCtr.RecDate  =          db03.dbRecCtr.RecDate 
             db003.dbRecCtr.RecTotal  =          db03.dbRecCtr.RecTotal 
             db003.dbRecCtr.usercode  =          db03.dbRecCtr.usercode 
             db003.dbRecCtr.Validate =          db03.dbRecCtr.VALIDATE.
    
         END.
     END.
    
    
    
    /*disconnect landing database from main database*/
    DISCONNECT dbac.
    DISCONNECT db03.
    
MESSAGE "Synchronisation Complete." VIEW-AS ALERT-BOX.
END PROCEDURE.
