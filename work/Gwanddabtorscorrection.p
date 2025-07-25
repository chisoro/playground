DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wstar LIKE dbtmf.Tarif.
DEF VAR wscnt AS DEC.
DEF VAR wsrate AS DEC FORM ">>>>>9.999999-".
DEF VAR wsamt AS DEC FORM "zzzzzzzzz9.9999-".
DEF VAR wsd AS CHAR FORM "x(40)".
DEF VAR wsAcc LIKE munbmf.acc.
/* Ratepayer Account */ 
wsrate = 5498.7200.
OUTPUT TO c:\simacc\work\debtorsleft.csv.
FIND LAST muntmf WHERE muntmf.TYPE = 3 NO-ERROR.
IF AVAILABLE muntmf THEN
    wstar = muntmf.tarif.
FOR EACH muncmf WHERE company = 0 NO-LOCK:
    IF NOT CAN-FIND(FIRST dbcmf WHERE dbcmf.dbacc = muncmf.acc) THEN DO:
    ASSIGN wscnt = wscnt + 1.
    EXPORT DELIMITER ','  muncmf.acc.
    CREATE dbcmf.
    ASSIGN dbcmf.AccBal     = ROUND((muncmf.balance / wsrate),2)
           dbcmf.AccStat    = muncmf.active
           dbcmf.Add1       = muncmf.addr1
           dbcmf.add2       = muncmf.addr2
           dbcmf.Add3       = muncmf.addr3
           dbcmf.BalBf      = ROUND((muncmf.balance / wsrate),2)
           dbcmf.Cell       = DEC(muncmf.cell)
           dbcmf.Cons       = muncmf.cons-code           
           dbcmf.Credate    = muncmf.eff-date
           dbcmf.dbAcc      = muncmf.acc
           dbcmf.DeedNo     = muncmf.deed-no
           dbcmf.email      = muncmf.eMail-acc
           dbcmf.emailAdd   = muncmf.email
           dbcmf.Name       = muncmf.NAME    
           dbcmf.RegID      = muncmf.idno
           dbcmf.SiteValue  = muncmf.site
           dbcmf.BldValue   = muncmf.bldg
           dbcmf.Size       = muncmf.area
           dbcmf.stno       = muncmf.st-no 
           dbcmf.street     =  muncmf.st-name
           dbcmf.lpdate     = muncmf.last-pay-dte
           dbcmf.Sms        = muncmf.sms-acc
           dbcmf.SortName   = muncmf.sortname
           dbcmf.StandNo    = STRING(muncmf.erf-no)
           dbcmf.Suburb     = muncmf.suburb
           dbcmf.Usr        = "1"
           dbcmf.VatNo      = INT(muncmf.vat-reg-no)
           dbcmf.Ward       = INT(muncmf.ward)
           dbcmf.BldValue   = muncmf.nr-bldg
           dbcmf.Rate-tarif = muncmf.rates-tarif
           dbcmf.RNextP     = 202307
           dbcmf.POwner     = YES WHEN muncmf.OWNER = "O"
           dbcmf.POwner     = NO WHEN muncmf.OWNER = "T".
    j = 0.
    DO X = 1 TO 6:
        IF muncmf.sv-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = muncmf.sv-tar[X]
                   dbcmf.Units[j] = muncmf.sv-pts[X]
                   dbcmf.NextP[j] = 202307.
        END.
    END.
   
    DO X = 1 TO 6:
       IF muncmf.av-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = wstar + muncmf.av-tar[X]
                   dbcmf.Units[j] = muncmf.av-pts[X]
                   dbcmf.NextP[j] = 202307.
        END.    
    END.
    END.
END.
/*
input from c:\simacc\work\debtorsleft.csv.
REPEAT :
    import delimiter "," wsacc.
    FOR EACH munbmf WHERE company = 0 AND munbmf.acc = wsacc NO-LOCK:
        IF NOT CAN-FIND (FIRST dbblf WHERE dbblf.dbacc = munbmf.acc AND dbblf.sgrp = munbmf.TYPE)
             THEN DO:
            wscnt = wscnt + 1.
            DISPLAY munbmf.acc WITH TITLE "Balance file". PAUSE 0.
            CREATE dbblf.
            ASSIGN dbblf.dbAcc = munbmf.acc
                   dbblf.Sgrp = munbmf.TYPE.
            DO X = 1 TO 14:
                dbblf.vat = ROUND((munbmf.vat-amt / wsrate),2).
                ASSIGN dbblf.amt[X] = ROUND((munbmf.bal[X] / wsrate),2)
                       dbblf.Int    = dbblf.Int  + munbmf.Interest[X]
                       dbblf.dbAmt[2] = dbblf.dbAmt[2] + munbmf.bal[X] + munbmf.Interest[X].
            END.
              dbblf.dbAmt[1] = ROUND((dbblf.dbAmt[2] / wsrate),2).
              dbblf.Int    = ROUND((dbblf.INT / wsrate),2).
        END.   
    END.

    FOR EACH munthf WHERE company = 0 AND munthf.acc = wsacc:
    DISPLAY munthf.acc period WITH TITLE "Historical Detors Data".
    PAUSE 0.
    FIND FIRST muntmf WHERE muntmf.tarif = munthf.tarif 
                        AND muntmf.TYPE = munthf.tar-type NO-ERROR.
    IF AVAILABLE muntmf THEN
        wsd = muntmf.descrip + "(" + munthf.ref + ")".  
    ELSE
        ASSIGN wsd = "Interest Charge" + "(" + munthf.ref + ")" WHEN munthf.prog = "mun056m.p" OR munthf.prog = "mun056.p"
               wsd = "Receipt" + "(" + munthf.ref + ")"        WHEN munthf.prog = "mun116.p"
               wsd = "Journal Adjusment" + "(" + munthf.ref + ")" WHEN munthf.prog = "mun038.p".
        CREATE dbhtf.
        ASSIGN dbhtf.Accper     = munthf.period
               dbhtf.Amt        = munthf.amt
               dbhtf.dbAcc      = munthf.acc
               dbhtf.DESCRIP    = wsd
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

    FOR EACH dbblf WHERE dbblf.dbacc = wsacc: 
    DISPLAY dbblf.dbacc.   pause 0.
    CREATE dbhtf.
    ASSIGN dbhtf.Accper     =  202307
               dbhtf.Amt        = dbamt[2] * -1
               dbhtf.dbAcc      = dbblf.dbacc
               dbhtf.DESCRIP    = "Rebase Set-off"
               dbhtf.Iledger    = 0
               dbhtf.PoDate     = 07/13/23
               dbhtf.prog       = ""
               dbhtf.Ref        = "Set-off"
               dbhtf.Seq        = 0 
                dbhtf.Sgrp       = dbblf.sgrp
               dbhtf.Tarif      = 0
               dbhtf.TransID    = 20230712160000
               dbhtf.trDate     = 07/13/23
               dbhtf.UID        = "1"
               dbhtf.UID2       = "1"
               dbhtf.Vat        = ROUND((dbblf.vat * wsrate  * -1),2)
               dbhtf.type       = 0.
    CREATE dbhtf.
    ASSIGN dbhtf.Accper     = 202307
               dbhtf.Amt        = dbamt[1]
               dbhtf.dbAcc      = dbblf.dbacc
               dbhtf.DESCRIP    = "Rebase Take-on"
               dbhtf.Iledger    = 0
               dbhtf.PoDate     = 07/13/23
               dbhtf.prog       = "dbjnl02.p"
               dbhtf.Ref        = "Rebase-takeon"
               dbhtf.Seq        = 0 
               dbhtf.Sgrp       = dbblf.sgrp
               dbhtf.Tarif      = 0
               dbhtf.TransID    = 20230713000000
               dbhtf.trDate     = 07/13/23
               dbhtf.UID        = "1"
               dbhtf.UID2       = "1"
               dbhtf.Vat        = dbblf.vat
               dbhtf.type       = 0.
    END. 

END.*/
