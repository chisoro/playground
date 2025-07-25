/*....Program   :MigrateProm.p
      Notes     : Migrate Promun Data to the SimAcc system.
*************************************************************/
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wstar LIKE dbtmf.Tarif.
DEF VAR wscnt AS DEC.
DEF VAR wsrate AS DEC FORM ">>>>>9.999999-".
DEF VAR wsamt AS DEC FORM "zzzzzzzzz9.9999-".
DEF VAR wsd AS CHAR FORM "x(40)".
/* Import General Ledger *//*

FOR EACH iglsvf NO-LOCK:
    CREATE glsubvote.
    ASSIGN GLSUBVOTE.SUBVOTE  = iglsvf.sub-vote-no 
            GLSUBVOTE.DESCRIP = iglsvf.description.
END. taken
    
FOR EACH iglscf NO-LOCK:
    CREATE glsubcat.
    ASSIGN glsubcat.SUBCAT  = iglscf.sub-cat 
           glsubcat.DESCRIP = iglscf.description.
END. taken

FOR EACH iglcmf NO-LOCK:
    CREATE glcat.
    ASSIGN GLCAT.CAT     = iglcmf.category
           GLCAT.DESCRIP = iglcmf.description. 
END. taken

FOR EACH iglbranch NO-LOCK:
    CREATE GLFUND.
    ASSIGN GLFUND.FUND     = iglbranch.branch 
           GLFUND.DESCRIP  = iglbranch.description.
END. taken

FOR EACH iglitem NO-LOCK:
    CREATE glitem.
    ASSIGN GLITEM.ITEM    = iglitem.item
           GLITEM.DESCRIP = iglitem.description.
END. taken

FOR EACH iglamf NO-LOCK:
    DISPLAY iglamf.acct-no.
    PAUSE 0.
    CREATE glmf.
    ASSIGN glmf.acct          = iglamf.acct-no
           glmf.ACCTYPE       = iglamf.type
           glmf.CAT           = iglamf.cat
           glmf.DESCRIPTION   = iglamf.DESCRIPTION
           glmf.DrCr          = iglamf.norm-bal
           glmf.ITEM          = iglamf.ITEM
           glmf.subcat        = iglamf.sub-cat
           glmf.FUND          = iglamf.branch
           glmf.SUBVOTE       = iglamf.sub-vote
           glmf.dept          = iglamf.vote
           glmf.VOTE          = iglamf.vote.
    IF subvote >= 0 AND subvote <= 5 THEN
        ASSIGN glmf.CLAS = 2
               glmf.ACCTYPE = "E".
    IF subvote >= 6 AND subvote <= 9 THEN
    ASSIGN glmf.CLAS = 1
        glmf.ACCTYPE  = "I".
    IF SUBSTR(STRING(glmf.acct),4,2) = "21" THEN
    ASSIGN glmf.CLAS = 3.
    IF SUBSTR(STRING(glmf.acct),4,2) = "22" THEN
    glmf.CLAS = 3.
    IF SUBSTR(STRING(glmf.acct),4,2) = "23" THEN
    glmf.CLAS = 4.
    IF SUBSTR(STRING(glmf.acct),4,2) = "24" THEN
    glmf.CLAS = 4.
    IF SUBSTR(STRING(glmf.acct),4,2) = "25" THEN
    glmf.CLAS = 5.
END. taken

FOR EACH iglvmf NO-LOCK:
    CREATE glvote.
    ASSIGN GLVOTE.VOTE    = iglvmf.vote-no 
           GLVOTE.DESCRIP = iglvmf.description.
    CREATE glDept.
    ASSIGN glDept.dept    = iglvmf.vote-no 
           glDept.DESCRIP = iglvmf.description.
    FIND FIRST glmf WHERE glmf.dept = gldept.dept NO-ERROR.
    IF AVAILABLE glmf THEN
        gldept.fund = glmf.fund.
END. taken*/


/*

 FOR EACH iglabf NO-LOCK:
    DISPLAY iglabf.acct-no.
    PAUSE 0.
    CREATE glbal.
    ASSIGN GLBAL.acct  = iglabf.acct-no
           GLBAL.BFBAL = iglabf.bf-bal 
           GLBAL.YEAR  = iglabf.bal-year.
    DO X = 1 TO 12:
        ASSIGN glbal.amt[X] = iglabf.amount[X]
               glbal.budget[X] = glbal.budget[X].
    END.
END.  
 FOR EACH igltdf NO-LOCK:
    DISPLAY igltdf.acct-no.
    PAUSE 0.
    CREATE gltdf.
    ASSIGN gltdf.acct     = igltdf.acct-no
           gltdf.AMT      = igltdf.amount
           gltdf.CREDATE  = igltdf.eff-date
           gltdf.DESCRIP  = igltdf.description
           gltdf.period   = igltdf.per
           gltdf.REF      = igltdf.ref
           gltdf.source   = igltdf.source
           gltdf.TransID  = igltdf.link-no
           gltdf.trDATE   = igltdf.datex
           gltdf.UID      = igltdf.signon
           gltdf.UID2     = igltdf.signon.
END.


/* Cashbook */
FOR EACH cshtdesc NO-LOCK:
    CREATE cbtype.
    ASSIGN cbtype.TranType = cshtdesc.tran-type
           cbtype.DrCr     = cshtdesc.td-db-or-cr
           cbtype.descrip  = cshtdesc.td-descr.
END.


FOR EACH cshbnk NO-LOCK:
    CREATE cbkmf.
    ASSIGN cbkmf.Acb     = 0
           cbkmf.Bal     = 0 
           cbkmf.bank    = cshbnk.bank
           cbkmf.descrip = cshbnk.bnk-name
           cbkmf.Ledger  = DEC(cshbnk.bnk-gl-acct)
           cbkmf.StatBal = 0
           cbkmf.StatNo  = 0
           cbkmf.txtCur  = "ZWL".
END.*/
/*
FOR EACH cshtdf NO-LOCK:
    CREATE CBTrans.
    ASSIGN CBTrans.Acb      = 0   
           CBTrans.Accper   = cshtdf.tdf-period
           CBTrans.amount   = cshtdf.tdf-amount
           CBTrans.bank     = cshtdf.bank
           CBTrans.Descrip  = cshtdf.tdf-narrat
           CBTrans.Ledger   = DEC(cshtdf.tdf-gl-acct) 
           CBTrans.OpCode   = cshtdf.signon
           CBTrans.PoDate   = cshtdf.eff-date
           CBTrans.prog     = ""
           CBTrans.Recon    = cshtdf.tdf-recon
           CBTrans.Ref      = cshtdf.tdf-refer
           CBTrans.StatNo   = 0
           CBTrans.TransID  = cshtdf.link-no
           CBTrans.TranType = cshtdf.tran-type
           CBTrans.trDate   = cshtdf.tdf-date
           CBTrans.UID      = cshtdf.signon
           CBTrans.UID2     = cshtdf.signon.
END. 
/*receipting items */  
FOR EACH municf NO-LOCK:
    CREATE dbRcod.
        ASSIGN dbRCod.rCode    = municf.code 
               dbRCod.Descrip  = municf.desc-eng
               dbRCod.Ledger   = municf.alloc
               dbRCod.Acb      = municf.bank
               dbRCod.Sgrp     = municf.bmf-type
               dbRCod.target   =  municf.inc-type
               dbRCod.vat%     =  municf.vat.
END.

/* Import Debtors Billing */
FOR EACH muntyp NO-LOCK: /*service/BT */
    CREATE dbsgr.
    ASSIGN dbsgr.ctrLedger =  DEC(muntyp.dr-alloc)
           dbsgr.Descrip   = muntyp.des-eng
           dbsgr.Sgrp      = muntyp.bmf-typ
           dbsgr.IntLedger = DEC(muntyp.cr-alloc).
END.

FOR EACH munsbdf NO-LOCK: /*surbub */
    CREATE dbsmf.
    ASSIGN dbsmf.suburb    = munsbdf.suburb
           dbsmf.Descrip   = munsbdf.descr.
END.

FOR EACH muncdesc NO-LOCK: /* Consumer */
    CREATE dbctf. 
    ASSIGN dbctf.cons     = muncdesc.type 
           dbctf.Descrip  = muncdesc.des-eng.
           dbctf.Interest = 0.
END.

FOR EACH munwdf NO-LOCK: /* ward */
    CREATE dbwrd.
    ASSIGN dbwrd.Ward    = INT(munwdf.ward) 
           dbwrd.Descrip = munwdf.descr.
END.

/* Migrate Tariffs */
FIND LAST muntmf WHERE muntmf.TYPE = 3 NO-ERROR.
IF AVAILABLE muntmf THEN
    wstar = muntmf.tarif.
FOR EACH muntmf NO-LOCK: /* tariff */
    DISPLAY muntmf.tarif.
    PAUSE 0.
    CREATE dbtmf.
    ASSIGN dbtmf.Tarif      = muntmf.tarif 
           dbtmf.Tarif      = wstar + muntmf.tarif WHEN muntmf.TYPE = 4
           dbtmf.Sgrp       = muntmf.bmf-type
           dbtmf.SOURCE     = 1 WHEN muntmf.src-flag = "P"
           dbtmf.SOURCE     = 2 WHEN muntmf.src-flag = "A"
           dbtmf.SOURCE     = 3 WHEN muntmf.src-flag = "w"
           dbtmf.SOURCE     = 5 WHEN muntmf.src-flag = "s"
           dbtmf.LValue     = muntmf.assess[1]
           dbtmf.BValue     = muntmf.assess[2]
           dbtmf.Iledger    = DEC(muntmf.ivote)
           dbtmf.freq       = muntmf.freq
           dbtmf.freq       = "A" WHEN  muntmf.freq = "Y"
           dbtmf.Descrip    = muntmf.descrip
           dbtmf.Charge     = muntmf.avl-amt WHEN muntmf.TYPE = 4
           dbtmf.Charge     = muntmf.rate[1] WHEN muntmf.TYPE = 3
           dbtmf.Charge     = muntmf.basic[1] WHEN muntmf.TYPE = 1
           dbtmf.type       = muntmf.type
           dbtmf.type       = 3 WHEN muntmf.TYPE = 4
           dbtmf.Vat%       = muntmf.vat-pcnt.
    DO X = 1 TO 5:
        ASSIGN dbtmf.Tear[X]     = muntmf.range[X]
               dbtmf.TCharge[X]  = muntmf.rate[X]
               dbtmf.facto[X]    = muntmf.factor[X].
    END.
END.*/

/* Ratepayer Account */
wsrate = 5498.72.
FIND LAST muntmf WHERE muntmf.TYPE = 3 NO-ERROR.
IF AVAILABLE muntmf THEN
    wstar = muntmf.tarif.
FOR EACH muncmf WHERE company = 0 /*AND acc >= 51000000 */ NO-LOCK:
    IF NOT CAN-FIND(FIRST dbcmf WHERE dbcmf.dbacc = muncmf.acc) THEN DO:
    ASSIGN wscnt = wscnt + 1.
    DISPLAY muncmf.acc WITH TITLE "Master file". PAUSE 0.
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
           dbcmf.RNextP     = 202305
           dbcmf.POwner     = YES WHEN muncmf.OWNER = "O"
           dbcmf.POwner     = NO WHEN muncmf.OWNER = "T".
    j = 0.
    DO X = 1 TO 6:
        IF muncmf.sv-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = muncmf.sv-tar[X]
                   dbcmf.Units[j] = muncmf.sv-pts[X]
                   dbcmf.NextP[j] = 202305.
        END.
    END.
    DO X = 1 TO 6:
       IF muncmf.av-tar[X] <> 0 THEN DO:
            j = j + 1.
            ASSIGN dbcmf.tar[j] = wstar + muncmf.av-tar[X]
                   dbcmf.Units[j] = muncmf.av-pts[X]
                   dbcmf.NextP[j] = 202305.
        END.    
    END.
    END.
END.

FOR EACH munbmf WHERE company = 0 AND munbmf.acc < 999999999998 NO-LOCK:
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

FOR EACH munrmf WHERE company = 0 AND munrmf.tarif <> 0 AND ew = "w" NO-LOCK:
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
           dbmmf.wNextP  = 202305.
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


FOR EACH munthf WHERE company = 0  AND acc < 999999999998:
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


/*
FOR EACH crdamf:
  CREATE crdmf.
  ASSIGN
      Crdmf.Add1 = crdamf.addr1
      Crdmf.Acc = crdamf.account
      Crdmf.Add2 = crdamf.addr2
      Crdmf.Bal = crdamf.bal
      Crdmf.BrCode = crdamf.br-code
      Crdmf.Contact = crdamf.contact
      Crdmf.discount = crdamf.disc-perc
      Crdmf.email = crdamf.email
      Crdmf.Name = crdamf.name
      Crdmf.VatAmt = 0
      Crdmf.town = crdamf.city
      Crdmf.VatNo = crdamf.vat-reg-no
      Crdmf.YtdAmt = crdamf.ytd-pur
      Crdmf.RegNo = crdamf.reg-no
      Crdmf.Stat = 0
      Crdmf.terms = crdamf.terms
      Crdmf.cell = int(crdamf.cell)
      Crdmf.Phone = int(crdamf.phone).
END. 

FOR EACH exd.crdtmf:
    CREATE db002.crdtmf.
    ASSIGN
        db002.Crdtmf.Acc = exd.crdtmf.account
        db002.Crdtmf.Amt = exd.crdtmf.amt
        db002.Crdtmf.CreDate = exd.crdtmf.eff-date
        db002.Crdtmf.DatePaid = ?
        db002.Crdtmf.decBal = 0.00
        db002.Crdtmf.Descrip = exd.crdtmf.descr
        db002.Crdtmf.Ledger = 0
        db002.Crdtmf.OrdNo = 0
        db002.Crdtmf.Period = exd.crdtmf.period
        db002.Crdtmf.Quantity = 1
        db002.Crdtmf.Ref = exd.crdtmf.invoice-no
        db002.Crdtmf.TransID = exd.crdtmf.link-no
        db002.Crdtmf.TrDate = exd.crdtmf.inv-date
        db002.Crdtmf.TrType = exd.crdtmf.type 
        db002.Crdtmf.UID = exd.crdtmf.signon
        db002.Crdtmf.VAT = exd.crdtmf.vat-amt 
        db002.Crdtmf.vatBal = 0.
             
END. 

FOR EACH exd.crdtaf:
    CREATE db002.crdtaf.
    ASSIGN
        db002.crdtaf.amount = exd.crdtaf.gl-amt
        db002.crdtaf.Acc =  INT(exd.crdtaf.account)
        db002.crdtaf.Dept = 0
        db002.crdtaf.des =  exd.crdtaf.gl-acc
        db002.crdtaf.Descrip = exd.crdtaf.description
        db002.crdtaf.Fund = 0
        db002.crdtaf.Ledger = 0
        db002.crdtaf.LineSeq = exd.crdtaf.seq-no
        db002.crdtaf.period = exd.crdtaf.period
        db002.crdtaf.Proj = 0
        db002.crdtaf.TransID =  0
        db002.crdtaf.trDate = exd.crdtaf.inv-date
        db002.crdtaf.UID = "0"
        db002.crdtaf.VAT = 0
        db002.crdtaf.ref = exd.crdtaf.invoice-no. 
  find first  db002.crdtmf WHERE  db002.Crdtmf.Ref =   db002.crdtaf.ref NO-ERROR.
  IF AVAILABLE db002.crdtmf THEN
       db002.crdtaf.TransID = db002.Crdtmf.TransID.
END. 
*/
