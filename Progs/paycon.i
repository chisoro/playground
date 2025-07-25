/*Payroll costing consolidation include file */

PROCEDURE ProView.ip: 
    FOR EACH paycost:
        ASSIGN wskey = STRING(paycost.proj) + STRING(paycost.fund) + STRING(paycost.dept) + STRING(paycost.Ledger).
    END.
    VIEW FRAME frm-View.
    ENABLE ALL WITH FRAME frm-View.
    OPEN QUERY qryView FOR EACH paycost WHERE paycost.Paysys = wsSys NO-LOCK.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-View.
    HIDE FRAME frm-View.
END PROCEDURE.

PROCEDURE Report.ip:
    FOR EACH paycost WHERE paycost.Paysys = wsSys BREAK BY wskey:
        ACCUMULATE paycost.Amount(TOTAL BY wskey).
        IF LAST-OF(wskey) THEN DO:
            ASSIGN wsAmt = (ACCUM TOTAL BY wskey  paycost.Amount).
                   wsLedger = paycost.Ledger.
            IF wsAmt = 0 THEN NEXT.
            RUN glupdate.ip.
            IF LINE-COUNTER(a) + 3 > PAGE-SIZE(a) THEN DO:
               PAGE STREAM a.
               DISPLAY STREAM a WITH FRAME frm-hdr.   
            END.
            DISPLAY STREAM a paycost.Proj paycost.Fund paycost.Dept wsLedger wsDes wsAmt @ wsDr WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    END.
    FIND FIRST Payctr NO-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = Payctr.Ledger NO-LOCK NO-ERROR.
    ASSIGN wsAmt    = (ACCUM TOTAL paycost.Amount) * -1
           wsLedger = Payctr.Ledger
           wsdes = glmf.DESCRIPTION.
    IF wsAmt = 0 THEN NEXT.
    RUN glupdate.ip.
    DISPLAY STREAM a   wsDes wsLedger wsAmt @ wsCr WITH FRAME frm-rpt.
    DOWN  STREAM a  WITH FRAME frm-rpt.
    UNDERLINE STREAM a wsDr wsCr WITH FRAME frm-rpt.
    DISPLAY STREAM a wsAmt * -1 @ wsDr wsAmt @ wsCr WITH FRAME frm-rpt.
    DOWN  STREAM a  WITH FRAME frm-rpt.
    FOR EACH paycost WHERE paycost.Paysys = wsSys.
        DELETE paycost.
    END.
    RETURN.
END PROCEDURE.

PROCEDURE glupdate.ip:
    wsAmt = ROUND((wsAmt * wsRate),2).
    FIND FIRST glmf WHERE glmf.acct = wsLedger NO-ERROR.
            IF AVAILABLE glmf THEN
                wsDes = glmf.DESCRIPTION.
            ELSE wsDes = "******Invalid ledger ******".
    CREATE gltdf.
    ASSIGN gltdf.proj = paycost.proj
           gltdf.fund = paycost.fund
           gltdf.dept = paycost.dept
           gltdf.acct = wsLedger
           gltdf.period = st-per
           gltdf.CREDATE = TODAY 
           gltdf.TransID = wsTransID
           gltdf.source  = "HR"
           gltdf.REF     = "HR" + string(wsSys) + "/" + string(st-per)
           gltdf.DESCRIP = "HR CONSOLIDATION"
           gltdf.AMT     = wsAmt
           gltdf.trDATE  = Paysys.CurDate
           gltdf.UID     = varUser
           gltdf.UID2    = varUser.
    /* update glbal */
    FIND FIRST glbal WHERE GLBAL.acct = wsLedger 
                   AND GLBAL.YEAR = INT(SUBSTR(STRING(st-per),1,4)) NO-ERROR.
    IF NOT AVAILABLE glbal THEN DO:
        CREATE glbal.
        ASSIGN glbal.acct = wsLedger
               glbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)).
    END.
    ASSIGN glbal.amt[INT(SUBSTR(STRING(st-per),5,2))]=
                glbal.amt[INT(SUBSTR(STRING(st-per),5,2))] + wsAmt.
    /* update glPBal */
    FIND FIRST glPbal WHERE glPbal.Proj = paycost.proj  AND glPbal.acct = wsLedger 
                        AND glPbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)) NO-ERROR.
    IF NOT AVAILABLE glPbal THEN DO:
        CREATE glPbal.
        ASSIGN glPbal.Proj = paycost.proj 
               glPbal.acct = wsLedger
               glPbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)).
    END.
    ASSIGN glPbal.amt[INT(SUBSTR(STRING(st-per),5,2))]=
                glPbal.amt[INT(SUBSTR(STRING(st-per),5,2))] + wsAmt.
    /* update glFbal */
    FIND FIRST glFbal WHERE glFbal.fund = paycost.fund AND glFbal.acct = wsLedger 
                        AND glFbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)) NO-ERROR.
    IF NOT AVAILABLE glFbal THEN DO:
        CREATE glFbal.
        ASSIGN glFbal.fund = paycost.fund 
               glFbal.acct = wsLedger
               glFbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)).
    END.
    ASSIGN glFbal.amt[INT(SUBSTR(STRING(st-per),5,2))]=
                glFbal.amt[INT(SUBSTR(STRING(st-per),5,2))] + wsAmt.
    /* update glDbal */
    FIND FIRST glFbal WHERE glDbal.dept = paycost.dept AND glDbal.acct = wsLedger 
                        AND glDbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)) NO-ERROR.
    IF NOT AVAILABLE glDbal THEN DO:
        CREATE glDbal.
        ASSIGN glDbal.dept = paycost.dept 
               glDbal.acct = wsLedger
               glDbal.YEAR = INT(SUBSTR(STRING(st-per),1,4)).
    END.
    ASSIGN glDbal.amt[INT(SUBSTR(STRING(st-per),5,2))]=
                glDbal.amt[INT(SUBSTR(STRING(st-per),5,2))] + wsAmt.
END.
