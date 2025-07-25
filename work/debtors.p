DEF VAR wsLedger AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsList AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR X AS INT.
OUTPUT TO g:\simacc\work\debtors.csv.
FOR EACH dbsgr:
    ASSIGN wsLedger = 0
           wslist = 0.
    FOR EACH glbal WHERE YEAR = 2019 AND glbal.acct = dbsgr.ctrLedger:
        wsledger = wsledger + glbal.bfbal.
        DO X = 1 TO 12:
            wsledger = wsledger + glbal.amt[X].
        END.
    END.
    FOR EACH dbblf WHERE dbblf.sgrp = dbsgr.sgrp:
        DO X = 1 TO 15:
            wslist = wslist + dbblf.amt[X].
        END.
    END.
    EXPORT DELIMITER ',' dbsgr.Descrip dbsgr.Sgrp dbsgr.ctrLedger 
        wsLedger wsList wsList - wsLedger.
END.
/* Cashbook */
OUTPUT TO g:\simacc\work\cashbook.csv.
FOR EACH cbkmf WHERE cbkmf.Acb <> 0:
    ASSIGN wsLedger = 0
           wslist = 0.
    FOR EACH glbal WHERE YEAR = 2019 AND glbal.acct = cbkmf.Ledger:
        wsledger = wsledger + glbal.bfbal.
        DO X = 1 TO 12:
            wsledger = wsledger + glbal.amt[X].
        END.
    END.
    FOR EACH cbtrans WHERE CBTrans.Acb = cbkmf.Acb:
        wslist = wslist + CBTrans.amount.
    END.
     EXPORT DELIMITER ',' cbkmf.descrip cbkmf.Acb cbkmf.Ledger
          wsLedger wsList wsList - wsLedger.
END.
