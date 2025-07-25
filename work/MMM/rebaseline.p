DEF VAR wsa AS DEC.
DEF VAR wsrate AS DEC FORM "zzzzzz9.99999" INIT 5498.7200.
DEF VAR X AS INT.
FOR EACH dbhtf WHERE (ref = "set-off" OR ref = "rebase-takeon") AND accper = 202307 :
    DISPLAY accper ref.
    PAUSE 0.
    DELETE dbhtf.
END.

/*FOR EACH dbcmf  WHERE dbcmf.dbacc > 90006300 AND dbcmf.dbacc <= 5000000000:
    wsa = 0.
    DISPLAY dbcmf.dbacc.
    PAUSE 0.
    FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK.
        wsa = wsa + dbamt[1].
    END.
    ASSIGN AccBal = wsa
           BalBf = wsa.
END.*/
FOR EACH dbblf: /*WHERE dbblf.dbacc > 90006300 AND dbblf.dbacc <= 5000000000: */
    DISPLAY dbacc.   pause 0.
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
