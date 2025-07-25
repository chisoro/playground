DEF VAR wsamt AS DEC FORM "zzz,zzz,zz9.99-".
FOR EACH dbmf
    FOR EACH dbhtf WHERE dbhtf.dbAcc = 100001 AND dbhtf.dbacc = dbmf.dbacc
        AND dbhtf.Accper <= 201812 BY dbhtf.Sgrp:
        IF FIRST-OF(dbhtf.Sgrp) THEN
            wsAmt = 0.
        wsamt = wsamt + amt.
        DISPLAY acc dbhtf.Accper amt. PAUSE 0.
    END.
    CREATE dbBatch.Age = 5
           dbBatch.Amt = wsamt * 10
           dbBatch.dbacc = dbmf.dbacc
           dbBatch.DESCRIP = dbBatch.Iledger dbBatch.intBatch dbBatch.Proj dbBatch.Ref dbBatch.Seq dbBatch.Sgrp dbBatch.trDate dbBatch.Vat
END.
