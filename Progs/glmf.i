procedure Search.ip.
    hCol = browse  brw-{&tmptable}:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Account" then
            do:
               OPEN QUERY qry-{&tmptable} 
                   FOR EACH bfr{&tmptable} NO-LOCK
                            where bfr{&tmptable}.acct >= DEC(wsSearch:SCREEN-VALUE)
                               BY bfr{&tmptable}.acct.

            END.
            when "Description" then
            do:
               OPEN QUERY qry-{&tmptable} 
                   FOR EACH bfr{&tmptable} NO-LOCK
                            where bfr{&tmptable}.descrip >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.descrip.

            END. 
            when "Dept" then
            do:
               OPEN QUERY qry-{&tmptable} 
                   FOR EACH bfr{&tmptable} NO-LOCK
                            where bfr{&tmptable}.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY bfr{&tmptable}.dept BY bfr{&tmptable}.acct.

            END.
        END.
        RETURN.
    END.
