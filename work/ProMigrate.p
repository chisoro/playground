/*FOR EACH mundept:
    CREATE dbsmf.
        ASSIGN dbsmf.suburb = mundept.code
               dbsmf.Descrip =  mundept.desc-eng.
END.

FOR EACH dbcmf:
    DISPLAY dbcmf.dbacc.
    PAUSE 0.
    FIND FIRST muncmf WHERE muncmf.acc = dbcmf.dbacc AND muncmf.company = 0 NO-ERROR.
    IF AVAILABLE muncmf THEN
        ASSIGN dbcmf.Suburb = muncmf.dept.
END.

FIND FIRST dbsmf WHERE dbsmf.suburb = 1 NO-ERROR.
IF AVAILABLE dbsmf THEN
    DELETE dbsmf. 
    
FOR EACH dbctf where cons <= 16:
    DELETE dbctf.
END.
FOR EACH muncdesc: /* Consumer */
    CREATE dbctf. 
    ASSIGN dbctf.cons     = muncdesc.type 
           dbctf.Descrip  = muncdesc.des-eng
           dbctf.Interest = muncdesc.ov-int-rate[1].
END.*/
