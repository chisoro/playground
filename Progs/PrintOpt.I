define variable ll-ok       as    logical initial no        no-undo.
define variable prt-file    as    character                 no-undo.
DEF VAR g-prt-file LIKE prt-file.
define variable Wh          as    widget-handle             no-undo.
define variable ll-canc     as    logical initial yes       no-undo.
define variable lc-line     as    character format "x(132)" no-undo.
define variable hed         as    handle                    no-undo.
define variable hfrm        as    handle                    no-undo.
define variable li-linec    as    integer                   no-undo.
define variable l-idir-name as    character extent 3        no-undo.
define variable l-dir-name  as    character                 no-undo.
define variable l-exist     as    log                       no-undo.
/*define variable w-orientation as  character                 no-undo. */
define variable w-letterhead as  char                       no-undo.
define variable w-fontsize as  decimal                      no-undo.
define variable lc-options as    char format "x(40)"        no-undo.
define variable ix         as    int                        no-undo.
define variable ll-ix-fnd  as     log                       no-undo.
DEF VAR  g-prt-output AS INT.
DEF VAR VarFileout    AS CHAR.

form lc-line
    with frame file-prt-frame
    no-box stream-io
    down no-labels
    width 132.

define button btnOK
     label "&OK"
     size 15 by 1.14.

define button btnCancel
     label "&Cancel"
     size 15 by 1.14
     tooltip "Cancel Print".

&if defined(noesc) &then
    define rectangle rect-1
         edge-pixels 2 graphic-edge no-fill 
         size 70 BY 2.
&else
    define rectangle rect-1
         edge-pixels 2 graphic-edge no-fill 
         size 86 BY 2.
&endif

define frame print-selection
     skip(1)
     space(5)
     lc-options VIEW-AS COMBO-BOX
        LIST-ITEMS "Print to PDF file", "Print to System Printer", "Output to File"
        SIZE 35 by 1 INNER-LINES 8
        LABEL "Print Options"
     btnOK
     &if defined(noesc) &then
     &else
     btnCancel
     &endif
     RECT-1         at row 1.5 col 2
    with view-as dialog-box keep-tab-order 
         side-labels no-underline three-d scrollable 
         &if defined(prt-title) &then
            title "{&prt-title}"
         &else
            title " Report Output Destination"
         &endif.


on 'enter' of lc-options apply 'choose' to btnOk.

on 'choose' of btnOk
do:
    case lc-options:screen-value :
        when "Print to System Printer" then
        do:
            hide frame print-selection no-pause.
            file-info:file-name = this-procedure:file-name.
            prt-file = file-info:file-name.
            prt-file = substring(prt-file,1,index(prt-file,".") - 1).
            find simctr no-lock no-error.     
            if avail simctr then
            do:
                do ix = 1 to num-entries(simctr.repdir) :
                    if index(prt-file,entry(ix,simctr.repdir)) <> 0 then
                    do:
                        system-dialog printer-setup update ll-ok in window current-window.
                        if not ll-ok then 
                        do:
                            assign ll-canc = true.
                            apply 'f13' to frame print-selection.
                            return no-apply.
                        end.
                        g-prt-output = 1.
                        hide frame print-selection no-pause.
                        &if defined(manualstream) &then 
                        &else 
                            output {&stream-name} to printer 
                        &endif
                        &if defined(notpaged) &then &else paged &endif 
                        &if defined(pagesize) &then page-size {&pagesize} &endif.
                        &if defined(to-file) &then
                            output {&file-stream} to value({&to-file}).
                        &endif
                        session:set-wait-state("wait").
                        ll-canc = false.
                        &if defined(use-file) &then
                            input {&file-stream} from {&use-file}.
                            repeat:
                                lc-line = "".
                                import {&file-stream} unformatted lc-line.
                                if index(lc-line,chr(12)) <> 0 then page {&stream-name}.  
                                display {&stream-name} lc-line with frame file-prt-frame.
                                down {&stream-name} with frame file-prt-frame.
                            end.
                            input {&file-stream} close.
                        &else
                          run {&print-prog}.
                        &endif
                        session:set-wait-state("").
                        &if defined(keep-open) &then
                        &else
                            &if defined(manualstream) &then 
                            &else 
                                output {&stream-name} close. 
                            &endif
                            &if defined(to-file) &then
                                output {&file-stream} close.
                            &endif
                        &endif
                        apply 'f13' to frame print-selection.
                        /* return */
                        return.
                    end.
                end.
            end.
            if r-index(prt-file,"\") <> 0 then
                prt-file = substring(prt-file,r-index(prt-file,"\") + 1).
            assign prt-file = prt-file       + "_"
                + string(day(today),"99")    + "_"
                + string(month(today),"99")  + "_"
                + string(year(today),"9999") + "_"
                + substr(string(time,"HH:MM:SS"),1,2)
                + substr(string(time,"HH:MM:SS"),4,2)
                + substr(string(time,"HH:MM:SS"),7,2)
                + ".txt".

            FIND FIRST simctr no-lock no-error.
            if not avail simctr then
            do:
               MESSAGE "Control file not setup....Please configure Report deirctory" skip
                    view-as alert-box info.
               apply 'f13' to frame print-selection.
               return.
            end.
                   l-exist    = false.
            input from os-dir(simctr.repdir) no-echo.
            repeat:
               import l-idir-name.
               if l-idir-name[3] <> "D" then next.
               if l-idir-name[1] = "."
               or l-idir-name[1] = ".." then next.
               if l-idir-name[1] = l-dir-name then
                  l-exist = true.
            end.
            input close. 
            l-dir-name = simctr.repdir.
            if l-exist = false then
            do:
               l-idir-name[1] = "".
               os-create-dir value(l-dir-name).
               l-idir-name[1] = string(os-error).
               if int(l-idir-name[1]) <> 0 then
                  MESSAGE "Control file not setup....Please configure Report deirctory" skip
                    view-as alert-box info buttons ok.
            end.
            assign l-dir-name   = l-dir-name 
                   prt-file     = l-dir-name + prt-file
                   g-prt-output = 3
                   g-prt-file   = prt-file.
            &if defined(manualstream) &then 
            &else
                output {&stream-name} to value(prt-file)
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            &if defined(use-file) &then
                input {&file-stream} from {&use-file}.
                repeat:
                    lc-line = "".
                    import {&file-stream} unformatted lc-line.
                    if index(lc-line,chr(12)) <> 0 then page {&stream-name}.
                    display {&stream-name} lc-line with frame file-prt-frame.
                    down {&stream-name} with frame file-prt-frame.
                end.
                input {&file-stream} close.
            &else
             run {&print-prog}.
            &endif
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
               &if defined(manualstream) &then
               &else
                   output {&stream-name} close.
               &endif
               &if defined(to-file) &then
                   output {&file-stream} close.
               &endif
            &endif
            file-info:file-name = prt-file.
            if file-info:file-size = 0 then
            do:
                message "No report was produced. Probably due to lack of data."
                    view-as alert-box info.
                os-delete value(prt-file).
                apply 'f13' to frame print-selection.
                return.
            end.
            system-dialog printer-setup update ll-ok in window current-window.
            if not ll-ok then 
            do:
                assign ll-canc = true.
                apply 'f13' to frame print-selection.
                return no-apply.
            end.
            g-prt-output = 1.
            hide frame print-selection no-pause.
            &if defined(manualstream) &then 
            &else 
                output {&stream-name} to printer 
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            input from value(prt-file).
            repeat:
                lc-line = "".
                import unformatted lc-line.
                if index(lc-line,chr(12)) <> 0 then page {&stream-name}. 
                display {&stream-name} lc-line with frame file-prt-frame.
                down {&stream-name} with frame file-prt-frame.
            end.
            input close.
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
                &if defined(manualstream) &then 
                &else 
                    output {&stream-name} close. 
                &endif
                &if defined(to-file) &then
                    output {&file-stream} close.
                &endif
            &endif
            apply 'f13' to frame print-selection.
            OS-DELETE VALUE(prt-file).
           return. 
            
        end.

        when "Output to File" then
        do:
            prt-file = "".
            system-dialog get-file prt-file save-as title "Specify File Destination..." 
                update ll-ok.
            if not ll-ok or prt-file = "" then 
            do:
                assign ll-canc = true.
                apply 'f13' to frame print-selection.
                return no-apply.
            end.
            g-prt-output = 2.
            g-prt-file = prt-file.
            hide frame print-selection no-pause.
            &if defined(manualstream) &then
            &else
                output {&stream-name} to value(prt-file)
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            &if defined(use-file) &then
                input {&file-stream} from {&use-file}.
                repeat:
                    lc-line = "".
                    import {&file-stream} unformatted lc-line.
                    display {&stream-name} lc-line with frame file-prt-frame.
                    down {&stream-name} with frame file-prt-frame.
                end.
                input {&file-stream} close.
            &else
             run {&print-prog}.
            &endif
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
                &if defined(manualstream) &then 
                &else 
                    output {&stream-name} close. 
                &endif
                &if defined(to-file) &then
                    output {&file-stream} close.
                &endif
            &endif
            
            file-info:file-name = prt-file.
            if file-info:file-size = 0 then
            do:
                message "No report was produced. Probably due to lack of data."
                    view-as alert-box info.
                os-delete value(prt-file).
            end.

            apply 'f13' to frame print-selection.
             /*OS-DELETE VALUE(prt-file). */
             return.
        end.

        when "Print to PDF file" then
        do:
            def var hProcess as int no-undo.
            hide frame print-selection no-pause.
            g-prt-output = 4.

            find simctr  no-lock 
                no-error.
            if not avail simctr then
            do:
               MESSAGE "Control file not setup....Please configure Report deirctory" skip
                    view-as alert-box info buttons ok.
               apply 'f13' to frame print-selection.
               return.
            end.
            l-exist    = false.

            input from os-dir(simctr.repdir) no-echo.
            repeat:
               import l-idir-name.
               if l-idir-name[3] <> "D" then next.
               if l-idir-name[1] = "."
               or l-idir-name[1] = ".." then next.
               if l-idir-name[1] = l-dir-name then
                  l-exist = true.
            end.
            input close.

            l-dir-name = simctr.repdir.

            if l-exist = false then
            do:
               l-idir-name[1] = "".
               os-create-dir value(l-dir-name).
               l-idir-name[1] = string(os-error).
               if int(l-idir-name[1]) <> 0 then
                  MESSAGE "Control file not setup....Please configure Report deirctory" skip
                    view-as alert-box info buttons ok.
            end.

            l-dir-name = l-dir-name.
            file-info:file-name = this-procedure:file-name.
            prt-file = file-info:file-name.
            prt-file = substring(prt-file,1,index(prt-file,".") - 1).
            if r-index(prt-file,"\") <> 0 then
                prt-file = substring(prt-file,r-index(prt-file,"\") + 1).
            prt-file = simctr.repdir + prt-file.
            assign prt-file = prt-file       + "_"
                + string(day(today),"99")    + "_"
                + string(month(today),"99")  + "_"
                + string(year(today),"9999") + "_"
                + substr(string(time,"HH:MM:SS"),1,2)
                + substr(string(time,"HH:MM:SS"),4,2)
                + substr(string(time,"HH:MM:SS"),7,2).
            
            &if defined(manualstream) &then 
            &else
                output {&stream-name} to value(prt-file)
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            &if defined(use-file) &then
                input {&file-stream} from {&use-file}.
                repeat:
                    lc-line = "".
                    import {&file-stream} unformatted lc-line.
                    display {&stream-name} lc-line with frame file-prt-frame.
                    down {&stream-name} with frame file-prt-frame.
                end.
                input {&file-stream} close.
            &else
               run {&print-prog}.
            &endif
            &if defined(keep-open) &then
            &else
               &if defined(manualstream) &then
               &else
                   output {&stream-name} close.
               &endif
               &if defined(to-file) &then
                   output {&file-stream} close.
               &endif
            &endif
            file-info:file-name = prt-file.
            if file-info:file-size = 0 then
            do:
                session:set-wait-state("").
                message "No report was produced. Probably due to lack of data."
                    view-as alert-box info.
                os-delete value(prt-file).
                apply 'f13' to frame print-selection.
                return.
            end.
            create frame hfrm.
            create editor hed
            assign frame     = hfrm
                   large     = true
                   word-wrap = false.
            hed:read-file(prt-file).
            hed:move-to-eof().
            li-linec = hed:cursor-line - 1.

            &if defined(pgorientation) &then
               w-orientation = {&pgorientation}.
            &else
               w-orientation = "Landscape".
            &endif

            &if defined(letterhead) &then
               w-letterhead = {&letterhead}.
            &else
               w-letterhead = "".
            &endif

            &if defined(fontsize) &then
               w-fontsize = {&fontsize}.
            &else
               w-fontsize = 10.0.
            &endif

            run pdf_out.p (prt-file, w-fontsize, li-linec,                     
                w-orientation, w-letterhead).

            if return-value = "cancelled" then
            do:
                os-delete value(prt-file).
                message "Operation cancelled by the user."
                    view-as alert-box info.
                apply 'f13' to frame print-selection.
                return.
            end.
            ll-ix-fnd = false.
            find simctr  no-lock no-error.  
            if avail simctr then
            do:
                do ix = 1 to num-entries(simctr.repdir) :
                    if index(prt-file,entry(ix,simctr.repdir)) <> 0 then
                    do:
                        ll-ix-fnd = true.
                        leave.
                    end.
                end.
            end.
            varFileOut = prt-file + ".pdf".
            FILE-INFO:FILE-NAME = varFileout.
            IF FILE-INFO:FILE-SIZE <> 0 THEN DO:
                varFileOut = "START" + chr(32) + prt-file + ".pdf".
                OS-COMMAND NO-WAIT VALUE(varFileout).
            END.
            apply 'f13' to frame print-selection.
            OS-DELETE VALUE(prt-file).
            return.
        end.

        otherwise 
        do:
            g-prt-output = 5.
            hide frame print-selection no-pause.
            &if defined(manualstream) &then 
            &else 
                output {&stream-name} to value(lc-options:screen-value)
            &endif
            &if defined(notpaged) &then &else paged &endif 
            &if defined(pagesize) &then page-size {&pagesize} &endif.
            &if defined(to-file) &then
                output {&file-stream} to value({&to-file}).
            &endif
            session:set-wait-state("wait").
            ll-canc = false.
            &if defined(use-file) &then
                input {&file-stream} from {&use-file}.
                repeat:
                    lc-line = "".
                    import {&file-stream} unformatted lc-line.
                    if index(lc-line,chr(12)) <> 0 then page {&stream-name}.
                    display {&stream-name} lc-line with frame file-prt-frame.
                    down {&stream-name} with frame file-prt-frame.
                end.
                input {&file-stream} close.
            &else
               run {&print-prog}.
            &endif
            session:set-wait-state("").
            &if defined(keep-open) &then
            &else
                &if defined(manualstream) &then 
                &else 
                    output {&stream-name} close. 
                &endif
                &if defined(to-file) &then
                    output {&file-stream} close.
                &endif
            &endif
            apply 'f13' to frame print-selection.
            return.
        end.
    end case.
end.

&if defined(noesc) &then
    on 'esc' of frame print-selection return no-apply.
&else
on 'esc','f4','window-close' of frame print-selection
    or 'choose' of btnCancel in frame print-selection
        apply 'f13' to frame print-selection.
&endif


Wh = current-window:first-child no-error.
lc-options:SCREEN-VALUE = "Print to PDF file".
view frame print-selection.
enable all with frame print-selection.

assign g-prt-output = ?
       g-prt-file   = ?
       ll-canc      = true.


wait-for 'f13' of frame print-selection or
    close of this-procedure.

hide frame print-selection no-pause.

pause 0.

if valid-handle(Wh) then
do:
    view Wh.
    apply 'entry' to Wh.
end.


