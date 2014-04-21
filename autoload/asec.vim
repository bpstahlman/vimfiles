" TODO: This is one of the things that would be in the user-provided config.
let s:listfile = 'asec-files.list'
fu! asec#init()
    " -- Notes --
    " Refresh builds list file(s): modes are js|php|<none=all>
    " [L]Grf and [LGrd] ignore listfiles, anchoring their search from the
    " current file's dir and the cwd, respectively.
    "
    com! -nargs=? Refresh call asec#refresh(<q-args>)
    com! -bang -nargs=* Gr  call asec#ack(<q-bang>, 0, '', <f-args>)
    com! -bang -nargs=* Grp call asec#ack(<q-bang>, 0, 'php', <f-args>)
    com! -bang -nargs=* Grj call asec#ack(<q-bang>, 0, 'js', <f-args>)
    com! -bang -nargs=* Grf call asec#ack(<q-bang>, 0, 'file', <f-args>)
    com! -bang -nargs=* Grd call asec#ack(<q-bang>, 0, 'dir', <f-args>)

    com! -bang -nargs=* LGr  call asec#ack(<q-bang>, 1, '', <f-args>)
    com! -bang -nargs=* LGrp call asec#ack(<q-bang>, 1, 'php', <f-args>)
    com! -bang -nargs=* LGrj call asec#ack(<q-bang>, 1, 'js', <f-args>)
    com! -bang -nargs=* LGrf call asec#ack(<q-bang>, 1, 'file', <f-args>)
    com! -bang -nargs=* LGrd call asec#ack(<q-bang>, 1, 'dir', <f-args>)

endfu
fu! asec#refresh(mode)
    call s:save_state()
    try
        let cfgs = []
        " TODO: Parameterize - No reason for this to be asec-centric. Allow
        " user to define a dictionary elsewhere, perhaps passing it in a call
        " to init or somesuch... No reason this can't be a more generic
        " mechanism... If so, [L]Grp and [L]Grj would go away, and there'd be
        " mechanism for supplying a list of what would end up being keys into
        " the supplied config: e.g., php, js, etc...
        if (a:mode == 'js' || a:mode == '')
            call add(cfgs, {
                        \'root': ['public', ';asec/src'],
                        \'prunes': ['./resources/js', './shared/extjs', './help/transition/jquery.js'],
                        \'names': ['*.js']})
        endif
        if (a:mode == 'php' || a:mode == '')
            call add(cfgs, {
                        \'root': ['private', ';asec/src'],
                        \'prunes': ['./extensions', './framework', './gii', './tests', './vendors'],
                        \'names': ['*.php']})
        endif
        for cfg in cfgs
            let rootdir = call('finddir', cfg.root)
            if rootdir == ''
                throw "Couldn't locate project base."
            endif
            exe 'lcd ' . rootdir
            " Pattern: Note that the part within prune parens (along with the following -o is optional, contingent upon
            " existence of items in prunes.
            " Assumption: Will always be at least 1 -iname predicate.
            " find ( ( -path EXC_DIR1 -o -path EXC_DIR2 ... ) -prune -false ) -o -iname GLOB1 -o -iname GLOB2 ...
            let oprn = ' ' . shellescape('(') . ' '
            let cprn = ' ' . shellescape(')') . ' '
            " Note: silent avoids the annoying 'Hit enter' prompt.
            let exestr = 'silent !find '
            if len(cfg.prunes)
                call map(cfg.prunes, 'shellescape(v:val)')
                let exestr .= oprn . oprn . ' -path ' . join(cfg.prunes, ' -o -path ') . cprn . ' -prune -false ' . cprn . ' -o '
            endif
            call map(cfg.names, 'shellescape(v:val)')
            let exestr .= ' -iname ' . join(cfg.names, ' -o -iname ') . ' >' . s:listfile
            " Run the find...
            exe exestr
        endfor
    finally
        call s:restore_state()
    endtry
endfu
fu! asec#ack(bang, use_ll, mode, ...)
    call s:save_state()
    try
        " Find dir from which to grep.
        let grepdir = ''
        if a:mode == 'js'
            let grepdir = finddir('public', ';public')
        elseif a:mode == 'php'
            let grepdir = finddir('private', ';private')
        elseif a:mode == 'file'
            let grepdir = expand('%:h')
        elseif a:mode == 'dir'
            let grepdir = getcwd()
        else
            throw "Invalid grep mode `" . a:mode . "'. Supported values: php|js|file|dir"
        endif
        if grepdir == ''
            throw "Couldn't locate project base."
        endif
        exe 'lcd ' . grepdir
        let args = a:000[:]
        call map(args, 'shellescape(v:val)')

        let files_from = ' '
        if (a:mode != 'file' && a:mode != 'dir')
            if !filereadable(s:listfile)
                " TODO: Decide whether to abort, or eventually, continue with next list file
                throw "List file unreadable."
            endif
            let files_from = ' --files-from=' . s:listfile . ' '
        endif
        " TODO: Check for list file existence.
        echo (a:use_ll ? 'l' : '') . 'grep' . (a:bang == '!' ? 'add' : '')
                    \. files_from
                    \. join(args, ' ')
        exe (a:use_ll ? 'l' : '') . 'grep' . (a:bang == '!' ? 'add' : '')
                    \. files_from
                    \. join(args, ' ')
        echomsg "Args: " . join(args, ' ')

    finally
        call s:restore_state()
    endtry
endfu

fu! s:save_state()
    let s:cwd_save = getcwd()
    let s:grepprg_save = &grepprg
endfu
fu! s:restore_state()
    exe 'lcd ' . s:cwd_save
    let &grepprg = s:grepprg_save
endfu

" vim:ts=4:sw=4:et
