" TODO: This is one of the things that would be in the user-provided config.
" TODO: Eventually, may provide a different way to set all this: e.g., function or
" global-vars.
let g:prack_listfile = 'asec-files.list'

" TODO: Remove this comment once I no longer need the big test config in ~/tmp/prack_module_cfg.vim
let g:prack_module_cfg = [
    \{
        \'name': 'php',
        \'shortname': 'p',
        \'root': ['private', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
            \.' -prune -false \) -o -iname ''*.php'''
    \},
    \{
        \'name': 'js',
        \'shortname': 'j',
        \'root': ['public', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./resources/js -o -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
            \.' -prune -false \) -o -iname ''*.js'''
    \}
\]

" >>> Functions used to start/stop the plugin
fu! s:start()
    if exists('s:started')
        " Subsequent start implies stop.
        call s:stop()
    endif
    try
        call s:process_cfg()
        " Only now that we know initialization was successful...
        let s:started = 1
    catch
        echoerr "Cannot start Prack, due to the following error, encountered while processing configuration: " . v:exception
        " TODO: Is this necessary? We haven't done any setup yet... We may
        " have set some static vars (e.g., s:longnames/s:shortnames), but
        " they'd be set next time...
        call s:stop()
    endtry
endfu
fu! s:stop()
    unlet! s:started
    " TODO: General cleanup. E.g., delete commands.
endfu
" <<<
" >>> Functions used to process config
" Process the subproject/module short/long name options.
" Output: Sets the following data structures:
" s:shortnames
"   Dictionary mapping short (single-char) options to the index of the
"   corresponding submodule (in config array).
" s:longnames
"   Sorted List of Dictionaries of the following form:
"   { 'name': <longname>, 're': <regex matching name>, 'idx': <submodule index> }
"   Note: Array is sorted by longname.
fu! s:process_cfg()
    " Determine basename of files used to hold lists of project files.
    if exists('g:prack_listfile') && g:prack_listfile != ''
        let s:listfile = g:prack_listfile
    elseif
        let s:listfile = 'prack-files.list'
    endif

    " TEMP DEBUG
    "so ~/tmp/prack_module_cfg.txt
    if !len(g:prack_module_cfg)
        " TODO: Consider throwing exception here.
        echoerr "Must define at least one project module within g:prack_module_cfg"
        return
    endif
    " Build snapshot of global config until re-initialization.
    let s:module_cfg = []
    " Short (single-char) names stored in a hash
    " Long names stored in sorted array of patterns employing \%[...]
    let s:shortnames = {}
    let longnames = []
    let lname_to_index = {}
    let ln = len(g:prack_module_cfg)
    let i = 0
    " Note: Index into s:module_cfg (i.e., valid configs only)
    let cfg_idx = 0
    while i < ln
        let cfg = g:prack_module_cfg[i]
        if !has_key(cfg, 'root')
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `root' pattern specified."|echohl None
            let i = i + 1 | continue
        endif
        if !has_key(cfg, 'find')
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `find' string specified."|echohl None
            let i = i + 1 | continue
        endif
        let selectable = 0
        if has_key(cfg, 'name')
            " TODO: Don't hardcode these patterns...
            if (cfg.name !~ '^[a-zA-Z0-9_]\+$')
                echohl WarningMsg|echomsg "Ignoring invalid (long) name `" . cfg.name|echohl None
                            \. "': must be sequence of characters matching [a-zA-Z0-9_]."
            else
                if !has_key(lname_to_index, cfg.name)
                    let selectable = 1
                    let lname_to_index[cfg.name] = cfg_idx
                    call add(longnames, cfg.name)
                else
                    echohl WarningMsg|echomsg "Warning: Name " . cfg.name . " used multiple times. All but first usage ignored."|echohl None
                endif
            endif
        endif
        if has_key(cfg, 'shortname')
            if (cfg.shortname !~ '^[a-zA-Z0-9_]$')
                echohl WarningMsg|echomsg "Ignoring invalid shortname `" . cfg.shortname|echohl None
                            \. "': must be single character matching [a-zA-Z0-9_]."
            else
                if !has_key(s:shortnames, cfg.shortname)
                    let selectable = 1
                    let s:shortnames[cfg.shortname] = cfg_idx
                else
                    echohl WarningMsg|echomsg "Warning: Name " . cfg.shortname . " used multiple times. All but first usage ignored."|echohl None
                endif
            endif
        endif
        " Subproject must be selectable by at least 1 of shortname/longname
        if !selectable
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": at least 1 name or shortname must be specified."|echohl None
            let i = i + 1 | continue
        endif
        " Current config item not skipped: i.e., valid subproject.
        call add(s:module_cfg, deepcopy(cfg, 1))
        " Save original index, in case it's needed for reporting.
        " TODO: Perhaps save a title string (e.g., `--longname, -shortname')
        " for reporting purposes...
        let s:module_cfg[cfg_idx].orig_idx = i
        let cfg_idx = cfg_idx + 1
        let i = i + 1
    endwhile
    " If no valid subprojects, no point in continuing...
    if !len(s:module_cfg)
        throw "No valid subprojects. :help prack-config"
    endif
    " Process the long options to determine the portions required for
    " uniqueness.
    call sort(longnames)
    let ln = len(longnames)
    let i = 1
    let reqs = [matchstr(longnames[0], '^.')]
    while i < ln
        " Get common portion + 1 char (if possible)
        let ml = matchlist(longnames[i], '\(\%[' . longnames[i-1] . ']\)\(.\)\?')
        let [cmn, nxt] = ml[1:2]
        " Is it possible we need to lengthen previous req?
        if len(cmn) >= len(reqs[i-1])
            " Can we lengthen previous req?
            if len(reqs[i-1]) < len(longnames[i-1])
                " Set to common portion + 1 char (if possible)
                let reqs[i-1] = matchstr(longnames[i-1], cmn . '.\?')
            endif
        endif
        " Take only as much of current as is required for uniqueness:
        " namely, common portion + 1 char.
        call add(reqs, cmn . nxt)
        let i = i + 1
    endwhile
    " Build an array of patterns and corresponding indices.
    " Note: Arrays reqs and longnames are parallel, and original indices can
    " be looked up in lname_to_index.
    let s:longnames = []
    let i = 0
    while i < ln
        let re = reqs[i]
        " Is the long name longer than the required portion?
        if len(longnames[i]) > len(reqs[i])
            " Append the optional part within \%[...]
            let re .= '\%[' . longnames[i][len(reqs[i]):] . ']'
        endif
        call add(s:longnames, {'name': longnames[i], 're': re, 'idx': lname_to_index[longnames[i]]})
        let i = i + 1
    endwhile
endfu
" Return s:module_cfg index corresponding to input short/long option name (or
" -1 if matching config not found).
" Input(s):
"   opt
"     Short or long option name, prefixed with 's:' or 'l:' to indicate
"     short/long.
fu! s:get_cfg_idx(opt)
    let [is_short, name] = [a:opt[0] == 's', a:opt[2:]]
    if is_short
        if has_key(s:shortnames, name)
            let idx = s:shortnames[name]
        elseif
            return -1
        endif
    else
        " Look for name in s:longnames (array of {'name', ..., 're': ..., 'idx': ...})
        for lname in s:longnames
            if name <# lname.name
                break
            elseif name ==# lname.name || name =~# lname.re
                return lname.idx
            endif
        endfor
        " Not found!
        return -1
    endif
endfu
" <<<
" >>> Functions pertaining to file processing
" Convert occurrences of * and **[[SIGN]NUMBER] in input glob to corresponding
" pattern, using the the rules specified in Vim help on 'file-searching': in a
" nutshell...
" * matches any number of non-slash characters.
" ** matches 0 .. NUMBER directories, with the following pre-processing on
" NUMBER:
"   NUMBER ommitted or NUMBER < 0
"       30 (the default)
"   NUMBER == 0
"       Simply remove the **0
"   NUMBER > 100
"       100 (the max)
" Note: `0 .. NUMBER' means that...
"   /a/**/*.c
" ...will match /a/foo.c
" Implication: The pattern we construct can't require the `/' following the
" `**'. This is the reason that the regex used to parse the glob treats a
" trailing `/' as part of the match.
" Design Decision: Don't complicate algorithm to allow for escaped slashes in
" glob. Anyone who does such a thing can expect indeterminate behavior, even
" if I were to go to great lengths here to handle it.
" Design Decision: (sign number) is optional but a sign without a number is
" invalid (i.e., the starstar won't be recognized).
fu! Convert_stars(glob)
    let re_star = '\%(\\.\|[^*]\)*\z 

    " Vim_Bug: NFA engine may have issues with this...
    " !!!!!!!!!!!!!!UNDER CONSTRUCTION!!!!!!!!!!!!!!!!
    echo substitute('ab\*cd', '\%#=1\%(\(\\.\)\@>\|\([^*]\)\)\*', '|\1:\2|', '')
    echo substitute('ab\zcd', '\%#=1\%(\%(\%(\\.\)\@>\|[^z]\)*\)\@>\zsz\ze\%([^z]\|$\)', '{{foo}}', 'g')
    let re_starstar = '\%(^\|/\)\@<=\%(\*\*\)\%(\([-+]\)\?\(0\|[1-9][0-9]*\)\)\?\(/\|$\)'
    " Vim_Bug: Both \f and [^\f] match `:'
    " Design Decision: Defer grouping of dir_seg to avoid redundant grouping.
    let dir_seg = '\%(\\[^\f]\|[^/]\)\+'
    let patt = ''
    let [si, sio] = [0, 0]
    while si >= 0
        let si = match(a:glob, re_starstar, sio)
        " Accumulate up to (but not including) match or to end of string.
        let patt .= a:glob[sio : si >= 0 ? si - 1 : -1]
        if si >= 0
            " Assumption: matchlist guaranteed to succeed.
            " Note: matchlist seems to return a minimum of 10 elements, even
            " when fewer submatches...
            let [match, sign, number, slash; rest] = matchlist(a:glob, re_starstar, si)
            if number == ''
                let number = 100
            elseif sign == '-'
                " Assumption: Presence of sign implies non-empty number.
                let number = 30
            else
                let number = number + 0
                if number > 100
                    let number = 100
                endif
            endif
            " Build the pattern.
            " Note: Omission of number == 0 case ensures removal of a **0 and
            " any subsequent slash.
            if number >= 1
                " Number between 1 and 100 inclusive
                " Note: In the number==1 case, brace quantifier will be {,0}:
                " pointless, but valid.
                let patt .= '\%(' . dir_seg . '\%(/' . dir_seg . '\)\{,' . (number - 1) . '}' . slash . '\)\?'
            endif
            let sio = si + strlen(match)
        endif
    endwhile
    return patt
endfu
fu! Test_Convert_stars(glob)
    let files = readfile("C:/Users/stahlmanb/tmp/files.lst")
    let patt = Convert_stars(a:glob)
    for f in files
        echo "Path:  " . f
        echo "Patt:  " . patt
        let m = matchstr(f, '^' . patt)
        echo "Match: " . m
    endfor
endfu

fu! Glob_to_patt(glob)
    let patt = ''
    " Handle leading anchor (if any)
	" ./ works just like Vim
	" .// specifies the current working dir
	" all other paths must match from the beginning
    if a:glob =~ '^\.//'
        " Use fnamemodify to ensure trailing slash.
        let patt = fnamemodify(getcwd(), ':p')
    elseif a:glob =~ '^\./'
        " Note: This will default to same as .// if no current file.
        let patt = fnamemodify(expand('%:h'))
    endif
    " Convert **N everywhere in glob
endfu
" Canonicalize the filenames in listfile, converting to be relative to rootdir
" if possible.
" Format: Canonical form uses `/', and has no `.' or `..' components, leading
" or otherwise.
" Note: Nonexistent paths cannot be fully canonicalized; they will be retained
" in list in partially canonicalized form: expand converts slashes but not .
" and ..
" Caveat: If there happen to be special chars in the pathname (unlikely),
" expand will attempt to expand, so it's best to ensure pathnames don't have
" `*', `**', $ENV, etc...
" Return: The canonicalized list.
fu! Canonicalize_listfile(rootdir, listfile)
    let fnames = readfile(a:listfile)
    " Note: We must set 'shellslash' for canonicalization.
    " TODO: Perhaps do the save/restore only at top level?
    call s:save_state()
    try
        exe 'lcd ' . a:rootdir
        " Get rootdir (local cwd) in canonical form, relativizing to rootdir
        " if possible.
        " Note: Use fnamemodify and expand in 2-step process to canonicalize:
        " 1. fnamemodify with :p gets full path (but possibly with incorrect
        "    slashes)
        " 2. expand canonicalizes the slashes in the path returned by
        "    fnamemodify.
        " Note: Could use `:.' modifier to relativize, but this strips the
        " drive name (when it's default), which I don't want.
        " Note: fnamemodify ensures a `/' at the end of a directory name, and
        " expand won't remove it.
        let rootdir_p = expand(fnamemodify('.', ':p'))
        " Build canonicalized list within loop.
        let fnames_c = []
        for fname in fnames
            " Start with canonical path.
            let fname = expand(fnamemodify(fname, ':p'))
            " Can we make it relative to rootdir?
            let ei = matchend(fname, '^' . rootdir_p)
            if ei > 0
                " Keep only the relative part.
                let fname = fname[ei :]
            endif
            " Add canonical name to list.
            call add(fnames_c, fname)
        endfor
    catch
        " TODO: What error?
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call s:restore_state()
    endtry
    " Return canonicalized list.
    return fnames_c
endfu
" <<<
" >>> Functions used during command execution
" Save any state that needs to be changed to perform the grep.
" TODO: Perhaps a string of flags indicating which state to save? (Need to add
" ability to save regex engine to use.)
fu! s:save_state()
    let s:ssl_save = &ssl
    let s:cwd_save = getcwd()
    let s:grepprg_save = &grepprg
endfu
" Restore state that was changed to perform the grep.
fu! s:restore_state()
    let &ssl = s:ssl_save
    exe 'lcd ' . s:cwd_save
    let &grepprg = s:grepprg_save
endfu
" Convert string containing only long and short options into list in which
" each element represents a single option in one of the following forms:
" s:<opt_char>
" l:<opt_name
" Note: Order of options is preserved. If an option is supplied multiple
" times, it will be included only once in the returned list.
" Example:
" --php -abc --js
" ==> ['l:php', 's:a', 's:b', 's:c', 'l:js']
fu! s:extract_opts(optstr)
    let opts = []
    let uniq = {}
    let segs = split(a:optstr, '\s\+')
    for seg in segs
        if seg[0:1] == '--'
            let opt = seg[2:]
            let k = 'l:' . opt
            if !has_key(uniq, k)
                let uniq[k] = 1
                call add(opts, k)
            endif
        else
            let i = 1
            let ln = strlen(seg)
            while i < ln
                " Assumption: Caller guarantees non-empty segments; hence,
                " this call to matchstr will always succeed.
                let c = matchstr(seg, '.', i)
                let i += strlen(c)
                let k = 's:' . c
                if !has_key(uniq, k)
                    let uniq[k] = 1
                    call add(opts, k)
                endif
            endwhile
        endif
    endfor
    return opts
endfu
" Break input command line into 2 pieces: 1) a string containing the prack
" options, and 2) everything else.
" Returns a Dictionary with the following keys:
"   opt
"     List of all short and long options, in which long options are
"     represented as 'l:loptname', and short options as 's:soptname'.
"   rem
"     command line remaining after prack option removal
fu! s:parse_cmdline(cmdline)
    " Example valid forms: (Note that Cmd will already have been stripped.)
    "   Cmd --
    "   Cmd --abcd -efg
    "   Cmd --abcd -efg --
    "   Cmd --abcd -efg -- --ack --options
    "   Cmd --abcd -efg non prack options
    "   Cmd non prack options
    " 3 part regex, 2 of them capturing:
    "   1. Sequence of long/short prack opts (optional, captured)
    "   2. `--' Separating prack opts from anything that follows (optional)
    "   3. Everything else (possibly empty, captured)
    " Test Note: Tested on 28Dec2013
    " TODO: Bug in new Vim regex engine precludes use. Need to force old,
    " either with \%#=1 or using option.
    "                                      (---------------------------1---------------------------)  <--------------2-------------->     (-3--)
    let m = matchlist(a:cmdline, '\%#=1^\s*\(\%(--\?[^-[:space:]]\+\)\%(\s\+--\?[^-[:space:]]\+\)*\)\?\%(\%(^\|\s\+\)--\%(\s\+\|$\)\)\?\s*\(.*\)$')
    let [optstr, remstr] = m[1:2]
    return {'opts': s:extract_opts(optstr), 'rem': remstr}
endfu
" <<<
" >>> Functions invoked by commands
fu! s:refresh(opts)
    call s:save_state()
    try
        let pcl = s:parse_cmdline(a:opts)
        if pcl.rem != ''
            echoerr "Invalid arguments specified in Refresh command: `" . pcl.rem . "'"
        endif
        " Get list of config indices.
        " Design Decision: Abort on bad option, even if multiple specified.
        let cfg_idxs = []
        for opt in pcl.opts
            let cfg_idx = s:get_cfg_idx(opt)
            if cfg_idx == -1
                echoerr "Invalid subproject option: " . (opt[0] == 'l' ? '--' : '-') . opt[2:]
            endif
            call add(cfg_idxs, cfg_idx)
        endfor
        " Process the selected configs.
        for cfg_idx in cfg_idxs
            let cfg = s:module_cfg[cfg_idx]
            let rootdir = call('finddir', cfg.root)
            if rootdir == ''
                throw "Couldn't locate project base. Make sure your cwd is within the project."
            endif
            exe 'lcd ' . rootdir
            " Note: silent avoids the annoying 'Hit enter' prompt.
            exe 'silent !' . cfg.find . ' >' . s:listfile
            " Canonicalize
            call s:canonicalize_listfile(s:listfile)
        endfor
    catch /Vim(echoerr)/
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call s:restore_state()
    endtry
endfu
fu! s:ack(bang, use_ll, mode, args)
    call s:save_state()
    try
        " Break raw command line into parsed prack options, and string
        " containing everything else (essentially, the ack options).
        let pcl = s:parse_cmdline(a:args)
        " Get list of config indices.
        " Design Decision: Abort on bad option, even if multiple specified.
        " TODO: Refactor to make this part of command line parsing, since it's used in several places.
        let sels = []
        for opt in pcl.opts
            " Note: In case it's needed for display, convert s:a to -a and l:abc to --abc
            let [idx, opt] = [s:get_cfg_idx(opt), (opt[0] == 'l' ? '--' : '-') . opt[2:]]
            if idx == -1
                echoerr "Invalid subproject option: " . opt
            else
                call add(sels, {'idx': idx, 'opt': opt})
            endif
        endfor
        if a:mode != 'file' && a:mode != 'dir'
            " Process the selected configs.
            for sel in sels
                let cfg = s:module_cfg[sel.idx]
                let rootdir = call('finddir', cfg.root)
                if rootdir == ''
                    throw "Couldn't locate project base. Make sure your cwd is within the project."
                endif
                exe 'lcd ' . rootdir
                if !filereadable(s:listfile)
                    " TODO: Decide whether to abort, or eventually, continue with next list file
                    echohl WarningMsg|echomsg "Warning: Skipping unreadable listfile `"
                                \. getcwd() . "/" . s:listfile . "', selected by "
                                \. sel.opt|echohl None
                endif
                " Run [l]grep[add] with appropriate args.
                exe (a:use_ll ? 'l' : '') . 'grep' . (a:bang == '!' ? 'add' : '')
                            \. ' --files-from=' . s:listfile
                            \. ' ' . pcl.rem
            endfor
        else
            " Run [l]grep[add] with appropriate args.
            exe (a:use_ll ? 'l' : '') . 'grep' . (a:bang == '!' ? 'add' : '')
                        \. ' ' . pcl.rem
        endif
    catch /Vim(echoerr)/
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call s:restore_state()
    endtry
endfu
" <<<
" >>> Commands
" -- Notes --
" Refresh builds list file(s) specified by command line options
" [L]Grf and [LGrd] ignore listfiles, anchoring their search from the
" current file's dir and the cwd, respectively.
"
com! PrackStart call <SID>start()
com! PrackStop call <SID>stop()
com! -nargs=? Refresh call <SID>refresh(<q-args>)
com! -bang -nargs=* Gr  call <SID>ack(<q-bang>, 0, '', <q-args>)
com! -bang -nargs=* Grf call <SID>ack(<q-bang>, 0, 'file', <q-args>)
com! -bang -nargs=* Grd call <SID>ack(<q-bang>, 0, 'dir', <q-args>)

com! -bang -nargs=* LGr  call <SID>ack(<q-bang>, 1, '', <q-args>)
com! -bang -nargs=* LGrf call <SID>ack(<q-bang>, 1, 'file', <q-args>)
com! -bang -nargs=* LGrd call <SID>ack(<q-bang>, 1, 'dir', <q-args>)
" <<<

" vim:ts=4:sw=4:et:fdm=marker:fmr=>>>,<<<
