"let g:prack_grepprg = grep -n $* /dev/null
"let g:prack_grepformat = %f:%l:%m,%f:%l%m,%f  %l%m

let g:prack_config = {
    \'listfile': 'files.list',
    \'maxgrepsize': 50000,
    \'projects': {
        \'asec': {
            \'foo': 'bar',
            \'subprojects': {
                \'php': {
                    \'shortname': 'p',
                    \'root': ['private', ';asec/src'],
                    \'find': 'find . \( \( '
                        \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
                        \.' -prune -false \) -o -iname ''*.php'''
                \},
                \'js': {
                    \'shortname': 'j',
                    \'root': ['public', ';asec/src'],
                    \'find': 'find . \( \( '
                        \.' -path ./resources/js -o -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
                        \.' -prune -false \) -o -iname ''*.js'''
                \}
            \}
        \}
    \}
\}

" The structure above will be mirrored by a list which contains only valid
" (though possibly disabled) entries, and whose values are dictionaries
" containing all of the original keys, plus the following:
" TODO: Consider making it mirror the above exactly: complexity engendered by
" culling invalid entries isn't worth much, especially now that we have
" concept of 'disabled'...
"
" rootdir
"   Root directory in canonical form
" files
"   List of file paths in canonicial form (possibly sorted)

" Note: This one probably won't be part of the structure.
" file_filter
"   Array in which each element is a 2 element array as follows:
"   0: index into s:files
"   1: byte offset into indexed path in s:files

" >>> Functions used to start/stop the plugin
fu! s:start()
    if exists('s:started')
        " Subsequent start implies stop.
        call s:stop()
    endif
    try
        call s:process_cfg()
        " TODO: Do caching here...
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
" >>> Static data used to process options
" Notes:
" -'type' is return value of type()
" -No need to specify 'type' if there's a 'default'.
" -'required' means user *must* provide a value.
"  TODO: It's actually unnecessary, as it can be deduced from presence/absence
"  of others: e.g., 'default', 'vim'
let s:opt_cfg = {
    \'listfiles': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1,
        \'default': 'files.list'
    \},
    \'maxgrepsize': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 0,
        \'default': 50000
    \},
    \'grepprg': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'default': 'grep -n $* /dev/null',
        \'vim': 'grepprg'
    \},
    \'grepformat': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'default': '%f:%l:%m,%f:%l%m,%f  %l%m',
        \'vim': 'grepformat'
    \},
    \'find': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1,
        \'required': 1
    \},
    \'root': {
        \'#comment': "Would be highly unusual to define root globally, but it could be made to work with appropriate subproject-specific finds...",
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1,
        \'required': 1
    \}
\}
" <<<
" >>> Functions used to process config
" Note: ... is base, defaults to {}
fu! s:opts_create(opts, lvl, ...)
    let base = a:0 > 0 ? a:1 : {}
    let opts = {'opts': a:opts, 'lvl': a:lvl, 'base': base}
    " Optional input object filled with the following flags:
    " def, set, vim, lvl
    " Assumption: Prior validation is such that failure to find a value for
    " the option (possibly default) implies internal error.
    fu! opts.get(name, ...) dict
        let value = ''
        if a:0 > 0
            " Empty input object.
            call filter(flags, 0)
        endif
        call extend(flags, {'def': 0, 'set': 0, 'vim': '', 'lvl': -1})
        let obj = self
        while !empty(obj)
            if has_key(obj.opts, a:name)
                let flags.lvl = obj.lvl
                let flags.set = 1
                let value = obj.opts[a:name]
                break
            endif
            " Move up to parent
            let obj = obj.base
        endwhile
        " Is this a valid option?
        " TODO: Consider whether to add 'invalid' option (noting that that
        " would amount to an internal error).
        if has_key(s:opt_cfg, a:name)
            " Is there a corresponding Vim option?
            if has_key(s:opt_cfg[a:name], 'vim')
                let flags.vim = s:opt_cfg[a:name].vim
            endif
            if !flags.set && has_key(s:opt_cfg[a:name], 'default')
                let flags.def = 1
                let flags.set = 1
                let value = s:opt_cfg[a:name].default
            endif
        endfu
        return value
    endfu
    fu! opts.set(name, value) dict
        let self.opts[a:name] = a:value
    endfu
    return opts
endfu
" TODO: This test function is obsolete, now that format of the get method has
" changed!
fu! Test_opts_create()
    let opts = s:opts_create({'boo': 1, 'hoo': 2, 'yong': 3}, 0, {})
    let p_opts = s:opts_create({'hoo': 21, 'lala': 49}, 1, opts)
    let sp_opts = s:opts_create({'egg': 234, 'foo': 523, 'yong': 299, 'grepper': 923}, 2, p_opts)

    call opts.set('fooper', 'blooper')

    let objs = [p_opts, sp_opts]
    let keys = ['boo', 'hoo', 'yong', 'lala', 'egg', 'foo', 'grepper', 'fooper']
    for obj in objs
        let lvl = obj.lvl
        for key in keys
            let flags = {}
            let value = obj.get(key, flags)
            echo key . "\t: L" . lvl . "\t|" . value . "|\tFound at lvl: " . flags.lvl
        endfor
    endfor
endfu
" TODO: Something that differentiates this from runtime (cmdline) option
" processing.
fu! s:extract_opts(raw, lvl, base)
    let opts = s:opts_create({}, lvl, base)
    for [opt_name, opt_val] in items(raw)
        " TODO: As long as opts are not segregated in a subkey like 'opts', if
        " we want to warn about non-existent opts, we need to maintain a set
        " of names that are not opts, but are expected: e.g., 'shortname'.
        " Decision: Don't complicate things unnecessarily by treating things
        " like 'shortname' as options.
        if !has_key(s:opt_cfg, opt_name) | continue | endif
        " Option exists.
        let opt_cfg = s:opt_cfg[opt_name]
        if lvl < opt_cfg.minlvl || lvl > opt_cfg.maxlvl
            " Option can't be set at this level; ignore.
            " TODO: Add warning!
            continue
        endif
        " Validate type
        if type(opt_val) != opt_cfg.type
            " TODO: Flesh out the error message. Also, should we just skip for
            " now?
            throw "Invalid value supplied for option " . opt_name
        endif
        " Option value is valid.
        call opts.set(opt_name, opt_val)
    endfor
    " Make sure all options that are required by this level have been set.
    for [opt_name, opt_cfg] in items(s:opt_cfg)
        if opt_cfg.required && a:lvl > opt_cfg.maxlvl
            let flags = {}
            call opts.get(opt_name, flags)
            if !flags.set
                throw "Missing required option: " . opt_name
            endif
        endif
    endfor
    return opts
endfu
" Process the subproject/module short/long name options.
" Output: Sets the following data structures:
" s:shortnames
"   Dictionary mapping short (single-char) options to the index of the
"   corresponding submodule (in config array).
" s:longnames
"   Sorted List of Dictionaries of the following form:
"   { 'name': <longname>, 're': <regex matching name>, 'idx': <submodule index> }
"   Note: Array is sorted by longname.
fu! s:process_cfg(p_name)
    if !has_key(g:prack_config, p_name)
        throw "No configuration found. :help TODO prack_config???"
    endif
    " TODO: extract_opts will throw on (eg) missing required args - handle
    " somehow...
    let g_opt = s:extract_opts(g:prack_config, 0, {})
    let p_cfg = g:prack_config[p_name]
    let s:p_opt = s:extract_opts(p_cfg, 1, g_opt)
    if !has_key(p_cfg, 'subprojects')
        throw "No subprojects defined for project " . p_name
    endif
    if type(p_cfg.subprojects) != 4
        throw "Invalid subprojects definition for project " . a:p_name . ". Must be Dictionary."
    endif

    " Build snapshot of project opt/cfg until re-initialization.
    let s:sp_cfg = []
    let s:sp_opt = []
    " Short (single-char) names stored in a hash
    " Long names stored in sorted array of patterns employing \%[...]
    let s:shortnames = {}
    let longnames = []
    let lname_to_index = {}
    let i = 0
    " Note: Index into s:sp_cfg (i.e., valid configs only)
    let cfg_idx = 0
    let sf = s:sf_create()
    for [sp_name, sp_cfg] in items(p_cfg.subprojects)
        let s:sp_opt[cfg_idx] = s:opts_create(sp_cfg, 2, s:p_opt)
        let selectable = 0
        " TODO: Don't hardcode these patterns...
        if (sp_name !~ '^[a-zA-Z0-9_]\+$')
            echohl WarningMsg|echomsg "Ignoring invalid subproject name `" . sp_name|echohl None
                        \. "': must be sequence of characters matching [a-zA-Z0-9_]."
        else
            if !has_key(lname_to_index, sp_name)
                let selectable = 1
                let lname_to_index[sp_name] = cfg_idx
                call add(longnames, sp_name)
            else
                echohl WarningMsg|echomsg "Warning: Name " . sp_name . " used multiple times. All but first usage ignored."|echohl None
            endif
        endif
        if has_key(sp_cfg, 'shortname')
            if (sp_cfg.shortname !~ '^[a-zA-Z0-9_]$')
                echohl WarningMsg|echomsg "Ignoring invalid shortname `" . sp_cfg.shortname|echohl None
                            \. "': must be single character matching [a-zA-Z0-9_]."
            else
                if !has_key(s:shortnames, sp_cfg.shortname)
                    let selectable = 1
                    let s:shortnames[sp_cfg.shortname] = cfg_idx
                else
                    echohl WarningMsg|echomsg "Warning: Name " . sp_cfg.shortname . " used multiple times. All but first usage ignored."|echohl None
                endif
            endif
        endif
        " Subproject must be selectable by at least 1 of shortname/longname
        if !selectable
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": at least 1 name or shortname must be specified."|echohl None
            let i = i + 1 | continue
        endif
        " Current config item not skipped: i.e., valid subproject.
        call add(s:sp_cfg, {})
        " Cache reference for convenience below...
        let cfg = s:sp_cfg[-1]
        " Add name, which was represented only as key of source object
        cfg.name = sp_name
        " Save original index, in case it's needed for reporting.
        " TODO: Perhaps save a title string (e.g., `--longname, -shortname')
        " for reporting purposes...
        let cfg.orig_idx = i
        let rootdir = call('finddir', s:sp_opt.get('root'))
        if rootdir == ''
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": Couldn't locate project base. Make sure your cwd is within the project."|echohl None
            " TODO: Don't set disabled - remove (or don't add) to list. Note
            " that this currently is also set in cache_listfile...
            let cfg.disabled = 1
            let i = i + 1 | continue
        endif
        " Save full path of rootdir.
        let cfg.rootdir = s:canonicalize_path(rootdir, '')
        " Build cache, but don't force refresh.
        call s:cache_listfile(cfg, 0)
        let cfg_idx = cfg_idx + 1 " Keep up with sp_idx
        let i = i + 1 " Keep up with original idx
    endfor
    call sf.destroy()
    " If no valid subprojects, no point in continuing...
    if !len(s:sp_cfg)
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
    "echo "Config:"
    "echo s:sp_cfg
    "echo "longnames:"
    "echo s:longnames
    "echo "shortnames:"
    "echo s:shortnames
    let g:shortnames = s:shortnames

    return
    " UNDER CONSTRUCTION!!!!! Pick up here...
    " Verify required options!!!!!!!
    " TODO: This one is mandatory at sp level.
    if !has_key(sp_cfg, 'root')
        echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `root' pattern specified."|echohl None
        let i = i + 1 | continue
    endif
    " TODO: This one could be set higher.
    if !has_key(sp_cfg, 'find')
        echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `find' string specified."|echohl None
        let i = i + 1 | continue
    endif
endfu
fu! s:process_cfg_old()
    " Determine basename of files used to hold lists of project files.
    if exists('g:prack_listfile') && g:prack_listfile != ''
        let s:listfile = g:prack_listfile
    elseif
        let s:listfile = 'prack-files.list'
    endif

    if !len(g:prack_config)
        " TODO: Consider throwing exception here.
        echoerr "Must define at least one project module within g:prack_submodule_cfg"
        return
    endif
    " Build snapshot of global config until re-initialization.
    let s:sp_cfg = []
    " Short (single-char) names stored in a hash
    " Long names stored in sorted array of patterns employing \%[...]
    let s:shortnames = {}
    let longnames = []
    let lname_to_index = {}
    let ln = len(g:prack_submodule_cfg)
    let i = 0
    " Note: Index into s:sp_cfg (i.e., valid configs only)
    let cfg_idx = 0
    let sf = s:sf_create()
    while i < ln
        let cfg = g:prack_submodule_cfg[i]
        if !has_key(cfg, 'root')
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `root' pattern specified."|echohl None
            let i = i + 1 | continue
        endif
        if !has_key(cfg, 'find')
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": no `find' string specified."|echohl None
            let i = i + 1 | continue
        endif
        let selectable = 0
        " TODO: Probably require valid name, since we sometimes expect one.
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
        call add(s:sp_cfg, deepcopy(cfg, 1))
        let cache_cfg = s:sp_cfg[-1]
        " Save original index, in case it's needed for reporting.
        " TODO: Perhaps save a title string (e.g., `--longname, -shortname')
        " for reporting purposes...
        let cache_cfg.orig_idx = i
        let rootdir = call('finddir', cfg.root)
        if rootdir == ''
            echohl WarningMsg|echomsg "Warning: Skipping invalid subproject config at index " . i . ": Couldn't locate project base. Make sure your cwd is within the project."|echohl None
            let cache_cfg.disabled = 1
            let i = i + 1 | continue
        endif
        " Save full path of rootdir.
        let cache_cfg.rootdir = s:canonicalize_path(rootdir, '')
        " Build cache, but don't force refresh.
        call s:cache_listfile(cache_cfg, 0)
        let cfg_idx = cfg_idx + 1
        let i = i + 1
    endwhile
    call sf.destroy()
    " If no valid subprojects, no point in continuing...
    if !len(s:sp_cfg)
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
    "echo "Config:"
    "echo s:sp_cfg
    "echo "longnames:"
    "echo s:longnames
    "echo "shortnames:"
    "echo s:shortnames
    let g:shortnames = s:shortnames

endfu

" Return s:sp_cfg index corresponding to input short/long option name (or
" -1 if matching config not found).
" Input(s):
"   opt
"     Short or long option name, prefixed with 's:' or 'l:' to indicate
"     short/long.
fu! s:get_cfg_idx(opt)
    let [is_short, name] = [a:opt[0] == 's', a:opt[2:]]
    if is_short
        if has_key(s:shortnames, name)
            return s:shortnames[name]
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

" Try various ways to get a subproject-specific value for specified option.
fu! s:get_opt(sp_idx, opt, default_to_vim)
    if has_key(s:sp_cfg[a:sp_idx], a:opt)
        return s:sp_cfg[a:sp_idx][a:opt]
    elseif exists('g:prack_' . a:opt)
        " TODO: Revisit this when plugin global options are reworked.
        return g:prack_{a:opt}
    elseif a:default_to_vim
        " Default to the Vim option value in effect for current window.
        return getwinvar(0, '&' . a:opt)
    else
        throw "Internal error: No value found for option " . a:opt
    endif
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
" TODO: Have this done once at Refresh and initial load and saved in a
" persistent structure.
fu! s:cache_listfile(cache_cfg, force_refresh)
    let success = 0
    " See whether the listfile exists and is readable.
    let listfile_path = a:cache_cfg.rootdir . s:listfile
    let listfile_path_found = findfile(s:listfile, a:cache_cfg.rootdir)
    " TODO: Rely upon higher-level try/catch?
    let sf = s:sf_create()
    try
        " Both listfile generation and file canonicalization require us to be
        " in root dir.
        call sf.pushd(a:cache_cfg.rootdir)
        " Do we need to create/update the listfile?
        if listfile_path_found == '' || a:force_refresh
            let v:errmsg = ''
            " Note: silent avoids the annoying 'Hit enter' prompt.
            exe 'silent !' . a:cache_cfg.find . ' >' . s:listfile
            if v:errmsg != ''
                throw "Encountered error attempting to build listfile `" . listfile_path . "' for subproject "
                    \. a:cache_cfg.name . ": " . v:errmsg . ". Disabling subproject."
            endif
        endif
        " If here, we expect to have a file to read.
        if !filereadable(listfile_path)
            throw "Couldn't open listfile `" . listfile_path . "' for subproject "
                \. a:cache_cfg.name . ". Disabling subproject."
        endif
        let files_raw = readfile(listfile_path)
        " Note: We must set 'shellslash' for canonicalization.
        call sf.setopt('shellslash', 1, {'boolean': 1})
        " Build canonicalized list within loop.
        let files = []
        for file_raw in files_raw
            "echo "Before: " file_raw
            let file_raw = s:canonicalize_path(file_raw, a:cache_cfg.rootdir)
            "echo "After: " file_raw
            "echo "rootdir: " a:cache_cfg.rootdir
            " Add canonical name to list.
            call add(files, file_raw)
        endfor
        " TODO: Make this optional so we can skip if user's find or whatever
        " ensures sorted.
        " TODO: Should we uniquify? Doing so could permit some optimizations
        " (e.g., find_insert_idx).
        call sort(files)
        let a:cache_cfg.files = files
        "echo s:sp_cfg
        "echo a:cache_cfg
        let success = 1
    catch
        call s:warn(v:exception)
        let a:cache_cfg.disabled = 1
        let a:cache_cfg.files = []
    finally
        call sf.destroy()
    endtry
    return success
endfu
" <<<
" >>> Functions pertaining to file processing
" Convert occurrences of * and **[[SIGN]NUMBER] in input glob to corresponding
" pattern; also, build array of constraints representing the content and
" relative location of fixed strings within the glob (which can be used to
" prevent costly regex matches when a simple string match is sufficient to
" disqualify a candidate match target).
" Note: 'partial' argument determines whether the generated pattern will be
" anchored at end.
" CAVEAT: partial and end_anchor are mutually exclusive (since we don't know
" where to apply end anchor if we're not anchoring at end).
" Note: The * and ** constructs are treated in a manner nearly identical to
" what is described in Vim help on 'file-searching': in a nutshell...
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
" if I were to go to great lengths to handle it here.
" Design Decision: Don't complicate pattern to allow for backslash-escaped
" `*': Vim doesn't even support this in a glob (at least not in Windows).
" Design Decision: (sign number) is optional but a sign without a number is
" invalid (i.e., the starstar won't be recognized).
" Algorithm Note: A fully general approach to performing both sets of
" replacements (* and **) would involve either 1) processing both in parallel
" (i.e., in single loop with prioritization/ordering logic), or 2) processing
" in separate loops, calculating the replacements but deferring the actual
" substitutions until all calculations have been stored into some sort of
" sorted structure, which facilitates building the new string.
" However... There's a much simpler approach, which relies upon the fact that
" the replacement for `*' can never result in a spurious **.
" Rationale: Lookbehind/lookahead ensure no adjacent `*', and the replacement
" contains only 1.
" Approach: Replace all occurrences of * first, then ** in separate loop.
" Returns: dict with the following keys:
"   'patt'
"     the regex pattern corresponding to the input glob
"     Note: If input glob contained leading/trailing fixed strings, they will
"     not be represented in patt, but in start_anchor/end_anchor,
"     respectively.
"   'start_anchor'
"     If non-empty, string that must match at start of canonical path.
"   'end_anchor'
"     If non-empty, string that must match at end of canonical path.
"   'constraints'
"     List of constraint strings, which must match exactly somewhere within
"     region that excludes any start/end anchors.
fu! s:glob_to_patt(glob, partial)
    let re_star = '\%(^\|[^*]\)\@<=\*\%([^*]\|$\)\@='
    let star = '[^/]*'
    let re_starstar = '\%(^\|/\)\@<=\%(\*\*\)\%(\([-+]\)\?\(0\|[1-9][0-9]*\)\)\?\(/\|$\)'
    " Extract the fixed string constraints.
    " TODO: Consider integrating this into the * and ** processing (as part of
    " refactoring that combines their processing).
    " Note: Deferring putting the constraint generation into separate function
    " because of refactoring possibility.
    let fixeds = split(a:glob, re_star . '\|' . re_starstar, 1)
    let constraints = []
    " Assumption: Because keepempty was set in call to split, non-empty
    " leading/trailing strings imply start/end anchors (subject to caveat
    " below).
    " Caveat: If partial matching, end_anchor will be treated as normal fixed
    " constraint.
    let nf = len(fixeds)
    let start_anchor = nf > 0 ? fixeds[0] : ''
    let end_anchor = !a:partial && nf > 1 ? fixeds[-1] : ''
    " Loop over interior fixeds: skip leading (and trailing if not partial).
    " Note: Loop won't be entered if there are 0 or 1 fixeds.
    let [i, ln] = [1, nf - (empty(end_anchor) ? 0 : 1)]
    while i < ln
        if fixeds[i] != ''
            call add(constraints, fixeds[i])
        endif
        let i += 1
    endwhile
    " Design Decision: Returned pattern excludes any fixed strings at
    " start/end, which are represented by start_anchor/end_anchor.
    " Note: end_anchor will be empty string here if unused.
    let glob = a:glob[len(start_anchor) : -len(end_anchor) - 1]
    " Process *
    let glob = substitute(glob, re_star, star, 'g')
    " Process **
    " Design Decision: Defer grouping of dir_seg to avoid redundant grouping.
    " TODO: Is there any point in supporting backslash-escaping of
    " non-filename chars? Possibly not, as they might not be handled correctly
    " elsewhere...
    let dir_seg = '\%(\\\%(\f\)\@!.\|[^/]\)\+'
    let patt = ''
    let [si, sio] = [0, 0]
    while si >= 0
        let si = match(glob, re_starstar, sio)
        if si > 0
            " Accumulate part before the match.
            let patt .= glob[sio : si - 1]
        elseif si < 0
            " No more matches: accumulate remainder of string
            let patt .= glob[sio : -1]
        endif
        if si >= 0
            " Assumption: matchlist guaranteed to succeed.
            " Note: matchlist seems to return a minimum of 10 elements, even
            " when fewer submatches...
            let [match, sign, number, slash; rest] = matchlist(glob, re_starstar, si)
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
                " Note: ** syntax never requires match of any directories.
                let patt .= '\%(' . dir_seg . '\%(/' . dir_seg . '\)\{,' . (number - 1) . '}' . slash . '\)\?'
            endif
            " Advance past match
            let sio = si + strlen(match)
        endif
    endwhile
    " Assumption: There can be no alternation in the top-level of pattern
    " (hence, no need for grouping parens).
    " Rationale: Globs don't support alternation.
    let patt = '^' . patt
    if !a:partial
        let patt .= '$'
    endif
    return {'patt': patt,
        \'start_anchor': start_anchor,
        \'end_anchor': end_anchor,
        \'constraints': constraints}
endfu

fu! Test_Convert_stars(glob)
    let files = readfile("C:/Users/stahlmanb/tmp/files.lst")
    let patt = s:glob_to_patt(a:glob)
    for f in files
        "echo "Path:  " . f
        "echo "Patt:  " . patt
        let m = matchstr(f, '^' . patt)
        "echo "Match: " . m
    endfor
endfu

" Return a filtered version of cfg.files comprising only the files located
" hierarchically within specified dir.
" TODO: Remove if not needed... Work in progress...
fu! s:get_filtered_files(cfg, dir)
    let files = a:cfg.files
    let files_filter = []
    let idx = 0
    for file in files
        if a:dir == ''
            call add(files_filter, [idx, 0])
        else
            " Is 
        endif
        let idx = idx + 1
    endfor
endfu

" Find region within input files array, for which constraints do not preclude
" a match.
" Returns: a range of the form [start_index, end_index], else -1 (if no such
" range exists)
fu! s:get_matching_files_bracket(constraints, files)
    let ln = len(files)
    if len(constraints) == 0 || constraints[0].anchor != 's'
        " No start anchor means we'll be searching the entire list.
        return [0, ln - 1]
    endif
    " If here, we have a constraint that anchors at start.
    let fixed = constraints[0].string

    let [si1, si2] = [0, ln - 1]
    let si = ln / 2
    let ei = -1
    let [ei1, ei2] = [-1, -1]
    " Find start
    while 1
        let file = files[si]
        let cmp = s:strcmp(file, fixed)
        if cmp < 0
            " Before start of range
            let si1 = si
            let si = si1 + (si2 - si1) / 2
        elseif cmp > 0
            " After end of range
            let si
            let ei = si
        else
            " In range
        endif
    endwhile
endfu

" Helper routine for get_matching_files
fu! s:get_matching_files_match(patt_info, files)
    " Cache some vars for efficiency.
    let c_lst = a:patt_info.constraints
    let patt = a:patt_info.patt
    let c_len = len(c_lst)
    let f_lst = a:files
    "echo "constraints:"
    "echo c_lst
    "echo "patt: " . patt
    " Note: We don't apply patt to portions of filename covered by anchors.
    let [sanch, eanch] = [a:patt_info.start_anchor, a:patt_info.end_anchor]
    let [c_sanch_len, c_eanch_len] = [len(sanch), len(eanch)]
    " If possible, narrow the region of file list to be searched.
    " Rationale: No point in checking constraints where the start anchor has
    " already precluded a match.
    if sanch != ''
        let [f_s_i, f_e_i] = s:bracket(f_lst, sanch, s:compare_file_fn)
    else
        let [f_s_i, f_e_i] = [0, len(f_lst) - 1]
    endif
    "echo "sanch: " . sanch
    "echo "eanch: " . eanch
    " TODO: Consider returning List of indices instead of actual files.
    " !!!!!! UNDER CONSTRUCTION !!!!!!!! (27Feb2014)
    let matches = []
    let f_i = f_s_i
    while f_i <= f_e_i
        let f_raw = f_lst[f_i]
        let f_raw_len = len(f_raw)
        " Start/end anchor checks are quickest to check, and may permit
        " short-circuiting.
        if c_sanch_len + c_eanch_len > f_raw_len
            " Filename too short.
            let f_i += 1 | continue
        endif
        if sanch != '' && f_raw[0 : c_sanch_len - 1] != sanch ||
            \eanch != '' && f_raw[-c_eanch_len : ] != eanch
            " Skip to next file.
            "echo "Skipping due to start/end anchor fail"
            let f_i += 1 | continue
        endif
        if f_raw_len == c_sanch_len + c_eanch_len
            " Anchored constraints were sufficient to determine match.
            " Note: Couldn't be any floating constraints or glob.
            " Note: In theory, we could get here because of empty pattern, but
            " we should probably check that before calling this method.
            call add(matches, f_raw)
            let f_i += 1 | continue
        endif
        " Couldn't confirm or deny match using only anchored constraints.
        " Discard any anchored portions before subsequent processing.
        let f = f_raw[c_sanch_len : -1 - c_eanch_len]
        if c_len > 0
            " Loop through the unanchored constraints, short-circuiting on
            " disqualification.
            " Keep up with earliest point at which constraint could match.
            " Rationale: Unanchored constraint match can't start before end of
            " earliest possible match of preceding unanchored constraint.
            let ms_ci = 0
            for c in c_lst
                let c_slen = len(c)
                let i = stridx(f, c, ms_ci)
                if i == -1
                    "echo "Skipping due to floating anchor "
                    "echo "f: " . f . "  c: " . c . "  ms_ci: " . ms_ci
                    break
                else
                    " Don't search the matched portion (or anything prior) again.
                    let ms_ci = i + c_slen
                endif
            endfor
            " Since VimL has no labeled continues...
            if i < 0
                " At least one floating constraint failed.
                let f_i += 1 | continue
            endif
        endif
        " If here, match can't be precluded; time to try regex pattern match.
        " Note: Match target is the portion of file excluding any
        " start/end anchor. The patt must match at start of this portion;
        " whether it must match at end is determined by patt itself.
        " TODO: Should let user's fileignorecase or wildignorecase setting
        " determine =~ or =~?.
        "echo "Matching " . f . " against " . patt
        if f =~ patt
            "echo "Success!"
            call add(matches, f_raw)
        endif
        let f_i += 1 " Advance to next file.
    endwhile
    return matches
endfu

fu! s:get_matching_files(cfg, glob, partial)
    let matches = []
    let sf = s:sf_create()
    try
        let anchor_dir = ''
        " Handle leading anchor (if any)
        " ./ works just like Vim
        " .// specifies the current working dir
        " all other paths must match from the beginning
        " TODO - UNDER CONSTRUCTION!!!!!!!!
        " To speed things up, extract all fixed strings (non-*, non-**) within
        " the glob, and skip the pattern match if any aren't found in the
        " target file string.
        let glob = a:glob
        if glob =~ '^\.//'
            " Use fnamemodify to ensure trailing slash.
            let anchor_dir = s:canonicalize_path(getcwd(), a:cfg.rootdir)
            let glob = anchor_dir . glob[3:] " strip .//
        elseif a:glob =~ '^\./'
            " Note: This will default to same as .// if no current file.
            let anchor_dir = s:canonicalize_path(expand('%:h'), a:cfg.rootdir)
            let glob = anchor_dir . glob[2:] " strip ./
        endif
        " Convert * and ** in glob, and extract fixed string constraints into
        " a special structure to be used for optimization.
        " Rationale: A fixed string at head allows us to delineate (using
        " adaptive search) a region within file list, outside of which matches
        " cannot exist. Limiting search to this region can speed things up
        " considerably.
        let patt_info = s:glob_to_patt(glob, a:partial)
        "echo "patt_info: "
        "echo patt_info
        let matches = s:get_matching_files_match(patt_info, a:cfg.files)
    catch
        " TODO: What error?
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call sf.destroy()
    endtry
    return matches
endfu

fu! Test_get_matching_files(cfg_idx, glob, partial)
    let matches = s:get_matching_files(s:sp_cfg[a:cfg_idx], a:glob, a:partial)
    for m in matches
        echo m
    endfor
endfu

" Canonicalize the input path, attempting to make relative to rootdir (if
" non-empty).
fu! s:canonicalize_path(path, rootdir)
    " Note: Use fnamemodify and expand in 2-step process to canonicalize:
    " 1. fnamemodify with :p gets full path (but possibly with incorrect
    "    slashes)
    " 2. expand canonicalizes the slashes in the path returned by fnamemodify.
    " Note: Could use `:.' modifier to relativize, but this strips the
    " drive name (when it's default), which I don't want.
    " Note: fnamemodify ensures a `/' at the end of a directory name, and
    " expand won't remove it.
    "echo "Input path: " . a:path . " -- cwd: " . getcwd()
    let path = expand(fnamemodify(a:path, ':p'))
    "echo "path: " . path
    if (a:rootdir != '')
        let rootdir = expand(fnamemodify(a:rootdir, ':p'))
        "echo "rootdir: " . rootdir
        " Can we make it relative to rootdir?
        let ei = matchend(path, '^' . rootdir)
        if ei > 0
            " Keep only the relative part.
            let path = path[ei :]
        endif
    endif
    return path
endfu
" <<<
" >>> Functions used to read/write internal data structures.
" TODO - Remove if nothing goes here...
fu! Show_statics()
    let s:
endfu
" <<<
" >>> Functions for displaying Errors/Warnings
" TODO: Make warn vs. error configurable.
fu! s:warn(msg)
    try
        echohl WarningMsg
        echomsg "Warning: " . a:msg
    catch
    finally
        echohl None
    endtry
endfu
" <<<
" >>> Functions used to implement stack frames
fu! s:sf_create()
    " Create dictionary to encapsulate the stack frame.
    let sf = {'opts': {}, 'dirs': []}
    fu! sf.destroy() dict
        " Restore all modified settings to their saved values.
        for [name, val] in items(self.opts)
            " Note: No need to complicate data structures by keeping up with
            " whether option is boolean (since let-& can handle it
            " transparently.)
            " Note: :exe can access containing scope.
            exe 'let &l:' . name . ' = l:val'
        endfor
        if len(self.dirs)
            " Restore what was cwd at stack frame creation.
            exe 'lcd ' . self.dirs[0]
        endif
        " Design Decision: Ideally, destruction would happen automatically
        " when the object goes out of scope, but this is not how Vim script
        " works. Since caller may keep a reference to self beyond the destroy
        " call, safest course is to make it look as it did at creation.
        " Rationale: Subsequent attempts to use this stack frame (thought not
        " in keeping with its intent) should not be harmful.
        let self.opts = {}
        let self.dirs = []
    endfu
    " Inputs:
    " name - option name
    " val  - desired option value
    " ...  - optional extra args in a single dict.
    "        op      - assignment operator to use: e.g., +=, -=, etc...
    "                  (default =)
    "        boolean - true if the option to be set is a boolean option.
    "                  For boolean options, value determines whether set is of
    "                  form...  `set {opt-name}' or `set no{opt-name}'
    fu! sf.setopt(name, val, ...) dict
        if !has_key(self.opts, a:name)
            " Save only the value to be restored at stack frame destruction:
            " i.e., the first set after creation.
            " Note: No need to specify `&l:', since &opt gives it to us
            " automatically when global and local settings differ.
            exe 'let self.opts[a:name] = &' . a:name
        endif
        " Initialize defaults for the optional extra args
        let [op, boolean] = ['=', 0]
        if a:0
            " Caller supplied extra options. Process...
            let opt = a:1
            if has_key(opt, 'op') | let op = opt.op | endif
            if has_key(opt, 'boolean') | let boolean = !!opt.boolean | endif
        endif
        " Set the option
        " Note: Use set[l] in lieu of let-&.
        " Rationale: Supports more assignment operators: e.g., += doesn't work
        " for string-list options with let-&.
        exe 'setl '
            \. (boolean ? (!!a:val ? '' : 'no') : '')
            \. a:name
            \. (boolean ? '' : op . escape(a:val, ' \'))
    endfu
    " Change cwd, pushing the old onto a stack.
    fu! sf.pushd(dir) dict
        call add(self.dirs, getcwd())
        " Note: As mentioned in Vim doc (:help :filename), commands expecting
        " single filename don't require escaping.
        exe 'lcd ' . a:dir
    endfu
    fu! sf.popd() dict
        " Question: Should I add logic to handle underflow (which is
        " essentially an internal error)?
        if len(self.dirs)
            " lcd to popped dir.
            exe 'lcd ' . remove(self.dirs, -1)
        endif
    endfu
    " Return the stack frame
    return sf
endfu

" <<<
" >>> Functions used during command execution
" Save any state that needs to be changed to perform the grep.
" TODO: Perhaps a string of flags indicating which state to save? (Need to add
" ability to save regex engine to use.)
" Caveat: save/restore calls MUST occur in matched pairs; all but outermost
" calls are do-nothing.
" TODO!!!!!!!!!!!!!! Change this mechanism completely: use dict functions and
" stack frame instances...
" TODO: Remove this after replacing completely...
fu! s:save_state()
    if !exists('s:save_level')
        let s:save_level = 0
    endif
    if s:save_level == 0
        let s:ssl_save = &ssl
        let s:cwd_save = getcwd()
        let s:grepprg_save = &grepprg
    endif
    let s:save_level = s:save_level + 1
endfu
" Restore state that was changed to perform the grep.
fu! s:restore_state()
    if s:save_level > 0
        let s:save_level = s:save_level - 1
        if s:save_level == 0
            let &ssl = s:ssl_save
            exe 'lcd ' . s:cwd_save
            let &grepprg = s:grepprg_save
        endif
    endif
endfu
" Convert string containing only long and short options into list in which
" each element represents a single option in one of the following forms:
" s:<opt_char>
" l:<opt_name>
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
fu! s:parse_refresh_cmdline(cmdline)
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

" Parse input spec designating subproject(s) and glob.
" Input spec format:
" [-<sopts>][--<lopt>[,<lopt>]...][:<glob>]
" Note: Both the subproject and glob components are optional, but if non-null
" glob is specified, it is always preceded by a `:', to remove potential
" ambiguities, which arise because of the possibility of both `-' and `:'
" appearing in filenames.
" Returns: An object representation of the input spec, containing the
" following keys:
"   idxs: array of indices corresponding to selected subprojects, -1 if
"         unconstrained (i.e., subproject spec omitted)
"   glob: file glob as string, '' if glob omitted
"         Note: Omitted glob effectively selects all files in selected
"         subproject(s).
" Rationale: Here are some specs that would be ambiguous if the `:' were not
" mandatory...
" Ambiguous spec #1:
"   -bar
" Possible interpretations:
"   1. Selects any file in any of the 3 subprojects designated by short
"      options -b, -a, and -r
"   2. Specifies file -bar in any subproject
" Ambiguous spec #2:
"   :foo
" On a filesystem that allows `:' in a filename, is the `:' part of the glob,
" or is it an explicit (albeit unnecessary) separator between (omitted)
" subproject spec and glob `foo'?
fu! s:parse_spec(opt, throw)
    let oc = '[a-zA-Z0-9_]' " chars that can appear in option
    "                  <sopts>              <lopts>                            <glob>
    let re_opt = '^\%(-\('.oc.'\+\)\)\?\%(--\('.oc.'\+\%(,'.oc.'\+\)*\)\)\?\%(:\(.*\)\)\?'
    let ms = matchlist(a:opt, re_opt)
    if empty(ms)
        if a:throw
            throw "parse_spec: Bad option: " . a:opt
        else
            return -1
        endif
    endif
    let g:dbgms = ms
    " We have a valid spec.
    let [all, soptstr, loptstr, glob; rest] = ms
    " Extract the various short and long options.
    if empty(soptstr) && empty(loptstr)
        " Special case: Nothing before the `:' means no subproject
        " constraints.
        let sels = -1
    else
        let sels = []
        let sopts = split(soptstr, '\zs') "split chars
        let lopts = split(loptstr, ',')
        " Create a single array for looping, but remember dividing point.
        let num_short_opts = len(sopts)
        let opts = sopts + lopts
        let i = 0
        " Get list of config indices.
        " Design Decision: Abort on bad option, even if multiple specified.
        " TODO: Refactor to make this part of command line parsing, since it's used in several places.
        for opt in opts
            let is_short = i < num_short_opts
            let cfg_idx = s:get_cfg_idx((is_short ? 's:' : 'l:') . opt)
            " TODO: How to handle invalid specs? For now, just skipping, but
            " perhaps, in accordance with Design Decision above, we should
            " abort...
            if cfg_idx < 0
                call s:warn("Invalid subproject spec: " . (is_short ? '-' : '--') . opt)
                continue
            endif
            " Add index to list if not already added.
            if (-1 == index(sels, cfg_idx))
                call add(sels, cfg_idx)
            endif
            let i += 1
        endfor
    endif
    " Note: sels will be either a list of cfg_idx's or -1 for unconstrained.
    return {'idxs': sels, 'glob': glob}
endfu
" Convert input spec to corresponding list of files.
" TODO: More complete docs... E.g., document format somewhere.
" [{sp_idx: <idx>, files: [<path>, ...]}, ...]
" TODO: Need to support --php with no :<glob> following. Currently, the : is
" mandatory.
fu! s:get_files_for_spec(spec, partial, throw)
    let ret = []
    let opt = s:parse_spec(a:spec, a:throw)
    if type(opt) != 4 " Dict
        echoerr "Bad spec: " . a:spec
        return []
    endif
    if type(opt.idxs) == 3 " List
        let sp_idxs = opt.idxs
    else
        " No subproject constraints
        let sp_idxs = range(len(s:sp_cfg))
    endif
    let files = []
    for sp_idx in sp_idxs
        let fs = s:get_matching_files(s:sp_cfg[sp_idx], opt.glob, a:partial)
        " Accumulate subproject-specific object.
        call add(ret, {'idx': sp_idx, 'files': fs})
    endfor
    return ret
endfu
" TODO: Could probably combine with parse_refresh_cmdline with a bit of
" refactoring.
fu! s:parse_grep_cmdline(args)
    let argidx = 0 " Will point to first non-plugin arg at loop termination
    " Command line can contain multiple specs; simply concatenate the arrays
    " returned by each call to get_files_for_spec(), each of which corresponds
    " to a different spec, with each spec potentially involving multiple
    " subprojects).
    let sps = []
    for arg in a:args
        if arg == '--'
            let argidx += 1
            break
        elseif arg[0] != '-' && arg[0] != ':'
            break
        else
            " TODO: Decide about partial
            call extend(sps, s:get_files_for_spec(arg, 1, 1))
        endif
        let argidx += 1
    endfor
    " Return a dict containing array of parsed specs and array of non-plugin
    " args.
    "return {'pspecs' => sps, 'args' => a:args[argidx:]}
    return [sps, a:args[argidx :]]
endfu
" This one is only for :Edit, :Split, et al.
" It's fundamentally different from parse_grep_cmdline and
" parse_refresh_cmdline in that the option format is more rigid.
" Note: Currently, command is defined as taking only 1 arg, which means
" multiple args will be treated as 1; if I defined it as taking any number
" (like grep commands), I could probably commonize the basic command line
" parsing, with a post-parse step that checks for correct # of args.
fu! s:parse_edit_cmdline(cmd, bang, filespec)
    let sf = s:sf_create()
    try
        " UNDER CONSTRUCTION!!!!!
        " TODO: I'm thinking we need to have get_files_for_spec return a more
        " complex structure, which has subproject index broken out...
        let sps = s:get_files_for_spec(a:filespec, 1, 1)
        " 3 cases: 0 files, 1 file, multiple files
        let num_sps = len(sps)
        if num_sps > 1
            " Note: Don't bother looping over sps to determine exact number; all
            " that matters is it's more than 1.
            let cnt = 2
        elseif num_sps == 1
            " How many files in one and only matching sp?
            let cnt = len(sps[0].files)
        else
            let cnt = 0
        endif
        if cnt == 0
            echoerr "Can't find file matching " . a:filespec
        elseif cnt > 1
            echoerr "Too many files matching " . a:filespec
        endif
        " If error not raised by now, we have a single file to open; move to
        " its subproject to execute the applicable open command, then restore
        " cwd (by destroying stack frame).
        let sp_cfg = s:sp_cfg[sps[0].idx]
        call sf.pushd(sp_cfg.rootdir)
        " Note: For each of the plugin editing commands, there's a Vim
        " equivalent whose name is identical except for capitalization.
        exe tolower(a:cmd) . a:bang . ' ' . fnameescape(sp_cfg.files[0])

    catch /Vim(echoerr)/
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call sf.destroy()
    endtry
endfu
" Convert potentially abbreviated plugin command name to canonical version of
" the Vim equivalent: e.g.,
" Grepa => grepadd
" Lg => lgrep
" Lgrepa => lgrepadd
fu! s:canonicalize_grep_cmd(cmd)
    if a:cmd =~ '\<Gr\%[ep]\>'
        return 'grep'
    elseif a:cmd =~ '\<Grepa\%[dd]\>'
        return 'grepadd'
    elseif a:cmd =~ '\<Lgr\%[ep]\>'
        return 'lgrep'
    elseif a:cmd =~ '\<Lgrepa\%[dd]\>'
        return 'lgrepadd'
    endif
    throw "Internal error: Unknown grep command: " . a:cmd
endfu

" <<<
" >>> Functions used for completion
" Return list of filenames, filtered by the glob before the cursor, whose
" format is described by... TODO
" Important Note: Because the completion is used for commands that open files,
" the filenames we return must be relative to a known location: e.g., cwd or
" subproject root.
" Issue: When command is actually executed, we won't be able to tell from the
" subproject-relative filename alone which subproject it was in (though we
" could probably figure it out).
" Solution: Since we'll be processing the command line before executing the
" command, go ahead and prepend something that indicates which subproject the
" file was found in: e.g., php://some/file.php
" --php:**/foo/file1.php
" --js:./file2.js
" --cpp:.//**/file3.cpp
fu! s:complete_filenames(arg_lead, cmd_line, cursor_pos)
    let sps = s:get_files_for_spec(a:arg_lead, 1, 1)
    let files = []
    for sp in sps
        let sp_idx = sp.idx
        let sp_name = s:sp_cfg[sp_idx].name
        let sp_files = sp.files
        call map(sp_files, '"--" . l:sp_name . ":" . v:val')
        call extend(files, sp_files)
    endfor
    return files
endfu
" <<<
" >>> Functions invoked by commands
fu! s:refresh(opts)
    let sf = s:sf_create()
    try
        let pcl = s:parse_refresh_cmdline(a:opts)
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
            let cache_cfg = s:sp_cfg[cfg_idx]
            let rootdir = call('finddir', cache_cfg.root)
            if rootdir == ''
                throw "Couldn't locate project base. Make sure your cwd is within the project."
            endif
            let rootdir = s:canonicalize_path(rootdir, '')
            "exe 'lcd ' . rootdir
            call sf.pushd(rootdir)
            let cache_cfg.rootdir = rootdir
            call s:cache_listfile(cache_cfg, 1)
        endfor
    catch /Vim(echoerr)/
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call sf.destroy()
    endtry
endfu

" Input: List in following form...
" [{idx: <sp_idx>, files: <files>}, ...]
" ...in which the sp_idx's are unordered and non-unique, with the same files
" potentially appearing under multiple subprojects.
" Output: Equivalent list, but with each subproject represented only once (and
" only if it contains files), in fiducial order, with files sorted and unique
" within subproject
fu! s:sort_and_combine_pspecs(pspecs)
    let pspec_map = {}
    for pspec in a:pspecs
        if !has_key(pspec_map, pspec.idx)
            let pspec_map[pspec.idx] = []
        endif
        let files = pspec_map[pspec.idx]
        " Interleave current pspec.files with all previously-encountered files
        " for same subproject.
        let ins_idx = 0 " position in tgt list
        let idx = 0     " position in src list
        " Keep up with # of files in tgt list so we'll know when returned
        " ins_idx indicates position past end.
        let num_files = len(files)
        for f in pspec.files
            let [ins_idx, dup] = s:find_insert_idx(files, ins_idx, f)
            if ins_idx >= num_files
                " This and all subsequent files should be appended.
                " TODO: Is this faster than continuing with insert?
                call extend(files, pspec.files[idx : ])
                break
            elseif !dup
                " Non-duplicate, non-final
                call insert(files, f, ins_idx)
                let num_files += 1
            endif
            let idx += 1
        endfor
    endfor
    " Iterate the arrays stored in pspec_map in sp_idx order to build return
    " array.
    let _pspecs = []
    for sp_idx in sort(keys(pspec_map))
        " Discard empty subprojects.
        if !empty(pspec_map[sp_idx])
            call add(_pspecs, {'idx': sp_idx, 'files': pspec_map[sp_idx]})
        endif
    endfor
    return _pspecs
endfu
" TODO: Test only
fu! Test_sort_and_combine_pspecs()
    let pspecs = [
        \{'idx': 0, 'files': ["abc", "def", "ghi", "jkl", "wx"]},
        \{'idx': 2, 'files': ["abc", "def", "ghi", "jkl"]},
        \{'idx': 0, 'files': ["abc", "jkl"]},
        \{'idx': 0, 'files': ["a", "defg", "gh", "ghi", "uv", "wx", "yz"]}]
    let pspecs = s:sort_and_combine_pspecs(pspecs)
    echo pspecs
endfu
" Return an array of the following form:
" [{idx: <sp_idx>, files: escaped_file_list}]
" Form is similar to that returned by get_files_for_spec, except that files is
" a single command line escaped string, rather than a list of files. Also note
" that the pspecs in the input list will be transformed to ensure that the
" following constraints are met:
" -Each subproject represented only once unless maxlen constraint forces
"  processing single subproject with multiple greps.
" -Files are sorted within subproject
" -Files are unique within subproject
" -Subprojects are in fiducial order
fu! s:xargify_pspecs(pspecs, fixlen, maxlen)
    let pspecs = s:sort_and_combine_pspecs(a:pspecs)
    if empty(pspecs)
        return []
    endif
    let _pspecs = [] " transformed pspec list
    for pspec in pspecs
        if !exists('l:_pspec') || _pspec.idx != pspec.idx
            if exists('l:_pspec')
                " Accumulate
                call add(_pspecs, _pspec)
            endif
            let _pspec = {'idx': pspec.idx, 'files': ''}
            let cumlen = a:fixlen
        endif
        " Accumulate as many files as possible without exceeding maxlen
        for f in pspec.files
            " Escape and prepend space before length test.
            let f = ' ' . fnameescape(f)
            let len_f = strlen(f)
            " Decision: Always accumulate at least one file regardless of
            " maxlen constraint.
            if len(_pspec.files) && len_f + cumlen > a:maxlen
                " Accumulate early.
                call add(_pspecs, _pspec)
                let _pspec = {'idx': pspec.idx, 'files': ''}
                let cumlen = a:fixlen
            endif
            let _pspec.files .= f
            let cumlen += len_f
        endfor
    endfor
    if exists('l:_pspec')
        " Accumulate final.
        call add(_pspecs, _pspec)
    endif
    return _pspecs
endfu
fu! s:get_unconstrained_pspecs()
    let sp_idx = 0
    let ret = []
    for cfg in s:sp_cfg
        call add(ret, {idx: sp_idx, files: cfg.files})
        let sp_idx += 1
    endfor
    return ret
endfu
fu! s:grep(cmd, bang, ...)
    let sf = s:sf_create()
    try
        " Parse cmdline into an array of specs and trailing args (which will
        " be supplied to grepprg)
        let [pspecs, args] = s:parse_grep_cmdline(a:000)
        " Turn the array of args into escaped string.
        let grepargs = s:convert_arg_list_to_string(args, 0)
        " Convert plugin grep command into Vim grep command in canonical form.
        let grepcmd = s:canonicalize_grep_cmd(a:cmd)
        " Is this grep command naturally adding?
        let isadd = grepcmd[-3 : ] == 'add'
        if empty(pspecs)
            " No specs provided: search all files in all subprojects...
            " Note: This is different from non-empty pspecs that contain no
            " files...
            let pspecs = s:get_unconstrained_pspecs()
        endif
        " TODO: Where to configure max len? Also, calculate fixlen
        let pspecs = s:xargify_pspecs(pspecs, 100, 4096)

        " TODO: Consider creating an object used to access options as function
        " of project and subproject, which performs caching: e.g.,
        " opt.get(sp_idx, opt) Note: This would be cleaner than accessing
        " s:sp_cfg everywhere.

        " Process each subproject in turn...
        " Note: A subproject will be represented multiple times (in succession)
        " in the pspecs array iff xargification constraints prevent the
        " processing of all the subproject's files in a single grep.
        let sp_idx_prev = -1
        for pspec in pspecs
            let cfg = s:sp_cfg[pspec.idx]
            let grepadd = ''
            if pspec.idx != sp_idx_prev
                " First encounter with this subproject
                " Move to root of subproject.
                call sf.pushd(cfg.rootdir)
                " Note: If user has not overridden grepprg and grepformat,
                " keep current Vim setting.
                let grepprg = s:get_opt(pspec.idx, 'grepprg', 1)
                if !empty(grepprg) | call sf.setopt('grepprg', grepprg) | endif
                let grepformat = s:get_opt(pspec.idx, 'grepformat', 1)
                if !empty(grepformat) | call sf.setopt('grepformat', grepformat) | endif
            elseif sp_idx_prev != -1 && !isadd
                " This grep is overflow due to xargification, and the grep
                " command isn't adding by nature: thus, append 'add' to the
                " grep command's fiducial form.
                let grepadd = 'add'
            endif
            
            " Run [l]grep[add] with appropriate args.
            " TODO: Append escaped, joined args...
            exe grepcmd . grepadd . a:bang . ' ' . grepargs . pspec.files

            let sp_idx_prev = pspec.idx
        endfor
    catch /Vim(echoerr)/
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call sf.destroy()
    endtry
endfu
fu! s:ack(cmd, bang, ...)
    let sf = s:sf_create()
    try
        " Break raw command line into parsed prack options, and string
        " containing everything else (essentially, the ack options).
        " TODO: UNDER CONSTRUCTION!!!!!!!!!!!!!!!
        let [pspecs, args] = s:parse_grep_cmdline(a:000)
        echo pspecs
        echo args
        return
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
                let cfg = s:sp_cfg[sel.idx]
                let rootdir = call('finddir', cfg.root)
                if rootdir == ''
                    throw "Couldn't locate project base. Make sure your cwd is within the project."
                endif
                "exe 'lcd ' . rootdir
                call sf.pushd(rootdir)
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
        call sf.destroy()
    endtry
endfu
" <<<
" >>> Functions for general utility
" Calculate and return the bracket (defined as [start_index, end_index]) of
" the region containing only those values in the input list, which are within
" the region delineated by the input constraint. The input comparison function
" may be used to test a value like so:
"   fn(value, fixed) =>
"       -1  value before region 
"       0   value in region
"       1   value after region 
" Note: Performance is important, as values is expected to contain thousands,
" if not tens of thousands, of files: hence, the binary search algorithm.
fu! s:bracket(values, constraint, fn)
    let [sgn, off] = [1, 0]
    let ln = len(a:values)
    let is = [0, ln - 1]
    let eis = [ln - 1, 0]
    let cmps = ['', '']
    let ecmps = ['', '']
    let i = ln / 2
    let ret = [-1, -1]
    if ln == 0
        " Empty range: no bracket.
        return ret
    endif
    " If here, there's at least 1 point to try.
    while i >= 0
        " Compare candidate bracket edge value to constraint value using
        " provided comparison function, reversing sense of comparison for
        " offset == 1
        let cmp = sgn * a:fn(a:values[i], a:constraint)
        let imove = -1
        if cmp < 0
            " Out of range on near side.
            let dist = sgn * (is[1] - i)
            if dist < 4
                if dist == 0
                    " Can't move any further: no bracket.
                    let inext = -1
                elseif dist == 1
                    if cmps[1] isnot ''
                        " Adjacent point already tried.
                        if cmps[1] == 0
                            let ret[off] = is[1]
                        endif
                        let inext = -1
                    else
                        " Adjacent point is our last hope.
                        let imove = 0
                        let inext = is[1]
                    endif
                else
                    " Single-step toward region.
                    let imove = 0
                    let inext = i + sgn
                endif
            else
                " Still in jumping mode.
                let imove = 0
                let inext = i + sgn * dist / 2
            endif
        elseif cmp == 0
            " In range
            if off == 0 && i > eis[1]
                " Set left edge of region to be searched for bracket right
                " edge.
                " Note: This will always be set before bracket left edge is
                " found, and it will never move once set.
                let eis[1] = i
                let ecmps[1] = cmp
            endif
            let dist = sgn * (i - is[0])
            if dist < 4
                if dist == 0
                    " This point matches and there are no more to try: found
                    " bracket edge.
                    let ret[off] = i
                    let inext = -1
                elseif dist == 1
                    if cmps[0] isnot ''
                        " Adjacent point already tried.
                        let inext = -1
                        let ret[off] = i
                    else
                        " Potential match, but need to try adjacent point.
                        " TODO: Refactor to combine this case with one below
                        " (by combining the dist == 1 and cmps[0] isnot ''
                        " cases).
                        let imove = 1
                        let inext = i - sgn
                    endif
                else
                    " Single-step toward sought edge.
                    let imove = 1
                    let inext = i - sgn
                endif
            else
                " Still in jumping mode.
                let imove = 1
                let inext = i - sgn * dist / 2
            endif
        else
            " Out of range on far side
            if off == 0
                " Set right edge of region to be searched for bracket right
                " edge.
                " Note: This may move leftward on subsequent iterations.
                let eis[0] = i
                let ecmps[0] = cmp
            endif
            let dist = sgn * (i - is[0])
            if dist < 4
                if dist == 0
                    " List endpoint is too large: no bracket.
                    let inext = -1
                elseif dist == 1
                    if cmps[0] isnot ''
                        " Adjacent point already tried: no bracket.
                        let inext = -1
                    else
                        " Adjacent point is our last hope.
                        " TODO: This else could be refactored with subsequent,
                        " outer one. See earlier note.
                        let imove = 1
                        let inext = i - sgn
                    endif
                else
                    " Single-step toward region.
                    let imove = 1
                    let inext = i - sgn
                endif
            else
                " Still in jumping mode.
                let inext = i - sgn * dist / 2
                let imove = 1
            endif
        endif
        " Update for next iteration (or return).
        if inext == -1
            " Either we're done, or we need to transition to looking for end.
            if is[off] == -1
                " Failed to find sought endpoint.
                return ret
            elseif off == 0
                " Found left edge of bracket; transition to looking for right
                " edge.
                " Question: Should I copy or just assign reference?
                let is = eis
                let cmps = ecmps
                " Note: The following accounts for the sign switch naturally. 
                let i = is[0] + (is[1] - is[0]) / 2
                let [sgn, off] = [-1, 1]
            else
                " Success!
                return ret
            endif
        else
            " Not done with current endpoint search.
            if imove != -1
                " Move one of the search endpoints.
                let is[imove] = i
                let cmps[imove] = cmp
            endif
            let i = inext
        endif
    endwhile
    return ret
endfu
" TODO: Decide how to incorporate case-sensitivity - probably at higher level
" with appropriate setting of 'ignorecase' based fileignorecase or some such,
" as calls to this may be nested deeply.
fu! s:strcmp(a, b)
    return a:a < a:b ? -1 : a:a > a:b ? 1 : 0
endfu
fu! s:compare_file(file, fixed)
    " Does the constraint match at start?
    let filepart = strpart(a:file, 0, strlen(a:fixed))
    " TODO: Consider case-sensitivity...
    return s:strcmp(filepart, a:fixed)
endfu
let s:compare_file_fn = function('s:compare_file')
" Compares input str with input strs, starting at start_idx, and returns an
" array of the following form: [idx, dup]
" ...where dup is a flag that is set iff the input str is already in strs
" ...and idx is one of the following:
" insert-location
"     Index of element before which input element should be inserted
"     OR index of element with which it is duplicate (when dup == 1)
" len(strs)
"     Indicates input element (and all subsequent elements) can be appended.
" Note: matchcase operators used for comparison.
" Assumptions: Caller ensures that both input strs and list containing input
" str are sorted. 
fu! s:find_insert_idx(strs, start_idx, str)
    let idx = a:start_idx
    let len = len(a:strs)
    while idx < len
        if a:str <=# a:strs[idx]
            " Either input str goes before current, or it's a duplicate.
            " TODO: If individual subproject lists guaranteed uniqueness of
            " their elements, we could advance idx in case of duplicate;
            " currently, however, uniqueness is not explicitly enforced
            " (though find commands will naturally tend to produce unique
            " lists). Thus, safest course is to return idx without advancing,
            " thereby ensuring existing duplicate will be re-checked (probably
            " unnecessarily) on subsequent call.
            return [idx, a:str ==# a:strs[idx]]
        endif
        let idx += 1
    endwhile
    " Append.
    return [idx, 0]
endfu

fu! Test_bracket()
    let fixed = 'ad'
    let files = ['a', 'aa', 'abc', 'accd', 'bb', 'bda', 'ca', 'cb', 'cc', 'ccc', 'dab', 'dbbb', 'dcs', 'dcsa', 'dcsab/foo', 'efg', 'fad', 'foo', 'goob', 'hoo']
    echo s:bracket(files, fixed, s:compare_file_fn)
endfu

" Convert input list of command line args to a space-separated string list, in
" which the args are escaped in manner suitable for use in either internal
" (vim) or external (shell) command line.
fu! s:convert_arg_list_to_string(args, external)
    let mapexpr = a:external ? 'shellescape(v:val)' : 'escape(v:val, '' \'')'
    let args = map(copy(a:args), mapexpr)
    return join(args, " ")
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

com! -bang -nargs=* Grep  call s:grep('Grep', <q-bang>, <f-args>)
com! -bang -nargs=* Gr  call <SID>ack('Gr', <q-bang>, <f-args>)
com! -bang -nargs=* LGr  call <SID>ack('LGr', <q-bang>, <f-args>)

com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Split call s:parse_edit_cmdline('Split', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Edit call s:parse_edit_cmdline('Edit', <q-bang>, <f-args>)

com! -nargs=* -complete=customlist,<SID>complete_filenames Spq call FA(<q-args>)
" Quoted args play...
com! -nargs=* QA FA <q-args>
com! -nargs=* FA call FA(<f-args>)
fu! FA(...)
    for s in a:000
        echon "|" . s . "|"
    endfor
endfu
" Completion play...
let ls = ["/usr/var/somefile.txt", "/usr/src/chicken-scheme/foo.c", "/usr/bin/someutil", "somedir/someotherdir/somefile.scm"]
fu! Complete_customlist(A, L, P)
    echomsg "Completing customlist`" . a:A . "', Leading part is `" . a:L . "'"
    return g:ls
endfu
fu! Complete_custom(A, L, P)
    echomsg "Completing custom `" . a:A . "', Leading part is `" . a:L . "'"
    return join(g:ls, "\n")
endfu
com! -nargs=1 -complete=customlist,Complete_customlist CL echo "foo"
com! -nargs=1 -complete=custom,Complete_custom C echo "foo"
" <<<
" >>> Works in progress...

" <<<
" vim:ts=4:sw=4:et:fdm=marker:fmr=>>>,<<<
