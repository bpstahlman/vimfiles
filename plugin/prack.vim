" TODO: This is one of the things that would be in the user-provided config.
" TODO: Eventually, may provide a different way to set all this: e.g., function or
" global-vars.
let g:prack_listfile = 'files.list'

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

" The structure above will be mirrored by a list which contains only valid
" (though possibly disabled) entries, and whose values are dictionaries
" containing all of the original keys, plus the following:
" TODO: Consider making it mirror the above exactly: complexity engendered by
" culling invalid entries isn't worth much, especially now that we have
" concept of "disabled"...
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

    if !len(g:prack_module_cfg)
        " TODO: Consider throwing exception here.
        echoerr "Must define at least one project module within g:prack_module_cfg"
        return
    endif
    " Build snapshot of global config until re-initialization.
    let s:cache_cfg = []
    " Short (single-char) names stored in a hash
    " Long names stored in sorted array of patterns employing \%[...]
    let s:shortnames = {}
    let longnames = []
    let lname_to_index = {}
    let ln = len(g:prack_module_cfg)
    let i = 0
    " Note: Index into s:cache_cfg (i.e., valid configs only)
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
        call add(s:cache_cfg, deepcopy(cfg, 1))
        let cache_cfg = s:cache_cfg[-1]
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
        " TODO: Need to ensure cwd saved/restore properly...
        let cache_cfg.rootdir = s:canonicalize_path(rootdir, '')
        " Build cache, but don't force refresh.
        call s:cache_listfile(cache_cfg, 0)
        let cfg_idx = cfg_idx + 1
        let i = i + 1
    endwhile
    " If no valid subprojects, no point in continuing...
    if !len(s:cache_cfg)
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
" Return s:cache_cfg index corresponding to input short/long option name (or
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
    "call s:save_state()
    let sf = s:sf_create()
    try
        " Do we need to create/update the listfile?
        if listfile_path_found == '' || a:force_refresh
            let v:errmsg = ''
            "exe 'lcd ' . a:cache_cfg.rootdir
            call sf.pushd(a:cache_cfg.rootdir)
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
        let a:cache_cfg.files = files
        echo s:cache_cfg
        "echo a:cache_cfg
        let success = 1
    catch
        call s:warn(v:exception)
        let a:cache_cfg.disabled = 1
        let a:cache_cfg.files = []
    finally
        "call s:restore_state()
        call sf.destroy()
    endtry
    return success
endfu
" <<<
" >>> Functions pertaining to file processing
" Convert occurrences of * and **[[SIGN]NUMBER] in input glob to corresponding
" pattern; also, build array of constraints representing the content and
" location of fixed strings within the glob (which can be used to prevent
" costly regex matches when a simple string match is sufficient to disqualify
" the candidate match target).
" Note: 'partial' argument determines whether the generated pattern will be
" anchored at end.
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
" if I were to go to great lengths here to handle it.
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
"     the regex pattern
"   'anchored_at_start'
"     1 iff the first constraint is anchored at start.
"   'anchored_at_end'
"     1 iff the last constraint is anchored at end.
"   'constraints'
"     List of constraint strings, which must match exactly at locations
"     determined by anchored_at_start/anchored_at_end.
fu! s:glob_to_patt(glob, partial)
    let re_star = '\%(^\|[^*]\)\@<=\*\%([^*]\|$\)\@='
    let star = '[^/]*'
    let re_starstar = '\%(^\|/\)\@<=\%(\*\*\)\%(\([-+]\)\?\(0\|[1-9][0-9]*\)\)\?\(/\|$\)'
    " Extract the fixed string constraints.
    " TODO: Consider integrating this into the * and ** processing (as part of
    " refactoring that combines their processing).
    " Note: Deferring putting the constraint generation into separate function
    " because of refactoring possibility.
    " Note: Positive lookahead assertion used to ensure that a trailing slash
    " on ** is treated as part of the fixed string.
    let fixeds = split(a:glob, re_star . '\|' . re_starstar . '\@=', 1)
    let constraints = []
    let [anchored_at_start, anchored_at_end] = [0, 0]
    let [i, ln] = [0, len(fixeds)]
    " Assumption: Because keepempty was set in call to split, non-empty
    " leading/trailing strings imply start/end anchoring.
    while i < ln
        if fixeds[i] != ''
            call add(constraints, fixeds[i])
            " Note: Single string could be both start and end.
            if i == 0 | let anchored_at_start = 1 | endif
            if i == (ln - 1) | let anchored_at_end = 1 | endif
        endif
        let i = i + 1
    endwhile
    " Process *
    let glob = substitute(a:glob, re_star, star, 'g')
    " Process **
    " Vim_Bug: Both \f and [^\f] match `:'
    " Design Decision: Defer grouping of dir_seg to avoid redundant grouping.
    let dir_seg = '\%(\\[^\f]\|[^/]\)\+'
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
                let patt .= '\%(' . dir_seg . '\%(/' . dir_seg . '\)\{,' . (number - 1) . '}' . slash . '\)\?'
            endif
            " Advance past match
            let sio = si + strlen(match)
        endif
    endwhile
    if !a:partial
        let patt += '$'
    endif
    return {'patt': patt,
        \'anchored_at_start': anchored_at_start,
        \'anchored_at_end': anchored_at_end,
        \'constraints': constraints}
endfu
fu! Test_Convert_stars(glob)
    let files = readfile("C:/Users/stahlmanb/tmp/files.lst")
    let patt = s:glob_to_patt(a:glob)
    for f in files
        echo "Path:  " . f
        echo "Patt:  " . patt
        let m = matchstr(f, '^' . patt)
        echo "Match: " . m
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
fu! s:create_bracketer(min, max)
    let state = {
        \'min': -1, 'max': -1,
        \'si1': a:min, 'si2': a:max,
        \'ei1': -1, 'ei2': -1}
    fu! state.step(cmp) dict
    endfu
    return state
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
fu! s:get_matching_files_match(patt_info, anchor_dir, files)
    " First, bracket a region to search.
    if a:patt_info.anchored_at_start
        let [si, ei] = s:bracket(a:files, a:patt_info.constraints[0], s:compare_file_fn)
    else
        let [si, ei] = [0, len(a:files)]
    endif
    " TODO: Consider returning List of indices instead of actual files.
    " !!!!!! UNDER CONSTRUCTION !!!!!!!! (27Feb2014)
    let matches = []
    let i = si
    while i <= ei
        let file = ''
        if a:anchor_dir != ''
            " Anchor exists; skip a file that doesn't match.
            let idx = stridx(file_raw, a:anchor_dir)
            if idx == 0
                let idx = strlen(a:anchor_dir)
                let file = file_raw[idx :]
            endif
        else
            let file = file_raw
        endif
        " Do we have something to match against?
        " TODO: Sort files and short-circuit when possible.
        if file == ''
            continue
        endif
        " TODO: Should let user's fileignorecase or wildignorecase setting
        " determine =~ or =~?.
        "echo "Matching " . file . " against " . patt
        " TODO: Perhaps move the anchor into pattern generation.
        if file =~ '^' . patt
            call add(matches, file_raw)
        endif
        let i += 1
    endwhile
endfu
" TODO: Figure out how to get cfg here...
fu! s:get_matching_files(cfg, glob, partial)
    let matches = []
    "call s:save_state()
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
        " Note: Treat fixed strings at the head specially: i.e., can be used
        " to find starting point for search more quickly (eg, in bracketed
        " search) and to short-circuit when we've passed last possible match
        " in sorted list.
        let glob = a:glob
        if glob =~ '^\.//'
            " Use fnamemodify to ensure trailing slash.
            let anchor_dir = s:canonicalize_path(getcwd(), a:cfg.rootdir)
            let glob = glob[3:] " strip .//
        elseif a:glob =~ '^\./'
            " Note: This will default to same as .// if no current file.
            let anchor_dir = s:canonicalize_path(expand('%:h'), a:cfg.rootdir)
            let glob = glob[2:] " strip ./
        endif
        " Convert **N everywhere in glob
        let patt_info = s:glob_to_patt(glob, a:partial)
        let matches = s:get_matching_files_match(patt_info, anchor_dir, files)
    catch
        " TODO: What error?
        echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        "call s:restore_state()
        call sf.destroy()
    endtry
    return matches
endfu
fu! Test_get_matching_files(cfg_idx, glob, partial)
    let matches = s:get_matching_files(s:cache_cfg[a:cfg_idx], a:glob, a:partial)
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
    let path = expand(fnamemodify(a:path, ':p'))
    if (a:rootdir != '')
        let rootdir = expand(fnamemodify(a:rootdir, ':p'))
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
    "call s:save_state()
    let sf = s:sf_create()
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
            let cache_cfg = s:cache_cfg[cfg_idx]
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
        "call s:restore_state()
        call sf.destroy()
    endtry
endfu
fu! s:ack(bang, use_ll, mode, args)
    "call s:save_state()
    let sf = s:sf_create()
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
                let cfg = s:cache_cfg[sel.idx]
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
        "call s:restore_state()
        call sf.destroy()
    endtry
endfu
" <<<
" >>> Functions for general utility
" TODO: Decide how to incorporate case-sensitivity - probably at higher level
" with appropriate setting of 'ignorecase' based fileignorecase or some such,
" as calls to this may be nested deeply.
fu! s:strcmp(a, b)
    return a < b ? -1 : a > b ? 1 : 0
endfu

" Calculate and return the bracket (defined as [start_index, end_index]) of
" the region containing only those values in the input list, which are within
" the region delineated by the input constraint. The input comparison may be
" used to test a value like so:
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
        echo "i=" . i
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
                echo "Switching..."
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

fu! s:strcmp(a, b)
    return a:a < a:b ? -1 : a:a > a:b ? 1 : 0
endfu
fu! s:compare_file(file, fixed)
    " Does the constraint match at start?
    let filepart = strpart(a:file, 0, strlen(a:fixed))
    return s:strcmp(filepart, a:fixed)
endfu
let s:compare_file_fn = function('s:compare_file')

let fixed = 'd'
let files = ['a', 'aa', 'abc', 'accd', 'bb', 'bda', 'ca', 'cb', 'cc', 'ccc', 'dab', 'dbbb', 'dcs', 'dcsa', 'dcsab/foo', 'efg', 'fad', 'foo', 'goob', 'hoo']
"echo S_bracket(files, fixed, s:compare)
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
" >>> Works in progress...

" <<<
" vim:ts=4:sw=4:et:fdm=marker:fmr=>>>,<<<
