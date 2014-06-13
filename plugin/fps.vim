
" TODO: Move this elsewhere...
let g:fps_config = {
    \'listfile': 'files.list',
    \'maxgrepsize': 4095,
    \'grepprg': 'grep -n $* /dev/null',
    \'grepformat': '%f:%l:%m,%f:%l%m,%f  %l%m',
    \'projects': {
        \'asec': {
            \'subprojects': [
                \{
                    \'name': 'php',
                    \'shortname': 'p',
                    \'root': ['src/private', ';asec'],
                    \'find': 'find . \( \( '
                        \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
                        \.' -prune -false \) -o -iname ''*.php'''
                \},{
					\'name': 'js',
                    \'shortname': 'j',
                    \'root': ['src/public', ';asec'],
                    \'find': 'find . \( \( '
                        \.' -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
                        \.' -prune -false \) -o -iname ''*.js'''
                \},{
					\'name': 'foo',
                    \'shortname': 'f',
                    \'root': ['src/public', ';asec'],
                    \'find': 'find . \( \( '
                        \.' -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
                        \.' -prune -false \) -o -iname ''*.js'''
                \},{
					\'name': 'boo',
                    \'shortname': 'b',
                    \'root': ['src/public', ';asec'],
                    \'find': 'find . \( \( '
                        \.' -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
                        \.' -prune -false \) -o -iname ''*.js'''
                \},{
					\'name': 'baz',
                    \'shortname': 'z',
                    \'root': ['src/public', ';asec'],
                    \'find': 'find . \( \( '
                        \.' -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
                        \.' -prune -false \) -o -iname ''*.js'''
                \}
            \]
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

" >>> Functions related to project/subproject object representations
fu! s:prj_create(force_refresh, p_name, ...)
    let sp_filts = a:0 > 0 ? a:1 : []
    " Create dictionary to encapsulate the project config
    " spsel - Object facilitating access to individual sp's.
    " sprjs - Dict containing the following
    "   rootdir
    "   files
    "   opt
    let prj = {'name': a:p_name, 'spsel': {}, 'sprjs': {}}
    " Convert a short or long partial name to corresponding subproject
    " TODO: This should return an element of sprjs.
    fu! prj.get_sp(name) dict
        let sp_name = self.spsel.get_name(a:name)
        if sp_name != ''
            return self.sprjs[sp_name]
        else
            " TODO: Throw or return something empty?
            throw "Invalid subproject specification: `" . a:name . "'"
        endif
    endfu
    fu! prj.get_all_sps(...) dict
        let inc_dis = a:0 > 0 ? a:1 : 0
        let sprjs = []
        call self.spsel.iter_init(inc_dis)
        while self.spsel.iter_valid()
            call add(sprjs, self.sprjs[self.spsel.iter_current()])
            call self.spsel.iter_next()
        endwhile
        return sprjs
    endfu
    fu! prj.close() dict
        " TODO
    endfu
    " Builds sp map.
    fu! prj.open(force_refresh) dict
        " Assumption: spsel member has already been created and initialized;
        " thus, we've validated basic structure of g:fps_config.
        let p_raw = g:fps_config.projects[self.name]
        " TODO: UNDER CONSTRUCTION - rework all this... (This is old
        " process_cfg.)
        " Note: We don't need to preserve g_opt with s:var, as it will be
        " accessible via prototype of project-level opt.
        let g_opt = s:build_opts(g:fps_config, 0, {}) " TODO: No need to pass global...
        let p_opt = s:build_opts(p_raw, 1, g_opt)
        " Note: Iterate spsel directly, as sprjs hasn't been built yet.
        " While iterating, keep up with index into global subprojects list, of
        " which the list processed by the iterator may be only a subset.
        " Rationale: Making subprojects a Dictionary in global config would
        " simplify implementation, but I want user to be able to order the
        " subprojects, and specifying a List is the easiest way for him to do
        " so.
        call self.spsel.iter_init()
        let sp_idx = 0
        while self.spsel.iter_valid()
            try
                let sp_name = self.spsel.iter_current()
                " Determine index into global subprojects list, of which the
                " list processed by the iterator may be only a subset.
                while p_raw.subprojects[sp_idx].name != sp_name
                    let sp_idx += 1
                endwhile
                let sp_raw = p_raw.subprojects[sp_idx]
                let sp_opt = s:build_opts(sp_raw, 2, p_opt)
                " Don't commit to adding to s:sp_cfg[] yet...
                let sprj = {}
                let sprj.name = sp_name
                let sprj.rootdir = call('finddir', sp_opt.get('root'))
                if sprj.rootdir == ''
                    throw "Warning: Skipping subproject `" . sp_name . " due to invalid config: Couldn't locate project base. Make sure your cwd is within the project."
                endif
                " Convert rootdir to full path.
                let sprj.rootdir = s:canonicalize_path(sprj.rootdir, '')
                " Build cache, but don't force refresh. (TODO - Unless bang?)
                call s:cache_listfile(sprj, sp_opt, a:force_refresh)
                " Associate the options object.
                let sprj.opt = sp_opt
                " Accumulate the valid subproject.
                let self.sprjs[sp_name] = sprj
            catch
                " Invalid subproject
                call s:warn("Warning: Skipping subproject `" . sp_name . " due to error: " . v:exception)
                call self.spsel.disable(sp_name)
            finally
                call self.spsel.iter_next()
                let sp_idx += 1
            endtry
        endwhile
        " If no valid subprojects, no point in continuing...
        if empty(self.sprjs)
            throw "No valid subprojects. :help fps-config"
        endif
    endfu
    " Iterator methods are mostly pass-throughs.
    fu! prj.iter_init(...) dict
        call call(self.spsel.iter_init, a:000, self.spsel)
    endfu
    fu! prj.iter_current() dict
        return self.sprjs[self.spsel.iter_current()]
    endfu
    fu! prj.iter_valid() dict
        return self.spsel.iter_valid()
    endfu
    fu! prj.iter_next() dict
        call self.spsel.iter_next()
    endfu
    " Create the object that facilitates access to subprojects by name and via
    " iterator.
    let prj.spsel = s:spsel_create(a:p_name, sp_filts)
    " Initialize the project (or subset thereof).
    call prj.open(a:force_refresh)

    return prj
endfu
fu! s:spsel_create(p_name, ...)
    let sp_filts = a:0 > 0 ? a:1 : []
    let spsel = {'longnames': [], 'longpats': [], 'shortnames': {}, 'disabled': {}, 'iter_idx': 0}
    fu! spsel.disable(name)
        let self.disabled[a:name] = 1
    endfu
    fu! spsel.enable(name) dict
        " TODO: Perhaps delete...
        let self.disabled[a:name] = 0
    endfu
    fu! spsel.is_disabled(name) dict
        return has_key(self.disabled, a:name) && self.disabled[a:name]
    endfu
    fu! spsel.get_name(spec) dict
        let [is_short, name] = [a:spec[0] == 's', a:spec[2:]]
        if is_short
            if has_key(self.shortnames, name)
                return self.shortnames[name]
            endif
        else
            " Look for name in self.longpats
            for lpat in self.longpats
                if name ==# lpat.name || name =~# lpat.re
                    return lpat.name
                elseif name <# lpat.name
                    " We've passed the last possible match.
                    break
                endif
            endfor
        endif
        " Not found!
        " TODO: Throw exception?
        return ''
    endfu
    " Iterator methods used to iterate the longnames in order.
    fu! spsel.iter_init(...) dict
        let self.iter = {'inc_dis': a:0 > 0 ? a:1 : 0, 'idx': -1}
        " Use iter_next() to advance, throwing away return value.
        call self.iter_next()
    endfu
    fu! spsel.iter_current() dict
        return self.iter.idx == -2 ? '' : self.longnames[self.iter.idx]
    endfu
    fu! spsel.iter_next() dict
        if self.iter.idx == -2
            " Already passed end.
            return 0
        endif
        " Is there a next element?
        let [inc_dis, i, next, len] = [self.iter.inc_dis, self.iter.idx, -2, len(self.longnames)]
        while i < len - 1
            let i += 1 " pre-increment
            if inc_dis || !self.is_disabled(self.longnames[i])
                let next = i
                break
            endif
        endwhile
        " Save next, which may be -2, signifying iteration complete.
        let self.iter.idx = next
        return next != -2
    endfu
    fu! spsel.iter_valid() dict
        return self.iter.idx != -2
    endfu
    " Extract information from global plugin config.
    fu! spsel.init(p_name, ...) dict
        let sp_filts = a:0 > 0 ? a:1 : []
        " Make sure there's at least 1 project
        if !has_key(g:fps_config, 'projects')
            throw "No projects defined."
        endif
        if type(g:fps_config.projects) != 4
            throw "Invalid project definition. Must be Dictionary."
        endif
        " Does the named project exist?
        if !has_key(g:fps_config.projects, a:p_name)
            throw "No configuration found. :help TODO fps_config???"
        endif
        " TODO: build_opts will throw on (eg) missing required args - handle
        " somehow...
        let p_raw = g:fps_config.projects[a:p_name]
        " Make sure there's at least 1 subproject
        if !has_key(p_raw, 'subprojects')
            throw "No subprojects defined for project " . a:p_name
        endif
        if type(p_raw.subprojects) != 3
            throw "Invalid subprojects definition for project " . a:p_name . ". Must be List."
        endif

        " Short (single-char) names stored in a hash
        " Long names stored in sorted array of patterns employing \%[...]
        let shortnames = {}
        let longnames = []
        let lname_to_index = {}
        " TODO: I'm thinking perhaps I want subprojects to be ordered: i.e.,
        " in List of dicts with mandatory name property.
        " Rationale: As it is now, user can't control order in which sp's are
        " searched.
        for sp_raw in p_raw.subprojects
            try
                if !has_key(sp_raw, 'name')
                    throw "Subproject missing name property."
                endif
                let sp_name = sp_raw.name
                " TODO: Don't hardcode these patterns...
                if (sp_name !~ '^[a-zA-Z0-9_]\+$')
                    throw "Invalid subproject name `" . sp_name
                        \. "': must be sequence of characters matching [a-zA-Z0-9_]."
                else
                    if !has_key(lname_to_index, sp_name)
                        let lname_to_index[sp_name] = sp_name
                        call add(longnames, sp_name)
                    else
                        call s:warn("Warning: Name " . sp_name . " used multiple times. All but first usage ignored.")
                    endif
                endif
                if has_key(sp_raw, 'shortname')
                    if (sp_raw.shortname !~ '^[a-zA-Z0-9_]$')
                        call s:warn("Ignoring invalid shortname `" . sp_raw.shortname
                            \. "': must be single character matching [a-zA-Z0-9_].")
                    else
                        if !has_key(shortnames, sp_raw.shortname)
                            let shortnames[sp_raw.shortname] = sp_name
                        else
                            call s:warn("Warning: Name " . sp_raw.shortname . " used multiple times. All but first usage ignored.")
                        endif
                    endif
                endif
            catch
                " Invalid subproject
                call s:warn("Warning: Skipping invalid subproject config `" . sp_name . "': " . v:exception)
            endtry
        endfor
        " If no valid subprojects, no point in continuing...
        if empty(longnames)
            throw "No valid subprojects. :help fps-config"
        endif
        " Mapping of short names to long.
        let self.shortnames = shortnames
        " Array defining enumeration order.
        let self.longnames = longnames
        " Array of dicts used to perform longname matching.
        let self.longpats = self.process_longnames(longnames)
        if !empty(sp_filts)
            " Constrain project to subset of subprojects.
            call self.apply_sp_filter(sp_filts)
        endif
    endfu
    " Input: List of strings of following form:
    " ['s:shortopt1', 'l:longopt1', 'l:longopt2', 's:shortopt2']
    fu! spsel.apply_sp_filter(sp_filts) dict
        if empty(a:sp_filts)
            return
        endif
        " Build existence hash from input opts.
        let sel_names = {}
        for sp_filt in a:sp_filts
            let [is_short, name] = [sp_filt[0] == 's', sp_filt[2:]]
            if is_short && has_key(self.shortnames, name)
                " Set key corresponding to longname.
                let sel_names[self.shortnames[name]] = 1
            else
                " Get full long name.
                let longname = self.get_name(sp_filt)
                if longname == ''
                    " TODO: Right way to handle?
                    throw "Invalid subproject specification: " . name
                else
                    " Set key corresponding to *full* longname.
                    let sel_names[name] = 1
                endif
            endif
        endfor
        " Use sel_names existence hash to cull entries from
        " shortnames Dict and longnames List.
        for [shortname, longname] in items(self.shortnames)
            if !has_key(sel_names, longname)
                call remove(self.shortnames, shortname)
            endif
        endfor
        " Loop over longpats from end to simplify removal.
        let i = len(self.longpats) - 1
        while i >= 0
            if !has_key(sel_names, self.longpats[i].name)
                call remove(self.longpats, i)
            endif
            let i -= 1
        endwhile
        " Loop over longnames from end to simplify removal.
        let i = len(self.longnames) - 1
        while i >= 0
            if !has_key(sel_names, self.longnames[i])
                call remove(self.longnames, i)
            endif
            let i -= 1
        endwhile
    endfu
    " Private utility function
    " Process the long options to determine the portions required for
    " uniqueness.
    " Take raw input array of longnames and return array of the following:
    " {'name': <longname>, 're': <regex>}
    fu! spsel.process_longnames(longnames) dict
        " Caveat: Copy the input array before destructive sort.
        let longnames = a:longnames[:]
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
        " Note: Arrays reqs and longnames are parallel.
        let longpats = []
        let i = 0
        while i < ln
            let re = reqs[i]
            " Is the long name longer than the required portion?
            if len(longnames[i]) > len(reqs[i])
                " Append the optional part within \%[...]
                let re .= '\%[' . longnames[i][len(reqs[i]):] . ']'
            endif
            call add(longpats, {'name': longnames[i], 're': re})
            let i = i + 1
        endwhile
        return longpats
    endfu
    " Initialize.
    call spsel.init(a:p_name, sp_filts)
    return spsel
endfu

fu! Test_p_obj()
    let prj = s:prj_create('asec', ['l:php', 's:f', 'l:boo', 's:z'])
    let spsel = prj.spsel
    " Dynamically disable one.
    call spsel.disable('php')
    echomsg "Iterating normally..."
    call spsel.iter_init()
    while spsel.iter_valid()
        echo spsel.iter_current()
        call spsel.iter_next()
    endwhile
    echomsg "Iterating all..."
    call spsel.iter_init(1)
    while spsel.iter_valid()
        echo spsel.iter_current()
        call spsel.iter_next()
    endwhile
endfu

"<<<

" >>> Functions used to start/stop/refresh the plugin
" Inputs:
"   [<sp_spec>...] <p_name>
fu! s:open(bang, ...)
    " Close any currently open project.
    if exists('s:prj') && !empty(s:prj)
        call s:prj.close()
    endif
    "try
        let s:prj = s:prj_create(a:bang == '!', a:000[-1], a:000[0:-2])
        "call s:process_cfg(a:p_name, sp_names)
    "catch
        "throw "Cannot open project `" . a:000[-1] . "': " . v:exception
        " TODO: Is this necessary, given that s:prj hasn't been set? We may
        " have set some static vars (e.g., s:longnames/s:shortnames), but
        " they'd be set next time...
        if exists('s:prj') && !empty(s:prj)
            call s:prj.close()
        endif
    "endtry
endfu

fu! s:reopen(bang)
    " Close any currently open project.
    if !exists('s:prj') || empty(s:prj)
        throw "No open project. :help FPSOpen"
    endif
    let args = [a:bang]
    " Note: Pass true for 'include disabled' flag to ensure that an sp
    " disabled due to error will be enabled if error has been fixed.
    call extend(args, map(s:prj.get_all_sps(1), '"--" . v:val.name'))
    call add(args, s:prj.name)
    " Invoke open.
    " TODO: Could have prj keep up with whether it was invoked with a filter,
    " and if not, avoid passing a filter consisting of all sp's. Would be
    " difference without distinction unless user had added sp's to global
    " config since prior open...
    call call('s:open', args)
endfu

fu! s:close()
    if exists('s:prj') && !empty(s:prj)
        call s:prj.close()
        unlet! s:prj
        " TODO: General cleanup. E.g., delete commands (though this might be
        " handled by prj.close()).
    endif
endfu
" <<<
" >>> Static data used to process options
" Notes:
" -'type' is return value of type()
" -No need to specify 'type' if there's a 'default'.
" -'required' means user *must* provide a value.
"  TODO: It's actually unnecessary, as it can be deduced from presence/absence
"  of others: e.g., 'default', 'vim'
" TODO: I believe required and default are redundant: i.e., all options are
" required unless they have a default...
" TODO: Consider removing 'vim' and adding ability to specify conversion
" function (for converting raw value to opt value).
let s:opt_cfg = {
    \'listfile': {
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
    \'more': {
        \'#TODO:': 'Decide whether to support more than true/false.',
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 0,
        \'default': 0,
        \'vim': {'name': 'grepprg', 'boolean': 0}
    \},
    \'grepprg': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1,
        \'default': 'grep -n $* /dev/null',
        \'vim': {'name': 'grepprg', 'boolean': 0}
    \},
    \'grepformat': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1,
        \'default': '%f:%l:%m,%f:%l%m,%f  %l%m',
        \'vim': {'name': 'grepformat', 'boolean': 0}
    \},
    \'find': {
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 1
    \},
    \'root': {
        \'#comment': "Would be highly unusual to define root globally, but it could be made to work with appropriate subproject-specific finds...",
        \'minlvl': 0,
        \'maxlvl': 2,
        \'type': 3
    \}
\}
" <<<
" >>> Functions used to process config
" Note: ... is base, defaults to {}
" TODO: Consider whether all these return flags are needed, given that prior
" option processing should preclude possibility (eg) of invalid/missing opt.
fu! s:opts_create(opts, lvl, ...)
    let base = a:0 > 0 ? a:1 : {}
    let opts = {'opts': a:opts, 'lvl': a:lvl, 'base': base}
    " Optional input object filled with the following flags:
    " def, set, vim, lvl
    " Assumption: Prior validation is such that failure to find a value for
    " the option (possibly default) implies internal error.
    fu! opts.get(name, ...) dict
        if a:0 > 0
            let flags = a:1
            " Empty input object.
            call filter(flags, 0)
        else
            let flags = {}
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
        endif
        return exists('l:value') ? value : ''
    endfu
    fu! opts.set(name, value) dict
        let self.opts[a:name] = a:value
    endfu
    return opts
endfu
fu! Test_opts_create()
    let g_opts = s:opts_create({'boo': 1, 'hoo': 2, 'yong': 3}, 0, {})
    let p_opts = s:opts_create({'hoo': 21, 'lala': 49}, 1, g_opts)
    let sp_opts = s:opts_create({'egg': 234, 'foo': 523, 'yong': 299, 'grepper': 923}, 2, p_opts)

    call g_opts.set('fooper', 'boop')

    let objs = [p_opts, sp_opts]
    let keys = ['boo', 'hoo', 'yong', 'lala', 'egg', 'foo', 'grep', 'foop']
    for obj in objs
        let lvl = obj.lvl
        for key in keys
            let flags = {}
            let value = obj.get(key, flags)
            echo key . " \t(L" . lvl . ") \t|" . value . "| \t@" . flags.lvl . " \tFlags: " . string(flags)
        endfor
    endfor
endfu
" TODO: A name that differentiates this from runtime (cmdline) option
" processing. Is this even needed between caller and s:opts_create?
" TODO: No need for this. Put it in s:opts_create. This makes sense, as it
" would consolidate all the s:opt_cfg[] access.
fu! s:build_opts(raw, lvl, base)
    let opts = s:opts_create({}, a:lvl, a:base)
    for [opt_name, opt_val] in items(a:raw)
        " TODO: As long as opts are not segregated in a subkey like 'opts', if
        " we want to warn about non-existent opts, we need to maintain a set
        " of names that are not opts, but are expected: e.g., 'shortname'.
        " Decision: Don't complicate things unnecessarily by treating things
        " like 'shortname' as options.
        " TODO: Skip keys that begin with '#' (possibly with leading
        " whitespace) to permit a form of 'commenting'.
        if !has_key(s:opt_cfg, opt_name)
            unlet opt_val " E706 workaround
            continue
        endif
        " Option exists.
        let opt_cfg = s:opt_cfg[opt_name]
        if a:lvl < opt_cfg.minlvl || a:lvl > opt_cfg.maxlvl
            " Option can't be set at this level; ignore.
            " TODO: Add warning!
            unlet opt_val " E706 workaround
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
        unlet opt_val " E706 workaround
    endfor
    " Make sure all options that are required by this level have been set.
    for [opt_name, opt_cfg] in items(s:opt_cfg)
        if !has_key(opt_cfg, 'default') && !has_key(opt_cfg, 'vim') && a:lvl > opt_cfg.maxlvl
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
    " Make sure there's at least 1 project
    if !has_key(g:fps_config, 'projects')
        throw "No projects defined."
    endif
    if type(g:fps_config.projects) != 4
        throw "Invalid project definition. Must be Dictionary."
    endif
    " Does the named project exist?
    if !has_key(g:fps_config.projects, a:p_name)
        throw "No configuration found. :help TODO fps_config???"
    endif
    " TODO: build_opts will throw on (eg) missing required args - handle
    " somehow...
    let p_raw = g:fps_config.projects[a:p_name]
    " Make sure there's at least 1 subproject
    if !has_key(p_raw, 'subprojects')
        throw "No subprojects defined for project " . a:p_name
    endif
    if type(p_raw.subprojects) != 4
        throw "Invalid subprojects definition for project " . a:p_name . ". Must be Dictionary."
    endif
    " Note: We don't need to preserve g_opt with s:var, as it will be
    " accessible via prototype of project-level opt.
    let g_opt = s:build_opts(g:fps_config, 0, {}) " TODO: No need to pass global...
    let s:p_opt = s:build_opts(p_raw, 1, g_opt)

    " Build snapshot of project opt/cfg until re-initialization.
    let s:sp_opt = []
    let s:sp_cfg = []
    " Short (single-char) names stored in a hash
    " Long names stored in sorted array of patterns employing \%[...]
    let s:shortnames = {}
    let longnames = []
    let lname_to_index = {}
    let i = 0
    let cfg_idx = 0 " index into s:sp_cfg (i.e., valid configs only)
    " TODO: This dictionary loop is no longer valid. This function is going
    " away... See changes in the new project and sp objects.
    for [sp_name, sp_raw] in items(p_raw.subprojects)
        try
            let sp_opt = s:build_opts(sp_raw, 2, s:p_opt)
            " TODO: Don't hardcode these patterns...
            if (sp_name !~ '^[a-zA-Z0-9_]\+$')
                throw "Ignoring invalid subproject name `" . sp_name
                    \. "': must be sequence of characters matching [a-zA-Z0-9_]."
            else
                if !has_key(lname_to_index, sp_name)
                    let lname_to_index[sp_name] = cfg_idx
                    call add(longnames, sp_name)
                else
                    call s:warn("Warning: Name " . sp_name . " used multiple times. All but first usage ignored.")
                endif
            endif
            if has_key(sp_raw, 'shortname')
                if (sp_raw.shortname !~ '^[a-zA-Z0-9_]$')
                    call s:warn("Ignoring invalid shortname `" . sp_raw.shortname
                        \. "': must be single character matching [a-zA-Z0-9_].")
                else
                    if !has_key(s:shortnames, sp_raw.shortname)
                        let s:shortnames[sp_raw.shortname] = cfg_idx
                    else
                        call s:warn("Warning: Name " . sp_raw.shortname . " used multiple times. All but first usage ignored.")
                    endif
                endif
            endif
            " Don't commit to adding to s:sp_cfg[] yet...
            let sp_cfg = {}
            " Add name, which was represented only as key of source object
            let sp_cfg.name = sp_name
            " Save original index, in case it's needed for reporting.
            " TODO: Perhaps save a title string (e.g., `--longname, -shortname')
            " for reporting purposes...
            let sp_cfg.orig_idx = i
            let sp_cfg.rootdir = call('finddir', sp_opt.get('root'))
            if sp_cfg.rootdir == ''
                throw "Warning: Skipping invalid subproject config at index " . i . ": Couldn't locate project base. Make sure your cwd is within the project."
            endif
            " Save full path of rootdir.
            let sp_cfg.rootdir = s:canonicalize_path(sp_cfg.rootdir, '')
            " Build cache, but don't force refresh.
            call s:cache_listfile(sp_cfg, sp_opt, 0)
            " Current config item not skipped: i.e., valid subproject.
            call add(s:sp_cfg, sp_cfg)
            call add(s:sp_opt, sp_opt)
            let cfg_idx = cfg_idx + 1 " Keep up with sp_idx
        catch
            " Invalid subproject
            call s:warn("Warning: Skipping invalid subproject config at index " . i . ": " . v:exception)
            " TODO: Decide on disabled. Perhaps just remove... But consider
            " fact that disabling might be required on a refresh...
            let sp_cfg.disabled = 1
        finally
            let i = i + 1 " Keep up with original idx
        endtry
    endfor
    " If no valid subprojects, no point in continuing...
    if empty(s:sp_cfg)
        throw "No valid subprojects. :help fps-config"
    endif
    " TODO: Perhaps refactor some of this into function.
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
            if name ==# lname.name || name =~# lname.re
                return lname.idx
            elseif name <# lname.name
                " We've passed the last possible match.
                break
            endif
        endfor
        " Not found!
        return -1
    endif
endfu

" Try various ways to get a subproject-specific value for specified option.
" TODO: Remove if this approach remains unused.
fu! s:get_opt(sp_idx, opt, default_to_vim)
    if has_key(s:sp_cfg[a:sp_idx], a:opt)
        return s:sp_cfg[a:sp_idx][a:opt]
    elseif exists('g:fps_' . a:opt)
        " TODO: Revisit this when plugin global options are reworked.
        return g:fps_{a:opt}
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
" Exceptions: Leave for caller.
" TODO: Perhaps replace sp_cfg/sp_opt with an sp_idx.
fu! s:cache_listfile(sp_cfg, sp_opt, force_refresh)
    let listfile = a:sp_opt.get('listfile')
    " See whether the listfile exists and is readable.
    let listfile_path = a:sp_cfg.rootdir . listfile
    let listfile_path_found = findfile(listfile, a:sp_cfg.rootdir)
    let sf = s:sf_create()
    try
        " Both listfile generation and file canonicalization require us to be
        " in root dir.
        call sf.pushd(a:sp_cfg.rootdir)
        " Do we need to create/update the listfile?
        if listfile_path_found == '' || a:force_refresh
            let v:errmsg = ''
            " Note: silent avoids the annoying 'Hit enter' prompt.
            exe 'silent !' . a:sp_opt.get('find') . ' >' . listfile
            if v:errmsg != ''
                throw "Encountered error attempting to build listfile `" . listfile_path . "' for subproject "
                    \. a:sp_cfg.name . ": " . v:errmsg . ". Disabling subproject."
            endif
        endif
        " If here, we expect to have a file to read.
        if !filereadable(listfile_path)
            throw "Couldn't open listfile `" . listfile_path . "' for subproject "
                \. a:sp_cfg.name . ". Disabling subproject."
        endif
        let files_raw = readfile(listfile_path)
        " Note: We must set 'shellslash' for canonicalization.
        call sf.setopt('shellslash', 1, {'boolean': 1})
        " Build canonicalized list within loop.
        let files = []
        for file_raw in files_raw
            "echo "Before: " file_raw
            let file_raw = s:canonicalize_path(file_raw, a:sp_cfg.rootdir)
            "echo "After: " file_raw
            "echo "rootdir: " a:sp_cfg.rootdir
            " Add canonical name to list.
            call add(files, file_raw)
        endfor
    catch
        " TODO: Perhaps a Rethrow method to be used everywhere instead of
        " this (for stripping extra Vim(echoerr) at head).
        " Alternatively, a display function to strip them all from v:exception
        " before output...
        echoerr v:exception
    finally
        call sf.destroy()
    endtry
    " TODO: Make this optional so we can skip if user's find or whatever
    " ensures sorted.
    " TODO: Should we uniquify? Doing so could permit some optimizations
    " (e.g., find_insert_idx).
    call sort(files)
    let a:sp_cfg.files = files
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

fu! s:get_matching_files(sprj, glob, partial)
    let matches = []
    let sf = s:sf_create()
    try
        let anchor_dir = ''
        " Handle leading anchor (if any)
        " ./ works just like Vim
        " .// specifies the current working dir
        " all other paths must match from the beginning
        " TODO: Add support for any number of ../ just after the ./ or .//
        " Rationale: Very useful, as there are times when you want to start
        " the search from a near ancestor of the cwd or the current file's
        " dir.
        let glob = a:glob
        if glob =~ '^\.//'
            " Use fnamemodify to ensure trailing slash.
            let anchor_dir = s:canonicalize_path(getcwd(), a:sprj.rootdir)
            let glob = anchor_dir . glob[3:] " strip .//
        elseif a:glob =~ '^\./'
            " Note: This will default to same as .// if no current file.
            let anchor_dir = s:canonicalize_path(expand('%:h'), a:sprj.rootdir)
            let glob = anchor_dir . glob[2:] " strip ./
        endif
        " Convert * and ** in glob, and extract fixed string constraints into
        " a special structure to be used for optimization.
        " Rationale: A fixed string at head allows us to delineate (using
        " adaptive search) a region within file list, outside of which matches
        " cannot exist. Limiting search to this region can speed things up
        " considerably.
        let patt_info = s:glob_to_patt(glob, a:partial)
        let matches = s:get_matching_files_match(patt_info, a:sprj.files)
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
    echohl WarningMsg
    echomsg "Warning: " . a:msg
    echohl None
endfu
" Optional args:
" 1: strip leading Vim(echoerr)
" TODO: Decide whether we shouldn't just always do that. If so, no reason to
" have separate functions for warn and error; just make warn optional...
fu! s:err(msg, ...)
    let strip = a:0 && a:1
    let msg = strip ? substitute(a:msg, '^\s*Vim(echoerr)\s*:\s*', '', '') : a:msg
    echohl ErrorMsg
    echomsg msg
    echohl None
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
        " Note: Use set in lieu of let-&.
        " Implication: Saving/restoring an option will effectively destroy any
        " local setting.
        " Rationale: Supports more assignment operators: e.g., += doesn't work
        " for string-list options with let-&.
        " Design Decision: Use non-local sets.
        " Rationale: There are several issues with use of local options:
        " -Without a lot of messy logic and hidden window creation,
        " there's no way to tell whether a local setting exists, and if it
        " doesn't, we don't want to create one.
        " -When we split a window, local settings are not copied to the new window.
        " -The stack frame mechanism in its current form cannot restore local
        "  options in multiple windows.
        exe 'set '
            \. (boolean ? (!!a:val ? '' : 'no') : '')
            \. a:name
            \. (boolean ? '' : op . escape(a:val, ' |\'))
    endfu
    " Change cwd, pushing the old onto a stack.
    " Design Decision: Originally, lcd was used, but this complicates things
    " when we're splitting windows. Moreover, though intended to be less
    " obtrusive, it was probably more so, as its effect was to create a
    " (persistent) local cwd where the user had none previously.
    " Implication: If the user does have a local cwd set, it will be lost.
    fu! sf.pushd(dir) dict
        call add(self.dirs, getcwd())
        " Note: As mentioned in Vim doc (:help :filename), commands expecting
        " single filename don't require escaping.
        exe 'cd ' . a:dir
    endfu
    fu! sf.popd() dict
        " Question: Should I add logic to handle underflow (which is
        " essentially an internal error)?
        if len(self.dirs)
            " cd to popped dir.
            exe 'cd ' . remove(self.dirs, -1)
        endif
    endfu
    " Return the stack frame
    return sf
endfu

" <<<
" >>> Functions used during command execution
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

" Break input command line into 2 pieces:
" 1. a List of specs
" 2. everything else
" Returns: a List with the following elements:
"   0: List of specs
"   1: string representing command line remaining after spec removal
" Note: This function is not responsible for validating the specs; at this
" point, if something looks as though it could be a spec, we'll treat it as
" such. It's quite possible that one of the specs will subsequently fail
" validation.
" Rules: A spec begins with one of the following...
"     --<non-ws>
"      -<non-ws>
"      :<non-ws>
" ...and consumes everything till unescaped whitespace or end of string.
" Aside: As with GNU grep, a bare -- or - is not considered to be an option:
" e.g., `grep - foo.c' looks for a dash in foo.c. Of course, a bare -- is
" neither an option nor a pattern, but a marker signaling end of options.
" Since a bare `:' is pointless as a spec, I treat it like `--' and `-': i.e.,
" not a spec but part of the 'rest'.
" When there are no more specs, discard any optional separator of the
" following form...
"     [<ws>] [--] [<ws>]
" ...and take whatever follows (if anything) as the 'rest' parameter.
fu! s:parse_raw_cmdline(cmdline)
    " Note: The \@> backtracking-inhibit ensures that a -- can't look like
    " short option `-'.
    " TODO: Bug in new Vim regex engine precluded use on a similar pattern.
    " Had to force use of old, either with \%#=1 or using option. Does this
    " bug affect this pattern?
    let m = matchlist(a:cmdline,
        \'^\s*\(\%(\%(--\|-\|:\)\@>\%(\\.\|\S\)\+\)\%(\s\+\%(\%(--\|-\|:\)\@>\%(\\.\|\S\)\+\)\)*\)\?'
        \.'\%(\s*\%(--\)\?\s*\)\?'
        \.'\(.*\)')
    let [specstr, rest] = m[1:2]
    " Break the specs into List at unescaped whitespace.
    let specs = split(specstr, '\%(^\|[^\\]\)\%(\\\\\)*\zs\s\+\ze')
    " Remove only the backslashes that are escaping whitespace.
    call map(specs, 'substitute(v:val, ''\%(^\|[^\\]\)\%(\\\\\)*\zs\(\\\)\ze\s\+'', '''', ''g'')')
    return [specs, rest]
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
"   sprjs:  List of sprj objects corresponding to selected subprojects, -1 if
"           unconstrained (i.e., subproject spec omitted)
"   glob:   file glob as string, '' if glob omitted
"           Note: Omitted glob effectively selects all files in selected
"           subproject(s).
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
" Assumption: Input represents a spec and only a spec, such that it's safe
" (and desirable) to anchor pattern match at both beginning and end. Note that
" when it was anchored only at beginning, garbage after valid options went
" undetected: e.g., if user hit `/' instead of `:', producing an input of
" `-j/**/foo', the -j would be taken as valid short option, and the /**/foo
" would simply be discarded without error.
" Refactor_TODO: In order to be able to use this for new FPSOpen and variants
" (which need to get list of sp names up front), refactor to remove the part
" that checks long/short opt names. I'm thinking instead of a list of indices,
" have it return a list of s:<short-opt> and l:<long-opt> strings, or a list
" of lists or dicts.
fu! s:parse_spec(opt, throw)
    let oc = '[a-zA-Z0-9_]' " chars that can appear in option
    "                  <sopts>              <lopts>                            <glob>
    let re_opt = '^\%(-\('.oc.'\+\)\)\?\%(--\('.oc.'\+\%(,'.oc.'\+\)*\)\)\?\%(:\(.*\)\)\?$'
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
        " TODO: Decide whether we could use [] to represent this...
        let sprjs = -1
    else
        let dup = {}
        let sprjs = []
        let sopts = split(soptstr, '\zs') "split chars
        let lopts = split(loptstr, ',')
        " Create a single array for looping, but remember dividing point.
        let num_short_opts = len(sopts)
        let opts = sopts + lopts
        " Get list of config indices.
        " Design Decision: Abort on bad option, even if multiple specified.
        " TODO: Refactor to make this part of command line parsing, since it's used in several places.
        let i = 0
        for opt in opts
            let is_short = i < num_short_opts
            let sprj = s:prj.get_sp((is_short ? 's:' : 'l:') . opt)
            " TODO: How to handle invalid specs? Originally, just skipped, but
            " perhaps, in accordance with Design Decision above, we should
            " abort...
            " Yes! Need to abort, to prevent this seeming like unconstrained
            " search!
            if empty(sprj)
                call s:warn("Invalid subproject spec: " . (is_short ? '-' : '--') . opt)
                continue
            endif
            " Add sprj to list if not already added.
            if (!has_key(dup, sprj.name))
                call add(sprjs, sprj)
                let dup[sprj.name] = 1
            endif
        endfor
    endif
    " Note: sprjs will be either a list of sprj objects or -1 for unconstrained.
    return {'sprjs': sprjs, 'glob': glob}
endfu
" Process input spec, returning list of subprojects, and (if has_glob is set)
" a list of matching files.
" Return: Format depends upon has_glob input flag:
" true: 
"     [{sprj: <sprj>, files: [<path>, ...]}, ...]
" false:
"     [<sprj>...]
" TODO: Consider whether to dispense with all the 'throw' args...
fu! s:process_spec(spec, has_glob, partial, throw)
    let opt = s:parse_spec(a:spec, a:throw)
    if type(opt) != 4 " Dict
        echoerr "Bad spec: " . a:spec
        return []
    endif
    if type(opt.sprjs) == 3 " List
        let sprjs = opt.sprjs
    else
        " No subproject constraints
        " TODO: Better way to handle unconstrained. Perhaps just empty list?
        let sprjs = s:prj.get_all_sps()
    endif
    if !a:has_glob
        return sprjs
    else
        let ret = []
        for sprj in sprjs
            let files = s:get_matching_files(sprj, opt.glob, a:partial)
            " Accumulate subproject-specific object.
            call add(ret, {'sprj': sprj, 'files': files})
        endfor
        return ret
    endif
endfu
" Return: List containing 2 elements:
" 1. List of parsed specs (see s:process_spec header for 2 possible formats)
" 2. String representing everything else
fu! s:parse_cmdline(cmdline, has_glob)
    " Parse cmdline into an array of raw specs and everything else.
    let [specs, argstr] = s:parse_raw_cmdline(a:cmdline)
    " Command line can contain multiple specs, and each spec can comprise
    " multiple subprojects; concatenate the arrays returned by each call to
    " process_spec() to produce a flattened list of sp's.
    let pspecs = []
    for spec in specs
        " TODO: Decide about partial
        let pspec = s:process_spec(spec, a:has_glob, 1, 1)
        call extend(pspecs, pspec)
    endfor
    return [pspecs, argstr]
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
    " Get matching file sets, each of which is represented by an sprj and a
    " List containing a subset of its files.
    let fsets = s:process_spec(a:arg_lead, 1, 1, 1)
    let files = []
    for fset in fsets
        let sp_name = fset.sprj.name
        let sp_files = fset.files
        call map(sp_files, '"--" . l:sp_name . ":" . v:val')
        call extend(files, sp_files)
    endfor
    return files
endfu
" <<<
" >>> Functions invoked by commands
fu! s:refresh(...)
    " Note: Input should be 0 or more specs represented as a single string.
    " TODO: UNDER CONSTRUCTION!!!
    let cmdline = !a:0 ? '' : a:1
    " Parse cmdline into an array of specs (and hopefully, nothing else).
    let [sprjs, argstr] = s:parse_cmdline(cmdline, 0)
    echo sprjs
    if !empty(argstr)
        " Refresh command doesn't accept any non-spec args!
        echoerr "Non pspec args not accepted by this command: " . argstr
    endif
    " TODO: Probably need something along the lines of sort_and_combine_pspecs
    " here (to avoid multiple refreshes when same sp inadvertently specified
    " multiple times); if so, need to parameterize sort_and_combine_pspecs to
    " handle the case in which sprjs is a simple list of sp's rather than a
    " list of objects, each of which contains an sprj and a list of files.
    if empty(sprjs)
        " No specs provided: refresh all subprojects...
        let sprjs = s:get_unconstrained_pspecs(1)
    endif
    echo sprjs
    let sf = s:sf_create()
    try
        for sprj in sprjs
            " TODO - Fix this...
            let rootdir = call('finddir', sprj.opt.get('root'))
            if rootdir == ''
                throw "Couldn't locate project base. Make sure your cwd is within the project."
            endif
            let rootdir = s:canonicalize_path(rootdir, '')
            " Note: An sf.cd (TODO) would make more sense here than pushd...
            call sf.pushd(rootdir)
            let sprj.rootdir = rootdir
            call s:cache_listfile(sprj, sprj.opt, 1)
        endfor
    "catch /Vim(echoerr)/
        "echohl ErrorMsg|echomsg v:exception|echohl None
    finally
        call sf.destroy()
    endtry
endfu

" Input: List in following form...
" [{sprj: <sprj>, files: <files>}, ...]
" ...in which the sprj's are unordered and non-unique, with the same files
" potentially appearing multiple times for a single subproject.
" Output: Equivalent list, but with each subproject represented only once (and
" only if it contains files), in fiducial order, with files sorted and unique
" *within* subproject.
" Design Decision: Don't try to uniquify across subprojects - expense not
" justified by expected use case.
fu! s:sort_and_combine_pspecs(pspecs)
    let pspec_map = {}
    for pspec in a:pspecs
        let sp_name = pspec.sprj.name
        if !has_key(pspec_map, sp_name)
            let pspec_map[sp_name] = []
        endif
        let files = pspec_map[sp_name]
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
    " Convert pspec_map to a List for output, using prj's iterator to ensure
    " proper sp order.
    let _pspecs = []
    call s:prj.iter_init()
    while s:prj.iter_valid()
        let sprj = s:prj.iter_current()
        if has_key(pspec_map, sprj.name)
            " Discard empty subprojects.
            if !empty(pspec_map[sprj.name])
                call add(_pspecs, {'sprj': sprj, 'files': pspec_map[sprj.name]})
            endif
        endif
        call s:prj.iter_next()
    endwhile
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
" [{sprj: <sprj>, files: <files>}]
" Note: Form of list elements is similar to that returned by
" process_spec, but with following differences:
" -Subprojects may be broken up to ensure that maxgrepsize constraints are not
"  violated.
" -List of files is converted to a string list, with escaping suitable either
"  for use with user's shell.
"  Note: Currently, shellescape is used unconditionally (i.e., no 'shell'
"  input arg), because xargification is required only for external commands.
fu! s:xargify_pspecs(pspecs, fixlen)
    let pspecs = s:sort_and_combine_pspecs(a:pspecs)
    if empty(pspecs)
        return []
    endif
    let _pspecs = [] " transformed pspec list
    for pspec in pspecs
        if !exists('l:_pspec') || _pspec.sprj isnot pspec.sprj
            if exists('l:_pspec')
                " Accumulate old.
                call add(_pspecs, _pspec)
            endif
            " First time we've seen this sprj.
            let _pspec = {'sprj': pspec.sprj, 'files': ''}
            let cumlen = a:fixlen
        endif
        let maxgrepsize = pspec.sprj.opt.get('maxgrepsize')
        " Accumulate as many files as possible without exceeding maxgrepsize
        for f in pspec.files
            " Escape and prepend space before length test.
            let f = ' ' . shellescape(f)
            let len_f = strlen(f)
            " Decision: Always accumulate at least one file regardless of
            " maxgrepsize constraint.
            if len(_pspec.files) && len_f + cumlen > maxgrepsize
                " Accumulate early.
                call add(_pspecs, _pspec)
                let _pspec = {'sprj': pspec.sprj, 'files': ''}
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
" Prepare list of files for use in command line by converting list to a single
" string, in which each filename is properly escaped, either for use on Vim
" command line, or if shell arg is set, for user's shell.
" Important Note: This is a mutator method: list is modified in place.
fu! s:convert_file_list_to_string(pspecs, shell)
    for pspec in a:pspecs
        let fs = ''
        for f in pspec.files
            let fs .= ' ' . (a:shell ? shellescape(f) : fnameescape(f))
        endfor
        " Replace the list with the accumulated string.
        " Note: Confirmed that Vim permits changing Dictionary value type
        let pspec.files = fs
    endfor
endfu
" TODO: Rework this.
" TODO: Test new omit_files option, which allows return of simple of list of
" sp's. Decide on the sense of the optional flag: should it instead be
" want_files? Consistency with has_glob sense?
fu! s:get_unconstrained_pspecs(...)
    let omit_files = a:0 && a:1
    let ret = []
    call s:prj.iter_init()
    while s:prj.iter_valid()
        let sprj = s:prj.iter_current()
        " TODO: Make copy of files? Better way to handle unconstrained?
        call add(ret, omit_files ? sprj : {'sprj': sprj, 'files': sprj.files[:]})
        call s:prj.iter_next()
    endwhile
    return ret
endfu

" TODO: Move this to util section, possibly wrapping in object to facilitate
" cleanup.
fu! s:get_tempfile()
    let base = 'fpstmp'
    let name = base
    let i = 1
    while glob(name)
        if i > 100
            throw "Can't find suitable name for temporary file in `" . getcwd() . "'"
        endif
        let name = base . printf("%03d", i)
        let i += 1
    endwhile
    " Write a placeholder to make sure we can write here.
    " Note: Subsequent calls to writefile will overwrite. Caller responsible
    " for deleting.
    if writefile([], name, 1)
        throw "Can't write temporary file in `" . getcwd() . "'"
    endif
    return name
endfu

" Return: True iff matches found.
fu! s:do_int_grep(sf, pspecs, argstr, bang, use_ll, is_adding)
    let got_match = 0
    let add = a:is_adding ? 'add' : ''
    for pspec in a:pspecs
        " TODO: Decide on this: caller has used convert_file_list_to_string to
        " mutate the files member of sp_cfg. Is that best way? (Keep in mind
        " that the mutated structure is not part of s:cfg, but a derivative.)
        "let files = ''
        "for file in sp_cfg.files
        "    let files .= ' ' . fnameescape(file)
        "endfor
        " Move to root of subproject to perform grep.
        " Rationale: File paths are relative to this directory.
        call a:sf.pushd(pspec.sprj.rootdir)

        " Note: Always inhibit jump here, as that's handled by caller. Don't
        " use 'silent', as we want to see the periodic progress indicator
        " displayed by vimgrep.
        " Error Handling: Don't suppress errors, but catch 'error' E480 to
        " avoid treating failed match as error.
        try
            exe 'noautocmd ' . (a:use_ll ? 'l' : '') . 'vimgrep' . add . ' /' . a:argstr . '/j' . pspec.files
            let got_match = 1
        catch /:E480/
        endtry
        " Make sure we create at most 1 list.
        let add = 'add'
    endfor
    " Vim Kludge: If we don't redraw status line after internal grep,
    " subsequent attempts to display (e.g.) warnings fail: apparently, it has
    " something to do with the automatic update of status line during vimgrep.
    redrawstatus
    return got_match
endfu

" Return: True iff we've added something to a qf/ll list (could be only
" unparseable error text).
fu! s:do_ext_grep(sf, pspecs, argstr, bang, use_ll, is_adding)
    " Process each subproject in turn: in the root dir of each, build and run
    " (with system()) a script that accomplishes the grep, catching the result
    " to be fed into the applicable cexpr variant.
    " Note: A subproject will be represented multiple times (in succession) in
    " the pspecs array iff xargification constraints prevent the processing of
    " all the subproject's files in a single grep. In such cases, the inner
    " loop is used to put multiple command lines into a single script.
    let idx = 0
    let len = len(a:pspecs)
    " TODO: Perhaps refactor the inner loop, then use idx instead of force_add.
    " Rationale: The inner loop is complicated unnecessarily because of
    " reluctance to use a lookahead index, and this is silly optimization...
    let force_add = 0
    let got_match = 0
    while idx < len
        if idx == 0
            " Boostrap
            let pspec_next = a:pspecs[0]
        endif
        " Note: pspec represents the first of the set of commands destined for
        " the current script.
        let pspec = pspec_next
        let scriptlines = []
        while idx < len && pspec_next.sprj is pspec.sprj
            " Accumulate another line for script
            " TODO: Use grepprg option (parsing $* etc...)
            call add(scriptlines, 'grep -n ' . a:argstr . ' ' . pspec_next.files)
            let idx += 1
            if idx < len
                let pspec_next = a:pspecs[idx]
            endif
        endwhile
        " Ready to process all batches for current sp.
        let sprj = pspec.sprj
        " Note: If user has not overridden grepprg and grepformat,
        " keep current Vim setting.
        let flags = {}
        let grepprg = sprj.opt.get('grepprg', flags)
        if flags.set | call a:sf.setopt('grepprg', grepprg) | endif
        " Note: Plugin option grepformat corresponds to Vim option
        " 'errorformat' (because we're using cexpr et al.)
        let grepformat = sprj.opt.get('grepformat', flags)
        if flags.set | call a:sf.setopt('errorformat', grepformat) | endif
        " Move to root of subproject and create the temporary script file.
        " Rationale: Avoid potential cross-platform issues with absolute
        " paths.
        call a:sf.pushd(sprj.rootdir)
        " This try and associated 'finally' ensures cleanup of tempfile
        " created in current dir.
        " Note: Intentionally placing call to get_tempfile outside try.
        " Rationale: Abnormal return from get_tempfile obviates need for
        " cleanup.
        let scriptname = s:get_tempfile()
        try
            call writefile(scriptlines, scriptname)
            " TODO: Introduce plugin-specific shell/shellcmdflag options.
            " Note: Ideally, would use stdin arg to system(), as this would
            " obviate need for tempfile in current dir, but this would work
            " only for shells that will parse stdin.
            " Caveat: Some shells won't have ./ in PATH, so we can't simply
            " use bare tempfile name.
            " TODO: Don't hardcode the ./ - allow user to override with an
            " option.
            let result = system(&shell . ' ' . &shellcmdflag . ' ./' . scriptname)
            if !empty(result)
                let got_match = 1
            endif
            " Design Decision: Perform the applicable add without regard to
            " whether we got match.
            " Rationale: Vim's grep commands always affect qf/ll, even when no
            " matches; thus, mine should as well...
            " Use one of the following to add files: cexpr!, lexpr!, caddexpr, laddexpr
            " Note: Use bang (if supported) to inhibit jump, and silent to suppress its output.
            " Rationale: Both jump and output will be handled in command-
            " appropriate way at conclusion of loop.
            " Rationale: cexpr and grep command variants handle bang/inhibit
            " differently, but we need them to work the same for Find and
            " Grep.
            let addcmd = (a:use_ll ? 'l' : 'c')
                \. (a:is_adding || force_add ? 'add' : '')
                \. 'expr' . (!a:is_adding && idx == 0 ? '!' : '')
            " TODO: Investigate why I get some sort of ATTENTION error when a
            " swapfile exists for one of the files in result...
            exe 'silent ' . addcmd . ' l:result'
        finally
            " Design Decision: Could test for failure and report, but odds of
            " failure are low, as is the cost (a file left in sp root).
            call delete(scriptname)
        endtry
        let force_add = 1
    endwhile
    " Explanation: With external grep, it's not obvious whether matches were
    " found: the only way to tell whether text returned by system() represents
    " grep matches or errors is to attempt to parse with cexpr et al and see
    " what happens to the list. This would be messy, however, because
    " getqflist() doesn't provide *efficient* access to the list (but actually
    " builds the whole thing each time it's called). I could redirect and
    " parse the output of clist/cc/etc..., but once again, messy...
    " Solution: If none of the grep scripts returned anything, we know we
    " couldn't have found matches, and can return indication of this so that
    " caller can warn; otherwise, assume there *could* have been matches; user
    " will be able to see any errors in the qf/ll list.
    return got_match
endfu

fu! s:grep(cmd, bang, cmdline)
    let sf = s:sf_create()
    try
        " Parse cmdline into an array of parsed specs and everything else.
        let [pspecs, argstr] = s:parse_cmdline(a:cmdline, 1)
        if empty(pspecs)
            " No specs provided: search all files in all subprojects...
            " Note: This is different from non-empty pspecs that contain no
            " files (handled later)...
            let pspecs = s:get_unconstrained_pspecs()
        endif
        if empty(argstr)
            echoerr "Must specify at least 1 pattern for grep."
        endif
        " Extract some parameters from command name.
        let use_ll = a:cmd =~? '^[st]\?l'
        let is_adding = a:cmd[-3:] == 'add'
        let win_cmd = a:cmd =~? '^s' ? 'new' : a:cmd =~? '^t' ? 'tabnew' : ''
        let external = a:cmd !~? 'vgrep'
        " Prepare pspec list by sorting, combining and uniquifying.
        let pspecs = s:sort_and_combine_pspecs(pspecs)
        " Assumption: Matchless subprojects were culled by
        " sort_and_combine_pspecs.
        if empty(pspecs)
            echoerr "No file(s) to grep."
        endif
        if external
            " TODO: Where to configure max len? Also, calculate fixlen
            " Note: xargify_pspecs handles sorting, combining, and uniquifying.
            let pspecs = s:xargify_pspecs(pspecs, 100)
        else
            " No xargification required for internal command; however, we will
            " need to convert file lists to strings suitable for use on Vim
            " command line.
            call s:convert_file_list_to_string(pspecs, 0)
        endif
        if win_cmd != ''
            " Open new window in manner determined by command
            exe win_cmd
        endif
        if external
            let got_match = s:do_ext_grep(sf, pspecs, argstr, a:bang, use_ll, is_adding)
        else
            let got_match = s:do_int_grep(sf, pspecs, argstr, a:bang, use_ll, is_adding)
        endif
        if !got_match
            " Note: If here, we know there were no matches (though absence of
            " true matches doesn't ensure we'll get here).
            call s:warn("No match found: " . argstr)
        endif
        " Design Decision: Could make this an elseif, but I'm thinking I
        " should always do the cc/ll (ignoring any E42 since we've already
        " warned if no match).
        if !a:bang
            " Jump to *current* match (for add variants, this will be whatever
            " was current before the command).
            " Note: Handle the E42 generated by ll/cc when list is empty.
            try
                exe use_ll ? 'll' : 'cc'
            catch /:E42/
            endtry
        else
            " Without this, user gets no feedback.
            " TODO: How/whether to get file count. If expensive/complex, perhaps
            " don't bother.
            echomsg "Added " . '?' . " matches to " . (use_ll ? 'location' : 'quickfix') . " list"
        endif
    finally
        call sf.destroy()
    endtry
endfu
fu! s:find(cmd, bang, cmdline)
    let sf = s:sf_create()
    try
        " Parse cmdline into an array of specs (and hopefully, nothing else).
        let [pspecs, args] = s:parse_cmdline(a:cmdline, 1)
        if !empty(args)
            " Find commands don't accept any non-spec args!
            echoerr "Non pspec arg supplied to " . a:cmd . " command."
        endif
        " TODO: Question: Is it possible for pspecs to be empty at this point
        " (i.e., given -nargs=1, and given that we apparently haven't aborted
        " on bad spec)?
        if empty(pspecs)
            echoerr "No file(s) found."
        endif
        " Combine, sort, uniquify, etc... the pspecs list.
        " Assumption: The returned list contains no empty sp's.
        let pspecs = s:sort_and_combine_pspecs(pspecs)
        let sp_cnt = len(pspecs)
        " Determine whether we're in single or multi-file regime (bearing in
        " mind that we've already determined pspecs is non-empty).
        let mode = sp_cnt > 1 ? 'multi' : sp_cnt > 0 ? (len(pspecs[0].files) > 1 ? 'multi' : 'single') : 'none'
        if mode == 'none'
            " TODO: Decide how to handle. Originally, was affecting the qf/ll
            " list, as Vim's grep commands do on failed match, but in light of
            " Find's special treatment of single file match, I'm thinking
            " perhaps zero file match should be special also. In particular,
            " I'm thinking it doesn't make sense to create an empty qf/ll. For
            " that matter, it may not make sense to do it for grep either...
            call s:err("No matching files")
            return
        endif
        " Extract some parameters from command name.
        let use_ll = a:cmd =~? '^[st]\?l'
        let is_adding = a:cmd[-3:] == 'add'
        let win_cmd = a:cmd =~? '^s' ? 'new' : a:cmd =~? '^t' ? 'tabnew' : ''
        " Note: If only 1 match, the special logic in the following 'if' can
        " short-circuit the rest of the function.
        if mode == 'single' && !a:bang && !use_ll && !is_adding
            let [files, sprj] = [pspecs[0].files, pspecs[0].sprj]
            " Move temporarily to sp root to open file.
            " Note: Unnecessary but safe if filename happens to be absolute.
            call sf.pushd(sprj.rootdir)
            " Run applicable file open command without bang to ensure safety.
            exe (!empty(win_cmd) ? win_cmd : 'edit') . ' ' . fnameescape(files[0])
            " Note: try-finally will handle state restoration.
            return
        endif
        " If we get here, we're going to be updating qf/ll list.
        " TODO: Consider whether to make the 'errorformat' set local to
        " the window for commands that create a new window. Weigh against
        " benefits of consistency with common case of using an existing
        " window.
        call sf.setopt('errorformat', '%f|%m')
        if win_cmd != ''
            " Open new window in manner determined by command
            exe win_cmd
        endif
        " Loop over subprojects
        let file_cnt = 0
        let sp_idx = 0
        for pspec in pspecs
            let sprj = pspec.sprj
            " Make a copy for mutation.
            let files = pspec.files[:]
            " Design Decision: Without an error message, the `(i of N)'
            " message in the status area looks a bit bare...
            call map(files, 'v:val . "|" . v:val')
            " Move to the directory to which files in pspec are relative.
            " Rationale: Vim remembers the directory that was current when
            " qf/ll was updated, and will keep the qf/ll list in sync as
            " cwd is subsequently changed.
            " TODO: cd semantics make more sense than pushd in loop...
            call sf.pushd(sprj.rootdir)
            " Use one of the following to add files: cexpr!, lexpr!, caddexpr, laddexpr
            " Note: Use bang (if supported) to inhibit jump, and silent to suppress its output.
            " Rationale: Both jump and output will be handled in command-
            " appropriate way at conclusion of loop.
            " Rationale: cexpr and grep command variants handle bang/inhibit
            " differently, but we need them to work the same for Find and
            " Grep.
            let addcmd = (use_ll ? 'l' : 'c')
                \. (is_adding || sp_idx > 0 ? 'add' : '')
                \. 'expr' . (!is_adding && sp_idx == 0 ? '!' : '')
            exe 'silent ' . addcmd . ' l:files'
            "echomsg addcmd . ' files'
            let file_cnt += len(files)
            let sp_idx += 1
        endfor
        if !a:bang
            " Jump to *current* match (for add variants, this will be whatever
            " was current before the command).
            exe use_ll ? 'll' : 'cc'
        else
            " Without this, user gets no feedback.
            echomsg "Added " . file_cnt . " files to " . (use_ll ? 'location' : 'quickfix') . " list"
        endif
        " Exception Note: Unlike Grep commands, 'No matches' error not
        " possible here: hence, no need for special catch.
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
    "echomsg "Args: " . string(a:args)
    " TODO: Any reason to set shellescape's optional special arg? Think about
    " stuff like # and % on command line. Where/when is it processed?
    " TODO: I'm thinking I need to add a layer of escaping on `|' here.
    " Rationale: Since my commands don't have the -bar arg, a `|' will be
    " included in args, *but* the string we return is going to be :execute'd
    " (as a grep or whatever), and at that point, we need the bar escaped.
    " Bit TODO: Investigate :! and shellescape() using args.sh and FA command...
    " TODO: Consider -nargs=1 so I can get raw command line and process it all
    " myself...
    let mapexpr = a:external ? 'shellescape(v:val)' : 'escape(v:val, '' \'')'
    let args = map(copy(a:args), mapexpr)
    return join(args, " ")
endfu
" <<<
" >>> Commands
"
" TODO: -bang for refresh and add completion maybe... Hmmm... Maybe not,
" because this would entail reading some config at Vim startup...
" TODO: Move all but load/open command into corresponding function.
" Avoid: Better for Grep et al. not to exist than to get errors trying to run
" it too soon.
" Open project (or subset thereof if sp specs are given), forcing file refresh
" iff bang supplied.
com! -bang -nargs=+ FPSOpen call s:open(<q-bang>, <f-args>)
" Reopen current project (taking into account any subset specified in
" FPSOpen), forcing file refresh iff bang supplied.
com! -bang -nargs=0 FPSReopen call s:reopen(<q-bang>)
" Force file refresh for specified subprojects (defaults to all in active
" subset).
com! -nargs=? FPSRefresh call s:refresh(<f-args>)
com! FPSClose call s:close()

" Grep commands
" Grep non-adding variants
" Internal variants
com! -bang -nargs=1 Vgrep call s:grep('Vgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Svgrep call s:grep('Svgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Tvgrep call s:grep('Tvgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Lvgrep call s:grep('Lvgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Slvgrep call s:grep('Slvgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Tlvgrep call s:grep('Tlvgrep', <q-bang>, <f-args>)
" External variants
com! -bang -nargs=1 Grep call s:grep('Grep', <q-bang>, <f-args>)
com! -bang -nargs=1 Sgrep call s:grep('Sgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Tgrep call s:grep('Tgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Lgrep call s:grep('Lgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Slgrep call s:grep('Slgrep', <q-bang>, <f-args>)
com! -bang -nargs=1 Tlgrep call s:grep('Tlgrep', <q-bang>, <f-args>)

" Grep adding variants
" Design Decision: Don't support window-creating/adding variants for location
" list.
" Rationale: Not much use case for it: location list isn't even copied to new
" window, so implementation would be difficult. Moreover, you typically use a
" window-creating variant to avoid losing your place in the current window,
" but with adding variants, the only type of jump that occurs is to what was
" *current* entry prior to the command (which often means no movement at all).
" Bottom Line: The complexity engendered by
" window-creating/adding/location-list variants would not be justified by the
" expected use case.
" Internal variants
com! -bang -nargs=1 Vgrepadd call s:grep('Vgrepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Svgrepadd call s:grep('Svgrepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Tvgrepadd call s:grep('Tvgrepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Lvgrepadd call s:grep('Lvgrepadd', <q-bang>, <f-args>)
" External variants
com! -bang -nargs=1 Grepadd call s:grep('Grepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Sgrepadd call s:grep('Sgrepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Tgrepadd call s:grep('Tgrepadd', <q-bang>, <f-args>)
com! -bang -nargs=1 Lgrepadd call s:grep('Lgrepadd', <q-bang>, <f-args>)

" Find commands
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Find call s:find('Find', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Sfind call s:find('Sfind', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Tfind call s:find('Tfind', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Lfind call s:find('Lfind', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Slfind call s:find('Slfind', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Tlfind call s:find('Tlfind', <q-bang>, <f-args>)

" Find add variants
" Design Decision: See note on Grep add variants for rationale regarding
" omission of window-creating location-list variants.
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Findadd call s:find('Findadd', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Sfindadd call s:find('Sfindadd', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Tfindadd call s:find('Tfindadd', <q-bang>, <f-args>)
com! -bang -nargs=1 -complete=customlist,<SID>complete_filenames Lfindadd call s:find('Lfindadd', <q-bang>, <f-args>)

" <<<

" The remainder of this file needn't be parsed.
finish

" >>> Code graveyard
com! -nargs=* -complete=customlist,<SID>complete_filenames Spq call FA(<q-args>)
" Quoted args play...
com! -nargs=* QA FA <q-args>
com! -nargs=+ FA call FA(<f-args>)
com! -nargs=1 FA1 call FA(<f-args>)
com! -nargs=1 QA1 call FA(<q-args>)
com! -nargs=1 Eg call Eg(<f-args>)
com! -nargs=1 Egr call Egr(<f-args>)
com! -nargs=* CLR call CLR(<f-args>)
com! -nargs=* CLG call CLG(<f-args>)
fu! s:escape_args(args)
    let s = ''
    for arg in a:args
        let s .= ' ' . shellescape(arg)
    endfor
    return s
endfu
fu! CLR(...)
    echomsg 'Args: ' . s:escape_args(a:000)
    exe '!C:/Users/stahlmanb/tmp/args.sh' . s:escape_args(a:000)
endfu
fu! CLG(...)
    echomsg 'Args: ' . s:escape_args(a:000)
    let gp_save = &l:grepprg
    let &l:grepprg = 'C:/Users/stahlmanb/tmp/args.sh -n $* /dev/null'
    exe 'grep ' . s:escape_args(a:000)
    let &l:grepprg = gp_save
endfu
fu! Egr(cl)
    exe '!C:/Users/stahlmanb/tmp/args.sh ' . a:cl
endfu

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
com! -nargs=1 -complete=customlist,Complete_customlist CL call FA(<f-args>)
com! -nargs=1 -complete=custom,Complete_custom C echo "foo"
" <<<
" >>> Running notes - (Probably should move this to a readme or something in
"     Github)

" Prack:< Get rid of last vestiges of this name: e.g., g:fps_config should
" bear new name (TBD).
" 
" Commenting-options:< I don't like not being able to comment options in the big
" option dict.
" Note: Vim provides no way to insert comments within a multi-line Dictionary
" initialization. Of course, users could define the dictionary however they
" like (e.g., breaking up into many assignments), but the standard way will
" probably be to start with a template from the help and tailor it.
" Possible-approaches:<
" 	1. A valid key cannot begin with a `#' sign, so you could "comment out"
" 	   options without removing them like so...
" 	   {
"             \'listfile': 'files.list',
"             \'#grepprg': 'leaving this one here but commenting...'
"        \}
" 	2. Use some sort of parsed .ini file, rather than a global Dictionary, to
" 	   specify options.
" 
" Listfiles:< Long-term, I'm thinking it would be nice to permit user to have
" the listfiles stored in a tempdir (e.g., $TMP, $TEMP, etc...), with names
" created from project and subproject, rather than having to have them clutter
" up his work tree.
" Could have 'listfile' option (may rename) support some sort of special
" symbolic vars, which facilitate naming the file and/or path.
" Note: See note on Listfile-handling earlier...
" 
" Add-options:<
" 'more' - When a grep engenders multiple greps, it's annoying to have to keep
" hitting space.
" 
" Disabled-subprojects:< Do I want to keep this concept? Or simply ensure that
" s:sp_cfg never contains entries that would be disabled.
" Note: This is easy to ensure at load time, but what about after that? E.g., if
" a find command fails...
" Caveat: Could delete from list at that point, but consider that there may be
" several other data structures, which rely upon specific subproject indices.
" Would need to adjust all of them. Simpler approach may be simply to set
" disabled for the subproject, but then I'd need to be sure to check this
" everywhere...
" Note: Currently, this isn't really handled at all...
" Question: Would it make more sense to store everything in a hash rather than
" an List? Look at how sp_cfg[] is used, and see what the implications would be.
" 
" Load-not-Open:< Originally, it was Start, but now that I'm passing a project
" name, I'm thinking Load makes more sense.
" 
" Load-vs-Refresh:< I'm thinking there could be some overlap here: option
" processing, list file caching, etc... Need to decide what should be done when.
" Note: I'm thinking it might make sense to have a -bang argument at least for
" Load, which would force refreshing file list. Might even be able to do without
" a separate Refresh command. Could just have...
" Load <project>   " loads named project
" Load! <project>  " loads named project, forcing file refresh
" Load             " re-loads current project
" Load!            " re-loads current project, forcing file refresh
" 
" Comment...Comment...Comment!!!!:< Lots of code written, not much commenting
" the past few days...
" 
" Idea:< Instead of giving error when one of the Edit commands gets more than 1
" file match, what about presenting the list to user and allowing him to
" downselect somehow? Of course, this is conceptually what we do with
" completion, but still... I don't really like the fact that we leave the spspec
" at the head of a file, and nothing really is done with it when the command is
" executed. (See Big idea further down...)
" Note: Even if I don't add the down-selection, I need to provide a way for user
" to interrupt and get immediately back to the starting point, without cycling
" through all possible matches.
" Question: Does Vim provide a mechanism for interrupting the completion? No. In
" fact, there's no autocommand event that would let us know when completion is
" terminated (as there is for insert mode completion).
" Note: There is a way to alter the history, though, so if I can know when the
" completion has terminated abnormally (e.g., due to CTRL-C), I could add the
" original command line (e.g., the one on which completion was attempted) after
" removing the final one (containing whatever happened to be the last path
" completed).
" Rationale: Canceling completion typically means user made a mistake with the
" pspec/glob and wants to alter it somehow.
" :help histadd histdel
" Autocommands:< CmdwinEnter, CmdwinLeave, CompleteDone 
" NO!!!:< Those are for cmdline *window* and *insert-mode* completion. Not what
" I need...
" Conclusion:< No good way to do this; perhaps just provide a convenient cmdline
" mapping or two permitting user easily to recover the last cmdline on which
" edit completion was attempted. Could be both normal mode mappings (that bring
" up the command line), and mappings for when command line has already been
" entered.
" TODO: Flesh out the commands...
" 
" Issue: Completion works here...
" :Edit --js --php:<TAB>
" ...or even here...
" :Edit --js  <TAB>
" ...even though :Edit is listed as -nargs=1.
" Look at this closely.
" 
" Big-idea!!!!:< Solves the issues outlined above, while harmonizing the 2 sets
" of commands (edit/grep) in a way that will permit consolidation of
" command-line handling!!!
" Idea:<Make all the edit commands work with quickfix/location lists too
" whenever there are more than a single match! Eg,
" 	:Edit --js:**/*Panel.js
" ...would load all <...>Panel.js files into the quickfix list.
" ...additionally, it would :edit the first one in the list.
" Question: If there's only 1 in the list, should we add it to quickfix list, or
" should the default be simply to edit according to the specific edit command
" (e.g., e, sp) when there's only 1 match? I'm thinking this would make sense,
" as it would allow for the original use-case unmodified: e.g., use these
" commands when you want to edit a single file, but don't know its path, and so
" want to use the edit commands' completion to find it first.
" 	Bang:< 2 possible uses for bang:
" 	1. Inhibit opening first file when there are multiple matches: i.e.,
" 	   command used only to get the file list into the qf/ll.
" 	2. If only 1 match, still add to qf/ll.
" 	Hmmm...:< Both use cases could be supported by having the meaning of !
" 	change as a function of # of matches: i.e., single match means use #2,
" 	multiple matches means use #1.
" 	Drawback:< Doesn't support the following use case: I want a single match
" 	to go to qf/ll, but if there happen to be multiple, I want the normal
" 	behavior: i.e., jump to first *and* populate quickfix.
" 	Note: This seems to be a contrived use case. If user is using bang,
" 	chances are it's because he's wanting to build up a list of files for
" 	working with later; in fact, if he's doing this when there's only a single
" 	match, chances are he's using the :Editadd or :Splitadd form. He probably
" 	knows to expect only 1 match; perhaps he's already gotten it via
" 	completion. The point is, though bang is used in 2 slightly different ways
" 	here, there's probably not much overlap in the use cases involved.
" 	Heuristic:< Could think of bang as emphasizing the adding to list over
" 	edit: e.g., if there happens to be only 1 match, put it in the list
" 	because that was the whole point of the edit command.
" Aside: In the words of the story writer from Elf, "I'm psyched out of my
" *mind* about this!"
" Implementation:< Shouldn't be hard at all. Re-uses mostly existing stuff...
" 
" Auto-set-options:< I'm thinking that if s:opt_cfg contained a bit more
" information on options with corresponding Vim options, I could have them set
" automatically without the client code needing to contain much logic: e.g.,
" given a stack frame "sf" and an option "opt", could set something like grepprg
" with the following:
" opt.set_in_sf('grepprg', sf)
" ...or
" sf.setopt_obj('grepprg', opt)
" Rationale: The wrapper would first look at the opt object to determine whether
" the Vim option is being overridden. If so, it would look at the extra info in
" s:opt_cfg to determine how to set (e.g., is it a boolean or string opt?) If
" not overridden by user config, it would know simply to leave it alone.
" Important Note: Look at sf."  before implementing anything. Try not to
" duplicate logic...
" Note: Currently, s:opt_cfg 'vim' key is just the name of a Vim option;
" however, it would need to be an object more info: at least name and type.
" <<<
" vim:ts=4:sw=4:et:fdm=marker:fmr=>>>,<<<
