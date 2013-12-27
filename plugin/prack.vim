" TODO: This is one of the things that would be in the user-provided config.
" TODO: Eventually, may provide a different way to set all this: e.g., function or
" global-vars.
let g:prack_listfile = 'asec-files.list'

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
    \},
    \{
        \'name': 'phpsystb',
        \'shortname': 'r',
        \'root': ['private', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
            \.' -prune -false \) -o -iname ''*.php'''
    \},
    \{
        \'name': 'phpsy',
        \'shortname': 'q',
        \'root': ['private', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
            \.' -prune -false \) -o -iname ''*.php'''
    \},
    \{
        \'name': 'phpsysqa',
        \'shortname': 'd',
        \'root': ['private', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
            \.' -prune -false \) -o -iname ''*.php'''
    \},
    \{
        \'name': 'qt',
        \'shortname': 'Z',
        \'root': ['public', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./resources/js -o -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
            \.' -prune -false \) -o -iname ''*.js'''
    \},
    \{
        \'name': 'phpsysa',
        \'shortname': 'T',
        \'root': ['private', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./extensions -o -path ./framework -o -path ./gii -o -path ./tests -o -path ./vendors \) '
            \.' -prune -false \) -o -iname ''*.php'''
    \},
    \{
        \'name': 'ptu',
        \'shortname': 'f',
        \'root': ['public', ';asec/src'],
        \'find': 'find . \( \( '
            \.' -path ./resources/js -o -path ./shared/extjs -o -path ./help/transition/jquery.js \) '
            \.' -prune -false \) -o -iname ''*.js'''
    \},
\]


fu! s:refresh(mode)
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
            let exestr .= ' -iname ' . join(cfg.names, ' -o -iname ') . ' >' . g:prack_listfile
            " Run the find...
            exe exestr
        endfor
    finally
        call s:restore_state()
    endtry
endfu
fu! s:ack(bang, use_ll, mode, ...)
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
            if !filereadable(g:prack_listfile)
                " TODO: Decide whether to abort, or eventually, continue with next list file
                throw "List file unreadable."
            endif
            let files_from = ' --files-from=' . g:prack_listfile . ' '
        endif
        " TODO: Check for list file existence.
        exe (a:use_ll ? 'l' : '') . 'grep' . (a:bang == '!' ? 'add' : '')
                    \. files_from
                    \. join(args, ' ')

    finally
        call s:restore_state()
    endtry
endfu

" Returns the following struct:
" {
"   opt: <List of all short and long options>,
"   rem: <command line remaining after option removal>
" }
"
fu! s:save_state()
    let s:cwd_save = getcwd()
    let s:grepprg_save = &grepprg
endfu
fu! s:restore_state()
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
fu! s:process_names()
	" TEMP DEBUG
	"let g:prack_module_cfg = readfile('C:/Users/stahlmanb/tmp/names.txt')
	" TODO: Might want to deepcopy the config if we're going to continue to
	" refer to it.
	if !len(g:prack_module_cfg)
		" TODO: Consider throwing exception here.
		echoerr "Must define at least one project module within g:prack_module_cfg"
		return
	endif
	" Short (single-char) names stored in a hash
	" Long names stored in sorted array of patterns employing \%[...]
	let s:shortnames = {}
	let longnames = []
	let lname_to_index = {}
	let ln = len(g:prack_module_cfg)
	let i = 0
	while i < ln
		let cfg = g:prack_module_cfg[i]
		let selectable = 0
		if has_key(cfg, 'name')
			" TODO: Don't hardcode these patterns...
			if (cfg.name !~ '^[a-zA-Z0-9_]\+$')
				echomsg "Ignoring invalid (long) name `" . cfg.name
							\. "': must be sequence of characters matching [a-zA-Z0-9_]."
			else
				if !has_key(lname_to_index, cfg.name)
					let selectable = 1
					let lname_to_index[cfg.name] = i
					call add(longnames, cfg.name)
				else
					echomsg "Warning: Name " . cfg.name . " used multiple times. All but first usage ignored."
				endif
			endif
		endif
		if has_key(cfg, 'shortname')
			if (cfg.shortname !~ '^[a-zA-Z0-9_]$')
				echomsg "Ignoring invalid shortname `" . cfg.shortname
							\. "': must be single character matching [a-zA-Z0-9_]."
			else
				if !has_key(s:shortnames, cfg.shortname)
					let selectable = 1
					let s:shortnames[cfg.shortname] = i
				else
					echomsg "Warning: Name " . cfg.name . " used multiple times. All but first usage ignored."
				endif
			endif
		endif
		if !selectable
			echomsg "Warning: Config item at index " . i . " has neither short nor long option name."
		endif
		let i = i + 1
	endwhile
	" Process the long options.
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
		call add(s:longnames, {'re': re, 'idx': lname_to_index[longnames[i]]})
		let i = i + 1
	endwhile
endfu

fu! s:parse_cmdline(cmdline)
	let m = matchlist(a:cmdline, '^\(\%(\s*--\?[a-zA-Z_]\+\)*\)\%(\s\+--\s\+\)\?\(.*\)$')
	let [optstr, remstr] = m[1:2]
	return {'opt': Extract_opts(optstr), 'rem': remstr}
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

" -- Notes --
" Refresh builds list file(s) specified by command line options
" [L]Grf and [LGrd] ignore listfiles, anchoring their search from the
" current file's dir and the cwd, respectively.
"
com! -nargs=? Refresh call <SID>refresh(<q-args>)
com! -bang -nargs=* Gr  call <SID>ack(<q-bang>, 0, '', <q-args>)
com! -bang -nargs=* Grf call <SID>ack(<q-bang>, 0, 'file', <q-args>)
com! -bang -nargs=* Grd call <SID>ack(<q-bang>, 0, 'dir', <q-args>)

com! -bang -nargs=* LGr  call <SID>ack(<q-bang>, 1, '', <q-args>)
com! -bang -nargs=* LGrf call <SID>ack(<q-bang>, 1, 'file', <q-args>)
com! -bang -nargs=* LGrd call <SID>ack(<q-bang>, 1, 'dir', <q-args>)

" vim:ts=4:sw=4:et
