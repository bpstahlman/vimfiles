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
"   Array of Dictionaries of the following form:
"   { 're': <regex matching name>, 'idx': <submodule index> }
fu! s:process_cfg()
	" TEMP DEBUG
	"so ~/tmp/prack_module_cfg.txt
	if !len(g:prack_module_cfg)
		" TODO: Consider throwing exception here.
		echoerr "Must define at least one project module within g:prack_module_cfg"
		return
	endif
	" Use snapshot of global config until re-initialization.
	let s:module_cfg = deepcopy(g:prack_module_cfg, 1)
	" Short (single-char) names stored in a hash
	" Long names stored in sorted array of patterns employing \%[...]
	let s:shortnames = {}
	let longnames = []
	let lname_to_index = {}
	let ln = len(s:module_cfg)
	" Note: i indexes the subproject config snapshot
	let i = 0
	" Note: valid_cnt will diverge from i if there are invalid config items.
	" Ok because the indexes into s:shortnames and s:longnames aren't
	" significant.
	let valid_cnt = 0
	while i < ln
		let cfg = s:module_cfg[i]
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
					let lname_to_index[cfg.name] = i
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
					let s:shortnames[cfg.shortname] = i
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
		let valid_cnt = valid_cnt + 1
		let i = i + 1
	endwhile
	" If no valid subprojects, no point in continuing...
	if !valid_cnt
		throw "No valid subprojects."
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
		call add(s:longnames, {'re': re, 'idx': lname_to_index[longnames[i]]})
		let i = i + 1
	endwhile
endfu
" <<<
" >>> Functions used during command execution
" Save any state that needs to be changed to perform the grep.
fu! s:save_state()
	let s:cwd_save = getcwd()
	let s:grepprg_save = &grepprg
endfu
" Restore state that was changed to perform the grep.
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
	return {'opt': s:extract_opts(optstr), 'rem': remstr}
endfu
" <<<
" >>> Functions invoked by commands
fu! s:refresh(opts)
	call s:save_state()
	try
		let pcl = s:parse_cmdline(a:opts)
		if pcl.rem != ''
			" TODO: Might need to move inside try if we're going to save/restore regex engine opt.
			echoerr "Invalid arguments specified in Refresh command: `" . pcl.rem . "'"
			return
		endif
		" Extract list of long and short opts.
		let opts = s:extract_opts(a:opts)

		for cfg in s:module_cfg
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
