" Overview:
" IMPORTANT NOTE: Vim doesn't let you send a cr with system(), so you can't send <CR> to end the pattern used for
" the backwards search. Fortunately, screen's stuff command takes a string that supports octal escapes, so the
" workaround is to supply a literal `\015' for the cr.
" Trick: To have screen parse the \015 escape, we have to force screen to reparse the stuffed string. You do so by
" putting it in quotes:
" Reference: http://lists.gnu.org/archive/html/screen-users/2004-12/msg00031.html
" Note: Attempting to use 'stuff' command without quotes around its arguments seems problematic: seems to work in many
" cases, but you'll have trouble with things like cr.
" Note: Use of eval command causes parsing to be done on each subsequent command line arg, with each interpreted as a
" separate screen command *and* its arguments: e.g., for the following shell argument to screen...
"     'stuff "some stuff with \015 escape codes"'
" ...the \015 is actually interpreted by screen's string parser.
" TODO: Rework to remove the surrounding quotes, which I don't think are necessary...
" TODO: Just discovered that I can send commands to a detached screen (tested with -S and -dRR); however, the
" scrollback buffer will contain only whatever's new - but this is all we need...
" Example: This one shows how you can get CR into a stuffed stream (necessary when doing a forward or backward search in
" scrollback buffer).
" screen -S racket -p 0 -X eval 'readbuf /tmp/in.txt' 'paste .' copy 'stuff "g /bar\015Y"' 'writebuf /tmp/out.txt'
" Explanation:
" -works even when screen is detached, BUT the scrollback buffer will have access only to the stuff added by the
"  readbuf: it won't be able to see anything added to the buffer before it was detached.
"  Question: Some sort of race-condition? (I have access to all of it if I just reattach normally.)
"  Note: -dRR makes no difference here; still can't access old scrollback.
" -The -p 0 is supposed to be needed to select the 1st window, but it works without it.
"

" TODO: To support both screen and tmux, refactor so that send_<...> does just the send; have selection text obtained
" elsewhere...
" TODO: For screen at least, no need to break up the send and recv.
" TODO: Question: Is a delay required? Probably so... Actually, if we need to get feedback from the buffer, might want
" to keep them separate to facilitate looping for expected response...
" Note: Used the following to demonstrate the timing issue...
"(define max 200000)
"(for ([i max])
"     (if (>= i (- max 10)) (display i) #f))
" screen -S racket -p 0 -X eval 'readbuf /tmp/in.txt' 'paste .' copy 'stuff "g GY"' 'writebuf /tmp/out.txt'
" Explanation: The copy doesn't get the output of the (for ...) because it happens too soon.
" Solution: Evaluate a marker string (perhaps same one used in comment at beginning) after all expressions sent to the
" repl. This top-level string should not appear in the buffer until the user's expression has been evaluated. We can
" just keep checking the buffer (with separate screen calls) until we have it or time out (user configurable parameter).
" Killing Session: screen -X -S [session # you want to kill] quit
" Explanation: the kill command works only for current window, quit is for entire session.
" Starting Detached: -d -m option combination starts screen in detached mode. Verified that it works from Windows GVim.
" This is great: makes it easy for me to start from the plugin; in fact, I can use screen -list to see whether a session
" exists already, and start detached if not...
let g:exch_dir = 'C:/Users/stahlmanb/tmp'
let g:pathconv = ['C:/', '/cygdrive/c/']
let g:sess_name = 'screpl'
" TODO: Put in hierarchical config.
let g:sess_cmd = '/cygdrive/c/Apps/Racket/Racket'
let g:win_name = 'screpl'


vmap <buffer> <C-CR> :<C-W>call <SID>cmd_send_selection()<CR>

com! -nargs=1 ScreplStart call s:cmd_start_session(<f-args>)

fu! s:start_session_maybe(sess_name)
	" Use quiet and list options to check existence of appropriately-named session.
	let info = system('screen -q -ls ' . a:sess_name)
	if v:shell_error > 10
		" We've got one that can be reattached.
		return
	else if v:shell_error == 10
		echo "Warning: Found at least 1 " . a:sess_name . " session, but can't attach."
	endif
	" Create the session detached.
	call system('screen -d -m -S ' . a:sess_name . ' ' . g:sess_cmd) 
endfu

fu! s:cmd_start_session(sess_name)
	" TODO: Test for failure (e.g., no screen program)?
	call s:ensure_session(a:sess_name)

	let b:screpl_sess_name = a:sess_name
	let b:screpl_rsp_bufname = 'screpl-' . a:sess_name
	try
		exe 'below new ' . b:screpl_rsp_bufname
		set bt=nofile
	finally
		wincmd p
	endtry
	let b:screpl_rsp_bufnr = bufnr('#')
	let b:screpl_exch_dir = exists('g:exch_dir') ? g:exch_dir : fnamemodify(tempname(), ':h')
	let basename = 'screpl-' . b:screpl_sess_name
	let b:screpl_exch_file = b:screpl_exch_dir . '/' . basename
	" TODO: Do we need shellslash normalization?
	if !empty(g:pathconv)
		" TODO: Consider subst flags...
		let b:screpl_exch_file_ext = substitute(b:screpl_exch_file, g:pathconv[0], g:pathconv[1], 'g')
	endif
endfu

fu! s:cmd_send_selection()
	norm `<"zyv`>
	" TODO: Should this stuff be here or in send_selection? (Keep in mind I may eventually support tmux as well).
	let mkr = ';;; CMD-' . reltimestr(reltime())
	let lines = split(@z, '\n')
	call insert(lines, mkr)
	call add(lines, '"CMD-' . reltimestr(reltime()) . '"')
	" send_and_get won't return till the marker string is evaluated (to itself).
	let resp = s:send_and_get(lines)

	try
		let winnr = winnr()
		exe bufwinnr(b:screpl_rsp_bufnr) . 'wincmd w'
		call append(line('$'), resp)
		norm G
	finally
		exe winnr . 'wincmd w'
	endtry
endfu

fu! s:send_and_get(lines)
	let end_mkr = a:lines[-1]
	call writefile(a:lines, b:screpl_exch_file)
	" TODO: Escaping for filenames?
	" Template: screen -S sess_name -p 0 -X eval 'readbuf path_ext' 'paste .'
	call system('screen -S ' . shellescape(b:screpl_sess_name)
		\. ' -X eval ' . shellescape('readbuf ' . b:screpl_exch_file_ext)
		\. ' ' . shellescape(' paste .'))
	let resp = s:recv_response(end_mkr)
endfu

fu! s:recv_response(end_mkr)
	let start = reltime()
	while 1
		" Copy from beginning of start marker to eol before end marker.
		" Template: screen -S sess_name -X eval copy 'stuff "?{marker-string}\015 Gk$ "' 'writebuf path_ext'
		" Note: The octal literal \015 must be parsed by screen, not Vim.
		call system('screen -S ' . shellescape(b:screpl_sess_name)
			\. " -X eval copy " . shellescape("stuff '?" . a:mkr ."\\015 Gk$ '")
			\. ' ' . shellescape('writebuf ' . b:screpl_exch_file_ext))
		" TODO: Need to loop until we detect end marker (which will be a bare string marker expression, safe for any
		" lisp/scheme)...
		let lines = readfile(b:screpl_exch_file)
		" Look for 2 occurrences of marker string at end
		" Question: Can we constrain to final 2 lines? I think that should be safe...
		if lines[-1] =~ a:end_mkr && lines[-1] =~ a:end_mkr
			" Got it!
			break
		endif
		" UNDER CONSTRUCTION...
		" Convert to millisecond value
		let delta = substitute(substitute(reltimestr(reltime(start)), '\.\(\d\{3}\).*', '\1', ''), '\s\+', '', 'g') + 0
		if delta > g:resp_timeout
			throw "Timed out awaiting response from REPL."
		endif
	endwhile
	" Replace the final 2 lines (the marker string and its self-evaluation) with a single blank line.
	" Vim Idiosyncrasy: No list splice, and range modification doesn't permit src/tgt with different lengths.
	call remove(lines, -2, -1)
	call add(lines, '') " add trailing blank line
	return lines
endfu

" vim:ts=4:sw=4:tw=120
