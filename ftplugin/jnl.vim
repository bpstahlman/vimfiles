" Jnl: Set of Vim plugins (syntax and ftplugin) for implementing a file-based
" journaling system.
" File: This is the jnl ftplugin file, which contains mappings and functions
" for creating and editing journal entries.
" Creation:	2008 Feb 27
" Last Change: 2008 Feb 27
" Maintainer:	Brett Pershing Stahlman <brettstahlman@comcast.net>
" License:	This file is placed in the public domain.
" Options <<<
" Certain options have a way of getting changed while editing...
setlocal formatoptions=tcq
" Note: autoindent is currently used for 'o' command, but may not be needed
" long-term (TODO)
setlocal autoindent
" >>>
" Regular expressions <<<
let s:re_mkr1      = '\%(^>> \t  \t\t \t$\)'
let s:re_mkr2      = '\%(^->$\)'
let s:re_key_pre_t     = '^title:'
let s:re_key_pre_c     = '^ctime:'
let s:re_key_pre_m     = '^mtime:'
let s:re_key_pre_s     = '^stags:'
let s:re_key_pre_l     = '^chlog:'
let s:re_key_post      = '\s*\%(\n\t\s*\)*\zs\S\?'
let s:re_key_pre_any   = s:re_key_pre_t.'\|'.s:re_key_pre_c.'\|'.s:re_key_pre_m.'\|'.s:re_key_pre_s.'\|'.s:re_key_pre_l

" Define patterns that match an entire entity for the purposes of determining
" our location for formatting/indenting.
" key
" Strict format
let s:re_lip_key = '\%('.s:re_key_pre_any.'\)'
	\.'\%(.\|\n\t\)\{-}\ze\n\?'
	\.'\%('.s:re_key_pre_any.'\|'.s:re_mkr2.'\|'.s:re_mkr1.'\|\%$\)'
" Loose format
" Note: Assume that the required Tab at the beginning of each subsequent line
" could have been lost; i.e., key is terminated only by one of the following:
" -another key
" -end of header
" -end of entry
" -end of file
let s:re_lip_key_loose = '\%('.s:re_key_pre_any.'\)'
	\.'\_.\{-}\ze\n\?'
	\.'\%('.s:re_key_pre_any.'\|'.s:re_mkr2.'\|'.s:re_mkr1.'\|\%$\)'

let s:re_comment_beg   = '\%(\/<\*\)'
let s:re_comment_mid   = '\%(-+-\)'
let s:re_comment_end   = '\%(\*>\/\)'
let s:re_lip_comment   =
	\'^\s*'.s:re_comment_beg
	\.'\%(.\|\n\%(\s*\%('.s:re_comment_mid.'\|'
	\.s:re_comment_end.'\s*$\)\)\@=\)\{-}'
	\.s:re_comment_end.'\s*$'
" Appendix marker is a specialization of the generic comment
let s:re_lip_apdxmkr_pre  =
	\'^'.s:re_comment_beg
	\.'\s*Appendix\s\+'
" TODO - Decide whether 'mid' token should be required to appear at the
" beginning of the line.
let s:re_lip_apdxmkr_post  =
	\'\>\%(.\|\n\%(\s*\%('.s:re_comment_mid.'\|'
	\.s:re_comment_end.'\s*$\)\)\@=\)\{-}'
	\.s:re_comment_end.'\s*$'
let s:re_lip_apdxmkr  =
	\s:re_lip_apdxmkr_pre.'[\a-zA-Z]\+'.s:re_lip_apdxmkr_post
" Define regex matching even an incorrectly formatted comment (i.e., no '-+-'
" on subsequent lines or something other than whitespace after the closing
" '*>/').
let s:re_lip_comment_loose  =
	\'^\s*'.s:re_comment_beg.'\_.\{-}'.s:re_comment_end
" Define loose appendix marker
let s:re_lip_apdxmkr_loose_pre = '^'.s:re_comment_beg.'\s*Appendix\s\+'
let s:re_lip_apdxmkr_loose_post = '\>\_.\{-}'.s:re_comment_end
let s:re_lip_apdxmkr_loose = s:re_lip_apdxmkr_loose_pre.'[a-zA-Z]\+'.s:re_lip_apdxmkr_loose_post

" Define pattern matching the bullet or number of a bulletted or numbered
" item.
" Note: The pattern will not consume any leading or trailing whitespace,
" though a constraint may be placed upon trailing whitespace (e.g., that it
" must exist).
" Also note: Pattern contains no capturing parens...
let s:re_bul_or_num = '\%(--\@!\|[1-9][0-9]*[.)]\%(\s\|$\)\@=\)'
" >>>
" Exact string matches <<<
let s:mkr1    = ">> \t  \t\t \t"
let s:mkr2    = '->'
let s:comment_beg   = '/<*'
let s:comment_mid   = '-+-'
let s:comment_end   = '*>/'
" >>>
" Internal (helper) functions <<<
" Function: s:Goto_this_body() <<<
" Description: Helper function for s:Jnl_bracket_jump. Moves to start of body,
" assuming cursor is positioned at start of entry.
" Note: This function may be called even for entries with null bodies, in
" which case, it will position cursor on the last line of the entry.
" Important Note: This concept of 'body' is different from the one employed by
" s:Jnl_goto, which does not consider a null body to be a valid jump target.
fu! s:Goto_this_body()
	" Determine last line on which we should look for mkr2
	let end_l = search(s:re_mkr1, 'Wn')
	if end_l == 0
		" Last line of file is last line of entry
		let end_l = line('$')
	else
		" One line prior to start of next entry
		let end_l = end_l - 1
	endif
	" Jump to mkr2, taking care not to look outside this entry
	let m2_l = search(s:re_mkr2, 'W', end_l)
	if m2_l == 0
		" No mkr2 within this entry - go to last line of entry
		call cursor(end_l, 1)
	endif
endfu
" >>>
" Function: s:Is_line_in_pattern() <<<
fu! s:Is_line_in_pattern(line, patt, range)
	" Start at beginning of input line
	call cursor(a:line, 1)
	" Look backwards for the input pattern within range
	" Note: Due to the way search works with 'b' modifier, we first need to
	" look forward to be sure we're not exactly on it.
	let l1 = search(a:patt, 'Wnc', a:line)
	if l1 == 0
		" We're not exactly on it so look backwards
		let l1 = search(a:patt, 'Wb', a:range[0])
		if l1 == 0
			" Not within the pattern
			return [0, 0]
		endif
	endif
	" If here, we *could be* within the pattern. Locate the pattern end to
	" determine whether we are...
	" Note: We're currently positioned on pattern start. The following search
	" should always succeed.
	let l2 = search(a:patt, 'Wnce', a:range[1])
	if a:line <= l2
		return [l1, l2]
	else
		" Not within the pattern
		return [0, 0]
	endif
endfu
" >>>
" Function: s:Get_loc_in_entry() <<<
" Important Note: This function is working but currently unused
" Return: A Dictionary with the following keys:
" lvl1
" 	name:
" 		'h' = header
" 		'b' = body
" 		'a' = appendix
" 		''  = not within entry
"	range[0..1] - Starting and ending line of lvl1 construct
" lvl2
" 	name:
" 		'm1'  = mkr1
" 		'm2'  = mkr2
" 		'ams' = appendix marker strict
" 		'aml' = appendix marker loose
" 		'ks'  = key strict
" 		'kl'  = key loose
" 		'cs'  = comment strict
" 		'cl'  = comment loose
" 		''    = not within special region
" 	range[0..1] - Starting and ending line of lvl2 construct
fu! Get_loc_in_entry(line)
	" Define the default return Dictionary, which will be changed below if
	" we're within an entry
	let dict = { 'lvl1': { 'name': '', 'range': [0, 0] }, 'lvl2': { 'name': '', 'range': [0, 0] }  }
	" Cache text of current line
	let linetext = getline(a:line)
	" Move to mkr1 line
	if linetext !~ s:re_mkr1
		" Attempt to find beginning of current entry (if we're within one)
		let m1_l = search(s:re_mkr1, 'bW')
		if m1_l == 0
			" We're not within an entry
			return dict
		endif
	endif
	" We're on mkr1 of an entry
	" Determine last line on which we should look for anything within this
	" entry.
	let end_l = search(s:re_mkr1, 'Wn')
	if end_l == 0
		" Last line of file is last line of entry
		let end_l = line('$')
	else
		" Back up one line to last line of this entry
		let end_l = end_l - 1
	endif
	" Locate the start of the body
	let m2_l = search(s:re_mkr2, 'W', end_l)
	if m2_l == 0
		" Entry has null body - we must be within header
		let dict.lvl1.name = 'h'
		let dict.lvl1.range[0] = m1_l
		let dict.lvl1.range[1] = end_l
	else
		" Entry has a body
		if a:line < m2_l
			" Input position is within header
			let dict.lvl1.name = 'h'
			let dict.lvl1.range[0] = m1_l
			let dict.lvl1.range[1] = m2_l - 1
		else
			" Input position not within header
			" Locate first (if any appendix)
			let apdx_l = search(s:re_lip_apdxmkr_loose, 'W', end_l)
			if apdx_l == 0
				" No appendices within this entry
				let dict.lvl1.name = 'b'
				let dict.lvl1.range[0] = m2_l
				let dict.lvl1.range[1] = end_l
			elseif a:line < apdx_l
				" Input line is prior to first appendix
				let dict.lvl1.name = 'b'
				let dict.lvl1.range[0] = m2_l
				let dict.lvl1.range[1] = apdx_l - 1
			else
				" Input line is after first appendix. Thus, we know that lvl1
				" will be 'a'
				let dict.lvl1.name = 'a'
				" Keep looking for the next appendix until we find the one
				" within which input position lies
				while dict.lvl1.range[1] == 0
					let prev_apdx_l = apdx_l
					let apdx_l = search(s:re_lip_apdxmkr_loose, 'W', end_l)
					if apdx_l == 0
						" No more appendices
						let dict.lvl1.range[0] = prev_apdx_l
						let dict.lvl1.range[1] = end_l
					elseif a:line < apdx_l
						let dict.lvl1.range[0] = prev_apdx_l
						let dict.lvl1.range[1] = apdx_l - 1
					endif
				endwhile
			endif
		endif
	endif
	" lvl1 dict has been completely filled now
	" Note: We won't get here if we're not within an entry
	if dict.lvl1.name == 'h'
		" We're within header
		if a:line == dict.lvl1.range[0]
			" We're on mkr1
			let dict.lvl2.name = 'm1'
			let dict.lvl2.range[0] = a:line
			let dict.lvl2.range[1] = a:line
			return dict
		endif
		" Are we within a (strictly formatted) key?
		let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_key,
			\ [dict.lvl1.range[0], dict.lvl1.range[1]])
		if [line1, line2] != [0, 0]
			" We're within a key
			let dict.lvl2.name = 'ks'
			let dict.lvl2.range[0] = line1
			let dict.lvl2.range[1] = line2
			return dict
		endif
		" What about a loosely formatted key?
		let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_key_loose,
			\ [dict.lvl1.range[0], dict.lvl1.range[1]])
		if [line1, line2] != [0, 0]
			" We're within a key
			let dict.lvl2.name = 'kl'
			let dict.lvl2.range[0] = line1
			let dict.lvl2.range[1] = line2
			return dict
		endif
	else
		" We're within body or appendix
		if dict.lvl1.name = 'b' && a:line == dict.lvl1.range[0]
			" We're on mkr2
			let dict.lvl2.name = 'm2'
			let dict.lvl2.range[0] = a:line
			let dict.lvl2.range[1] = a:line
			return dict
		endif
		if dict.lvl1.name = 'a'
			" Check for strict appendix marker
			let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_apdxmkr,
				\ [dict.lvl1.range[0], dict.lvl1.range[1]])
			if [line1, line2] != [0, 0]
				" We're within a strict appendix marker
				let dict.lvl2.name = 'ams'
				let dict.lvl2.range[0] = line1
				let dict.lvl2.range[1] = line2
				return dict
			endif
			" Check for loose appendix marker
			let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_apdxmkr_loose,
				\ [dict.lvl1.range[0], dict.lvl1.range[1]])
			if [line1, line2] != [0, 0]
				" We're within a strict appendix marker
				let dict.lvl2.name = 'aml'
				let dict.lvl2.range[0] = line1
				let dict.lvl2.range[1] = line2
				return dict
			endif
		endif
		" Check for strict comment
		let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_comment,
			\ [dict.lvl1.range[0], dict.lvl1.range[1]])
		if [line1, line2] != [0, 0]
			" We're within a strict comment
			let dict.lvl2.name = 'cs'
			let dict.lvl2.range[0] = line1
			let dict.lvl2.range[1] = line2
			return dict
		endif
		" Check for loose comment
		let [line1, line2] = s:Is_line_in_pattern(a:line, s:re_lip_comment_loose,
			\ [dict.lvl1.range[0], dict.lvl1.range[1]])
		if [line1, line2] != [0, 0]
			" We're within a strict comment
			let dict.lvl2.name = 'cl'
			let dict.lvl2.range[0] = line1
			let dict.lvl2.range[1] = line2
			return dict
		endif
	endif
	return dict
endfu
" >>>
" >>>
" Formatting <<<
nnoremap <buffer> o :call <SID>Format_open_new_line()<CR>
setlocal formatexpr=Jnl_format()

" Function: Jnl_format() <<<
" Called by: Vim, whenever a non-whitespace char is typed or formatting is
" performed.
" Note: Bram has acknowledged (bug) that the 'formatexpr' function is
" currently called when gw and variants are used, although documentation
" states it is not.
fu! Jnl_format()
	if mode() =~ '[iR]'
		" Call the internal helper function that will also be used for the 'o'
		" command map
		" TODO - Get rid of the List return, which is no longer necessary, now
		" that Format_linebreak does all formatting...
		let [vim_ret, break] = s:Format_linebreak(v:lnum, col('.'), virtcol('.'), v:char)
		return vim_ret
	else
		" Format range of lines
		call s:Format_internal()
		return 0
	endif
endfu
" >>>
" Function: Get_indent_str() <<<
" Description: Return string of whitespace comprising tabs followed by spaces
" required to get a leading indent of virtcol columns. Takes current 'tabstop'
" setting into account
fu! s:Get_indent_str(virtcol)
	let tabs = a:virtcol / &tabstop
	let spaces = a:virtcol % &tabstop
	let ret_str = ''
	let i = 0
	while i < tabs
		let ret_str = ret_str."\<Tab>"
		let i = i + 1
	endwhile
	let i = 0
	while i < spaces
		let ret_str = ret_str." "
		let i = i + 1
	endwhile
	return ret_str
endfu
" >>>
" Function: Get_indent_str_from_str() <<<
" Description: Return string of whitespace comprising tabs followed by spaces
" required to get a leading indent string containing the same number of
" virtual columns as the input string. Takes current 'tabstop' setting into
" account
" Assumptions: The input string contains no multi-byte chars
fu! s:Get_indent_str_from_str(str)
	" Loop over characters in input string, counting number of virtual columns
	" represented
	let ci = 0
	let len = strlen(a:str)
	let vcols = 0
	let ts_next = &tabstop
	while ci < len
		if a:str[ci] == "\<Tab>"
			let vcols = ts_next
		else
			let vcols = vcols + 1
		endif
		if vcols >= ts_next
			let ts_next = ts_next + &tabstop
		endif
		let ci = ci + 1
	endwhile
	" Return a string of tabs and spaces in fiducial form
	return s:Get_indent_str(vcols)
endfu
" >>>
" Function: s:Get_linebreak_info() <<<
fu! s:Get_linebreak_info(line, col, vcol)
	" Obtain text of line under consideration
	let linetext = getline(a:line)
	" Determine whether it's alright to break the line
	" A successful match will produce the following submatches:
	" \1 Initial whitespace
	" \2 Everything from just after initial whitespace to run of
	"    whitespace that permits break
	" \3 Everything from just after run of whitespace that permits
	"    break to cursor position
	" \4 Everything after the cursor position
	" Find breaking run of whitespace (if it exists)
	" Note: Can't break on run of whitespace at BOL
	" Note: There are some additional constraints:
	" -Don't break after the number or bullet of a list
	" -Don't break at whitespace directly after the s:re_comment_mid
	" -Don't break until after the 'Appendix X' of an appendix marker
	" TODO - Decide whether it's a appropriate to permit Appendix A not at
	" beginning of line...
	let re =
		\'\%#=1'
		\.'^\(\s*\)'
		\.'\('
			\.'\%('
				\.'\%('
					\.s:re_comment_beg.'\%(\s*Appendix\s\+[a-zA-Z]\+\)\?'
					\.'\|'.s:re_comment_mid.'\s*'
					\.'\|'.s:re_bul_or_num.'\s*'
				\.'\)'
				\.'\|\S'
			\.'\)\@>.\{-}'
		\.'\)'
		\.'\%<'.(a:vcol).'v'
		\.'\%('
			\.'\s\%(\s*\S*\%'.(&tw + 2).'v\)\@=\s*'
		\.'\|'
			\.'\%>'.(&tw + 1).'v\s\+'
		\.'\)'
		\.'\(.*\%'.(a:vcol).'v\)\?'
		\.'\(.*\)'
let g:dbg_re = re
	let matches = matchlist(linetext, re)
let g:matches = matches
	if len(matches) != 0
		" Save line break information
		let l:lnbrk = {}
		let l:lnbrk.linetext = matches[0]
		let l:lnbrk.init_ws = matches[1]
		let l:lnbrk.pre_ws_text = matches[2]
		let l:lnbrk.post_ws_text = matches[3]
		let l:lnbrk.post_cur_text = matches[4]
	else
		" Nowhere to break on this line
		let l:lnbrk = {}
	endif
	let g:lnbrk = l:lnbrk
	return l:lnbrk
endfu
" >>>
" Function: s:Get_linebreak_info_old2() <<<
" Note: This one was working, but is no longer needed, now that I have a
" the version that uses a single matchlist call working.
fu! s:Get_linebreak_info_old2(line, col, vcol)
	" Obtain text of line under consideration
	let linetext = getline(a:line)
	" Determine whether it's alright to break the line
	" A successful match will produce the following submatches:
	" \1 Initial whitespace
	" \2 Everything from just after initial whitespace to run of
	"    whitespace that permits break
	" \3 Everything from just after run of whitespace that permits
	"    break to cursor position
	" \4 Everything after the cursor position
	""""" Note: Bram has acknowledged bug that will prevent the
	""""" following pattern from working until fix is released.
	""""let matches = matchlist(linetext,
	""""	\'^\(\s*\)\(\S.\{-}\)'
	""""	\.'\%('
	""""	\.'\s\%(\s*\S*\%(\%'. (&tw + 2) .'c\|$\)\)\@=\s*'
	""""	\.'\|'
	""""	\.'\%(\%'. &tw .'c\S*\)\@<=\%(\s\+\)'
	""""	\.'\)\@>'
	""""	\.'\%(\(.*\)\%'.a:vcol.'c\)\?\(.*\)')
	""""if len(matches) != 0
	""""	" Save line break information
	""""	let l:lnbrk = {}
	""""	let l:lnbrk.all_text = matches[0]
	""""	let l:lnbrk.init_ws = matches[1]
	""""	let l:lnbrk.pre_ws_text = matches[2]
	""""	let l:lnbrk.post_ws_text = matches[3]
	""""	let l:lnbrk.post_cur_text = matches[4]
	""""endif
	" Find breaking run of whitespace (if it exists)
	" Search text only up to cursor position
	"let linetext = matchstr(linetext, '^.*\%'.a:vcol.'v')
	" First, look for whitespace that would permit break at or before
	" 'textwidth' column.
	" Note: Can't break on run of whitespace at BOL
	" Note: There are some additional constraints:
	" -Don't break after the number or bullet of a list
	" -Don't break at whitespace directly after the s:re_comment_mid
	" -Don't break until after the 'Appendix X' of an appendix marker
	let re_skip =
		\'^\s*\%('
		\.'\%('
			\.s:re_comment_beg.'\%(\s*Appendix\s\+[a-zA-Z]\+\)\?'
			\.'\|'.s:re_comment_mid.'\s*'
		\.'\)'
		\.'\|\S'
		\.'\)\@>'

	"\.'.*\%<'.(&tw + 2).'v\zs\s'
	let pos1 = match(linetext, re_skip
		\.'\%('
		\.'.\{-}\zs\s\s*\S*\%'.(&tw + 2).'v\s*'
		\.'\|'
		\.'.\{-}\%>'.(&tw + 1).'v\zs\s'
		\.'\)'
		\.'\%<'.(a:vcol + 1).'v')
	let g:pos1 = pos1
	if pos1 == -1
		" Nowhere to break on this line
		return {}
	endif
	" If here, the line will be broken, either by us or by Vim
	" Find the beginning of the 'post-whitespace' text
	let pos2 = matchend(linetext, '\s\+', pos1)
	let g:pos2 = pos2
	" Build the lnbrk Dict needed later
	let l:lnbrk = {}
	let l:lnbrk.linetext = linetext
	let l:lnbrk.pre_ws_text = strpart(linetext, 0, pos1)
	" TODO - Change col('.') to a parameter
	if a:col > pos2
		let l:lnbrk.post_ws_text = strpart(linetext, pos2, a:col - pos2)
		" Obtain any text after cursor pos
		"let l:lnbrk.post_cur_text = matchstr(linetext, '\%'.a:vcol.'v\s*\zs.*')
		let l:lnbrk.post_cur_text = strpart(linetext, a:col)
	else
		let l:lnbrk.post_ws_text = ''
		" Obtain any text after cursor pos
		let l:lnbrk.post_cur_text = strpart(linetext, pos2)
	endif
	let g:lnbrk = l:lnbrk
	return l:lnbrk
endfu
" >>>
" Function: s:Can_join_lines() <<<
fu! s:Can_join_lines(lnum)
	" Test the current line first
	let curline = getline(a:lnum)
	if curline =~ '^\s*$'
		" Don't append anything to a blank line
		return 0
	endif
	if curline =~ s:re_comment_end.'\s*$'
		" Don't append anything to the end of a comment or appendix marker
		return 0
	endif
	if curline =~ s:re_mkr1 || curline =~ s:re_mkr2
		" Don't append anything to the end of a comment or appendix marker
		return 0
	endif
	" Now test next line
	" TODO - Don't pull special colon generic construct up...
	let nextline = getline(a:lnum + 1)
	if nextline =~ '^\s*$'
		" Don't append a blank line to anything
		return 0
	endif
	if nextline =~ s:re_key_pre_any
		" Don't append beginning of a key to anything
		return 0
	endif
	if nextline =~ '^\s*'.s:re_comment_beg
		" Don't append beginning of a comment or appendix marker to anything
		return 0
	endif
	if nextline =~ s:re_mkr1 || nextline =~ s:re_mkr2
		" Don't append a mkr1 or mkr2 to anything
		return 0
	endif
	if nextline =~ '^\s*'.s:re_bul_or_num
		" Don't append a mkr1 or mkr2 to anything
		return 0
	endif
	if nextline =~ '^\s*'.s:re_comment_end
		" Don't append a comment end (appearing at the beginning of the line)
		" to anything.
		return 0
	endif
	" If here, must not have found reason not to join...
	return 1
endfu
" >>>
" Function: s:Join_lines() <<<
" Description: Join 2 lines, beginning at a:lnum
" Note: Works mostly like the Vim :join function; however, it is also aware of
" the special comment 'mid' marker, which needs to be removed from the head of
" the line being appended to the current one.
" Note: This function will not alter cursor position

" From the Vim help on :join et al.
" These commands, except "gJ", insert one space in place of the <EOL> unless
" there is trailing white space or the next line starts with a ')'.  These
" commands, except "gJ", delete any leading white space on the next line.  If
" the 'joinspaces' option is on, these commands insert two spaces after a '.',
" '!' or '?' (but if 'cpoptions' includes the 'j' flag, they insert two spaces
" only after a '.').
fu! s:Join_lines(lnum)
	let l1 = getline(a:lnum)
	let l2 = getline(a:lnum + 1)
	" Remove leading whitespace and/or comment mid string from next line
	let l2 = substitute(l2, '^\s*\%('.s:re_comment_mid.'\)\?\s*', '', '')
	if l1 !~ '\s\+$' && l2[0] != ')'
		let sp = ' '
	else
		let sp = ''
	endif
	" Re-build the current line
	call setline(a:lnum, l1.sp.l2)
	" Delete the next line (into black-hole register)
	exe (a:lnum + 1).'d _'
endfu
" >>>
" Function: s:Format_internal() <<<
fu! s:Format_internal()
	" Loop over the number of lines to be formatted
	" Note: The following variables may be adjusted within the loop
	let lnum = v:lnum
	let lnum_end = v:lnum + v:count - 1
	let g:dbglist = ''
	while lnum <= lnum_end
		" TODO - Decide once and for all about whether to adjust cursor
		" position and propagate throughout...
		call cursor(lnum, col('$'))
		let [unhandled, break_cnt] = s:Format_linebreak(lnum, col('$'), virtcol('$'), '')
		let g:dbglist = g:dbglist."\n".'lnum: '.lnum.' lnum_end: '.lnum_end.' X X='.unhandled.' '.linebreak
		if !break_cnt
			" Can we join this line to next and try again?
			if lnum < lnum_end && s:Can_join_lines(lnum)
				"exe lnum.','.(lnum + 1).'join'
				call s:Join_lines(lnum)
				let lnum_end = lnum_end - 1
			else
				" Advance to next line
				let lnum = lnum + 1
			endif
		else
			" Line was broken - make adjustments
			let lnum_end = lnum_end + break_cnt
			let lnum = lnum + break_cnt
		endif
	endwhile
endfu
" >>>
" Function: s:Get_next_line_leader() <<<
fu! s:Get_next_line_leader(linetext)
	" Handle comment and apdx_mkr
	" BUGFIX TODO - re_comment_end will currently match when the cursor is
	" before comment_end!
	let matches = matchlist(a:linetext,
		\'^\(\s*\)\%('.s:re_comment_beg.'\|'.s:re_comment_mid.'\)'
		\.'\%(.\{-}'.s:re_comment_end.'\)\@!')
	if len(matches) != 0
		" Note: Fiducialize leading whitespace representation.
		return s:Get_indent_str_from_str(matches[1]) . s:comment_mid . ' '
	endif
	" Handle bulleted ('-') and numbered ('1.' or '1)') lists
	" A successful match will produce the following submatches:
	" \1 initial whitespace
	" \2 '-' or number item (e.g. '1.' or '1)')
	" \3 optional whitespace following \2
	let matches = matchlist(a:linetext, '^\(\s*\)\('.s:re_bul_or_num.'\)\(\s*\)')
	if len(matches) != 0
		" Note: Just as Vim does when 'formatoptions' includes 'n' flag,
		" fiducialize the tab/space composition of an indent that extends to
		" the end of any whitespace immediately following the bullet or
		" number. (Originally, I was using tabs only up to the start of the
		" bullet or number, but my thinking has changed on this.)
		" Assumption: Call to Get_indent_str_from_str is multi-byte safe
		" because input is a sequence of whitespace and/or (ASCII)
		" number/bullet chars
		return s:Get_indent_str_from_str(matches[1] . matches[2] . matches[3])
	endif
	" Handle the 2nd line of a hdr key, which should be indented by 1 Tab
	if a:linetext =~ s:re_key_pre_any
		" Cause indent by 1 Tab char
		return "\<Tab>"
	endif
	" Nothing special about this line. Return leading whitespace if line
	" contains something other than whitespace; otherwise, return empty
	" string.
	" Assumption: Call to Get_indent_str_from_str is multi-byte safe because
	" input is a sequence of whitespace
	return s:Get_indent_str_from_str(matchstr(a:linetext, '^\s*\S\@='))
endfu
" >>>
" Function: s:Get_next_line() <<<
fu! s:Get_next_line(char, lnbrk)
	" Pass only the text up to cursor
	let leader = s:Get_next_line_leader(a:lnbrk.init_ws . a:lnbrk.pre_ws_text . a:lnbrk.post_ws_text)
	" Build the part before and after the cursor, taking into account any
	" special cases...
	" post_ws_text post_cur_text
	let nextline = {}
	let nextline.pre_cur_text = leader . a:lnbrk.post_ws_text
	let nextline.post_cur_text = a:lnbrk.post_cur_text
	" Handle any special cases...
	" Case: leader is s:re_comment_mid and post_ws_text begins with
	" s:re_comment_end. In such cases, it looks better to combine the mid and
	" end markers into a single end marker.
	" Note: End marker will never be after cursor pos when there is only
	" whitespace between mid and end.
	let nextline.pre_cur_text =
		\substitute(nextline.pre_cur_text,
		\'^\s*\zs'.s:re_comment_mid.'\s*'.s:re_comment_end,
		\s:comment_end, '')
	" Return the Dict
	return nextline
endfu
" >>>
" Function: s:Format_linebreak() <<<
" Vim's default line-breaking rules: Line is broken upon typing a
" non-whitespace char in a column past &tw, provided that there is a preceding
" whitespace character somewhere on the line, which is itself preceded by
" non-whitespace. If a run of whitespace that satisfies those criteria and
" would permit line to be broken at or before &tw column exists, it is used;
" otherwise, the earliest run of whitespace on the line is used. Once the
" 'breaking' run of whitespace has been selected, it is completely removed;
" moreover, the characters following the breaking whitespace are moved to the
" following line. Relative cursor position is maintained so that the character
" to be inserted (v:char) will be inserted at the proper location.
fu! s:Format_linebreak(lnum, col, vcol, char)
	" Perhaps change this...
	let lnum = a:lnum
	let col = a:col
	let vcol = a:vcol
	" Loop until until we've done all the breaking we can do...
	let break_cnt = 0
	while 1
		let l:lnbrk = {}
		" Note: a:vcol represents the column position that v:char would occupy
		" if the line were not broken. (Line-breaking is based upon virtual
		" column position.)
		if vcol > &tw
			" Find breaking run of whitespace (if it exists)
			let l:lnbrk = s:Get_linebreak_info(lnum, col, vcol)
		endif
		if !exists('l:lnbrk') || l:lnbrk == {}
			" Nothing else to do...
			return [0, break_cnt]
		endif
		" Set the current line
		" TODO - Decide whether to keep init_ws in lnbrk struct
		call setline(lnum, l:lnbrk.init_ws . l:lnbrk.pre_ws_text)
		" Touch next line only if something other than whitespace will go there.
		" Note: The test here includes any char about to be inserted by Vim. If
		" such a char exists, it is passed in a:char.)
		if a:char == '' && (l:lnbrk.post_ws_text == '' && l:lnbrk.post_cur_text == '')
			" Special case - Just clean up the trailing whitespace - nothing to
			" add on next line
			" TODO - Perhaps remove this, since cursor position is irrelevant
			call cursor(lnum, strlen(l:lnbrk.init_ws . l:lnbrk.pre_ws_text) + 1)
		else
			" Determine text for next line
			let nextline = s:Get_next_line(a:char, l:lnbrk)
			" Set the next line
			call append(lnum, nextline.pre_cur_text . nextline.post_cur_text)
			" Update the cursor position
			call cursor(lnum + 1, strlen(nextline.pre_cur_text) + 1)
			" Update count of breaks inserted
			let break_cnt = break_cnt + 1
		endif
		" TODO - Decide whether to do it this way...
		" Revisit this...
		let lnum = line('.')
		let col = col('.')
		let vcol = virtcol('.')
	endwhile
endfu
" >>>
" Function: s:Format_open_new_line() <<<
" Description: Implements the 'o' command
fu! s:Format_open_new_line()
	let leader = s:Get_next_line_leader(getline('.'))
	call append(line('.'), leader)
	call cursor(line('.') + 1, 1)
	startinsert!
endfu
" >>>
" >>>
" Mappings <<<
" Add or change an entry <<<
" Create a new journal entry <<<
nnoremap <buffer> <LocalLeader>ne :call <SID>Jnl_new_entry()<CR>
" >>>
" Create a new comment (below current line) <<<
nnoremap <buffer> <LocalLeader>nc :call <SID>Jnl_new_comment()<CR>
" >>>
" Update (modify) mtime <<<
nnoremap <buffer> <LocalLeader>mm :call <SID>Jnl_update_mtime()<CR>

"au! BufWritePre <buffer> call <SID>Jnl_update_mtime()
fu! Jnl_update_changelist()
	if !exists('s:changes_prev')
		let s:changes_prev = []
	endif
	redir => l:s
	silent changes
	redir END
	let l:changes_next = split(l:s, '\n')
	echo l:changes_next
endfu
" >>>
" >>>
" Move within or between entries <<<
" Bracket jumps <<<
" Normal mode
nnoremap <buffer> ]] :call <SID>Jnl_bracket_jump(']]')<CR>
nnoremap <buffer> ][ :call <SID>Jnl_bracket_jump('][')<CR>
nnoremap <buffer> [[ :call <SID>Jnl_bracket_jump('[[')<CR>
nnoremap <buffer> [] :call <SID>Jnl_bracket_jump('[]')<CR>
" Operator-pending mode
onoremap <buffer> ]] :call <SID>Jnl_bracket_jump(']]')<CR>
onoremap <buffer> ][ :call <SID>Jnl_bracket_jump('][')<CR>
onoremap <buffer> [[ :call <SID>Jnl_bracket_jump('[[')<CR>
onoremap <buffer> [] :call <SID>Jnl_bracket_jump('[]')<CR>
" Visual mode
vnoremap <buffer> ]] :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_bracket_jump(']]')<CR>
vnoremap <buffer> ][ :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_bracket_jump('][')<CR>
vnoremap <buffer> [[ :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_bracket_jump('[[')<CR>
vnoremap <buffer> [] :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_bracket_jump('[]')<CR>
" >>>
" Targeted goto's <<<
" Normal mode
nnoremap <buffer> <LocalLeader>gt :call <SID>Jnl_goto('t')<CR>
nnoremap <buffer> <LocalLeader>gc :call <SID>Jnl_goto('c')<CR>
nnoremap <buffer> <LocalLeader>gm :call <SID>Jnl_goto(')<CR>
nnoremap <buffer> <LocalLeader>gs :call <SID>Jnl_goto('s')<CR>
nnoremap <buffer> <LocalLeader>gb :call <SID>Jnl_goto('b')<CR>
nnoremap <buffer> <LocalLeader>gB :call <SID>Jnl_goto('B')<CR>
nnoremap <buffer> <LocalLeader>ga :call <SID>Jnl_goto('a')<CR>
nnoremap <buffer> <LocalLeader>gA :call <SID>Jnl_goto('A')<CR>
nnoremap <buffer> <LocalLeader>ge :call <SID>Jnl_goto('e')<CR>
" Operator-pending mode
onoremap <buffer> <LocalLeader>gt :call <SID>Jnl_goto('t')<CR>
onoremap <buffer> <LocalLeader>gc :call <SID>Jnl_goto('c')<CR>
onoremap <buffer> <LocalLeader>gm :call <SID>Jnl_goto('m')<CR>
onoremap <buffer> <LocalLeader>gs :call <SID>Jnl_goto('s')<CR>
onoremap <buffer> <LocalLeader>gb :call <SID>Jnl_goto('b')<CR>
onoremap <buffer> <LocalLeader>gB :call <SID>Jnl_goto('B')<CR>
onoremap <buffer> <LocalLeader>ga :call <SID>Jnl_goto('a')<CR>
onoremap <buffer> <LocalLeader>gA :call <SID>Jnl_goto('A')<CR>
onoremap <buffer> <LocalLeader>ge :call <SID>Jnl_goto('e')<CR>
" Visual mode
vnoremap <buffer> <LocalLeader>gt :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('t')<CR>
vnoremap <buffer> <LocalLeader>gc :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('c')<CR>
vnoremap <buffer> <LocalLeader>gm :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('m')<CR>
vnoremap <buffer> <LocalLeader>gs :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('s')<CR>
vnoremap <buffer> <LocalLeader>gb :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('b')<CR>
vnoremap <buffer> <LocalLeader>gB :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('B')<CR>
vnoremap <buffer> <LocalLeader>ga :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('a')<CR>
vnoremap <buffer> <LocalLeader>gA :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('A')<CR>
vnoremap <buffer> <LocalLeader>ge :<C-U>exe "normal! gv"<Bar>call <SID>Jnl_goto('e')<CR>
" >>>
" >>>
" >>>
" Mapping functions <<<
" Function: s:Jnl_update_mtime() <<<
" Description: Update the 'mtime' key with the current datetime
fu! s:Jnl_update_mtime()
	" Save current position
	let [savebuf, saveline, savecol, saveoff] = getpos('.')
	" Cache text of current line
	let linetext = getline('.')
	if linetext !~ s:re_mkr1
		" Attempt to find beginning of current entry (if we're within one)
		let m1_l = search(s:re_mkr1, 'bW')
		if m1_l == 0
			" We're not within an entry - no mtime to update
			" Return without changing cursor pos
			return
		endif
	endif
	" We're on mkr1 of an entry
	" Determine last line on which we should look for mtime key. Don't look
	" past header, and certainly don't look past end of entry.
	let end_l = search(s:re_mkr1.'\|'.s:re_mkr2, 'Wn')
	if end_l == 0
		" Last line of file is last line of entry
		let end_l = line('$')
	else
		" One line prior to start of body or next entry
		let end_l = end_l - 1
	endif
	" Jump to mtime key (if it exists), taking care not to look too far
	" Note: Intentionally not validating the entire header format
	let mtime_l = search(s:re_key_pre_m, 'W', end_l)
	if mtime_l == 0
		" No mtime key exists for this entry.
		" Restore position without updating anything.
		call cursor(saveline, savecol)
		return
	endif
	" If here, we're on what appears to be an mtime key. Replace the entire
	" line with a valid mtime key containing current time.
	call setline('.', 'mtime: '.strftime("%Y/%m/%d-%H:%M:%S"))
	" Restore the original cursor position.
	call cursor(saveline, savecol)
endfu
" >>>
" Function: s:Jnl_new_entry() <<<
" Description: Creates new journal entry
fu! s:Jnl_new_entry()
	normal! G
	let line = line('.')
	" Get ctime
	let time = strftime("%Y/%m/%d-%H:%M:%S")
	" Prompt user for a title
	let title = input("Enter a title: ")
	" Add the lines at the end of the buffer
	call append(line, s:mkr1)          | let line = line + 1
	call append(line, 'title: '.title) | let line = line + 1
	call cursor(line, 1)
	" Format the title
	" Note: The following will cause my 'formatexpr' to be invoked
	normal! gqgq
	" TODO - Get rid of the stuff below once I'm satisfied I don't need it...
	""""let line = line("']")
	""""" Determine number of lines into which title has been broken
	""""let num_title_lines = line("']") - line("'[") + 1
	""""if num_title_lines > 1
	""""	" Indent the 2nd line
	""""	exe "normal! jI\<Tab>"
	""""	" Reformat 2nd and subsequent lines with leading tab
	""""	" TODO - Perhaps use gq instead of gw now that I welcome the use of my
	""""	" own 'formatexpr'...
	""""	exe "normal! gw".(num_title_lines > 2 ? num_title_lines - 1 : '').'$'
	""""	let line = line("']")
	""""endif
	" Note: 'formatexpr' leaves us on last line formatted
	let line = line('.')
	call append(line, 'ctime: '.time) | let line = line + 1
	call append(line, 'mtime: '.time) | let line = line + 1
	call append(line, 'stags: ')      | let line = line + 1
	call append(line, s:mkr2)         | let line = line + 1
	call cursor(line, 1)
	" Open in the body, ready to insert
	normal! o
	startinsert
endfu
" >>>
" Function: s:Jnl_bracket_jump() <<<
fu! s:Jnl_bracket_jump(br_pair)
	" Save current position
	let [savebuf, saveline, savecol, saveoff] = getpos('.')
	" Cache text of current line
	let linetext = getline('.')
	if a:br_pair == ']]'
		" Fwd to next mkr1 (if it exists)
		let mkr1_l = search(s:re_mkr1, 'W')
		" Note: If there are no more mkr1's but we're within an entry, go to
		" end of file.
		" Rationale: ]] may be used to get to the end of the current entry's
		" body. We should be able to use it this way even if the current entry
		" is not followed by another.
		if mkr1_l == 0
			" Are we within an entry?
			let mkr1_l = search(s:re_mkr1, 'bWn')
			if mkr1_l != 0 || linetext =~ s:re_mkr1
				" We're within an entry. Move to last line of file.
				normal! G
			endif
		endif
	elseif a:br_pair == ']['
		" 2 cases:
		" 1) On mkr1 or inside hdr - Fwd to next mkr2 (or end of entry)
		" 2) On mkr2, inside body or not within entry at all - Fwd to next
		" mkr1, then fwd to next mkr2 (or end of entry)
		if linetext =~ s:re_mkr1
			let m1_l = line('.')
			" Location of previous mkr2 doesn't matter
			let m2_l = 0
		elseif linetext =~ s:re_mkr2
			let m2_l = line('.')
			" Location of previous mkr1 doesn't matter
			let m1_l = 0
		else
			let m1_l = search(s:re_mkr1, 'bWn')
			let m2_l = search(s:re_mkr2, 'bWn')
		endif
		if !(m1_l > m2_l)
			" On mkr2, inside body or not within an entry at all
			let m1_l = search(s:re_mkr1, 'W')
			if m1_l == 0
				" No more entries - return without changing cursor pos
				return
			endif
		endif
		call s:Goto_this_body()
	elseif a:br_pair == '[['
		" Back to prev mkr1 (if it exists)
		if linetext =~ s:re_mkr1
			" Note: We don't want a mkr1 line upon which we are currently
			" sitting.
			normal! 0
		endif
		let m1_l = search(s:re_mkr1, 'bW')
		if m1_l == 0
			" Jump failed. Since cursor position may have been changed above,
			" restore it now.
			call cursor(saveline, savecol)
		endif
	elseif a:br_pair == '[]'
		" 2 cases:
		" 1) On mkr1 line, inside hdr or on mkr2 line - Back to prev mkr1
		" (possibly at cursor pos), then back to prev mkr1 (not at cursor
		" pos), then fwd to next mkr2 (or end of entry)
		" Note: Restore cursor position and do nothing if there is no previous
		" entry.
		" 2) Inside body - Back to prev mkr1, then fwd to next mkr2
		if linetext =~ s:re_mkr1
			let m1_l = line('.')
			" Location of previous mkr2 doesn't matter
			let m2_l = 0
		elseif linetext =~ s:re_mkr2
			let m2_l = line('.')
			let m1_l = search(s:re_mkr1, 'bWn')
		else
			let m1_l = search(s:re_mkr1, 'bWn')
			let m2_l = search(s:re_mkr2, 'bWn')
		endif
		if m1_l == 0
			" Not within an entry
			" Note: We haven't yet altered cursor position
			return
		endif
		" Inside an entry
		" Jump back to already found mkr1, which marks the beginning of
		" the current entry
		call cursor(m1_l, 1)
		if m1_l > m2_l || m2_l == saveline
			" On mkr1 line, inside hdr or on mkr2 line
			" Back to mkr1 of prev entry (if it exists)
			let m1_l = search(s:re_mkr1, 'bW')
			if m1_l == 0
				" No previous entry!
				" Restore position
				call cursor(saveline, savecol)
				return
			endif
		endif
		" Fwd to next mkr2 (or end of entry)
		call s:Goto_this_body()
	endif
endfu
" >>>
" Function: s:Jnl_goto() <<<
" Jump to {target}, which should be one of the following letters:
" t - title
" c - ctime
" m - mtime
" s - stags
" b - body (beginning)
" B - body (end)
" a - beginning of appendix whose letter(s) is/are entered by user at the
"     prompt.
" A - end of appendix whose letter(s) is/are entered by user at the prompt.
" e - last line of entry
fu! s:Jnl_goto(target)
	" Note: No need to validate target, which is supplied from a mapping
	" Save current position
	let [savebuf, saveline, savecol, saveoff] = getpos('.')
	" Make sure jumplist is updated
	normal! m'
	" Cache text of current line
	let linetext = getline('.')
	" Move to mkr1 line
	if linetext !~ s:re_mkr1
		" Attempt to find beginning of current entry (if we're within one)
		let m1_l = search(s:re_mkr1, 'bW')
		if m1_l == 0
			" We're not within an entry
			" Return without changing cursor pos
			return
		endif
	endif
	" We're on mkr1 of an entry
	" Determine last line on which we should look for anything within this
	" entry.
	let end_l = search(s:re_mkr1, 'Wn')
	if end_l == 0
		" Last line of file is last line of entry
		let end_l = line('$')
	else
		" Back up one line to last line of this entry
		let end_l = end_l - 1
	endif
	if a:target == 'e'
		" Jump to the last line of the entry
		call cursor(end_l, 1)
	elseif a:target =~ '^[tcms]$'
		" Jump to requested key (if it exists), taking care not to look too
		" far
		" Note: Intentionally not validating the entire header format
		" Note: If possible, search pattern will put cursor on first
		" non-whitespace char within the key
		let key_l = search(s:re_key_pre_{a:target}.s:re_key_post, 'W', end_l)
		if key_l == 0
			" Specified key does not exist for this entry.
			" Restore position without updating anything.
			call cursor(saveline, savecol)
			return
		endif
	elseif a:target =~ '^[bBaA]$'
		" Jump to beginning of body (if it exists), taking care not to look
		" past end of entry
		let mkr2_l = search(s:re_mkr2, 'W', end_l)
		if mkr2_l == 0
			" This entry has no body; hence, goto target is invalid!
			call cursor(saveline, savecol)
			return
		endif
		" If here, entry has a body and we're positioned on its mkr2
		if a:target =~ '[bB]'
			" Determine where the first (if any) appendix lies within body
			" Note: The first appendix ends the body proper
			let apdx_l = search(s:re_lip_apdxmkr_loose, 'Wn', end_l)
			" Cache current line number
			let line = line('.')
			if line == apdx_l - 1 || line == end_l
				" There's no body to this body - remain on mkr2
				return
			endif
			" If here, body is not empty
			if a:target == 'b'
				" Move to beginning of first line of body
				call cursor(line + 1, 1)
			else " if a:target == 'B'
				" Move to beginning of last line of body
				if apdx_l == 0
					" Move to beginning of last line of entry
					call cursor(end_l, 1)
				else
					" Move to beginning of line before first appendix
					call cursor(apdx_l - 1, 1)
				endif
			endif
		else " if a:target =~ '[aA]'
			" Which appendix is desired?
			let apdx_ltr = input("Enter desired appendix letter(s): ")
			if apdx_ltr !~ '^[a-zA-Z]\+$'
				echoerr "Invalid appendix letter(s): `".apdx_ltr."'"
				call cursor(saveline, savecol)
				return
			endif
			" Attempt to jump to the end of the specified appendix' marker
			" (without moving past end of entry)
			let apdx_l =
				\ search(s:re_lip_apdxmkr_loose_pre.apdx_ltr.s:re_lip_apdxmkr_loose_post, 'We', end_l)
			if apdx_l == 0
				" Specified appendix doesn't exist!
				call cursor(saveline, savecol)
				return
			endif
			" Cache current line number
			let line = line('.')
			" Specified appendix exists and we're sitting on its marker.
			" Determine whether it's terminated by another appendix or by the
			" end of the entry
			let apdx_l = search(s:re_lip_apdxmkr_loose, 'Wn', end_l)
			if a:target == 'a'
				" Goto beginning of appendix
				if line == apdx_l - 1 || line == end_l
					" There's no body to this appendix - remain on its marker
					return
				else
					" Move to beginning of first line of appendix body
					call cursor(line + 1, 1)
				endif
			else " if a:target == 'A'
				" Goto end of appendix
				if line == apdx_l - 1 || line == end_l
					" There's no body to this appendix - remain on its marker
					return
				else
					" Move to beginning of last line of appendix body
					if apdx_l == 0
						" Move to beginning of last line of entry
						call cursor(end_l, 1)
					else
						" Move to beginning of line before next appendix
						call cursor(apdx_l - 1, 1)
					endif
				endif
			endif
		endif
	endif
endfu
" >>>
" Function: s:Jnl_new_comment() <<<
fu! s:Jnl_new_comment()
	let s = input('Enter comment or hit <Enter> to cancel: ')
	if s =~ '^\s*$'
		" Return without doing anything
		return
	endif
	" If here, we have a comment
	" Get indent from current line, converting tabs and spaces to fiducial
	" form
	let init_ws = s:Get_indent_str_from_str(matchstr(getline('.'), '^\s*'))
	" Append the comment line to the current one
	call append('.', init_ws . s:comment_beg .' ' . s . ' ' . s:comment_end)
	call cursor(line('.') + 1, 1)
	" Now format it
	let g:line_to_fmt = line('.') + 1
	let [vim_ret, break_cnt] = s:Format_linebreak(line('.'), col('$'), virtcol('$'), '')
	call cursor(line('.') + break_cnt, 1)
	let g:break_cnt = break_cnt
	" Now prepare to insert just below the newly-formatted comment
	" Note: Initial whitespace is still valid
	call append(line('.'), init_ws)
	call cursor(line('.') + 1, 1)
	startinsert!
endfu
" >>>
" >>>
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>>
