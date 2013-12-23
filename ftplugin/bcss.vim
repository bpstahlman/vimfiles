" This file contains Vim mappings for use with the bccs (Brett Campbell
" Stories Script) filetype

" Set options
set tw=78 sw=8 ts=8
set wrap linebreak
" Movement maps
nmap <LocalLeader>g :call <SID>goto_sc(v:count)<CR>

" Begin a new scene below cursor position
nmap <LocalLeader>ns :call <SID>new_sc()<CR>

" Begin recording a part in the appropriate subfolder
vmap <silent> <F8> :<C-U>call <SID>record_part()<CR>

" Jump to specified scene
fu! s:goto_sc(count)
	exe 'norm /^--sc\s*'.a:count.'\s*$/'."\<CR>"
endfu

fu! s:new_sc()
	let s:sc_template = "--sc ?\n"
		\ ."-synopsis-\n"
		\ ."-description-\n"
		\ ."-todo-\n"
		\ ."-body-\n"
	" Put the new scene text below via expression register
	exe 'norm "=s:sc_template' . "\<CR>p"
	" Renumber all scenes
	call s:renu_sc()
endfu

" Renumber all scenes in the file.
fu! s:renu_sc()
	" Remember starting location
	let l:line = line('.')
	let l:col = col('.')
	" Start at the beginning.
	norm gg
	" Keep up with scene #
	let l:scnum = 1
	let l:re_sc = '^\(--sc\s*\)\%(\d\+\|?\)\(\s*\)$'
	while search(l:re_sc, 'W')
		exe ':s/'.l:re_sc.'/\1'.l:scnum.'\2/'
		let l:scnum = l:scnum + 1
	endw
	" No more scenes. Go back to starting location.
	call cursor(l:line, l:col)
endfu

" Begin recording a line with Windows Recorder
" Exe: sndrec32.exe - should be in PATH
fu! s:record_part()
	" Backup z register
	let z_reg_save = @z
	" Position at start of visual selection
	call cursor(line("'<"), col("'<"))
	" Find the beginning of the part
	let curline = ''
	let linenr = line('.')
	while curline !~ '^\-'
		let curline = getline(linenr)
		if curline =~ '^\i\+:'
			" Save for later validation
			let part_startline = linenr
			break
		endif
		let linenr = linenr - 1
	endwhile
	if (!exists('l:part_startline'))
		echoerr "Can't find beginning of part"
		return
	endif
	" Make sure visual selection doesn't cross into another part
	" Check from line after the part start to the end of the selected area
	let linenr = linenr + 1
	while linenr <= line("'>")
		if getline(linenr) =~ '^\(\i\+\):'
			echoerr "Visual selection crosses a part boundary"
			return
		endif
		let linenr = linenr + 1
	endwhile
	" Whose line is it?
	norm 0
	let @z = substitute(getline(part_startline), '^\(\i\+\):.*', '\1', '')
	let speaker = @z
	" Determine char offset within part
	let char_off = line2byte(line("'<")) + col("'<") - 1
		\ - line2byte(part_startline)
	" Left pad with zeroes to get 4 digit number
	let char_off = repeat('0', 4 - strlen(char_off)) . char_off
	" Position at start of visual selection
	call cursor(line("'<"), col("'<"))
	" Yank to end of visual selection	
	silent exe 'norm "zy/\%'.line("'>").'l\%'.col("'>")."c/\<CR>"
	let part = @z
	" Strip out scene/sfx directives
	let part = substitute(part, '\[\[[^]]*\]\]', '', 'g')
	let part = substitute(part, '{{[^}]*}}', '', 'g')
	" Collapse multiple spaces (including newline) to one
	let part = substitute(part, '\_s\+', ' ', 'g')
	" Get rid of leading and trailing space
	let part = substitute(part, '^\s\+', '', 'g')
	let part = substitute(part, '\s\+$', '', 'g')
	" Replace illegal fname chars with _
	" Note: Currently, I'm allowing single quotes and spaces, though
	" they're not technically legal, since Andrew uses them and Windows XP
	" supports it. HOWEVER, note that this means the file path must be
	" wrapped in backslash-escaped double quotes for passage to system().
	let part = substitute(part, "\\%(\\f\\|[ ']\\)\\@!.", '_', 'g')
	" Make sure filename is not too long.
	if strlen(part) > 32
		let part = strpart(part, 0, 32)."___"
	endif
	" Determine the current scene
	let sc_linenr = search('^\-\-sc \([1-9]\d*\)\s*$', 'b')
	let sc_nr = substitute(getline(sc_linenr), '^\-\-sc \([1-9]\d*\)\s*$', '\1', '')
	" Count # of parts up to selected one
	let part_off = 0
	let linenr = line('.')
	while linenr <= line("'<")
		if getline(linenr) =~ '^\i\+:'
			let part_off = part_off + 1
		endif
		let linenr = linenr + 1
	endwhile
	" Left pad with zeroes to get 3 digit number
	let part_off = repeat('0', 3 - strlen(part_off)) . part_off
	" Build dir and file names
	let sc_dir = 'sc'.sc_nr
	"echo "Recording curline: ".part." in scene ".sc_nr
	" Save wav file name with relative path enclosed in double quotes (to
	" permit single quotes in filename)
	let wav_file = '\"'.sc_dir.'/'.part_off.char_off.'_'.speaker.'_'.part.'.wav\"'
	" Use mkdir with -p option to prevent error if dir exists
	call system('mkdir -p '.sc_dir."|cp tmpl.wav ".wav_file)
	call system('sndrec32.exe '.wav_file.' &')
	" Move to the end of the range just recorded
	call cursor(line("'>"), col("'>"))
	" Turn off visual selection
	call visualmode(1)

	" Restore z register
	let @z = z_reg_save


endfu

" Begin recording a line with Windows Recorder
" Exe: sndrec32.exe - should be in PATH
fu! s:old_record_part()
	let linenr = line('.')
	let curline = ''
	while curline !~ '^\-'
		let curline = getline(linenr)
		if curline =~ '^\i\+:'
			let line2rec = curline
			break
		endif
		let linenr = linenr - 1
	endwhile
	if (!exists('l:line2rec'))
		echoerr "Can't find curline to record"
		return
	endif
	" Position at start of dialog
	call cursor(linenr, 1)
	norm 0W
	" Copy to end of part (first line whose first col is not empty or end
	" of file)
	exe 'norm "zy/^\S\|\%$/'."\<CR>"
	" Strip out scene/sfx directives
	let part = @z
	let part = substitute(part, '\[\[[^]]*\]\]', '', 'g')
	let part = substitute(part, '{{[^}]*}}', '', 'g')
	" Collapse multiple spaces (including newline) to one
	let part = substitute(part, '\_s\+', ' ', 'g')
	" Get rid of leading and trailing space
	let part = substitute(part, '^\s\+', '', 'g')
	let part = substitute(part, '\s\+$', '', 'g')
	" Replace illegal fname chars with _
	" Note: Currently, I'm allowing single quotes and spaces, though
	" they're not technically legal, since Andrew uses them and Windows XP
	" supports it. HOWEVER, note that this means the file path must be
	" wrapped in backslash-escaped double quotes for passage to system().
	let part = substitute(part, "\\%(\\f\\|[ ']\\)\\@!.", '_', 'g')
	" Make sure filename is not too long.
	if strlen(part) > 32
		let part = strpart(part, 0, 32)."___"
	endif
	" Determine the current scene
	let sc_linenr = search('^\-\-sc \([1-9]\d*\)\s*$', 'bn')
	let sc_nr = substitute(getline(sc_linenr), '^\-\-sc \([1-9]\d*\)\s*$', '\1', '')
	" Use mkdir with -p option to prevent error if dir exists
	let sc_dir = 'sc'.sc_nr
	"echo "Recording curline: ".part." in scene ".sc_nr
	" Save wav file name with relative path enclosed in single quotes
	let wav_file = '\"'.sc_dir.'/'.part.'.wav\"'
	call system('mkdir -p '.sc_dir."|cp tmpl.wav ".wav_file)
	call system('sndrec32.exe '.wav_file)



endfu
" Calculate indent
fu! GetBcssIndent()
	let prevline = getline(v:lnum - 1)
	if prevline =~ '^[a-zA-Z]\+:'
		return &ts
	else
		" Autoindent
		return -1
	endif
endfu
" Cause indent to be calculated by the function above
set indentexpr=GetBcssIndent()

