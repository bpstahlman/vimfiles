" *** Story ***
syn region BcssStorySyn fold matchgroup=BcssStorySynHdr
	\ start='^\-\-Synopsis\-\-\s*$'
	\ matchgroup=NONE end='^--sc\s*\d\+\s*$'me=s-1

" *** Scene ***
" IMPORTANT NOTE: !!!VIM SYNTAX HIGHLIGHTING BUG!!!
" Currently (As of Vim 7 beta), a rs= cannot use an offset that points into
" the line following the line containing the start= match. Bram has
" acknowledged that this is undesirable behaviour ('Weird' to be exact), and
" will probably fix it. For now, just work around it...
" Note: May not need keepend because of the me=s-1
syn region BcssSc fold transparent matchgroup=BcssScHdr
	\ start='^--sc\s*\d\+\s*$'
	\ matchgroup=NONE end='\%$'me=s-1 end='^--sc\s*\d\+\s*$'me=s-1
	\ keepend

" *** Synopsis ***
syn region BcssSyn fold matchgroup=BcssSynHdr
	\ start='^-synopsis-\s*$'
	\ matchgroup=NONE end='^-description\-\s*$'me=s-1
	\ contained containedin=BcssSc
" *** Description ***
syn region BcssDesc fold matchgroup=BcssDescHdr
	\ start='^-description-\s*$'
	\ matchgroup=NONE end='^-todo\-\s*$'me=s-1
	\ contained containedin=BcssSc
" *** Todo ***
syn region BcssTodo fold matchgroup=BcssTodoHdr
	\ start='^-todo-\s*$'
	\ matchgroup=NONE end='^-body\-\s*$'me=s-1
	\ contained containedin=BcssSc
" *** Body ***
syn region BcssBody fold matchgroup=BcssBodyHdr
	\ start='^-body-\s*$'
	\ end='\%$'me=s-1 end='^--sc\s*\d\+\s*$'me=s-1
	\ contained containedin=BcssSc

" *** Syntax items contained within Body ***
syn match BcssSpeakerInitials /^[a-zA-Z]\+:/ contained containedin=BcssBody

" *** Scene / Effect Cues ***
syn cluster BcssTextRegions contains=BcssStorySyn,BcssSc,BcssSyn,BcssDesc,BcssTodo,BcssBody
syn region BcssSceneCue start=/\[\[/ end=/\]\]/
	\ contained containedin=@BcssTextRegions
syn region BcssEffectCue start=/{{/ end=/}}/
	\ contained containedin=@BcssTextRegions



" *** Define Highlighting ***
hi BcssStorySyn gui=none guifg=blue
hi BcssStorySynHdr gui=bold guifg=DarkGreen
hi BcssScHdr gui=bold,underline guifg=Purple
hi BcssSynHdr gui=italic guifg=Blue
hi BcssDescHdr gui=none guifg=Brown
hi link BcssTodoHdr Todo
hi BcssBodyHdr gui=bold guifg=DarkGreen

hi BcssSpeakerInitials gui=underline guifg=DarkGreen
hi BcssSceneCue guifg=blue
hi BcssEffectCue guifg=blue

" How to sync
syntax sync fromstart

" Set options
set foldmethod=syntax
