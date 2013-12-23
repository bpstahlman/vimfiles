" Jnl: Set of Vim plugins (syntax and ftplugin) for implementing a file-based
" journaling system.
" File: This is the jnl syntax file, which defines a syntax for automatic
" highlighting of journal entries.
" Creation:	2008 Feb 27
" Last Change: 2008 Feb 27
" Maintainer:	Brett Pershing Stahlman <brettstahlman@comcast.net>
" License:	This file is placed in the public domain.

" Regular expressions <<<
let s:re_mkr1     = '\%(^>> \t  \t\t \t$\)'
let s:re_mkr2     = '^->$'
let s:re_mkr      = s:re_mkr1.'\|'.s:re_mkr2
let s:re_key_beg  = '^[_[:alpha:]][-_[:alnum:]]*:'
let s:re_key_end  = '^\t\@!'
" TODO - Perform validation on the values
let s:re_year     = '\%(\d\d\d\d\)'
let s:re_month    = '\%(0[1-9]\|1[0-2]\)'
let s:re_day      = '\%(0[1-9]\|1[0-9]\|2[0-9]\|3[0-1]\)'
let s:re_hour     = '\%(0[1-9]\|1[0-9]\|2[0-3]\)'
let s:re_minsec   = '\%(0[0-9]\|1[0-9]\|2[0-9]\|3[0-9]\|4[0-9]\|5[0-9]\)'
let s:re_datetime = s:re_year.'/'.s:re_month.'/'.s:re_day.'-'.s:re_hour.':'.s:re_minsec.':'.s:re_minsec
" Special hdr keys
let s:re_title_key_beg = '^title:'
let s:re_time_key_beg  = '^[cm]time:'
let s:re_chlog_key_beg = '^chlog:'
" Special regions within body
" Dates of form <<< 26Feb2008 >>> within body and appendices
" Design decision: Permit (but don't require) spaces between the day, month
" and year fields. Require spaces between date and angle braces.
let s:re_monthname = '\%(Jan\|Feb\|Mar\|Apr\|May\|Jun\|Jul\|Aug\|Sep\|Oct\|Nov\|Dec\)'
let s:re_date_mkr = '^<<<\s\+'.s:re_day.'\s*'.s:re_monthname.'\s*'.s:re_year.'\s\+>>>\s*$'
" Special regions within body and appendices
" TODO - Perhaps create a less restrictive (i.e. not necessarily beginning of
" line) region for Note: and NOTE:
let s:re_colon_words_bol = '^\s*\%('
	\.'Note\|NOTE'
	\.'\|Also [nN]ote\|ALSO NOTE'
	\.'\|Important [nN]ote\|IMPORTANT NOTE'
	\.'\|Warning\|WARNING'
	\.'\|Caution\|CAUTION'
	\.'\|Caveat\|CAVEAT'
	\.'\|Error\|ERROR'
	\.'\|How[tT]o'
	\.'\|DONE\|Done'
	\.'\|Assumption'
	\.'\|Definition'
	\.'\|Description'
	\.'\|Discovery'
	\.'\|Pros\|Cons'
	\.'\|Advantages\?\|Disadvantages\?'
	\.'\|Issue'
	\.'\|Question'
	\.'\|Answer'
	\.'\|Recommendation'
	\.'\|Rationale'
	\.'\|Explanation'
	\.'\|Aside'
	\.'\|Root [cC]ause\%( [fF]ound\)\='
	\.'\|Background'
	\.'\|Example'
	\.'\|Problem'
	\.'\|Resolution'
	\.'\|Solution'
	\.'\|Bug'
	\.'\|Fix'
	\.'\|Bugfix'
	\.'\|Validation'
	\.'\|Testing'
	\.'\|Advantages\?'
	\.'\|Disadvantages\?'
	\.'\):'
" Generic colon construct
let s:re_colon_gen_mid_tok = '\%(-\)'
let s:re_colon_gen_end_tok = '\%(<\)'
let s:re_colon_gen = '^\s*\u\S\{-}:'.s:re_colon_gen_end_tok
" Define the components of an appendix marker
let s:re_comment_beg   = '\%(\/<\*\)'
let s:re_comment_mid   = '\%(-+-\)'
let s:re_comment_end   = '\%(\*>\/\)'
" >>>
" Exact strings <<<
" Todo and fixme
let s:todo_and_fixme_keywords = 'TODO Todo FIXME Fixme'
" Generic colon construct
let s:colon_gen_mid_tok = '-'
let s:colon_gen_end_tok = '<'
" >>>
" Folding <<<
" Make each journal entry a fold
" TODO - Decide whether to make bodies and/or appendices folds also
setlocal foldmethod=syntax
setlocal foldtext=JnlFoldText()
fu! JnlFoldText()
	" TODO - Clean this up!!!
	" IMPORTANT NOTE: Unlike the identexpr function, nasty side-effects occur
	" when the cursor position is moved from within foldexpr function!
	" Create some meaningful foldtext (i.e. the Vim default, with text from
	" first line removed)
	let l:foldtext = substitute(foldtext(), ':\s*\zs.*', '', '')
	if v:foldlevel == 1
		" Journal entry fold
		" Assumption: title line is first line within fold
		let l = v:foldstart + 1
		let titlestr = getline(l)
		if titlestr !~ '^title:'
			" Title key isn't in its proper place!
			return l:foldtext."<Untitled>"
		endif
		" Strip leading 'title: ' from first line
		let titlestr = substitute(titlestr, '^title:\s\+', '', '')
		" Concatenate any additional lines, stripping leading tabs
		let l = l + 1
		let line = getline(l)
		while line =~ '^\t'
			let titlestr = titlestr.' '
				\.substitute(getline(l), '^\t', '', '')
			let l = l + 1
			let line = getline(l)
		endwhile
		" Attempt to get the creation date also...
		" BUGFIX TODO: Don't use search() with no wrap - actually, don't use
		" search at all, since we don't know where cursor is and we can't move
		" cursor. Loop until end of entry, EOF, or end of header, looking for
		" the date...
		" Assumption: 'ctime:' entry is within header and has a valid date.
		" Design decision: I could validate both the existence and format of
		" the ctime entry, but in interest of efficiency, I won't...
		" Issue: search() doesn't work here, apparently because there's
		" something funny going on with cursor pos when this function is
		" called. Use the looping mechanism used above...
		" Note: At this point, l is pointing one line beyond end of title, and
		" line already holds the corresponding line text
		while 1
			" Don't go past end of header
			if line =~ s:re_mkr2
				let datestr = '<Unknown date>'
				break
			elseif line =~ '^ctime:\s*'
				" Don't display the time of day
				let datestr = matchstr(getline(l), '^ctime:\s*\zs.\{-}\ze-.*')
				break
			endif
			let l = l + 1
			let line = getline(l)
		endwhile
		" TODO - Perhaps shorten the text...
		" Design Decision: I'm keeping all the text (i.e., not displaying
		" ellipses) because I don't know how wide the Vim window is, and I
		" want as much of the title displayed as possible.
		return l:foldtext.datestr.': '.titlestr
	endif
endfu

" >>>
" TODO - Require header (which could be empty) and body.
" Highlight unexpected mkr1 and mkr2 as Error
" Always, however, sync up on mkr1. Never sync on mkr2.
" Syntax definition <<<
exe 'syn region JnlNone start=/./ end=/'.s:re_mkr1.'/me=s-1'
exe 'syn region JnlHdr matchgroup=JnlHdrStart start=/'.s:re_mkr1.'/'
	\.' matchgroup=NONE end=/'.s:re_mkr1.'/me=s-1'
	\.' keepend fold'


" JnlHdrErr is made lower priority than JnlKeys so that it will begin at the
" first character within JnlHdr that is not part of a valid key.
exe 'syn region JnlHdrErr start=/./ end=/'.s:re_mkr1.'/me=s-1'
	\.' contained containedin=JnlHdr'

exe 'syn region JnlBody start=/'.s:re_mkr2.'/'
	\.' end=/'.s:re_mkr1.'/me=s-1 contained containedin=JnlHdr,JnlHdrErr'

" Define a generic key
exe 'syn region JnlKey matchgroup=JnlKeyStart start=/'
	\.s:re_key_beg.'/ matchgroup=NONE end=/'.s:re_key_end.'/'
	\.' contained containedin=JnlHdr'
" Define 'special' keys
exe 'syn region JnlTitleKey matchgroup=JnlTitleKeyStart start=/'
	\.s:re_title_key_beg.'/ matchgroup=NONE end=/'.s:re_key_end.'/'
	\.' contained containedin=JnlHdr'

" ctime/mtime key
" The container
exe 'syn region JnlTimeKey matchgroup=JnlTimeKeyStart start=/'
	\.s:re_time_key_beg.'/ matchgroup=NONE end=/'.s:re_key_end.'/'
	\.' contained containedin=JnlHdr keepend'

exe 'syn region JnlTimeKeyErr start=/\S/ end=/'.s:re_key_end.'/'
	\.' contained containedin=JnlTimeKey'
exe 'syn match JnlTimeKeyDatetime +'.s:re_datetime
	\.'+ contained containedin=JnlTimeKey nextgroup=JnlTimeKeyErr skipnl skipwhite'

" mtime key
" The container
exe 'syn region JnlChlogKey matchgroup=JnlChlogKeyStart start=/'
	\.s:re_chlog_key_beg.'/ matchgroup=NONE end=/'.s:re_key_end.'/'
	\.' contained containedin=JnlHdr keepend'

exe 'syn match JnlChlogKeyErr /\S\+/'
	\.' contained containedin=JnlChlogKey'
exe 'syn match JnlChlogKeyDatetime +'.s:re_datetime
	\.'+ contained containedin=JnlChlogKey'

" Dates within body/appendices
exe 'syn match JnlDateMkr /'.s:re_date_mkr.'/'
	\.' contained containedin=JnlBody,JnlApdx'

" Comment region
exe 'syn region JnlComment matchgroup=JnlCommentMg start=/'
	\.'^\(\s*\)\zs\/<\*/'
	\.' end=/\*>\//'
	\.' contained containedin=JnlBody,JnlApdx keepend'
	\.' nextgroup=JnlCommentTrailingText'

exe 'syn match JnlCommentMid /'
	\.'^\s*-+-'.'/'
	\.' contained containedin=JnlComment'

exe 'syn match JnlCommentTrailingText /.*/'
	\.' contained'

" Note: JnlCommentBadLine does't match on the line containing the start marker
" because of matchgroup
" TODO - If bad line is completely blank, perhaps cause next line to be
" highlighted Error, since the Error highlighting won't be visible on blank
" line...
" Alternatively, we could say that an empty line is valid within a comment.
exe 'syn match JnlCommentBadLine /'
	\.'^\%(\s*\%('.s:re_comment_mid.'\|'.s:re_comment_end.'\)\)\@!'
	\.'.*/ contained containedin=JnlComment'

" Appendix marker
" Note: This one must come after JnlComment, since JnlComment would also match
" an appendix marker
" Note: ^ and ^\@! are used in JnlApdxBody and JnlApdxMkrTrailingText
" patterns, respectively, to disambiguate the two.
exe 'syn region JnlApdxMkr matchgroup=JnlApdxMkrMg start=/'
	\.'^\/<\*\%(\s*Appendix\s\+[a-zA-Z]\+\>\)\@='
	\.'/ end=/\*>\//'
	\.' contained containedin=JnlBody keepend'
	\.' nextgroup=JnlApdxMkrTrailingText,JnlApdxBody skipnl'

" Permit 'Appendix X' to be highlighted specially
syn match JnlApdxMkr_Name /Appendix\s\+[a-zA-Z]\+\>/ contained containedin=JnlApdxMkr
	\ nextgroup=JnlApdxMkr_Rest skipnl

syn match JnlApdxMkr_Rest /\_.*/ contained

exe 'syn match JnlApdxMkrMid /'
	\.'^\s*-+-'.'/'
	\.' contained containedin=JnlApdxMkr_Rest'

" Note: The '^' in the start pattern for JnlApdxBody permits this one to match
" the trailing text.
exe 'syn match JnlApdxMkrTrailingText /^\@!.*/'
	\.' contained nextgroup=JnlApdxBody skipnl'

" Note: JnlApdxMkrBadLine does't match on the line containing the start marker
" because of matchgroup
" TODO - If bad line is completely blank, perhaps cause next line to be
" highlighted Error, since the Error highlighting won't be visible on blank
" line...
" Alternatively, we could say that an empty line is valid within a comment.
exe 'syn match JnlApdxMkrBadLine /'
	\.'^\%('.s:re_comment_mid.'\|'.s:re_comment_end.'\)\@!'
	\.'.*/ contained containedin=JnlApdxMkr_Rest'


" Highlight special words and word combinations appearing just before a colon
" as first non-whitespace on line.
exe 'syn match JnlColonWordsBol /'
	\.s:re_colon_words_bol.'/ contained containedin=JnlBody,JnlApdx'

" Highlight TODO and FIXME specially
exe 'syn keyword JnlTodoAndFixme '
	\.s:todo_and_fixme_keywords.' contained containedin=JnlBody,JnlApdx'

" Handle the following, more generic, colon construct
" Something-to-be-emphasized:< any thing here...
" Note: The '-' and '<' should be concealed
exe 'syn match JnlColonGen /'.s:re_colon_gen
	\.'/ contained containedin=JnlBody,JnlApdx'
exe 'syn match JnlColonGen_Conceal /'.s:re_colon_gen_mid_tok
	\.'/ contained containedin=JnlColonGen'
exe 'syn match JnlColonGen_Conceal /:'.s:re_colon_gen_end_tok
	\.'/ms=s+1 contained containedin=JnlColonGen'

" >>>
" Cluster definitions <<<
" >>>
" Synchronization <<<
" Note: Use 'syn-sync-fourth' method with 'grouphere' to sync on the 3 types
" of markers.
" Note: Due to the nature of synching, it is impossible to know what came
" before the marker upon which synchronization occurs; hence, the 'unexpected'
" regions are not used.
exe 'syn sync match JnlSyncMkr1 grouphere JnlHdr /'.s:re_mkr1.'/'
" >>>
" Highlighting <<<
hi      JnlBodyStart        guifg=green
hi link JnlHdrStart         String
hi      JnlHdr              guifg=blue      
"hi      JnlBody             guifg=green gui=italic     
hi      JnlKeyStart         gui=bold        
hi      JnlTitleKeyStart    gui=bold,italic guifg=blue 
hi      JnlTitleKey         gui=italic      
hi      JnlTimeKeyStart     gui=bold,italic
hi link JnlTimeKeyErr       Error
hi link JnlTimeKeyDatetime  String
hi      JnlChlogKeyStart    gui=bold,underline
hi link JnlChlogKeyErr      Error
hi link JnlChlogKeyDatetime Comment
hi      JnlDateMkr          gui=bold
hi      JnlColonWordsBol    gui=bold
hi link JnlTodoAndFixme     Todo
hi      JnlColonGen         gui=bold
hi link JnlColonGen_Conceal Ignore

hi      JnlTripleAsterisk    gui=bold,italic
hi      JnlTripleAngle      gui=bold,italic
hi link JnlHdrErr           Error           
hi link JnlNone             Folded
" Comments and Appendix markers
hi link JnlCommentMg        Type
hi link JnlApdxMkrMg        Type
hi link JnlCommentMid       String
hi link JnlApdxMkrMid       String
hi      JnlComment          gui=bold
hi      JnlApdxMkr          gui=italic,bold
hi      JnlApdxMkr_Name     gui=italic,bold
hi      JnlApdxMkr_Rest     gui=italic
hi link JnlCommentBadLine   Error
hi link JnlApdxMkrBadLine   Error
hi link JnlCommentTrailingText Error
hi link JnlApdxMkrTrailingText Error
" >>>
	" vim: sw=4 ts=4 foldmethod=marker foldmarker=<<<,>>>
