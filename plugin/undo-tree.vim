fu! s:Make_node(e)
	return {
		\ 'seq': a:e.seq
		\,'time': a:e.time
		\,'children': []
		\}
endfu
fu! s:Insert_sorted(ls, o)
	let i = 0
	for x in a:ls
		if a:o.seq < x.seq
			call insert(a:ls, a:o, i)
			return a:ls
		endif
		let i += 1
	endfor
	return add(a:ls, a:o)
endfu
fu! s:Recur_tree(entries)
	let ret = []
	for e in a:entries
		let o = s:Make_node(e)
		if has_key(e, 'alt')
			" Build sibs recursively and add self, keeping list sorted.
			let sibs = s:Insert_sorted(s:Recur_tree(e.alt), o)
		else
			" Any sibs are at higher level.
			let sibs = [o]
		endif
		if empty(ret)
			" First child represents the level to be returned.
			let ret = sibs
		else
			" Assumption: can't get here before tail set.
			let tail.children = sibs
		endif
		" Note: tail can point anywhere within sibs list.
		let tail = o
	endfor
	return ret
endfu
fu! s:Display_undo_tree(tree, ...)
	let lvl = a:0 ? a:1 : 0
	" Print the node itself
	" TODO: Special format for root node, or just handle with ternaries?
	echo printf("%s%4d: %s"
		\, repeat("\t", lvl)
		\, a:tree.seq
		\, (lvl ? strftime("%T", a:tree.time) : "origin"))
	" Recurse on any children.
	for child in a:tree.children
		call s:Display_undo_tree(child, lvl + 1)
	endfor
endfu
fu! s:Build_undo_tree()
	let tree = undotree()
	let root = {'seq': 0, 'children': []}
	if has_key(tree, 'entries')
		let root.children = s:Recur_tree(tree.entries)
	endif
	return root
endfu

" 1: Fri 14 Oct 2016 09:30:10 AM CDT: |  |
" 2: Fri 14 Oct 2016 09:30:11 AM CDT: |  |
"         9: Fri 14 Oct 2016 09:40:44 AM CDT: |  |
"        10: Fri 14 Oct 2016 09:41:04 AM CDT: |  |
" 4: Fri 14 Oct 2016 09:30:42 AM CDT: |  |
"         6: Fri 14 Oct 2016 09:34:04 AM CDT: |  |
"                 3: Fri 14 Oct 2016 09:30:17 AM CDT: |  |
"                 8: Fri 14 Oct 2016 09:37:21 AM CDT: | N|
"         7: Fri 14 Oct 2016 09:34:09 AM CDT: |  |
" 5: Fri 14 Oct 2016 09:30:51 AM CDT: | C|

fu! s:Recurse_tree(tree, lvl)
	for t in a:tree
		echo printf("%s%2d: %s: |%2s| %s"
			\, repeat("\t", a:lvl)
			\, t.seq
			\, strftime("%c", t.time)
			\, ((has_key(t, 'newhead') ? "N" : "") . (has_key(t, 'curhead') ? "C" : ""))
			\, (has_key(t, 'save') ? ("save_cnt=" . t.save) : ""))
		if has_key(t, 'alt')
			call s:Recurse_tree(t.alt, a:lvl + 1)
		endif
	endfor

endfu
fu! s:Show_undo_tree()
	let t = undotree()
	echo printf("seq_last:\t%d", t.seq_last)
	echo printf("seq_cur:\t%d", t.seq_cur)
	echo printf("time_cur:\t%s", strftime("%c", t.time_cur))
	echo printf("save_last:\t%d", t.save_last)
	echo printf("save_cur:\t%d", t.save_cur)
	echo printf("synced:\t%d", t.synced)

	call s:Recurse_tree(t.entries, 0)
endfu

nmap <F7> :call <SID>Show_undo_tree()<CR>
nmap <F8> :call <SID>Display_undo_tree(<SID>Build_undo_tree())<CR>
nmap <F9> :echo string(<SID>Build_undo_tree())<CR>
" vim:ts=4:sw=4:tw=80

