
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
" =============================================================
fu! s:Make_node(e, parent)
	return {
		\ 'seq': a:e.seq
		\,'time': a:e.time
		\,'children': {'fst': {}, 'cur': {}, 'lst': {}}
		\,'prev': {}
		\,'next': {}
		\,'parent': a:parent
		\}
endfu
fu! s:Make_root(tree, ...)
	let ret = a:0 ? a:1 : {}
	" Note: Hide Vim's data under 'meta' key to future-proof.
	" TODO: Cleaner (loopless) way in Vim script to do this?
	let ret.meta = {}
	for k in filter(keys(a:tree), 'v:val != "entries"')
		let ret.meta[k] = a:tree[k]
	endfor
	let ret.parent = {}
	" This will be set later in build traversal unless cur is root, so go ahead
	" and initialize to root.
	let ret.cur = ret
	return ret
endfu
fu! s:Insert_sorted(ls, o)
	" Note: cur may be changed at higher level.
	let a:ls.cur = a:o
	let x = a:ls.fst
	while !empty(x)
		if a:o.seq < x.seq
			" Insert before.
			" Assumption: Can't get here if lst empty.
			let a:o.next = x
			let a:o.prev = x.prev
			let x.prev.next = a:o
			let x.prev = a:o
			if x is a:ls.fst
				" List has new head.
				let a:ls.fst = a:o
			endif
			return a:ls
		endif
		let x = x.next
	endwhile
	" Either empty list, or object must be appended.
	if empty(a:ls.fst)
		" Empty list
		let [a:ls.fst, a:ls.lst, a:ls.cur] = [a:o, a:o, a:o]
		let [a:o.prev, a:o.next] = [{}, {}]
		return a:ls
	endif
	" Append to non-empty list
	let a:ls.lst.next = a:o
	let a:o.prev = a:ls.lst
	let a:o.next = {}
	let a:ls.lst = a:o
	return a:ls
endfu
fu! s:Build_undo_tree(...)
	if !a:0
		" Top-level call
		let tree = undotree()
		let entries = tree.entries
		" Create root, merging in undotree() properties.
		let root = s:Make_root(tree,
			\{'seq': 0, 'children': {'fst': {}, 'cur': {}, 'lst': {}}})
		let parent = root
	else
		let [entries, parent, root] = a:000
	endif

	let ret = {}
	for e in entries
		let o = s:Make_node(e, parent)
		if o.seq == root.meta.seq_cur
			" Store a ptr to the current node on the root.
			" Note: This is what will be manipulated henceforth.
			let root.cur = o
		endif
		if has_key(e, 'alt')
			" Build sibs recursively and add self, keeping list sorted.
			" TODO: Consider whether to assign to parent.children instead
			let sibs = s:Insert_sorted(s:Build_undo_tree(e.alt, parent, root), o)
		else
			" Any sibs are at higher level.
			let sibs = {'fst': o, 'cur': o, 'lst': o}
		endif
		let parent.children = sibs
		if empty(ret)
			" First child represents the level to be returned.
			" Note: This should be equivalent to parent.children.
			let ret = sibs
		endif
		" Note: parent can point anywhere within sibs list.
		let parent = o
	endfor
	" Root (seq==0) is special case: no one to return siblings to. In that case,
	" we make sibs child of root, and return root itself.
	return !a:0 ? root : ret
endfu
fu! s:Display_undo_tree(tree, ...)
	if a:tree.seq == 0
		let [root, lvl, is_main] = [a:tree, 0, 1]
	else
		" Non-root
		let [root, lvl, is_main] = a:000
	endif
	" Print the node itself
	" TODO: Special format for root node, or just handle with ternaries?
	if is_main
		if a:tree is root.cur
			echohl Error
		else
			echohl Todo
		endif
	endif
	echo printf("%s%4d: %s"
		\, repeat("\t", lvl)
		\, a:tree.seq
		\, (lvl ? strftime("%T", a:tree.time) : "origin"))
	if is_main
		echohl None
	endif
	" Recurse on any children.
	let [x, cur] = [a:tree.children.fst, a:tree.children.cur]
	while !empty(x)
		" Can't get back on main branch once we've left it.
		call s:Display_undo_tree(x, root, lvl + 1, is_main && x is cur)
		let x = x.next
	endwhile
endfu

" Define methods for tree navigation
fu! s:Move_up() dict
	if !empty(self.cur.parent)
		let self.cur = self.cur.parent
		" Undo
		normal! u
	endif
endfu
fu! s:Move_down() dict
	if !empty(self.cur.children.cur)
		let self.cur = self.cur.children.cur
		" Redo
		exe "normal! \<C-R>"
	endif
endfu
fu! s:Move_left() dict
	" If current node has children, and the node on current undo/redo branch
	" isn't the left-most, attempt to move cur ptr leftward.
	let children = self.cur.children
	if !empty(children.cur) && !empty(children.cur.prev)
		let children.cur = children.cur.prev
	endif
endfu
fu! s:Move_right() dict
	" If current node has children, and the node on current undo/redo branch
	" isn't the right-most, attempt to move cur ptr rightward.
	let children = self.cur.children
	if !empty(children.cur) && !empty(children.cur.next)
		let children.cur = children.cur.next
	endif
endfu

fu! Make_undo_tree()
	let me = s:Build_undo_tree()
	let me.up = function('s:Move_up')
	let me.down = function('s:Move_down')
	let me.left = function('s:Move_left')
	let me.right = function('s:Move_right')
	return me
endfu

" Implementation of 'Drawing Trees' algorithm (Andrew Kennedy)
" Since Vim has no zip function.
" Note: If one array is shorter than the other, missing elements are {}.
fu! s:Zip(xs, ys)
	let [nx, ny, n] = [len(a:xs), len(a:ys), max(nx, ny)]
	let ret = []
	let i = 0
	while i < n
		call add(ret, [i < nx ? a:xs[i] : {}, i < ny ? a:ys[i] : {}])
		let i += 1
	endwhile
	return ret
endfu

fu! s:Unzip(xs)
	let n = len(a:xs)
	let ret = [[], []]
	for x in a:xs
		call add(ret[0], x[0])
		call add(ret[1], x[1])
	endfor
	return ret
endfu

fu! s:Move_tree(t, dx)
	" TODO_ENCAPSULATE
	return {'node': a:t.node, 'x': a:t.x + a:dx, 'children': a:t.children}
endfu

fu! s:Move_extent(e, dx)
	" TODO: Is copy needed? Do it for now to be safe, but re-evaluate later.
	return map(copy(a:e), '[v:val[0] + dx, v:val[1] + a:dx]')
endfu

fu! s:Merge(e1, e2)
	let [n1, n2, n] = [len(e1), len(e2), max(nx, ny)]
	let ret = []
	let i = 0
	while i < n
		" Note: When one array shorter than the other, take both values from the
		" longer one.
		" TODO: There are more efficient ways, especially if one is much longer.
		call add(ret, [i < n1 ? a:e1[0] : a:e2[0], i < n2 ? a:e2[1] : a:e1[1]])
	endwhile
	return ret
endfu

fu! s:Merge_list(es)
	let [i, n] = [1, len(a:es)]
	" Note: Although s:Merge could handle 1st element of [], handling first
	" element specially is more efficient.
	let ret = n ? copy(a:es[0]) : []
	while i < n
		let ret = s:Merge(ret, a:es[i])
	endwhile
	return ret
endfu

fu! s:Fit(e1, e2)
	let ret = 0
	" Note: Use min() in lieu of max() since unmatched levels aren't constrained
	" by neighboring child.
	let [n1, n2, n] = [len(a:e1), len(a:e2), min(nx, ny)]
	let i = 0
	while i < n
		" TODO: Make min separation (1) configurable? At least, don't hard-code.
		let d = a:e1[i][1] - a:e2[i][0] + 1
		if d > ret
			let ret = d
		endif
	endwhile
	return ret
endfu

" Caveat: Vim doesn't do TCO, so implement both left and right folds without
" recursion.
fu! s:Fitlistl(es)
	let [ret, acc] = [[], []]
	for e in a:es
		let x = s:Fit(acc, e)
		" TODO: Doesn't appear that the original extent will be needed after the
		" move, so there's probably potential for optimization.
		let acc = s:Merge(acc, s:Move_extent(e, x))
		call add(ret, x)
	endfor
	return ret
endfu

fu! s:Flip_extent(e)
	return map(a:e, '[-v:val[1], -v:val[0]]')
endfu

" TODO: Consider whether it's better to implement in terms of Fitlistl, or to
" implement Fitlistr independently.
fu! s:Fitlistr(es)
	let es = reverse(a:es)
	let es = map(es, 's:Flip_extent(v:val)')
	let es = s:Fitlistl(es)
	let es = map(es, '-v:val')
	let es = reverse(es)
	return es
endfu

" TODO: Perhaps inline this.
fu! s:Mean(x, y)
	return (a:x + a:y) / 2.0
endfu

fu! s:Fitlist(es)
	" Note: Avoid extra function call to s:Mean.
	" TODO: A less functional style would probably render this more efficient.
	return map(s:Zip(s:Fitlistl(a:es), s:Fitlistr(a:es)),
		\ '(v:val[0] + v:val[1]) / 2.0')
endfu

" Note: Input tree is the undo-tree object. From that, build one that looks like
" this:
" {'node': <ut-node>, 'x': <position relative to parent>, 'children': <array of these nodes>}
" TODO: Convert arrays used in these methods from Vim lists to singly-linked
" lists. (Perhaps wait till I've tested basic algorithm.)
fu! s:Design(t)
	let lvl = 0
	let descend = 0
	let [t, tc] = [a:t, t.children.fst]
	let [ret, sret] = [[], []]
	while 1
		if descend
			let [trees, extents, ret] = [[], [], []]
			let lvl += 1
		elseif !empty(sret)
			" Return from lower level
			let lvl -= 1
			if len(stack) == 0
				" Done
				return sret[0]
			endif
			" Pop back up to higher level.
			" TODO: Check this...
			" UNDER CONSTRUCTION
			let [t, tc, trees, extents, ret] = remove(stack, -1)
		endif
		let descend = 0

		while tc
			if empty(sret)
				" Not handling return: either descending or base case
				if !empty(tc.children.fst)
					call add(stack, [t, tc, trees, extents, ret])
					let t = tc
					let tc = tc.children.fst
					let descend = 1
					break
				else
					" Base case: i.e., no lower level, so use initial value, and
					" no need to bother with stack.
					let sret = [[], []]
				endif
			endif
			" Either we've just returned (ascending) or we've reached base case.
			" Accumulate the subtree representred by sret
			call add(trees, stree[0])
			call add(extents, stree[1])

			let tc = tc.next
		endwhile
		if !descend && empty(tc.next)
			" No more children.
			" Do final processing and return.
			" Note: Everything here is built from trees and extents, which are
			" unique to a level.
			let positions = s:Fitlist(extents)
			let ptrees = map(s:Zip(trees, positions), 's:Move_tree(v:val)')
			let pextents = map(s:Zip(extents, positions), 's:Move_extent(v:val)')
			" TODO: Calculate label width instead of this silly hardcode.
			let resultextent = insert(s:Merge_list(pextents), [-2, 2])
			let resulttree = {'node': t, 'x': 0, 'children': ptrees}
			" Setting sret causes return processing at head of loop.
			let sret = [resulttree, resultextent]
		endif
	endwhile
endfu

nmap <F7> :let ut = Make_undo_tree()<CR>
nmap <F8> :call <SID>Display_undo_tree(ut)<CR>
nmap <C-Up> :call ut.up()<CR>
nmap <C-Down> :call ut.down()<CR>
nmap <C-Left> :call ut.left()<CR>
nmap <C-Right> :call ut.right()<CR>

nmap <F9> :echo string(<SID>Show_undo_tree())<CR>
" vim:ts=4:sw=4:tw=80

