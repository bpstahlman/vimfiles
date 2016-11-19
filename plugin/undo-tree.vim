
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
" TODO: Use this in tree display building.
fu! s:Node_get_geom() dict
	let w = len(self.seq) + 2
	let xs = -(w - 1) / 2
	let xe = xs + w - 1
	return {'w': w, 'e': [xs, xe]}
endfu
" =============================================================
" TODO: Consider pulling everything but 'next' under an 'el' property so that
" these elements can be used with the list functions further down. Hmm... Not
" yet clear whether structural members like children and parent would also need
" to be in el.
fu! s:Make_node(e, parent, lvl)
	return {
		\ 'seq': a:e.seq
		\,'time': a:e.time
		\,'lvl': a:lvl
		\,'children': {'fst': {}, 'cur': {}, 'lst': {}}
		\,'prev': {}
		\,'next': {}
		\,'parent': a:parent
		\,'Get_geom': function('s:Node_get_geom')
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
	let ret.lvl = 0
	let ret.parent = {}
	" This will be set later in build traversal.
	let ret.cur = {}
	" Note: No inheritance forces kludgy stuff like this.
	" TODO: Consider better way.
	let ret.Get_geom = function('s:Node_get_geom')
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
			" Caveat: Don't add 'next' key to empty sentinel object.
			if !empty(x.prev)
				let x.prev.next = a:o
			endif
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
	if a:0 == 1
		" Top-level call
		let tree = a:1
		let entries = tree.entries
		" Create root, merging in undotree() properties.
		let root = s:Make_root(tree, {
			\'seq': 0, 'prev': {}, 'next': {}, 'parent': {},
			\'children': {'fst': {}, 'cur': {}, 'lst': {}}})
		let parent = root
		" First entry will be at level 1.
		let lvl = 1
	else
		let [entries, parent, root, lvl] = a:000
	endif

	let ret = {}
	for e in entries
		let o = s:Make_node(e, parent, lvl)
		" Caveat: seq_cur isn't always in sync. Just after loading a file, for
		" instance, curhead is correct, but seq_cur isn't. Rework this.
		if has_key(e, 'curhead')
			" Store a ptr to the current node on the root.
			" Note: This is what will be manipulated henceforth.
			let root.cur = parent
		endif
		if has_key(e, 'alt')
			" Build sibs recursively and add self, keeping list sorted.
			" TODO: Consider whether to assign to parent.children instead
			let sibs = s:Insert_sorted(
				\ s:Build_undo_tree(e.alt, parent, root, lvl), o)
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
		let lvl += 1
	endfor
	if a:0 == 1 && empty(root.cur)
		" We fell off the end without finding 'curhead'; parent is the last
		" object encountered (root if top level had no entries[]).
		let root.cur = parent
	endif
	" Root (seq==0) is special case: no one to return siblings to. In that case,
	" we make sibs child of root, and return root itself.
	return a:0 == 1 ? root : ret
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
		" TODO: Consider whether any advantage to using u and <C-R> rather than
		" :undo <seq>
		"normal! u
	endif
endfu
fu! s:Move_down() dict
	if !empty(self.cur.children.cur)
		let self.cur = self.cur.children.cur
		" Redo
		"exe "normal! \<C-R>"
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

" Design Decision: undotree() always passed in to avoid redundant calls.
fu! s:Make_undo_tree(undotree)
	let me = s:Build_undo_tree(a:undotree)
	let me.up = function('s:Move_up')
	let me.down = function('s:Move_down')
	let me.left = function('s:Move_left')
	let me.right = function('s:Move_right')
	return me
endfu

" Implementation of 'Drawing Trees' algorithm (Andrew Kennedy)
" Since Vim has no zip function.
" Note: If one array is shorter than the other, missing elements are {}.
" List implementation:
" {'el': <element1>, 'next': {'el': <element2>, 'next': {...'next': {}
fu! S_Cons(val, xs)
	return {'el': a:val, 'next': a:xs}
endfu
" Note: Best when you maintain pointer to final element of list.
fu! S_Conc(val, xs)
	let x = a:xs
	while !empty(x)
		let x = x.next
	endwhile
	" Modify the (previously empty) tail in place.
	let [x.el, x.next] = [a:val, {}]
	" Return new tail el
	return x
endfu
fu! S_Set_cdr(val, cons)
	let a:cons.next = {'el': a:val, 'next': {}}
	return a:cons.next
endfu
fu! S_Foldl(f, acc, xs)
	" Note: Take type of xs into account, handling both arrays and lists.
	" TODO: More efficient way than converting to list.
	let [x, acc] = [type(a:xs) == 3 ? S_To_list(a:xs) : a:xs, a:acc]
	while !empty(x)
		let acc = a:f(acc, x.el)
		let x = x.next
	endwhile
	return acc
endfu
fu! S_To_list(ary)
	let ret = {}
	let tail = ret
	for x in a:ary
		let tail = S_Conc(x, tail)
	endfor
	return ret
endfu
fu! S_Reverse(xs)
	let [x, ret] = [a:xs, {}]
	while !empty(x)
		let ret = S_Cons(x.el, ret)
		let x = x.next
	endwhile
	return ret
endfu
fu! S_Map(f, xs)
	let [x, ret] = [a:xs, {}]
	" Note: tail will advance, ret will not.
	let tail = ret
	while !empty(x)
		let val = call(a:f, [x.el])
		" Note: Something tricky's going on here: ret and tail will be
		" equivalent after first iteration only.
		let tail = S_Conc(val, tail)
		let x = x.next
	endwhile
	return ret
endfu
fu! S_Zip(xs, ys, ...)
	let [x, y, ret] = [a:xs, a:ys, {}]
	let discard_unmatched = a:0 && a:1
	let tail = ret
	while !empty(x) || !empty(y)
		if discard_unmatched && (empty(x) || empty(y))
			" Special case: Certain applications don't need unmatched elements.
			break
		endif
		let tail = S_Conc([empty(x) ? {} : x.el, empty(y) ? {} : y.el], tail)
		if !empty(x) | let x = x.next | endif
		if !empty(y) | let y = y.next | endif
	endwhile
	return ret
endfu
fu! S_Unzip(xs)
	let [x, ret] = [a:xs, [{}, {}]]
	let tail = [ret[0], ret[1]]
	while !empty(x)
		let tail[0] = S_Conc(x.el[0], tail[0])
		let tail[1] = S_Conc(x.el[1], tail[1])
		let x = x.next
	endwhile
	return ret
endfu
" Note: Input is a zipped list of (trees, positions)
fu! s:Move_tree_fn(tree_pos)
	" Note: Copy the node but with changed x.
	" Caveat: This function no longer purely functional: modifying the node
	" itself.
	let new_x = a:tree_pos[0].x + a:tree_pos[1]
	" TODO: Should we use an accessor for this?
	let a:tree_pos[0].node.x = new_x
	return {
		\ 'node': a:tree_pos[0].node,
		\ 'x': new_x,
		\ 'ptrees': a:tree_pos[0].ptrees}
endfu

" Note: Meant to be used with S_Map
fu! s:Move_extent_fn(dx, e_el)
	return [a:e_el[0] + a:dx,  a:e_el[1] + a:dx]
endfu
fu! s:Move_extent(arg, ...)
	" Args: e, dx - may be passed either as regular args, or in array.
	" contained in an array.
	let [e, dx] = a:0 ? [a:arg, a:1] : [a:arg[0], a:arg[1]]
	" Create a partial that passes dx as first arg.
	return S_Map(function('s:Move_extent_fn', [dx]), e)
endfu

fu! s:Merge_fn(e_pair)
	return [
		\ empty(a:e_pair[0]) ? a:e_pair[1][0] : a:e_pair[0][0],
		\ empty(a:e_pair[1]) ? a:e_pair[0][1] : a:e_pair[1][1]]
endfu
fu! S_Merge(e1, e2)
	return S_Map(function('s:Merge_fn'), S_Zip(a:e1, a:e2))
endfu

fu! s:Merge_list(es)
	return S_Foldl(function('S_Merge'), {}, a:es)
endfu


fu! s:Fit_fn(max, e_pair)
	" Assumption: Caller ensures matched pairs (using discard_unmatched optional
	" arg to Zip).
	" TODO: Make min separation configurable? At least, don't hard-code.
	" Note: Space for any brackets is included in label width.
	let d = a:e_pair[0][1] - a:e_pair[1][0] + 1
	return d > a:max ? d : a:max
endfu
fu! s:Fit(e1, e2)
	return S_Foldl(function('s:Fit_fn'), 0, S_Zip(a:e1, a:e2, 1))

	"let ret = 0
	"" Note: Use min() in lieu of max() since unmatched levels aren't constrained
	"" by neighboring child.
	"let [n1, n2, n] = [len(a:e1), len(a:e2), min(nx, ny)]
	"let i = 0
	"while i < n
	"	" TODO: Make min separation (1) configurable? At least, don't hard-code.
	"	let d = a:e1[i][1] - a:e2[i][0] + 1
	"	if d > ret
	"		let ret = d
	"	endif
	"endwhile
	"return ret
endfu
" Caveat: Vim doesn't do TCO, so implement both left and right folds without
" recursion.
fu! s:Fitlistl_fn(acc, e)
	let [tail, acc] = a:acc
	let dx = s:Fit(acc, a:e)
	let tail = S_Conc(dx, tail)
	let acc = S_Merge(acc, s:Move_extent(a:e, dx))
	return [tail, acc]
endfu
fu! s:Fitlistl(es)
	let ret = {}
	let tail = ret
	let es = type(a:es) == 3 ? S_To_list(a:es) : a:es
	call S_Foldl(function('s:Fitlistl_fn'), [tail, {}], es)
	return ret
endfu
fu! s:Negate_fn(val)
	return -a:val
endfu
fu! s:Flip_extent_fn(e_el)
	return [-a:e_el[1], -a:e_el[0]]
endfu
fu! s:Flip_extent(e)
	return S_Map(function('s:Flip_extent_fn'), a:e)
endfu

" TODO: Consider whether it's better to implement in terms of Fitlistl, or to
" implement Fitlistr independently.
" TODO: Pick up here... Still implement in terms of Fitlistl?
" TODO: Test this one...
fu! s:Fitlistr(es)
	let es = type(a:es) == 3 ? S_To_list(a:es) : a:es
	let es = S_Reverse(es)
	let es = S_Map(function('s:Flip_extent'), es)
	let es = s:Fitlistl(es)
	let es = S_Map(function('s:Negate_fn'), es)
	let es = S_Reverse(es)
	return es
endfu

fu! s:Mean_fn(pair)
	return (a:pair[0] + a:pair[1]) / 2
endfu

fu! s:Fitlist(es)
	" Note: Avoid extra function call to s:Mean.
	return S_Map(function('s:Mean_fn'), S_Zip(s:Fitlistl(a:es), s:Fitlistr(a:es)))
endfu

fu! s:Design(t)
	" Walk the children.
	let [trees, extents] = [{}, {}]
	let [trees_tail, extents_tail] = [trees, extents]
	let tc = a:t.children.fst
	while !empty(tc)
		let [tree, extent] = s:Design(tc)
		let trees_tail = S_Conc(tree, trees_tail)
		let extents_tail = S_Conc(extent, extents_tail)
		let tc = tc.next
	endwhile
	let positions = s:Fitlist(extents)
	let ptrees = S_Map(function('s:Move_tree_fn'), S_Zip(trees, positions))
	let pextents = S_Map(function('s:Move_extent'), S_Zip(extents, positions))
	" Note: Leave space for surrounding [...]
	" TODO: Perhaps methodize getting label text/size somehow.
	let w = len(a:t.seq) + 2
	let e = [-w/2, w/2 + w%2]
	let resultextent = S_Cons(e, s:Merge_list(pextents))
	" TODO: Consider a cleaner way to put x on the actual node. Perhaps in a
	" later stage?
	let a:t.x = 0
	let resulttree = {'node': a:t, 'x': 0, 'ptrees': ptrees}
	return [resulttree, resultextent]
endfu

fu! s:Gridlines_add(idx, x, text) dict
	let num_lines = len(self.lines)
	if a:idx >= num_lines
		" Add lines as required.
		" TODO: Consider adding a little extra and culling later.
		call extend(self.lines, repeat([''], a:idx - num_lines + 1))
	endif
	let line_len = len(self.lines[a:idx])
	if a:x > line_len
		" Pre-pad with spaces as necessary.
		let self.lines[a:idx] .= repeat(' ', a:x - line_len)
	endif
	" Append or overwrite at x.
	" Note: 3rd component may always be empty. Depends on how I end up using.
	" TODO: Consider making more efficient if overwrite capability is not
	" needed.
	"echo "idx=" . a:idx . ", x=" . a:x . ", text=" . a:text
	let self.lines[a:idx] =
		\ self.lines[a:idx][: a:x - 1] . a:text . self.lines[a:idx][a:x + len(a:text) :]
endfu

fu! s:Make_gridlines()
	let me = {
		\ 'add': function('s:Gridlines_add'),
		\ 'lines': []
	\ }
	return me
endfu

" TODO: This one probably isn't needed.
fu! s:Swap_tuple_fn(x)
	return [a:x[1], a:x[0]]
endfu

" Note: Input tree is the undo-tree object. From that, build one that looks like
" this:
" {'node': <ut-node>, 'x': <position relative to parent>, 'children': <array of these nodes>}
" TODO: Convert arrays used in these methods from Vim lists to singly-linked
" lists. (Perhaps wait till I've tested basic algorithm.)
" Note: This function is work in progress. May be abandoned. If not abandoned,
" may be converted to cleaner BFS implementation.
fu! s:Design_nr(t)
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
			" Accumulate the subtree represented by sret
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

fu! s:Extent_min_fn(min, e_el)
	return a:e_el[0] < a:min ? a:e_el[0] : a:min
endfu
fu! s:Build_tree_display(tree, extent)
	" Tree is centered at 0, but we need its left edge at 0. Determine the bias.
	" Note: Can't really apply the bias in the tree itself since the x values in
	" tree are relative to parent. We *could* store absolute positions in the
	" tree, but I don't really like that.
	let x = abs(S_Foldl(function('s:Extent_min_fn'), 0, a:extent))
	" Breadth-first traversal
	" Fifo elements: [<node>, <absolute-parent_x>, <lvl>]
	let fifo = [[a:tree, x, 0]]
	" TODO: Make this part of some sort of global config.
	let rows_per_lvl = 3
	let lines = s:Make_gridlines()
	" Hash geom info by node id (seq number)
	let nodes = {}
	" Note: lines will be added later.
	let ret = {'geom': {'x_bias': x, 'nodes': nodes}, 'lines': []}
	while !empty(fifo)
		let [t, parent_x, lvl] = remove(fifo, 0)
		" Calculate absolute x position in grid.
		let x = parent_x + t.x
		" Get index of row containing the label.
		let lrow = lvl * rows_per_lvl
		" Hash node and its absolute x pos by seq number.
		" Rationale: Associates nodes with actual canvas location.
		" Note: +1 converts from 0-based string offsets to 1-based row/col.
		" Design Decision: Adding 'row' key to nodes object as convenience,
		" though it could be derived from lvl.
		" TODO: Decide whether the convenience justifies the denormalization.
		" Note: Currently, not even using 'row'.
		let nodes[t.seq] = {'node': t, 'col': x + 1, 'row': lrow + 1 }
		" Add this node's children
		let tc = t.children.fst
		while !empty(tc)
			" Note: Add parent x only, as child x can be calculated therefrom.
			call add(fifo, [tc, x, lvl + 1])
			let tc = tc.next
		endwhile
		" Process current node.
		" Build the label, leaving space for surrounding [...].
		let text = ' ' . t.seq . ' '
		" TODO: Think through rounding/truncating...
		" Design Decision: The -1 ensures that when len is even, we center on
		" the last char in the left half, not first in right.
		let text_x = x - (len(text) - 1) / 2
		call lines.add(lrow, text_x, text)
		" Draw lower vertical for all but root.
		if !empty(t.parent)
			if t.x < 0
				let [off, text] = [1, '/']
			elseif t.x > 0
				let [off, text] = [-1, '\']
			else
				let [off, text] = [0, '|']
			endif
			call lines.add(lrow - 1, x + off, text)
			" Calculate effective x to be used in subsequent calculations.
			" Note: "Effective" x positions can differ from t.x and t.prev.x
			" when the node is within 1 unit of the parent. In such cases, the
			" horizontal line's row is calculated as though the child were
			" aligned with the parent.
			" TODO: Consider making adjustment in tree design phase to obviate need
			" for the 2 special cases (child one position left/right of parent).
			" Rationale: Would probably look a bit nicer at the cost of making
			" some graphs *slightly* wider.
			let xe = abs(t.x) == 1 ? 0 : t.x
			if !empty(t.prev)
				let xe_prev = abs(t.prev.x) == 1 ? 0 : t.prev.x
				" Extend horiz header line from previous node, and if appropriate,
				" add upper vertical from parent (which is deferred to child row
				" because it's in the same row as horiz line).
				if xe_prev < 0
					" l->?
					let text_x = parent_x + xe_prev + 2
					if xe < 0
						" l->l
						let text = repeat('_', xe - xe_prev)
					elseif xe > 0
						" l->r
						let text = repeat('_', -xe_prev - 2)
							\ . '|' . repeat('_', xe - 2)
						"echo "l->r case: " . t.seq . ": " . text
					else
						" l->c
						let text = repeat('_', -xe_prev - 2) . '|'
					endif
				elseif xe_prev > 0
					" r->r
					" Caveat: If prev is 1 past parent x, don't overwrite the
					" vertical connector.
					let text_x = parent_x + (xe_prev > 1 ? xe_prev - 1 : xe_prev)
					let text = repeat('_', xe - xe_prev)
				else
					" c->r
					" Note: We get here when 'effective' xprev is 0, even when
					" node is a unit to the left or right of parent.
					let text_x = parent_x + 1
					let text = repeat('_', xe - 2)
				endif
				"echo printf("seq=%d lrow=%d: Adding `%s' at %d", t.seq, lrow-2, text, text_x)
				call lines.add(lrow - 2, text_x, text)
			elseif empty(t.next)
				" Special case: sole child of parent
				" Assumption: In all other cases, center vertical from parent will
				" be part of horizontal line.
				" Rationale: A balanced tree shouldn't permit everything to one
				" side. (TODO: Work through any corner cases to verify.)
				" Note: Could also handle it separately and take advantage of the
				" fact that gridlines can handle overwrite.
				call lines.add(lrow - 2, x, '|')
			endif
		endif
	endwhile
	" Add the lines array to return object.
	let ret.lines = lines.lines
	return ret
endfu

" TODO: Rework so that the parent hash holds an object with child's bnr and
" whatever data object the plugin wants to store in it. Also, perhaps it could
" hold a random magic number to give greater confidence on identity of child
" buffer.
let s:undo_infos = {}
let s:parent_to_child = {}
let s:child_to_parent = {}

let s:Debug_level = -1
fu! s:Dbg(lvl, fmt, ...)
	if a:lvl > s:Debug_level
		return
	endif
	echo call('printf', extend([a:fmt], a:000))
endfu

" Inputs:
" atom: the pattern to match
" pos: constraint pos (1-based [row, col])
" [len]: constraint length
fu! s:Make_regex(atom, pos, ...)
	if a:0
		" range
		" TODO: Make sure we don't need separate \%c for == case.
		let re = '\%' . a:pos[0] . 'l\&\%>' . (a:pos[1] - 1)
			\ . 'c\&\%<' . (a:pos[1] + a:1) . 'c'
	else
		" exact position
		let re = '\%' . a:pos[0] . 'l\&\%' . a:pos[1] . 'c'
	endif
	" TODO: Consider optimal way to apply constraint: e.g., is it faster to
	" check the atom or the constraint?
	let re .= '\&' . a:atom
	return re
endfu
" Inputs:
" ids: list of matchadd ids for the level being built.
" node: child node
" lvl: 0-based level of child
" col: child col
fu! s:Update_syn_tree_node(ids, node, lvl, col)
	let rows_per_lvl = 3
	let row = a:lvl * rows_per_lvl + 1
	let gi = a:node.Get_geom()
	let re = s:Make_regex('[[(]\?[0-9]\+[])]\?', [row, a:col + gi.e[0]], gi.w)
	" TODO: Don't hardcode the priorities like this.
	" Note: Priority 10 is default. It will override hlsearch and such, but
	" should be lowest of the undo-tree groups.
	call add(a:ids, matchadd('undo_redo_path', re, 10))
	if !a:lvl
		" Nothing else to do for root.
		return
	endif
	if a:node.x < 0
		let [off, text] = [1, '/']
	elseif a:node.x > 0
		" Escape for regex
		let [off, text] = [-1, '\\']
	else
		let [off, text] = [0, '|']
	endif
	let re = s:Make_regex(text, [row - 1, a:col + off])
	call add(a:ids, matchadd('undo_redo_path', re, 10))

endfu

" Assumption: Called from child buffer
fu! s:Clear_syn_tree() dict
	let self.ids = []
	call clearmatches()
endfu

" Add or erase brackets around input node from geom.nodes dictionary.
" gnode: {'node': {}, 'col': <colnr>, 'row': <rownr>}
" TODO: Make more generic by changing 'erase' flag to an arg that indicates the
" wrap char desired.
fu! s:Bracket_node(gnode, erase)
	let [tnode, row, col] = [a:gnode.node, a:gnode.row, a:gnode.col]
	let gi = tnode.Get_geom()
	let [scol, ecol] = [col + gi.e[0], col + gi.e[1]]
	"echomsg "gi: " . string(gi) . ", scol=" . scol . ", ecol=" . ecol
	" TODO: Make change manually? Or with setline()?
	let s = getline(row)
	call setline(row,
		\ strpart(s, 0, scol - 1) .
		\ (a:erase ? ' ' : '[') .
		\ strpart(s, scol, gi.w - 2) .
		\ (a:erase ? ' ' : ']') .
		\ strpart(s, ecol))
endfu

" Assumption: Input node is valid. (Note that root node can never be
" invalidated.)
" Assumption: Called from child buffer
fu! s:Update_syn_tree(node) dict
	let lvl = a:node.lvl
	" Remove any invalidated items
	" Assumption: Array of ids will either be empty, or have # of levels
	" dictated by height of tree.
	if len(self.ids)
		for ids in self.ids[lvl : ]
			for id in ids
				call matchdelete(id)
			endfor
		endfor
		" Now remove the actual list elements so we can blindly add below.
		call remove(self.ids, lvl, -1)
	endif
	let [tp, t] = [a:node.parent, a:node]
	" Note: Positions in geom.nodes are 1-based (col nr)
	" Note: If child is root, initialize colp to root's col.
	let colp = self.geom.nodes[(empty(tp) ? t : tp).seq].col
	while !empty(t)
		call add(self.ids, [])
		" Design Decision: Could also lookup x in self.geom.nodes, but this
		" avoids hash lookup.
		let col = colp + t.x
		call s:Update_syn_tree_node(self.ids[-1], t, lvl, col)
		let [t, colp] = [t.children.cur, col]
		let lvl += 1
	endwhile
endfu
fu! s:Update_syn(node, ...) dict
	let seq = a:0 ? a:1 : a:node.seq
	" TODO: Using call() so that I can keep s:Update_syn_tree as method without
	" adding it to public interface. Look for better way...
	call call('s:Update_syn_tree', [a:node], self)
	if self.seq >= 0
		" De-select old
		let old = self.geom.nodes[self.seq]
		" Get bounds of old
		"echomsg "s:Bracket_node  " . self.seq
		call s:Bracket_node(old, 1)
	endif
	" Select new
	let new = self.geom.nodes[seq]
	"echomsg "s:Bracket_node [" . seq
	call s:Bracket_node(new, 0)
	" Cache new
	let self.seq = seq
endfu
fu! s:Make_syn_tree(geom)
	let me = {
		\ 'ids': [],
		\ 'seq': -1,
		\ 'Clear': function('s:Clear_syn_tree'),
		\ 'Update': function('s:Update_syn'),
		\ 'geom': a:geom
		\ }

	return me
endfu

" For convenience, ensure these script-locals always exist.
let s:bufnr = -1
let s:undo_bufnr = -1
let s:undo_cache = {}
let s:actions = {}

" Safe to call if action not pending.
fu! s:Cancel_action(fn)
	if has_key(s:actions, a:fn)
		call remove(s:actions, a:fn)
	endif
endfu
fu! s:Defer_action(name, ...)
	" Save the args under the desired function function name.
	let s:actions[a:name] = a:000

	if !exists('#undo_tree_actions#CursorHold') || !exists('#undo_tree_actions#CursorHoldI')
		" Create global CursorHold autocmds for both normal and insert modes.
		" Rationale: Gives us best chance of processing queue right away.
		aug undo_tree_actions
			au!
			au CursorHold * call s:Do_deferred_actions()
			au CursorHoldI * call s:Do_deferred_actions()
		augroup END
	endif

	" Make sure we can restore old 'updatetime' once we're finished using it.
	let s:updatetime_save = &updatetime
	" Schedule queue processing as soon as possible.
	set updatetime=1
endfu
fu! s:Do_deferred_actions()
	" Shouldn't get here with s:updatetime_save unset, but just in case...
	if exists('s:updatetime_save')
		let &updatetime = s:updatetime_save
		unlet! s:updatetime_save
	endif
	for [name, args] in items(s:actions)
		call call(function('s:' . name, args))
	endfor
	let s:actions = {}
	" Make sure we're not invoked again until something else is queued.
	au! undo_tree_actions CursorHold
	au! undo_tree_actions CursorHoldI
endfu

" Make the current buffer no longer the current parent.
fu! s:Unconfigure_parent()
	" Remove the parent autocommands.
	au! undo_tree_parent
	let s:bufnr = -1
	let s:undo_cache = {}
endfu

fu! s:Unconfigure_child()
	" Remove the child autocommands.
	au! undo_tree_child
	" TODO: Perhaps also remove mappings.
endfu

" Goto specified window.
" Assumption: Called under internal control: hence, we need to suppress
" autocmds (especially BufEnter).
fu! s:Goto_win(winnr)
	exe 'noauto ' . a:winnr . "wincmd \<C-W>"
endfu

fu! s:Create_autocmds_in_child()
	" Create autocmds that will permit us to clean up when child buf deleted
	aug undo_tree_child
		au!
		au BufDelete <buffer> call s:Child_BufDelete(expand("<abuf>"))
		au BufEnter <buffer> call s:Child_BufEnter()
		au BufLeave <buffer> call s:Child_BufLeave()
		au BufWinLeave <buffer> call s:Child_BufWinLeave(expand("<abuf>"))
	augroup END
endfu

fu! s:Create_autocmds_in_parent()
	" Create autocmds that will permit us to clean up when parent buf deleted
	aug undo_tree_parent
		au!
		au BufWinLeave <buffer> call s:Parent_BufWinLeave(expand("<abuf>"))
	augroup END
endfu

" Called From: buffer being considered as candidate parent
fu! s:Is_valid_parent()
	" TODO More checks? E.g., 'buftype', etc...?
	" Question: Any way to use undotree() for this test? E.g., do nofile buffers
	" have undo trees?
	return &modifiable
endfu

fu! s:Configure_parent()
	call s:Create_autocmds_in_parent()
endfu

fu! s:Create_mappings_in_child()
	" Create buffer-local mapping(s)
	" To refresh the tree forcibly
	nnoremap <silent> <buffer> <leader>r :call <SID>Refresh_child()

	" Moving up/down and changing undo/redo path through tree.
	" Design Decision: No reason to avoid using regular Vim motion commands.
	" Rationale: Cursor movement is highly constrained.
	nnoremap <silent> <buffer> k :call <SID>Move_in_tree('up')<CR>
	nnoremap <silent> <buffer> j :call <SID>Move_in_tree('down')<CR>
	nnoremap <silent> <buffer> h :call <SID>Move_in_tree('left')<CR>
	nnoremap <silent> <buffer> l :call <SID>Move_in_tree('right')<CR>
	nnoremap <silent> <buffer> c :call <SID>Center_tree(1)<CR>
endfu

fu! s:Create_syntax_in_child()
	" TODO: Could do this once up front.
	hi undo_redo_path gui=bold cterm=bold term=bold
endfu

" Object to hold saved option settings for save/restore mechanism.
let s:opts = {}
" Note: No good way to get script name from inside function.
let s:script_path = expand('<sfile>:p')
" Return full path of script that last set option whose *full* name is input.
" Returns empty string if still at default.
fu! s:Get_opt_setter(name)
	" Caveat: verbose set seems to return a leading newline: allow for it, but
	" don't require it.
	exe 'silent! redir =>l:opt | silent! verbose set ' . a:name . '? | redir END'
	let fname = matchstr(opt, '^\(\s*\_s\)\?\s*' . a:name
		\ . '=.*\_s\s\+Last set from\s\+\zs.\{-}\ze\s*$')
	return empty(fname) ? '' : fnamemodify(fname, ':p')
endfu

fu! s:Restore_opt(name)
	if has_key(s:opts, a:name)
		exe 'let &' . a:name ' = s:opts[a:name]'
		call remove(s:opts, a:name)
	else
		" Getting here implies internal error or race-condition: to be safe,
		" just set back to default.
		exe 'set ' . a:name . '&'
	endif
endfu

" Rationale: The reason for checking setter is that if we somehow managed to do
" 2 consecutive overrides from this script, we might inadvertently save our own
" setting for subsequent restoration. This would be particularly bad if the
" option were 'guicursor'. Refusing to save a value we set ensures this can't
" happen (at the cost of a bit of complexity).
" TODO: Doing the save only in BufEnter (and in Configure_child when we're sure
" the BufEnter has been skipped) could also prevent the problem scenario, and
" would obviate the need for the more complex override mechanism. Consider
" whether this mechanism is justified.
" TODO: Consider adding a 'local' flag parameter. (May not be necessary, given
" that we typically wouldn't need to 'override' local params.)
fu! s:Override_opt(name, value)
	let setter = s:Get_opt_setter(a:name)
	if setter != s:script_path
		" Save old value.
		exe 'let s:opts[a:name] = &' . a:name
	endif
	" Override.
	exe 'let &' . a:name . ' = a:value'
endfu

fu! s:Configure_cursor_in_child()
	if has('gui')
		call s:Override_opt('guicursor', 'n-v:block-NONE')
	else
		call s:Override_opt('t_ve', '')
	endif
endfu

fu! s:Unconfigure_cursor_in_child()
	call s:Restore_opt(has('gui') ? 'guicursor' : 't_ve')
endfu

" TODO: Decide between this simple approach and the more complex approach in the
" non-<...>_child versions.
fu! s:Configure_cursor_in_child_simple()
	if has('gui')
		let s:cursor_save = &guicursor
		" TODO: Any advantage to creating special Cursor group?
		set guicursor=n-v:block-NONE
	else
		let s:cursor_save = &t_ve
		set t_ve=
	endif
endfu

fu! s:Unconfigure_cursor_in_child_simple()
	if exists('s:cursor_save')
		if has('gui')
			let &guicursor = s:cursor_save
		else
			let &t_ve = s:cursor_save
		endif
	endif
endfu

" TODO: Perhaps make it so stuff isn't redone unnecessarily (though all of this
" should be safe to redo).
" TODO: Make sure child has all requisite autocmds and such.
fu! s:Configure_child()
	call s:Create_autocmds_in_child()
	call s:Create_mappings_in_child()
	call s:Create_syntax_in_child()
	" Note: This will also be called in child BufEnter, but that call may have
	" been skipped due to :noauto.
	call s:Configure_cursor_in_child()
	" TODO: Decide whether separate function should be used for options.
	" Rationale: Tree display falls apart if 'wrap' on
	setl nowrap
endfu

fu! s:Is_child_configured()
	if s:undo_bufnr > 0
		if exists('#undo_tree_child#BufEnter#<buffer=' . s:undo_bufnr .'>')
			" Child hasn't been unconfigured.
			return 1
		endif
	endif
	return 0
endfu

" Hide any open child buffers.
fu! s:Delete_children()
	" Note: Intentionally deleting autocmds/maps.
	exe 'bd ' . s:undo_bufnr
endfu

" Hide all occurrences of child buffer in current tab.
" Assumption: Caller is about to show child in deterministic location, so this
" function must take care not to trigger BufWinLeave.
" TODO: Consider making this a 'hide all child bufs but one in specific window'
" so we could run it *after* positioning the child window, and could drop this
" assumption.
fu! s:Hide_children()
	let winnr = bufwinnr(s:undo_bufnr)
	while winnr > 0
		" Child is in a window. Attempt to hide it, noting that command will
		" fail (harmlessly) if it's the last window.
		" Caveat: 'noauto' modifier needed to prevent BufWinLeave firing for the
		" child.
		" Assumption: The hide is temporary. Caller will bring it back.
		exe 'silent! noauto ' . winnr . 'close'
		let winnr = bufwinnr(s:undo_bufnr)
	endwhile
endfu

" Design Decision: When parent buffer is no longer displayed, unconfigure parent
" and delete child.
" Rationale: Unconfiguring parent simplifies logic by avoiding need to make
" child smart about resurrecting parent in proper position, and deleting child
" gets rid of autocmds and such that might otherwise be problematic in absence
" of parent. (Note that the child's bufnr will still exist, and it can still be
" resurrected on next undo buffer open request.)
" Called From: parent buffer BufWinLeave autocmd
" Caveat: bnr is input because bufnr('%') is not guaranteed to work at this
" point.
fu! s:Parent_BufWinLeave(bnr)
	call s:Unconfigure_parent()
	if s:undo_bufnr > 0 && bufwinnr(s:undo_bufnr) > 0
		" Since we know we want to close child buffer, go ahead and empty it.
		" Rationale: In case our deferred delete of child fails, at least the
		" window will be empty.
		" TODO: Consider putting this in Unconfigure_child.
		silent %d
		" Also unconfigure child so we don't need to worry about it's autocmds
		" triggering again.
		" Rationale: Autocmds will be removed in deferred Delete_children, but
		" this eliminates race-condition.
		call s:Unconfigure_child()
		" Queue deletion of child buffer(s).
		call s:Defer_action('Delete_children')
	endif
endfu
fu! s:Child_BufWinLeave(bnr)
	silent %d
	call s:Unconfigure_child()
endfu

" Called From: child buffer BufDelete autocmd
" Note: User shouldn't typically delete the child manually, but if he does, we
" will lose the autocmds in the child buffer, so we need to ensure that a new
" one is created next time one is needed.
fu! s:Child_BufDelete(bnr)
	" Design Decision: No need to unparent.
	" Rationale: If user opens undo window on same parent again, we'll create a
	" new undo buf automatically, but the cache may still be valid.
	let s:undo_bufnr = -1
endfu

" Each time child buf is entered, we need to ensure it's still valid.
fu! s:Child_BufEnter()
	let p_wnr = s:bufnr > 0 ? bufwinnr(s:bufnr) : -1
	" Caveat: BufEnter will fire when parent is in process of deletion, but in
	" this case, parent will not have a window, and the child buffer will be
	" queued for hiding.
	if p_wnr > 0
		" Invoke non-forced refresh.
		call s:Refresh_undo_window(0)
		call s:Configure_cursor_in_child()
	endif
endfu

fu! s:Child_BufLeave()
	call s:Unconfigure_cursor_in_child()
endfu

" Assumption: Called from parent.
" Note: Always end up in child window.
fu! s:Position_child(clear)
	" TODO: More sophisticated and configurable algorithm for calculating child
	" window height. For now, no smaller than a configurable min, but no bigger
	" than parent.
	let min_child_height = 10
	" Expand the parent in proper direction
	" TODO: Introduce option to allow either horiz or vert split. For now, just
	" do horiz...
	wincmd _
	let wh = winheight(0)
	" Note: 'silent' prevents annoying and misleading [new file] msg.
	let cmd = ' +setl\ buftype=nofile\ bufhidden=hide\ noswapfile '
	let p_wnr = winnr()
	" Save viewport so we can restore after opening (or re-opening) child.
	" Rationale: Opening child window can effectively scroll the parent.
	let wsv = winsaveview()
	if s:undo_bufnr < 0
		" Create new window containing scratch buffer.
		" TODO: Any advantage to using +cmd arg like this?
		" Caveat: Wanted to wrap name in [...], but that makes Vim think it's
		" directory.
		exe 'silent belowright new '
			\ . cmd
			\ . expand('%') . '.undo'
		let s:undo_bufnr = bufnr('%')
	else
		" Note: 'noauto' modifier prevents premature attempt to refresh.
		" TODO: Take split direction into account.
		exe 'silent noauto belowright sb'
			\ . cmd
			\ . s:undo_bufnr
		if a:clear
			silent %d
		endif
	endif
	" Make sur the parent's viewport hasn't changed.
	let c_wnr = winnr()
	call s:Goto_win(p_wnr)
	call winrestview(wsv)
	call s:Goto_win(c_wnr)
	" We're in child buffer. Give it proper size.
	exe min([max([(wh / 2), min_child_height]), wh]) . 'wincmd _'

endfu

" Make sure an undo_buf exists, is visible, and is properly configured, creating
" if necessary.
" Assumption: In parent buffer
fu! s:Prepare_child(clear)
	" Hide (don't delete) any existing children.
	" Rationale: Facilitates deterministic window positioning.
	call s:Hide_children()
	call s:Position_child(a:clear)
	" Note: We call this even when buf already existed, to ensure it's got all
	" the mappings and autocmds we'll need. (It's unlikely, but possible, that
	" someone deleted maps/autocmds, for instance.)
	call s:Configure_child()
endfu

fu! s:Describe_node(...)
	" If user didn't pass a seq number, describe current node.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	let t = s:undo_cache.geom.geom.nodes[seq].node
	if t.seq == 0
		echo "At root of undo tree"
	else
		echo printf("State #%d: %s", t.seq, strftime("%c", t.time))
	endif
endfu

" Return object with following format:
"   visible: 1 if entire node is visible
"   dy: # of rows by which to shift view down to center
"   dx: # of cols by which to shift view right to center
fu! s:Calculate_tree_node_offset(...)
	let ret = {'visible': 1}
	" If user didn't pass a seq number, center on current.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	" Determine current horiz/vert view scroll amounts.
	let [vscroll, hscroll] = [line('.') - winline(), col('.') - wincol()]
	let [wh, ww] = [winheight(0), winwidth(0)]
	" Get the geom node corresponding to desired center.
	let gnode = s:undo_cache.geom.geom.nodes[seq]
	let [t, row, col] = [gnode.node, gnode.row, gnode.col]
	" Calculate 1-based offset from edge of window.
	let [row_rel, col_rel] = [row - vscroll, col - hscroll]
	" Check edges of node to see whether any part is not visible.
	let gi = t.Get_geom()
	" Note: +1 needed to ensure we center on middle of odd number of rows.
	if row_rel < 1 || row_rel > wh ||
		\ col_rel + gi.e[0] < 1 || col_rel + gi.e[1] > ww
		let ret.visible = 0
	endif
	" Calculate delta: pos delta => scroll view right/down to center.
	let ret.dy = row_rel - (wh + 1) / 2
	let ret.dx = col_rel - (ww + 1) / 2
	return ret
endfu

" If 'force' flag is true or requested node is even partially outside view,
" center tree on current node (default) or node whose seq is input.
" Assumption: In child buffer with consistent cache
fu! s:Center_tree(force, ...)
	" If user didn't pass a seq number, center on current.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	let oi = s:Calculate_tree_node_offset(seq)
	if !a:force && oi.visible
		" Not forcing and node is already fully visible.
		return
	endif
	" Perform required horiz/vert shifts (delta == 0 => no shift).
	if oi.dx
		exe 'normal! ' . abs(oi.dx) . (oi.dx > 0 ? 'zl' : 'zh')
	endif
	if oi.dy
		exe 'normal! ' . abs(oi.dy) . (oi.dy > 0 ? "\<C-E>" : "\<C-Y>")
	endif
endfu

" Called From: child buffer maps for moving in tree.
" Assumption: Can be invoked only when fresh undo data structures exist.
" Rationale: Invoked from undo buffer mappings, and freshness is checked in
" BufEnter. Hmm... What if user executes a :bd or something. Well, in that case,
" parent's BufDelete would fire, clearing data structures.
fu! s:Move_in_tree(dir) " entry
	"let [p_bnr, p_wnr] = s:Get_parent_bufwin()
	if s:bufnr < 0 || bufwinnr(s:bufnr) < 0
		echomsg "Warning: Cannot traverse undo tree for inactive parent buffer"
	endif
	" Grab tree and invoke specified movement method.
	" TODO: For aesthetics, make the method names capitalized.
	call s:undo_cache.tree[a:dir]()
	call s:Goto_win(bufwinnr(s:bufnr))
	" Note: Execute undo silently to avoid a hit-enter prompt.
	silent exe 'undo ' . s:undo_cache.tree.cur.seq
	" Return to child to update display.
	" Caveat: Don't trigger BufEnter
	noauto wincmd p
	" Update display.
	call s:undo_cache.syn.Update(s:undo_cache.tree.cur)
	" Center iff current node is not fully visible.
	call s:Center_tree(0)
	" Output summary of current change.
	call s:Describe_node()
endfu

" Invoked from child's <buffer> map.
" Assumption: Can't get here unless we're in true child buffer.
fu! s:Refresh_child() " entry
	" Make sure parent is still loaded and visible.
	" Note: Shouldn't get here if not, but it's possible (e.g., if user
	" intentionally opens unlisted undo buffer).
	" TODO: Consider removing the child autocmds and mappings when the child is
	" hidden. Could populate them only when undo buffer is opened.
	if s:bufnr < 0 || bufwinnr(s:bufnr) < 0
		echomsg "Warning: Cannot refresh undo buffer with no associated parent."
		return
	endif
	" There's a visible and active parent. Forcibly refresh
	let s:undo_cache = {}
	call s:Refresh_undo_window(1)
endfu

" This one's tied to mapping or command for opening undo on current buffer.
fu! s:Open_undo_window() " entry
	if !s:Is_valid_parent()
		echoerr "Can't display undo tree for this buffer."
		return
	endif
	" Cancel any pending delete of child buffer we're about to re-use.
	call s:Cancel_action('Delete_children')
	let p_bnr = bufnr('%')
	let contents_invalid = 0
	if s:bufnr != p_bnr
		" New parent (either no current parent or parent changing)
		if s:bufnr > 0
			" We have existing parent. Unconfigure old.
			call s:Unconfigure_parent()
		endif
		let s:bufnr = p_bnr
		call s:Configure_parent()
		" Request clearance (or creation) of undo buffer.
		let contents_invalid = 1
	else
		" Find out whether it's even possible that child's contents are valid.
		let contents_invalid = !s:Is_child_configured()
	endif
	call s:Prepare_child(contents_invalid)
	" Everything should be in order now with the 2 windows.
	call s:Refresh_undo_window(contents_invalid)
endfu

" Called From: One of the following...
" 1) undo buffer 'refresh' mapping
" 2) global mapping/command requesting undo window for parent
" 3) child BufEnter autocmd
" Assumption: Both parent and child windows are visible and data structures are
" consistent.
fu! s:Refresh_undo_window(contents_invalid)
	if s:bufnr < 0 || bufwinnr(s:bufnr) < 0
		" Note: This generally wouldn't happen because we delete the child (and
		" therefore its autocmds) when parent is removed from last window;
		" however, because the child deletion is deferred, it's theoretically
		" possible to get here.
		" TODO: Consider unconfiguring the child when we schedule the child
		" deletion. If we do that, we can remove this check.
		echomsg "Warning: Cannot refresh undo buffer with no associated parent."
		return
	endif
	" Make sure we're in parent buffer.
	call s:Goto_win(bufwinnr(s:bufnr))
	" Get Vim's undo tree to support freshness check, and if necessary, to serve
	" as basis of new tree.
	" TODO: Could avoid checking undotree() in some cases if we monitored
	" BufEnter on the parent.
	" Rationale: Buffer can be changed only when user visits it.
	let v_ut = undotree()
	" We want to move to the window even if it's up to date.
	call s:Goto_win(bufwinnr(s:undo_bufnr))

	let contents_invalid = a:contents_invalid
	" Do we need to update the tree or is cache still valid?
	if empty(s:undo_cache) || s:undo_cache.tree.meta.seq_last != v_ut.seq_last
		let contents_invalid = 1
		let tree = s:Make_undo_tree(v_ut)
		" TODO: Thinking I may no longer need tree returned, now that positions are
		" stored on b:undo_tree.
		let [ptree, extent] = s:Design(tree)
		let geom = s:Build_tree_display(tree, extent)
		" Now build highlighting object.
		let syn = s:Make_syn_tree(geom.geom)
		" Cache what we've built.
		let s:undo_cache = {
			\ 'tree': tree,
			\ 'geom': geom,
			\ 'syn': syn
		\ }
		let g:uc = s:undo_cache
	endif
	" Regardless of whether undo cache was valid, buffer contents may have been
	" clobbered (e.g., by user deletion).
	if contents_invalid
		" Replace undo buffer's contents with new tree.
		%d
		call append(0, s:undo_cache.geom.lines)
		" Now do highlighting
		call s:undo_cache.syn.Clear()
		let uc = s:undo_cache
		let cur = uc.tree.cur
		call s:undo_cache.syn.Update(s:undo_cache.tree, s:undo_cache.tree.cur.seq)
	endif

endfu

" Global mappings
nnoremap <silent> <leader>u :call <SID>Open_undo_window()<CR>

fu! s:Test_only()
	let xs = S_Cons("baz", {})
	let xs = S_Cons("bar", xs)
	let xs = S_Cons("foo", xs)
	let xs_upper = S_Map(function('toupper'), xs)
	let xs_rev = S_Reverse(xs)
	let xs_zipped = S_Zip(xs_upper, xs_rev)
	let xs_zswapped = S_Map(function('s:Swap_tuple_fn'), xs_zipped)
	let xs_unzipped = S_Unzip(xs_zswapped)
	let es = [[
			\ [1, 3], [-1, 4], [8, 10]], [
			\ [4, 5], [5, 8],  [10, 14], [9, 11]], [
			\ [6, 9], [8, 9],  [15, 19], [11, 16]]]
	let es = map(es, 'S_To_list(v:val)')
	let em = S_Merge(es[0], es[1])
	let em = S_Merge(em, es[2])
	let em2 = s:Merge_list(es)
	let em_moved = s:Move_extent(em2, 100)
	let es2 = [[
			\ [-3, 3], [-4, 5], [-7, 6]], [
			\ [-2, 5], [-1, 6],  [-5, 14], [-3, 11]], [
			\ [-5, 4], [-4, 5],  [-2, 15], [-1, 16]]]
	let es2 = map(es2, 'S_To_list(v:val)')
	let e_fit01 = s:Fit(es2[0], es2[1])
	let e_fit12 = s:Fit(es2[1], es2[2])
	let e_fitlistl = s:Fitlistl(es2)
	let e_fitlistr = s:Fitlistr(es2)
	let e_fitlist = s:Fitlist(es2)

	echo "xs: " . string(xs)
	echo "xs_upper: " . string(xs_upper)
	echo "xs_rev: " . string(xs_rev)
	echo "xs_zipped: " . string(xs_zipped)
	echo "xs_zswapped: " . string(xs_zswapped)
	echo "xs_unzipped: " . string(xs_unzipped)
	echo "em: " . string(em)
	echo "em2: " . string(em2)
	echo "em_moved: " . string(em_moved)
	echo "e_fit01: " . string(e_fit01)
	echo "e_fit12: " . string(e_fit12)
	echo "e_fitlistl: " . string(e_fitlistl)
	echo "e_fitlistr: " . string(e_fitlistr)
	echo "e_fitlist: " . string(e_fitlist)

endfu
" vim:ts=4:sw=4:tw=80

