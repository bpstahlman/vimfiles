
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
	" This will be set later in build traversal unless cur is root, so go ahead
	" and initialize to root.
	let ret.cur = ret
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
		let lvl = 0
	else
		let [entries, parent, root, lvl] = a:000
	endif

	let ret = {}
	for e in entries
		let o = s:Make_node(e, parent, lvl)
		if o.seq == root.meta.seq_cur
			" Store a ptr to the current node on the root.
			" Note: This is what will be manipulated henceforth.
			let root.cur = o
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
" Assumption: Input node is valid. (Note that root node can never be
" invalidated.)
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
		call s:Update_syn_tree_node(self.ids, t, lvl, col)
		let [t, colp] = [t.children.cur, col]
		let lvl += 1
	endwhile
endfu
fu! s:Make_syn_tree(geom)
	let me = {
		\ 'ids': [],
		\ 'Update': function('s:Update_syn_tree'),
		\ 'geom': a:geom
		\ }

	return me
endfu
fu! s:Handles_from_parent()
	" TODO: Perhaps some additional checks.
	" Buffer looks like buffer that could have undo buffer.
	let p_bnr = bufnr("%")
	let c_bnr = has_key(s:parent_to_child, p_bnr) ? s:parent_to_child[p_bnr] : -1
	return [p_bnr, c_bnr]
endfu
fu! s:Handles_from_child()
	if !s:Is_undo_buf()
		" Doesn't look like it could be an undo buffer.
		return [-1, -1]
	endif
	" Buffer looks like undo buffer.
	let c_bnr = bufnr("%")
	let p_bnr = has_key(s:child_to_parent, c_bnr) ? s:child_to_parent[c_bnr] : -1
	if p_bnr == -1
		" Must not really be undo buf.
		return [-1, -1]
	endif
	return [p_bnr, c_bnr]
endfu

fu! s:Is_undo_buf(bnr)
	let [bt, bh, sf] = map(['&buftype', '&bufhidden', '&swapfile'],
		\ 'getbufvar(a:bnr, v:val)')
	return bt == 'nofile' && bh == 'hide' && !sf
endfu
fu! s:Goto_win(winnr)
	exe a:winnr . "wincmd \<C-W>"
endfu
" Assumption: Window containing the buffer is visible.
fu! s:Goto_visible_buf(bufnr)
	call s:Goto_win(winbufnr(a:bufnr))
endfu
" Called From: parent buffer BufDelete autocmd
" Caveat: bnr is input because bufnr('%') is not guaranteed to work at this
" point.
fu! s:Parent_BufDelete(bnr)
	call s:Dbg(3, "Parent_BufDelete on buffer %d", a:bnr)
	if has_key(s:parent_to_child, a:bnr)
		let c_bnr = s:parent_to_child[a:bnr]
		" Remove child buffer, which has meaning only for lifetime of parent.
		" Double-check to be sure we don't delete something important!
		" Caveat: BufDelete help forbids changing to another buffer.
		if s:Is_undo_buf(c_bnr)
			" Yep. This is really our undo buffer.
			" Note: Intentionally using bd without bang for extra safety.
			" TODO: See whether bwipe would be more in order.
			exe 'bd ' . c_bnr
			call remove(s:parent_to_child, a:bnr)
			call remove(s:undo_infos, a:bnr)
			call remove(s:child_to_parent, c_bnr)
		else
			" TODO: Rework this.
			echoerr "Internal error! Refusing to delete undo buffer " . c_bnr
				\ . " (associated with parent buffer " . p_bnr
				\ . ") because it doesn't look like an undo buffer."
		endif
	endif
endfu
" Called From: child buffer BufDelete autocmd
fu! s:Child_BufDelete(bnr)
	call s:Dbg(3, "Child_BufDelete on buffer %d", a:bnr)
	if has_key(s:child_to_parent, a:bnr)
		let p_bnr = s:child_to_parent[a:bnr]
		call remove(s:parent_to_child, p_bnr)
		call remove(s:undo_infos, p_bnr)
		call remove(s:child_to_parent, a:bnr)
	endif
endfu
" Note: This is the actual BufEnter handler.
fu! s:Child_CursorHold()
	if exists('s:updatetime_save')
		" Restore the original time.
		let &updatetime = s:updatetime_save
		unlet! s:updatetime_save
		" Invoke non-forced refresh.
		call s:Refresh_undo_window(0)
	endif
endfu
" Called From: child buffer BufEnter autocmd
fu! s:Child_BufEnter()
	call s:Dbg(3, "Child_BufEnter on buffer %d", bufnr('%'))
	" Save updatetime so we can restore in CursorHold handler.
	let s:updatetime_save = &updatetime
	set updatetime=1
endfu

fu! s:Create_autocmds_in_child()
	" Create autocmds that will permit us to clean up when child buf deleted
	aug undo_tree_child
		au!
		au BufDelete <buffer> call s:Child_BufDelete(expand("<abuf>"))
		au BufEnter <buffer> call s:Child_BufEnter()
		au CursorHold <buffer> call s:Child_CursorHold()
	augroup END
endfu
fu! s:Create_autocmds_in_parent()
	" Create autocmds that will permit us to clean up when parent buf deleted
	aug undo_tree_parent
		au!
		au BufDelete <buffer> call s:Parent_BufDelete(expand("<abuf>"))
	augroup END
endfu
fu! s:Create_mappings_in_child()
	" Create buffer-local mapping(s)
	" To refresh the tree forcibly
	nmap <buffer> <leader>r :call <SID>Refresh_undo_window(1)

	" Moving up/down and changing undo/redo path through tree.
	" Design Decision: No reason to avoid using regular Vim motion commands.
	" Rationale: Cursor movement is highly constrained.
	nmap <buffer> k :call <SID>Move_in_tree('up')<CR>
	nmap <buffer> j :call <SID>Move_in_tree('down')<CR>
	nmap <buffer> h :call <SID>Move_in_tree('left')<CR>
	nmap <buffer> l :call <SID>Move_in_tree('right')<CR>
endfu
fu! s:Create_syntax_in_child()
	hi undo_redo_path gui=bold cterm=bold term=bold
endfu

" Called From: child buffer maps for moving in tree.
" Assumption: Can be invoked only when fresh undo data structures exist.
" Rationale: Invoked from undo buffer mappings, and freshness is checked in
" BufEnter.
fu! s:Move_in_tree(dir)
	let p_bnr = s:Ensure_parent_visible()
	" Grab tree and invoke specified movement method.
	call s:undo_infos[p_bnr].tree[a:dir]()
	" Return to child.
	wincmd p
	" TODO: Update display.
endfu

" Called From: parent buffer that does not yet have a child buffer.
" Pre Constraint: In buffer requiring the undo buf.
fu! s:Create_undo_buf()
	let p_bnr = bufnr("%")
	call s:Create_autocmds_in_parent()
	" Create new window containing scratch buffer.
	" TODO: Any advantage to using +cmd arg like this?
	" Caveat: Wanted to wrap name in [...], but that makes Vim think it's
	" directory.
	exe 'belowright new'
		\ . ' +setl\ buftype=nofile\ bufhidden=hide\ noswapfile'
		\ . ' ' . expand('%') . '.undo'
	let c_bnr = bufnr('%')
	call s:Create_autocmds_in_child()
	call s:Create_mappings_in_child()
	call s:Create_syntax_in_child()
	" Add data for this undo buffer to script-local vars, keyed by parent
	let s:parent_to_child[p_bnr] = c_bnr
	let s:undo_infos[p_bnr] = {}
	let s:child_to_parent[c_bnr] = p_bnr
endfu

" Called From: buffer being considered as candidate parent
fu! s:Is_valid_parent()
	" TODO More checks? E.g., 'buftype', etc...?
	" Question: Any way to use undotree() for this test? E.g., do nofile buffers
	" have undo trees?
	return &modifiable
endfu
" This one's tied to mapping or command for opening undo on current buffer.
fu! s:Open_undo_window()
	let [p_bnr, c_bnr] = s:Handles_from_parent()
	if c_bnr < 0
		" Have never used plugin on this buffer. Make sure it looks like
		" something that can have an undo buffer.
		if !s:Is_valid_parent()
			echoerr "Can't display undo tree for this buffer."
			return
		endif
		call s:Create_undo_buf()
	else
		" Already have entries for this one.
		" Ensure undo buffer is current and visible.
		call s:Ensure_child_visible()
	endif
	call s:Refresh_undo_window(0)
endfu

" Assumption: Called from child after verifying existence.
" Note: Always end up in parent window.
" Return: parent bufnr (as convenience)
fu! s:Ensure_parent_visible()
	let c_bnr = bufnr('%')
	let p_bnr = s:child_to_parent[c_bnr]
	let winnr = winbufnr(p_bnr)
	if winnr < 0
		exe 'aboveleft sb ' . p_bnr
	else
		call s:Goto_win(winnr)
	endif
	return bufnr('%')
endfu
" Assumption: Called from parent after verifying existence.
" Note: Always end up in child window.
" Return: child bufnr (as convenience)
fu! s:Ensure_child_visible()
	let p_bnr = bufnr('%')
	let c_bnr = s:parent_to_child[p_bnr]
	" TODO: Consider using temporary override of switchbuf=useopen and :sbuffer
	" to do this more simply.
	let winnr = winbufnr(c_bnr)
	if winnr < 0
		" TODO: Perhaps combine this somehow with creation to ensure similar
		" sizing/positioning.
		exe 'belowright sb ' . c_bnr
	else
		call s:Goto_win(winnr)
	endif
	return bufnr('%')
endfu
" Called From: One of the following
" 1) undo buffer mapping (force=1)
" 2) global mapping/command requesting undo window for parent (force=0)
" 3) child BufEnter autocmd (force=0)
" Assumption: Calling context ensures buffer-specific data structures exist.
" Rationale: Called either from child buffer mapping or global mapping/command
" function after parent-child link has been validated.)
fu! s:Refresh_undo_window(force)
	let c_bnr = bufnr('%')
	let p_bnr = s:child_to_parent[c_bnr]
	let cache = s:undo_infos[p_bnr]
	" TODO: Currently, it's not always the case that parent buffer is visible,
	" or even exists. (Can get here on BufEnter when we're entering only because
	" parent is going away.)
	call s:Goto_visible_buf(p_bnr)
	" Get Vim's undo tree to support freshness check, and if necessary, to serve
	" as basis of new tree.
	let v_ut = undotree()
	call s:Goto_visible_buf(c_bnr)

	if a:force || empty(cache) || cache.tree.meta.seq_last != v_ut.seq_last
		let tree = s:Make_undo_tree(v_ut)
		" TODO: Thinking I may no longer need tree returned, now that positions are
		" stored on b:undo_tree.
		let [ptree, extent] = s:Design(tree)
		let di = s:Build_tree_display(tree, extent)
		" Build and store new cache.
		" Replace undo buffer's contents with new tree.
		%d
		call append(0, di.lines)
		" Now do highlighting
		let syn = s:Make_syn_tree(di.geom)
		call syn.Update(tree)
		let s:undo_infos[p_bnr] = {
			\ 'tree': tree,
			\ 'geom': di.geom,
			\ 'syn': syn
		\ }
	endif
endfu

" Global mappings
nnoremap <leader>u :call <SID>Open_undo_window()<CR>

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

