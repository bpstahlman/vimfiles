
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

" Return the leaf node at end of current redo path.
fu! s:Node_get_leaf() dict
	let t = self
	while !empty(t.children.cur)
		let t = t.children.cur
	endwhile
	return t
endfu


" TODO: Add arg that selects between detailed and non-detailed display.
fu! s:Node_get_geom(detailed) dict
	" Note: Add the surrounding spaces, which will be converted to brackets upon
	" node selection.
	let text = a:detailed
		\ ? printf(" %d @ %c ", self.seq, self.time)
		\ : (' ' . self.seq . ' ')
	let w = len(text)
	" Design Decision: The -1 ensures that when len is even, we center on
	" the last char in the left half, not first in right.
	let xs = -(w - 1) / 2
	let xe = xs + w - 1
	return {'w': w, 'e': [xs, xe], 'text': text}
endfu

" Make this node the current one in the tree.
" Logic: Traverse upwards to root, adjusting each parent's children.cur pointer
" to point to the child in the traversal, and ultimately, adjusting root's
" special cur pointer to point to self.
" Implementation Note: Implementing non-recursively, which means self will
" always point to the original invocant node.
fu! s:Node_make_current() dict
	let [t, tp] = [self.parent, self]
	while !empty(t)
		let t.children.cur = tp
		let [t, tp] = [t.parent, t]
	endwhile
	" Hit root (tp == root).
	let tp.cur = self
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
		\,'Get_leaf': function('s:Node_get_leaf')
		\,'Get_geom': function('s:Node_get_geom')
		\,'Make_current': function('s:Node_make_current')
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
	let ret.Get_leaf = function('s:Node_get_leaf')
	let ret.Get_geom = function('s:Node_get_geom')
	let ret.Make_current = function('s:Node_make_current')
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
fu! s:Cons(val, xs)
	return {'el': a:val, 'next': a:xs}
endfu
" Note: Best when you maintain pointer to final element of list.
fu! s:Conc(val, xs)
	let x = a:xs
	while !empty(x)
		let x = x.next
	endwhile
	" Modify the (previously empty) tail in place.
	let [x.el, x.next] = [a:val, {}]
	" Return new tail el
	return x
endfu
fu! s:Set_cdr(val, cons)
	let a:cons.next = {'el': a:val, 'next': {}}
	return a:cons.next
endfu
fu! s:Foldl(f, acc, xs)
	" Note: Take type of xs into account, handling both arrays and lists.
	" TODO: More efficient way than converting to list.
	let [x, acc] = [type(a:xs) == 3 ? s:To_list(a:xs) : a:xs, a:acc]
	while !empty(x)
		let acc = a:f(acc, x.el)
		let x = x.next
	endwhile
	return acc
endfu
fu! s:To_list(ary)
	let ret = {}
	let tail = ret
	for x in a:ary
		let tail = s:Conc(x, tail)
	endfor
	return ret
endfu
fu! s:Reverse(xs)
	let [x, ret] = [a:xs, {}]
	while !empty(x)
		let ret = s:Cons(x.el, ret)
		let x = x.next
	endwhile
	return ret
endfu
fu! s:Map(f, xs)
	let [x, ret] = [a:xs, {}]
	" Note: tail will advance, ret will not.
	let tail = ret
	while !empty(x)
		let val = call(a:f, [x.el])
		" Note: Something tricky's going on here: ret and tail will be
		" equivalent after first iteration only.
		let tail = s:Conc(val, tail)
		let x = x.next
	endwhile
	return ret
endfu
fu! s:Zip(xs, ys, ...)
	let [x, y, ret] = [a:xs, a:ys, {}]
	let discard_unmatched = a:0 && a:1
	let tail = ret
	while !empty(x) || !empty(y)
		if discard_unmatched && (empty(x) || empty(y))
			" Special case: Certain applications don't need unmatched elements.
			break
		endif
		let tail = s:Conc([empty(x) ? {} : x.el, empty(y) ? {} : y.el], tail)
		if !empty(x) | let x = x.next | endif
		if !empty(y) | let y = y.next | endif
	endwhile
	return ret
endfu
fu! s:Unzip(xs)
	let [x, ret] = [a:xs, [{}, {}]]
	let tail = [ret[0], ret[1]]
	while !empty(x)
		let tail[0] = s:Conc(x.el[0], tail[0])
		let tail[1] = s:Conc(x.el[1], tail[1])
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

" Note: Meant to be used with s:Map
fu! s:Move_extent_fn(dx, e_el)
	return [a:e_el[0] + a:dx,  a:e_el[1] + a:dx]
endfu
fu! s:Move_extent(arg, ...)
	" Args: e, dx - may be passed either as regular args, or in array.
	" contained in an array.
	let [e, dx] = a:0 ? [a:arg, a:1] : [a:arg[0], a:arg[1]]
	" Create a partial that passes dx as first arg.
	return s:Map(function('s:Move_extent_fn', [dx]), e)
endfu

fu! s:Merge_fn(e_pair)
	return [
		\ empty(a:e_pair[0]) ? a:e_pair[1][0] : a:e_pair[0][0],
		\ empty(a:e_pair[1]) ? a:e_pair[0][1] : a:e_pair[1][1]]
endfu
fu! s:Merge(e1, e2)
	return s:Map(function('s:Merge_fn'), s:Zip(a:e1, a:e2))
endfu

fu! s:Merge_list(es)
	return s:Foldl(function('s:Merge'), {}, a:es)
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
	return s:Foldl(function('s:Fit_fn'), 0, s:Zip(a:e1, a:e2, 1))
endfu
" Caveat: Vim doesn't do TCO, so implement both left and right folds without
" recursion.
fu! s:Fitlistl_fn(acc, e)
	let [tail, acc] = a:acc
	let dx = s:Fit(acc, a:e)
	let tail = s:Conc(dx, tail)
	let acc = s:Merge(acc, s:Move_extent(a:e, dx))
	return [tail, acc]
endfu
fu! s:Fitlistl(es)
	let ret = {}
	let tail = ret
	let es = type(a:es) == 3 ? s:To_list(a:es) : a:es
	call s:Foldl(function('s:Fitlistl_fn'), [tail, {}], es)
	return ret
endfu
fu! s:Negate_fn(val)
	return -a:val
endfu
fu! s:Flip_extent_fn(e_el)
	return [-a:e_el[1], -a:e_el[0]]
endfu
fu! s:Flip_extent(e)
	return s:Map(function('s:Flip_extent_fn'), a:e)
endfu

" TODO: Consider whether it's better to implement in terms of Fitlistl, or to
" implement Fitlistr independently.
" TODO: Pick up here... Still implement in terms of Fitlistl?
" TODO: Test this one...
fu! s:Fitlistr(es)
	let es = type(a:es) == 3 ? s:To_list(a:es) : a:es
	let es = s:Reverse(es)
	let es = s:Map(function('s:Flip_extent'), es)
	let es = s:Fitlistl(es)
	let es = s:Map(function('s:Negate_fn'), es)
	let es = s:Reverse(es)
	return es
endfu

fu! s:Mean_fn(pair)
	let num = a:pair[0] + a:pair[1]
	if num % 2
		" Caveat: Effectively round down to prevent integer truncation towards 0
		" Rationale: Truncation impacts fitlistl and fitlistr lists differently,
		" resulting in nodes being positioned too close together.
		" TODO: Decide whether to round down or up.
		let num -= 1
	endif
	" Assumption: num is even
	return num / 2
endfu

fu! s:Fitlist(es)
	" Note: Avoid extra function call to s:Mean.
	return s:Map(function('s:Mean_fn'), s:Zip(s:Fitlistl(a:es), s:Fitlistr(a:es)))
endfu

" Modify list of child positions in place to ensure that no child is offset
" exactly 1 unit from its parent. If a child must be moved 1 unit left or right,
" move all siblings to its outside by the same amount.
fu! s:Optimize_child_positions(positions)
	let [p, p1, p2] = [a:positions, {}, {}]
	while !empty(p)
		let x = p.el
		if x > 1
			break
		elseif x == -1
			let p1 = p
		elseif x == 1
			let p2 = p
			break
		endif
		let p = p.next
	endwhile
	if !empty(p1)
		" Adjust from beginning to p1 (inclusive)
		let p = a:positions
		while !empty(p)
			let p.el -= 1
			if p is p1 | break | endif
			let p = p.next
		endwhile
	endif
	if !empty(p2)
		" Adjust from p2 to the end
		let p = p2
		while !empty(p)
			let p.el += 1
			let p = p.next
		endwhile
	endif
endfu

fu! s:Design(t, detailed)
	" Walk the children.
	let [trees, extents] = [{}, {}]
	let [trees_tail, extents_tail] = [trees, extents]
	let tc = a:t.children.fst
	while !empty(tc)
		let [tree, extent] = s:Design(tc, a:detailed)
		let trees_tail = s:Conc(tree, trees_tail)
		let extents_tail = s:Conc(extent, extents_tail)
		let tc = tc.next
	endwhile
	let positions = s:Fitlist(extents)
	" Modify list in place to ensure that no child is offset exactly 1 col from
	" parent.
	" Rationale: Prevents annoying visual disturbance in tree.
	call s:Optimize_child_positions(positions)
	let ptrees = s:Map(function('s:Move_tree_fn'), s:Zip(trees, positions))
	let pextents = s:Map(function('s:Move_extent'), s:Zip(extents, positions))
	let gi = a:t.Get_geom(a:detailed)
	let resultextent = s:Cons(gi.e, s:Merge_list(pextents))
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

" Special function used to generate accumulator in a fold over a list of
" extents.
" Accumulator is a list of 2 elements:
"   largest negative offset
"   largest positive offset
" Note: The result may be used to calculate bias and overall tree width.
fu! s:Extent_minmax_fn(acc, e_el)
	let [min, max] = [a:e_el[0], a:e_el[1]]
	return [
		\ min < a:acc[0] ? min : a:acc[0],
		\ max > a:acc[1] ? max : a:acc[1]]
endfu

" TODO: Pull generation of regexes into this function.
fu! s:Build_tree_display(tree, extent, detailed)
	" Tree is centered at 0, but we need its left edge at 0. Determine the bias,
	" and while we're at it, the width, both of which will be stored on the geom
	" object we create).
	" Note: Can't really apply the bias in the tree itself since the x values in
	" tree are relative to parent. We *could* store absolute positions in the
	" tree, but I don't really like that.
	let [xmin, xmax] = s:Foldl(function('s:Extent_minmax_fn'), [0, 0], a:extent)
	let [x, w] = [abs(xmin), xmax - xmin + 1]
	" Breadth-first traversal
	" Fifo elements: [<node>, <absolute-parent_x>, <lvl>]
	let fifo = [[a:tree, x, 0]]
	" TODO: Make this part of some sort of global config.
	let rows_per_lvl = 3
	let lines = s:Make_gridlines()
	" Hash geom info by node id (seq number)
	let nodes = {}
	" Note: lines will be added later.
	let ret = {'x_bias': x, 'width': w, 'nodes': nodes, 'lines': []}
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
		" TODO: text should also be returned by Get_geom, and we need to pass
		" arg indicating desired display mode (e.g., detailed or short).
		let gi = t.Get_geom(a:detailed)
		let text_x = x + gi.e[0]
		call lines.add(lrow, text_x, gi.text)
		" Draw lower vertical for all but root.
		if !empty(t.parent)
			if t.x < 0
				let [off, text] = [1, '/']
			elseif t.x > 0
				let [off, text] = [-1, '\']
			else
				let [off, text] = [0, '|']
			endif
			if abs(t.x) == 1
				" Can we shift the / or \ outward by 1 without visual annoyance?
				" TODO: Decide whether to shift only for multi-digit seq
				" numbers. Eventually, will probably put a space of padding on
				" odd-width extents so that all can be shifted.
				"if gi.w > 3
					let off += t.x
				"endif
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
					let text_x = parent_x
					let text = '|' . repeat('_', xe - 2)
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
" TODO: Let this be based entirely on lvl, with the regexes pulled from geom
" cache where they were stored by Build_tree_display.
fu! s:Update_syn_tree_node(ids, node, lvl, col, detailed)
	let rows_per_lvl = 3
	let row = a:lvl * rows_per_lvl + 1
	let gi = a:node.Get_geom(a:detailed)
	let re = s:Make_regex('[[(]\?[0-9]\+[])]\?', [row, a:col + gi.e[0]], gi.w)
	" TODO: Don't hardcode the priorities like this.
	" Note: Priority 10 is default. It will override hlsearch and such, but
	" should be lowest of the undo-tree groups.
	call add(a:ids, matchadd('undo_redo_path', re, 10))
	if !a:lvl
		" Nothing else to do for root.
		return
	endif

	" Vertical or diagonal up to horizontal header bar
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

	" Horizontal header bar
	" Note: repeat() will produce empty string for negative count, and this
	" will happen, for instance, when child is unit distance from parent.
	if a:node.x < 0
		let [off, text] = [2, repeat('_', -a:node.x - 2)]
	elseif a:node.x > 0
		let [off, text] = [-a:node.x + 1, repeat('_', a:node.x - 2)]
	else
		" Child directly under parent => no horizontal
		let [off, text] = [0, '']
	endif
	if !empty(text)
		" We have a piece of horizontal to display.
		let re = s:Make_regex(text, [row - 2, a:col + off])
		call add(a:ids, matchadd('undo_redo_path', re, 10))
	endif

	" Vertical to parent
	let re = s:Make_regex('|', [row - 2, a:col - a:node.x])
	call add(a:ids, matchadd('undo_redo_path', re, 10))

endfu

" Return nonzero iff tree needs to be updated.
" Note: No reason to introduce distinct dirty flag, since Clear() sets seq to
" -1, and Update() sets it to valid seq number.
fu! s:Is_syn_dirty() dict
	return self.seq < 0
endfu

" Assumption: Called from child buffer
fu! s:Clear_syn_tree() dict
	let self.ids = []
	let self.seq = -1
	call clearmatches()
endfu

" Add or erase brackets around input node from geom.nodes dictionary.
" gnode: {'node': {}, 'col': <colnr>, 'row': <rownr>}
" TODO: Make more generic by changing 'erase' flag to an arg that indicates the
" wrap char desired.
fu! s:Bracket_node(gnode, erase, detailed)
	let [tnode, row, col] = [a:gnode.node, a:gnode.row, a:gnode.col]
	let gi = tnode.Get_geom(a:detailed)
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
	" Note: Positions in self.geom.nodes are 1-based (col nr)
	" Note: If child is root, initialize colp to root's col.
	let colp = self.geom.nodes[(empty(tp) ? t : tp).seq].col
	while !empty(t)
		call add(self.ids, [])
		" Design Decision: Could also lookup x in self.geom.nodes, but this
		" avoids hash lookup.
		let col = colp + t.x
		call s:Update_syn_tree_node(self.ids[-1], t, lvl, col, self.detailed)
		let [t, colp] = [t.children.cur, col]
		let lvl += 1
	endwhile
endfu

fu! s:Get_common_ancestor(t1, t2)
	if empty(a:t1.parent) || empty(a:t2.parent)
		" One of the inputs was root.
		return empty(a:t1.parent) ? a:t1 : a:t2
	endif
	" Logic: Maintain 2 tree pointers, walking them up the tree in leapfrog
	" fashion (always moving the deeper of the two) until the 2 meet at the
	" desired ancestor (which will be root if nothing else).
	let ts = [a:t1, a:t2]
	while 1
		if ts[0] == ts[1]
			" Found common ancestor
			return ts[0]
		endif
		" Determine the deeper pointer and move it up one.
		let i = ts[0].lvl > ts[1].lvl ? 0 : 1
		let ts[i] = ts[i].parent
		" Have we hit root?
		if empty(ts[i].parent)
			" Pointers can no longer converge before root; ignore possibility of
			" internal error, and assume they'll converge at root.
			return ts[i]
		endif
	endwhile
endfu

" Update tree display. The input 'node' is current. Tree will be updated from
" common ancestor of current and old current (if it can be determined), else
" from root.
fu! s:Update_syn(cur) dict
	if self.seq >= 0
		" De-select old
		let old = self.geom.nodes[self.seq]
		call s:Bracket_node(old, 1, self.detailed)
		" Find common ancestor of old and new
		let anc = s:Get_common_ancestor(old.node, a:cur)
	else
		" Update from root.
		let anc = self.geom.nodes[0].node
	endif
	" TODO: Using call() so that I can keep s:Update_syn_tree as method without
	" adding it to public interface. Look for better way...
	call call('s:Update_syn_tree', [anc], self)
	" Select new
	call s:Bracket_node(self.geom.nodes[a:cur.seq], 0, self.detailed)
	" Cache new
	let self.seq = a:cur.seq
endfu

fu! s:Make_syn_tree(geom)
	let me = {
		\ 'ids': [],
		\ 'seq': -1,
		\ 'Clear': function('s:Clear_syn_tree'),
		\ 'Is_dirty': function('s:Is_syn_dirty'),
		\ 'Update': function('s:Update_syn'),
		\ 'geom': a:geom.Get(),
		\ 'detailed': a:geom.Is_detailed()
		\ }

	return me
endfu

" For convenience, ensure these script-locals always exist.
let [s:bufnr, s:winid] = [-1, -1]
let [s:undo_bufnr, s:undo_winid] = [-1, -1]
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
		call call(function('s:' . name), args)
	endfor
	let s:actions = {}
	" Make sure we're not invoked again until something else is queued.
	au! undo_tree_actions CursorHold
	au! undo_tree_actions CursorHoldI
endfu

" Make the current buffer no longer the current parent.
fu! s:Unconfigure_parent()
	" Skip if we've already been unconfigured.
	" Rationale: Make redundant calls (e.g. from different autocmds) harmless.
	if s:winid > 0
		" Remove the parent autocommands.
		au! undo_tree_parent
		let s:bufnr = -1
		let s:winid = -1
		let s:undo_cache = {}
	endif
endfu

" Note: Redundant calls are safe.
fu! s:Unconfigure_child()
	if s:undo_winid > 0
		noauto call win_gotoid(s:undo_winid)
		" Clear child winid, but leave its bufnr intact to facilitate reuse.
		let s:undo_winid = -1
		" TODO: Decide whether we should delete the cache and buffer contents at
		" this point.
		let s:undo_cache = {}
		" Since we know we want to close child buffer, go ahead and empty it.
		" Rationale: In case our deferred delete of child fails, at least the
		" window will be empty.
		silent %d
		" Remove the child autocommands.
		" Note: Deleting a buffer doesn't remove its buf-local autocmds.
		au! undo_tree_child
		" Caveat!: Cannot assume we'll be in child, as BufWinLeave autocmd doesn't
		" guarantee it.
		" Note: Though Is_child_configured checks only for autocmds, might be nice
		" to remove <buffer> mappings as well, but :mapclear doesn't support the
		" <buffer=N> form, and we can't guarantee we're in the child buffer. In any
		" event, we're safe, since child mappings check for existence of active
		" parent; moreover, when parent buffer leaves its last window, we do a
		" deferred :bdelete of the child, which does remove maps.
	endif
endfu

fu! s:Create_autocmds_in_child()
	" Create autocmds that will permit us to clean up when child buf deleted
	aug undo_tree_child
		au!
		au BufDelete <buffer> call s:Child_BufDelete(expand("<abuf>"))
		au BufEnter <buffer> call s:Child_BufEnter()
		au BufLeave <buffer> call s:Child_BufLeave()
		au TextChanged <buffer> call s:Child_TextChanged()
		au TextChangedI <buffer> call s:Child_TextChanged()
		au BufWinLeave <buffer> call s:Child_BufWinLeave(expand("<abuf>"))
		au WinEnter * call s:WinEnter()
		au WinLeave * call s:WinLeave()
	augroup END
endfu

fu! s:Create_autocmds_in_parent()
	" Create autocmds that will permit us to clean up when parent buf deleted
	aug undo_tree_parent
		au!
		au BufWinLeave <buffer> call s:Parent_BufWinLeave(expand("<abuf>"))
		au BufEnter call s:BufEnter()
	augroup END
endfu

" Called From: buffer being considered as candidate parent
" Return nonzero iff we can open an undo buffer for current buffer.
fu! s:In_potential_parent()
	" TODO More checks? E.g., 'buftype', etc...?
	return &modifiable && bufnr('%') != s:undo_bufnr
endfu

fu! s:Configure_parent()
	call s:Create_autocmds_in_parent()
endfu

fu! s:Create_mappings_in_child()
	" Create buffer-local mapping(s)
	" To refresh the tree forcibly
	nnoremap <silent> <nowait> <buffer> R :call <SID>Refresh_child()<CR>

	" Moving up/down and changing undo/redo path through tree.
	" Design Decision: No reason to avoid using regular Vim motion commands.
	" Rationale: Cursor movement is highly constrained.
	nnoremap <silent> <nowait> <buffer> k  :call <SID>Move_in_tree('up')<CR>
	nnoremap <silent> <nowait> <buffer> j  :call <SID>Move_in_tree('down')<CR>
	nnoremap <silent> <nowait> <buffer> h  :call <SID>Move_in_tree('left')<CR>
	nnoremap <silent> <nowait> <buffer> l  :call <SID>Move_in_tree('right')<CR>
	nnoremap <silent> <nowait> <buffer> C  :call <SID>Center_tree(1)<CR>
	nnoremap <silent> <nowait> <buffer> G  :<C-U>call <SID>Goto_node_in_tree(0)<CR>
	nnoremap <silent> <nowait> <buffer> gg :<C-U>call <SID>Goto_node_in_tree(1)<CR>
endfu

" Return string like ` ctermfg=<cterm_clr> guifg=<gui_clr>', taking
" 'path_hlgroup' option into account, defaulting to an appropriate default
" highlight group.
fu! s:Get_undoredo_path_color_attrs()
	let [ret, gid] = ['', 0]
	if exists('g:undotree_path_hlgroup')
		" Assumption: synIDtrans can handle 0
		let gid = synIDtrans(hlID(g:undotree_path_hlgroup))
	endif
	if !gid
		" Default to one of Vim's default highlight groups.
		let gid = synIDtrans(hlID('Title'))
	endif
	if gid
		" Design Decision: Could just let mode default to whatever's active, but
		" I like setting both ctermfg= and guifg=.
		for mode in ['cterm', 'gui']
			let clr = synIDattr(gid, 'fg')
			if !empty(clr)
				let ret .= ' ' . mode . 'fg=' . clr
			endif
		endfor
	endif
	return ret
endfu

fu! s:Create_syntax_in_child()
	" Design Decision: Could do this once up front, but re-doing it is cheap,
	" and allows changes to g:undotree_path_hlgroup to be taken into account.
	exe 'hi undo_redo_path gui=bold cterm=bold term=bold'
		\ .	s:Get_undoredo_path_color_attrs()
endfu

" Object to hold saved option settings for save/restore mechanism.
let s:opts = {}

" TODO: Remove s:script_path and s:Get_opt_setter if they end up not being
" needed.
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
	endif
endfu

" Rationale: The reason for refusing to save an already-overridden value is that
" if we somehow managed to do 2 consecutive overrides from this script, we might
" inadvertently save our own setting for subsequent restoration. This would be
" particularly bad if the option were 'guicursor'. Refusing to save an override
" ensures this can't happen (at the cost of a bit of complexity).
" TODO: Doing the save only in BufEnter (and in Configure_child when we're sure
" the BufEnter has been skipped) could also prevent the problem scenario, and
" would obviate the need for the more complex override mechanism. Consider
" whether this mechanism is justified.
" TODO: Consider adding a 'local' flag parameter. (May not be necessary, given
" that we typically wouldn't need to 'override' local params.)
fu! s:Override_opt(name, value)
	"let setter = s:Get_opt_setter(a:name)
	if !has_key(s:opts, a:name)
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

" Note: Redundant calls are safe.
fu! s:Unconfigure_cursor_in_child()
	call s:Restore_opt(has('gui') ? 'guicursor' : 't_ve')
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

fu! s:In_child()
	return s:undo_bufnr == bufnr('%') && s:undo_winid == win_getid()
endfu

" Return nonzero if active parent in current tab in window whose ID is s:winid.
" Note: This function does some paranoid checking.
fu! s:Is_active_parent()
	let wnr = win_id2win(s:winid)
	" Extra check, just in case Vim ever re-uses window IDs
	return wnr && s:bufnr == winbufnr(wnr)
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
	" Note: Intentionally deleting maps and such.
	exe 'bd ' . s:undo_bufnr
endfu

" Hide all occurrences of child buffer in any tab (with the exception of a child
" buffer that happens to be in current window). Leave cursor in the window that
" was active on function entry.
fu! s:Hide_children()
	let [wid_orig, tnr_cur] = [win_getid(), tabpagenr()]
	" Get a sorted list of [tnr, wnr, wid] triples, with the triple
	" corresponding to current window filtered out.
	" Note: The sort by tabnr minimizes the need for jumps between tabs when
	" closing windows.
	" Note: Yes. That's quite a bit of trouble to avoid harmless but redundant
	" calls to win_gotoid()...
	let triples = sort(map(filter(
		\ win_findbuf(s:undo_bufnr), 'v:val != wid_orig'),
		\ 'extend(win_id2tabwin(v:val), [v:val])'))
	for [tnr, wnr, wid] in triples
		if tnr != tnr_cur
			" Change tab without triggering autocmds.
			noauto call win_gotoid(wid)
			let tnr_cur = tnr
		endif
		" Attempt to hide the child, noting that command will fail (harmlessly)
		" if it's the last window.
		" Caveat: 'noauto' modifier needed to prevent BufWinLeave firing for the
		" child.
		exe 'silent! noauto ' . wnr . 'close'
	endfor
	" Return to starting window.
	noauto call win_gotoid(wid_orig)
endfu

fu! s:Orphan_child()
	call s:Unconfigure_parent()
	" TODO: Determine whether there could be any reason to call Delete_children
	" when s:undo_winid has already been cleared (-1). I'm thinking not.
	if s:undo_winid > 0
		" Also unconfigure child so we don't need to worry about its autocmds
		" triggering again.
		" Rationale: :bd doesn't delete <buffer> autocmds.
		call s:Unconfigure_child()
		" Queue deletion of child buffer(s).
		call s:Defer_action('Delete_children')
	endif
endfu

" Design Decision: When parent buffer leaves its original window, unconfigure
" parent and unconfigure/delete child buffer.
" Rationale: Unconfiguring parent simplifies logic by avoiding need to make
" child smart about resurrecting parent in proper position, and deleting child
" gets rid of maps and such that might otherwise be problematic in absence of
" parent. (Note that the child's bufnr will still exist, and it can still be
" resurrected on next open request.)
" Note: The reason we need both BufWinLeave and BufLeave is that a buffer can be
" deleted (e.g., with `:bd N') without triggering BufLeave, and the special
" parent window could be closed without deleting the buffer.

" Parent buffer is being deleted, or perhaps just hidden/unloaded.
fu! s:Parent_BufWinLeave(bnr)
	call s:Orphan_child()
endfu

" Detect when parent buffer has departed its original window.
fu! s:BufEnter()
	if s:winid == win_getid() && s:bufnr != bufnr('%')
		" Parent buffer is leaving original window!
		call s:Orphan_child()
	endif
endfu

" Child buffer is being deleted, or perhaps just hidden/unloaded.
" Note: BufDelete implies BufWinLeave (though the converse is not true), so
" there is some harmless redundancy.
fu! s:Child_BufWinLeave(bnr)
	call s:Unconfigure_child()
endfu

" Note: User shouldn't typically delete the child manually, but if he does, we
" will lose the maps in the child buffer, so we need to ensure that the child
" goes through full reconfiguration next time it's needed.
fu! s:Child_BufDelete(bnr)
	call s:Unconfigure_child()
endfu

" Each time child buf is entered in its special window, we need to check to see
" whether it's in need of refresh.
" Called When: Moving to child buf's special window.
" Assumption: Since child is unconfigured when child buffer first leaves special
" window, a BufEnter in the special child window should always imply window
" change, rather than (e.g.) :buffer command.
fu! s:Child_BufEnter()
	" Ignore BufEnter in non-special window.
	if s:In_child()
		" Now make sure parent is still active and in its special window.
		" Caveat: BufEnter will fire when parent is in process of deletion, but in
		" this case, parent will not have a window, and the child buffer will be
		" queued for hiding.
		" Assumption: window ID's are not re-used.
		" Assumption: If parent is closed with (e.g.) :q, parent BufWinLeave
		" fires before child BufEnter.
		if s:Is_active_parent()
			" TODO: Consider factoring this logic out into non-autocmd function.
			" Invoke non-forced refresh.
			call s:Refresh_cache(0)
			call s:Refresh_undo_window()
			call s:Center_tree(0)
			call s:Configure_cursor_in_child()
		endif
	endif
endfu

" Use this to determine when child is leaving its special window.
" Called When:
" 1) Child buf is leaving special window
" 2) Cursor is leaving special window for one that doesn't contain child buf
" Note: In both cases, we want to restore cursor, but in the 2nd case, WinLeave
" is going to fire after BufLeave.
" Caveat: WinLeave doesn't fire for :new, so we can't rely on it for 2nd case.
fu! s:Child_BufLeave()
	if s:undo_winid == win_getid()
		call s:Unconfigure_cursor_in_child()
	endif
endfu

" In case user changes child buffer contents.
fu! s:Child_TextChanged()
	if !empty(s:undo_cache)
		call s:undo_cache.Set_dirty()
	endif
endfu

" Note: WinEnter/Leave used to determine when cursor needs to be shown/hidden.
" Important Note: If child appears in multiple windows, only one gets special
" cursor treatment.
fu! s:WinEnter()
	if s:undo_winid == win_getid()
		call s:Configure_cursor_in_child()
	endif
endfu

fu! s:WinLeave()
	if s:undo_winid == win_getid()
		call s:Unconfigure_cursor_in_child()
	endif
endfu

" Calculate child size in the variable direction, taking into account the input
" splitinfo (applicable component only) and available space (in free direction).
" Assumption: undo cache is valid and parent is established
fu! s:Calc_child_size(splitinfo)
	let si = a:splitinfo
	let geom = s:undo_cache.geom.Get()
	let avail_sz = si.side =~? '[ab]'
		\ ? winheight(0) + winheight(win_id2win(s:winid))
		\ : winwidth(0) + winwidth(win_id2win(s:winid))
	if type(si.size) == 3
		" Size is range
		" Determine undo tree size
		" TODO: Geom is candidate for objectification...
		let tree_sz = si.side =~? '[ba]'
			\ ? len(geom.lines)
			\ : geom.width
		" Note: At this point, size of 0 means 'unconstrained'
		" Note: The avail_sz - 1 accounts for parent's min height/width.
		if si.size[1] > 0
			" Note: Can produce max of 0, but min of 1 will be imposed later.
			let max_sz = min([avail_sz - 1,
				\ si.pct[1] ? avail_sz * si.size[1] / 100 : si.size[1]])
		else
			let max_sz = avail_sz - 1
		endif
		" Note: Either the if or else block can produce min of 0: in either
		" case, we must eventually impose hard min of 1.
		if si.size[0] > 0
			let min_sz = si.pct[0] ? avail_sz * si.size[0] / 100 : si.size[0]
		else
			let min_sz = si.size[0]
		endif
		" Note: At this point, size of 0 really means 0
		" Grab sufficient space to accomodate tree if possible...
		let sz = min([max_sz, tree_sz])
		" ...but don't shrink below min
		let sz = max([sz, min_sz])
	else
		" Size is exact
		let sz = si.pct ? avail_sz * si.size / 100 : si.size
	endif
	" Impose hard min of 1
	return max([sz, 1])
endfu

" Assumption: Called from parent with undo cache valid
" Note: Always end up in child window.
fu! s:Position_child(splithow)
	let geom = s:undo_cache.geom.Get()
	" Use splithow hint to extract and parse applicable 'splitinfo' component.
	let si = s:Get_splitinfo(a:splithow)
	" Save viewport so we can restore after opening (or re-opening) child.
	" Rationale: Opening child window (e.g.) can effectively scroll the parent.
	let p_wsv = winsaveview()
	let p_wid = win_getid()
	" Configure the split
	let split_mod = si.side =~? '[al]' ? 'aboveleft' : 'belowright'
	if si.side =~? '[lr]'
		let split_mod .= ' vert'
	endif
	" Note: 'silent' prevents annoying and misleading [new file] msg.
	let cmd = ' +setl\ buftype=nofile\ bufhidden=hide\ noswapfile '
	if s:undo_bufnr < 0
		" Create new window containing scratch buffer.
		" Caveat: Wanted to wrap name in [...], but that makes Vim think it's
		" directory.
		exe 'silent ' . split_mod . ' new ' . cmd . expand('%') . '.undo'
		let [s:undo_bufnr, s:undo_winid] = [bufnr('%'), win_getid()]
	else
		" Re-using child buffer.
		if s:undo_winid > 0
			" Child buf is in special window. Save its viewport for restore
			" after we've pulled it into a new window.
			noauto call win_gotoid(s:undo_winid)
			let c_wsv = winsaveview()
			noauto call win_gotoid(s:winid)
		endif
		" Note: 'noauto' modifier prevents premature attempt to refresh.
		exe 'silent noauto ' . split_mod . ' sb' . cmd . s:undo_bufnr
		" Bufnr remains the same, but win ID has changed.
		let s:undo_winid = win_getid()
		if exists('l:c_wsv')
			call winrestview(c_wsv)
		endif
	endif
	" Now that we're safely in child, close all other windows containing child
	" (across all tabs).
	" Note: Deferring till this point avoids spurious BufWinLeave.
	call s:Hide_children()
	let size_cmd = si.side =~? '[ab]' ? '_' : '|'
	if si.side =~ '\u'
		" Expand the parent to occupy all available space.
		noauto call win_gotoid(s:winid)
		exe 'wincmd ' . size_cmd
	endif
	" Calculate desired size of child, taking options and available space into
	" account.
	let sz = s:Calc_child_size(si)
	" Make the child the calculated size.
	noauto call win_gotoid(s:undo_winid)
	exe sz . 'wincmd ' . size_cmd
	" Make sur the parent's viewport hasn't changed.
	noauto call win_gotoid(s:winid)
	call winrestview(p_wsv)
	" Note: Caller responsible for child's viewport.
	noauto call win_gotoid(s:undo_winid)

endfu

" If there's an active tab (i.e., one containing only the current parent and the
" child), return its tabnr, else -1.
fu! s:Get_reusable_tab()
	if s:bufnr < 0 || s:undo_bufnr < 0
		return -1
	endif
	for c_tabnr in map(win_findbuf(s:undo_bufnr), 'win_id2tabwin(v:val)[0]')
		" Get list of buffers in this tab (which we know contains child).
		let bufnrs = tabpagebuflist(c_tabnr)
		if len(bufnrs) != 2
			" Consider only tabs containing single parent and single child.
			continue
		endif
		" Note: We already know the tab contains child and one other buffer; if
		" the other buffer is parent, this tab's the one we're looking for.
		if index(bufnrs, s:bufnr) >= 0
			" Found a tab containing only a single parent and child
			return c_tabnr
		endif
	endfor
	return -1
endfu

" Make sure an undo_buf exists, is visible, and is properly configured, creating
" if necessary.
" Assumption: In parent buffer
fu! s:Prepare_child(splithow)
	call s:Position_child(a:splithow)
	" Note: We call this even when buf already existed, to ensure it's got all
	" the mappings and autocmds we'll need. (It's unlikely, but possible, that
	" someone deleted maps/autocmds, for instance.)
	call s:Configure_child()
endfu

fu! s:Describe_node(...)
	" If user didn't pass a seq number, describe current node.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	let t = s:undo_cache.geom.Get().nodes[seq].node
	if t.seq == 0
		echo "At root of undo tree"
	else
		echo printf("State #%d: %s", t.seq, strftime("%c", t.time))
	endif
endfu

" Undo parent buf to the undo state whose seq number is input.
" Post Condition: Doesn't change active window.
fu! s:Undo_to(seq)
	let wid = win_getid()
	if wid != s:winid
		noauto call win_gotoid(s:winid)
	endif
	" Note: Execute undo silently to avoid a hit-enter prompt.
	silent exe 'undo ' . a:seq
	if wid != s:winid
		" Return to starting window without triggering BufEnter.
		noauto call win_gotoid(wid)
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
	let gnode = s:undo_cache.geom.Get().nodes[seq]
	let [t, row, col] = [gnode.node, gnode.row, gnode.col]
	" Calculate 1-based offset from edge of window.
	let [row_rel, col_rel] = [row - vscroll, col - hscroll]
	" Check edges of node to see whether any part is not visible.
	let gi = t.Get_geom(s:undo_cache.Get_display_mode())
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

" Goto (and center on) the node whose seq number user supplied as count to a
" mapping.
fu! s:Goto_node_in_tree(root_is_default)
	" Note: You can't supply 0 as a count, but a user may want to go to the root
	" node. Also, there are 2 different maps that invoke this function, and
	" they should produce different behavior when invoked without count:
	" gg => root
	" G  => end of redo path
	" Note: When count is omitted, v:count will be 0.
	let seq = v:count
	if !seq
		if a:root_is_default
			let node = s:undo_cache.tree
		else
			" Default to leaf node at end of current redo path.
			let node = s:undo_cache.tree.cur.Get_leaf()
		endif
	else
		" Get the node, bearing in mind that there's no guarantee it exists.
		let geom = s:undo_cache.geom.Get()
		let gnode = has_key(geom.nodes, seq) ? geom.nodes[seq] : {}
		if empty(gnode)
			echomsg "Warning: Node " . seq . " does not exist."
			return
		endif
		let node = gnode.node
	endif

	" Node exists: make it current
	let s:undo_cache.tree.cur = node
	" Alter the undo-redo path to include it.
	call node.Make_current()
	" Accomplish the change in the actual buffer.
	call s:Undo_to(node.seq)
	" Center on the new current node.
	call s:Center_tree(1, node.seq)
	" Update display.
	call s:undo_cache.syn.Update(node)
	" Output summary of current change.
	call s:Describe_node()

endfu

" Called From: child buffer maps for moving in tree.
" Assumption: Can be invoked only when fresh undo data structures exist.
" Rationale: Invoked from undo buffer mappings, and freshness is checked in
" BufEnter. Hmm... What if user executes a :bd or something. Well, in that case,
" parent's BufDelete would fire, clearing data structures.
fu! s:Move_in_tree(dir) " entry
	if s:undo_bufnr < 0 || s:undo_winid != win_getid()
		" Ignore maps in non-special child window.
		" TODO: If we stick with this approach, should probably use normal! to
		" let the key sequence have its normal effect.
		return
	endif
	if s:bufnr < 0 || s:winid < 0 || !win_id2win(s:winid)
		echomsg "Warning: Cannot traverse undo tree for inactive parent buffer"
	endif
	" Grab tree and invoke specified movement method.
	" TODO: For aesthetics, make the method names capitalized.
	call s:undo_cache.tree[a:dir]()
	" Execute the actual undo in the parent buffer.
	call s:Undo_to(s:undo_cache.tree.cur.seq)
	" Update display.
	call s:undo_cache.syn.Update(s:undo_cache.tree.cur)
	" Center iff current node is not fully visible.
	call s:Center_tree(0)
	" Output summary of current change.
	call s:Describe_node()
endfu

" Invoked from child's <buffer> map.
" Assumption: Can't get here unless we're in true child buffer (though we will
" need to check to be sure we're in special window).
fu! s:Refresh_child() " entry
	if !s:In_child()
		" Ignore invocation from non-special child window.
		return
	endif
	" Make sure parent is still loaded and visible.
	" Note: Shouldn't get here if not, but it's possible (e.g., if user
	" intentionally opens unlisted undo buffer).
	" TODO: Consider removing the child autocmds and mappings when the child is
	" hidden. Could populate them only when undo buffer is opened.
	if !s:Is_active_parent()
		echomsg "Warning: Cannot refresh undo buffer with no associated parent."
		call s:Orphan_child()
		return
	endif
	" There's a visible and active parent. Forcibly refresh
	call s:Refresh_cache(1)
	" TODO: Decide whether to force the window refresh, or only the cache
	" refresh: i.e., if cache was up to date, perhaps we should pass 0 here...
	" Note: Has implications for centering tree...
	call s:Refresh_undo_window()
	call s:Center_tree(1)
endfu

fu! s:Build_active_geom(tree) dict
	" TODO: Thinking I may no longer need tree returned, now that positions are
	" stored on b:undo_tree.
	" Note: Though it seems a bit odd, I'm not using ptree here because
	" positions have already been placed on tree itself.
	let detailed = self.Is_detailed()
	let [ptree, extent] = s:Design(a:tree, detailed)
	let self.ary[self.idx] = s:Build_tree_display(a:tree, extent, detailed)
	let self.dirty = 1
endfu

fu! s:Toggle_active_geom() dict
	let self.idx = (self.idx + 1) % 2
	if empty(self.ary[self.idx])
		call self.Build()
	endif
endfu

fu! s:Get_active_geom() dict
	return self.ary[self.idx]
endfu

fu! s:Is_geom_dirty() dict
	return self.dirty
endfu

fu! s:Is_geom_detailed() dict
	return self.idx
endfu

fu! s:Clear_geom_dirty() dict
	let self.dirty = 0
endfu

fu! s:Toggle_display_mode() dict
	call self.geom.Toggle()
	let self.syn = s:Make_syn_tree(self.geom)
endfu

fu! s:Get_display_mode() dict
	return self.geom.Is_detailed()
endfu

" TODO: Probably get rid of this dirty flag, which indicates only whether
" changes have been made to undo buffer since cache was last refreshed.
fu! s:Set_cache_dirty() dict
	let self.dirty = 1
endfu

fu! s:Clear_cache_dirty() dict
	let self.dirty = 0
endfu

fu! s:Is_cache_dirty() dict
	return self.dirty
endfu

fu! s:Make_undo_cache(tree, detailed)
	let me = {
		\ 'dirty': 1,
		\ 'tree': a:tree,
		\ 'geom': {
			\ 'idx': !!a:detailed,
			\ 'ary': [{}, {}],
			\ 'dirty': 1,
			\ 'Build': function('s:Build_active_geom', a:tree),
			\ 'Toggle': function('s:Toggle_active_geom'),
			\ 'Is_dirty': function('s:Is_geom_dirty'),
			\ 'Is_detailed': function('s:Is_geom_detailed'),
			\ 'Clear_dirty': function('s:Clear_geom_dirty'),
			\ 'Get': function('s:Get_active_geom'),
		\ },
		\ 'syn': {},
		\ 'Toggle_display_mode': function('s:Toggle_display_mode'),
		\ 'Get_display_mode': function('s:Get_display_mode'),
		\ 'Set_dirty': function('s:Set_cache_dirty'),
		\ 'Clear_dirty': function('s:Clear_cache_dirty'),
		\ 'Is_dirty': function('s:Is_cache_dirty')
	\}
	call me.geom.Build(a:tree)
	" Now build highlighting object.
	let me.syn = s:Make_syn_tree(me.geom)
	return me
endfu

" Assumption: Called from parent with undo cache valid
" Return: nonzero if cache is rebuilt.
fu! s:Refresh_cache(force, ...)
	" Is this a forced re-init? E.g., should we revert to default display?
	let reinit = a:0 ? !!a:1 : 0
	" Make sure we're in parent.
	noauto call win_gotoid(s:winid)
	" Do we need to update the tree or is cache still valid?
	" Get Vim's undo tree to support freshness check, and if necessary, to serve
	" as basis of new tree.
	" TODO: Could avoid checking undotree() in some cases if we monitored
	" BufEnter on the parent.
	" Rationale: Buffer can be changed only when user visits it.
	let v_ut = undotree()
	if empty(s:undo_cache) || a:force
		\ || s:undo_cache.tree.meta.seq_last != v_ut.seq_last
		" Build a proper tree from Vim's representation
		let tree = s:Make_undo_tree(v_ut)
		" Determine starting display mode (simple or detailed)
		let detailed = !reinit && !empty(s:undo_cache)
			\ ? s:undo_cache.Get_display_mode()
			\ : exists('g:undotree_detailed') && !!g:undotree_detailed
		" Build a cache object.
		let s:undo_cache = s:Make_undo_cache(tree, detailed)

		" TODO: Remove this...
		let g:uc = s:undo_cache
		" Let caller know cache has been changed.
		return 1
	endif
	return 0
endfu

" This one's tied to mapping or command for opening undo on current buffer.
fu! s:Open_undo_window(...) " entry
	if !s:In_potential_parent()
		echoerr "Can't display undo tree for this buffer."
		return
	endif
	let splithow = a:0 ? a:1 : ''
	" Cancel any pending delete of child buffer we're about to re-use.
	call s:Cancel_action('Delete_children')
	let [p_bnr, p_wid] = [bufnr('%'), win_getid()]
	if p_bnr != s:bufnr
		" New parent (either no current parent or parent changing)
		if s:bufnr > 0
			" We have existing parent. Unconfigure old.
			call s:Unconfigure_parent()
		endif
		let [s:bufnr, s:winid] = [p_bnr, p_wid]
		call s:Configure_parent()
	elseif p_wid != s:winid
		" Same parent buffer but different window.
		let s:winid = p_wid
	endif
	" Make sure cache is up-to-date.
	call s:Refresh_cache(0, 1)
	" Note: Syntax is currently associated with child window, which always
	" changes, due to the way we do child positioning.
	call s:undo_cache.syn.Clear()
	" TODO: Need a way to restore child viewport in case in which nothing is
	" really changing: i.e., user hit \u with child already open - in such
	" cases, we wouldn't really want viewport to change.
	call s:Prepare_child(splithow)
	" Everything should be in order now with the 2 windows.
	call s:Refresh_undo_window()
	" Rationale: Forced centering when we open undo buffer prevents our seeing
	" entire tree, even when the child window can accommodate.
	call s:Center_tree(0)
endfu

" Called From: One of the following...
" 1) undo buffer 'refresh' mapping
" 2) global mapping/command requesting undo window for parent
" 3) child BufEnter autocmd
" Assumption: Both parent and child windows are visible (in special windows in
" curent tab) and cache is up-to-date, albeit possibly 'dirty' (i.e., in need of
" write to buffer).
fu! s:Refresh_undo_window()
	" We want to move to the special undo window even if all is up-to-date.
	noauto call win_gotoid(s:undo_winid)
	" Regardless of whether undo cache was valid, buffer contents may have been
	" clobbered (e.g., by user buffer deletion/text modification).
	" TODO: Consider refactoring flags: here are the things that could
	" potentially be invalid:
	" -cache itself has been changed, which means buffer contents are invalid
	" -buffer contents could have been invalidated by user modification
	"  (shouldn't happen)
	" -syntax could have been invalidated by \u making a new parent win special.
	if s:undo_cache.Is_dirty() || s:undo_cache.geom.Is_dirty()
		" Replace undo buffer's contents with new tree.
		silent %d
		call append(0, s:undo_cache.geom.Get().lines)
		" Append adds a final blank line, which serves no purpose, and which we
		" don't want to impact window sizing, so delete it.
		$d
		" Caveat: The preceding :$d can shift the viewport.
		norm! gg
		call s:undo_cache.Clear_dirty()
		call s:undo_cache.geom.Clear_dirty()
	endif
	" Note: Besides cache change, there are 2 other reasons syntax might be
	" dirty:
	"   buffer contents changed
	"   syntax invalidated by making different parent window special
	if s:undo_cache.syn.Is_dirty()
		" Update highlighting
		call s:undo_cache.syn.Update(s:undo_cache.tree.cur)
	endif
endfu

" Parse 'splitwin' opt.
" Format: comma-separated list of h, v, t components: e.g.,
" h10-20b,v20%-40%L,t10-10r
" h20l,v20%-40%L,t10-10r
" TODO: Get rid of tab component, and possibly even split option into distinct
" vert/hori options.
fu! s:Parse_split_opt(opt)
    let ret = {
	\ 'h': {'type': 'h', 'size': [10, 50], 'pct': [1, 1], 'side': 'b'},
	\ 'v': {'type': 'v', 'size': [10, 50], 'pct': [1, 1], 'side': 'l'},
	\ 't': {'type': 't', 'size': [10, 60], 'pct': [1, 1], 'side': 'b', 'tabpos': '+'}}
    " 1=split-type, 2=min-or-only, 3=[%], [4=max, 5=[%]], 6=side, 7=tabpos
	" Note: This regex disallows 0%
	" Rationale: 0 is valid only as special unconstrained indicator.
    let re = '\([hvt]\)'
	    \ . '\(0\|[1-9][0-9]*\(%\)\?\)'
	    \ . '\%(-\%(\(0\|[1-9][0-9]*\(%\)\?\)\)\)\?'
	    \ . '\([albrALBR]\)'
	    \ . '\([-+^$]\)\?'
    " Design Decision: Whitespace has no meaning in opt string, so just strip.
    for spec in split(substitute(a:opt, '\s\+', '', 'g'), ',')
		let ml = matchlist(spec, re)
		if empty(ml)
			echomsg "Warning: Ignoring invalid 'splitwin' option: `"
			\ . spec . "'"
			continue
		endif
		let [_, hvt, min, minpct, max, maxpct, side, tabpos; rest] = ml
		let el = {}
		if empty(max)
			let el.size = str2nr(min)
			let el.pct = !empty(minpct)
		else
			let [min, max] = [str2nr(min), str2nr(max)]
			" Make sure ranges are rational. (Keep in mind that 0 is always valid as
			" 'unconstrained' indicator.)
			if min && max && min > max
				echomsg "Warning: Ignoring invalid 'splitwin' component:"
					\ . " range min > range max: `" . spec . "'"
				continue
			endif
			let el.size = [min, max]
			let el.pct = [!empty(minpct), !empty(maxpct)]
		endif
		let el.side = tolower(side)
		let el.type = hvt
		if hvt == 't'
			" Default to opening tabs after current tab.
			let el.tabpos = empty(tabpos) ? '+' : tabpos
		else
			" non-tab type
			if !empty(tabpos)
				" Reject the invalid entry.
				echomsg "Warning: Ignoring invalid 'splitwin' component:"
					\ . " tab pos modifier in non-tab component: `" . spec . "'"
				continue
			endif
			" Validate side values against type.
			if hvt == 'h' && side !~? '[ab]' || hvt == 'v' && side !~? '[lr]'
				echomsg "Warning: Ignoring invalid 'splitwin' component:"
					\ . " invalid 'side' modifier: `" . spec . "'"
				continue
			endif

		endif
		let ret[hvt] = el
    endfor
    return ret
endfu

fu! s:Get_splitinfo(splithow)
	let si = s:Parse_split_opt(
		\ exists('g:undotree_splitwin') ? g:undotree_splitwin : '')
	" TODO: Don't hardcode 'h' as default.
	let splithow = !empty(a:splithow)
		\ ? a:splithow
		\ : exists('g:undotree_splithow') ? g:undotree_splithow : 'h'
	return si[splithow]
endfu

" Global mappings
nnoremap <silent> <leader>u :call <SID>Open_undo_window()<CR>
nnoremap <silent> <leader>hu :call <SID>Open_undo_window('h')<CR>
nnoremap <silent> <leader>vu :call <SID>Open_undo_window('v')<CR>
nnoremap <silent> <leader>tu :call <SID>Open_undo_window('t')<CR>

fu! s:Test_only()
	let xs = s:Cons("baz", {})
	let xs = s:Cons("bar", xs)
	let xs = s:Cons("foo", xs)
	let xs_upper = s:Map(function('toupper'), xs)
	let xs_rev = s:Reverse(xs)
	let xs_zipped = s:Zip(xs_upper, xs_rev)
	let xs_zswapped = s:Map(function('s:Swap_tuple_fn'), xs_zipped)
	let xs_unzipped = s:Unzip(xs_zswapped)
	let es = [[
			\ [1, 3], [-1, 4], [8, 10]], [
			\ [4, 5], [5, 8],  [10, 14], [9, 11]], [
			\ [6, 9], [8, 9],  [15, 19], [11, 16]]]
	let es = map(es, 's:To_list(v:val)')
	let em = s:Merge(es[0], es[1])
	let em = s:Merge(em, es[2])
	let em2 = s:Merge_list(es)
	let em_moved = s:Move_extent(em2, 100)
	let es2 = [[
			\ [-3, 3], [-4, 5], [-7, 6]], [
			\ [-2, 5], [-1, 6],  [-5, 14], [-3, 11]], [
			\ [-5, 4], [-4, 5],  [-2, 15], [-1, 16]]]
	let es2 = map(es2, 's:To_list(v:val)')
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
