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

fu! s:Seconds_to_dhms_str(seconds)
	let units = [[86400, 'd'], [3600, 'h'], [60, 'm'], [1, 's']]
	let ret = ''
	let s = a:seconds
	for unit in units
		let [us, s] = [s / unit[0], s % unit[0]]
		if us
			"if !empty(ret) | let ret .= ' ' | endif
			let ret .= us . unit[1]
		endif
	endfor
	return ret
endfu

" TODO: No real need to pass in the entire node. Refactor inputs.
fu! s:Make_node_geom(node, detailed, ...)
	" Note: Add the surrounding spaces, which will be converted to brackets upon
	" node selection.
	if a:detailed && a:node.seq
		let text = [printf(" #%s ", a:node.seq), a:1]
	else
		let text = [' ' . a:node.seq . ' ']
	endif
	let [w, es] = [[], []]
	let e = [0, 0]
	for t in text
		let w_ = len(t)
		call add(w, w_)
		" Design Decision: The -1 ensures that when len is even, we center on
		" the last char in the left half, not first in right.
		let xs_ = -(w_ - 1) / 2
		let e_ = [xs_, xs_ + w_ - 1]
		call add(es, e_)
		" Update merged extent.
		if e_[0] < e[0] | let e[0] = e_[0] | endif
		if e_[1] > e[1] | let e[1] = e_[1] | endif
	endfor
	" Note: es is array of extents ([[xs1, xe1], ...]; e is a single extent
	" ([xs_min, xe_max]) representing a fold of es.
	return {'w': w, 'es': es, 'e': e, 'text': text}
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
fu! s:Foreach(f, xs)
	let x = a:xs
	while !empty(x)
		call call(a:f, [x.el])
		let x = x.next
	endwhile
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
	" TODO: Is it still necessary to place 'x' on the node? I'm thinking not...
	let a:tree_pos[0].node.x = new_x
	return {
		\ 'node': a:tree_pos[0].node,
		\ 'x': new_x,
		\ 'geom': a:tree_pos[0].geom,
		\ 'trees': a:tree_pos[0].trees}
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
	" Note: +2 gives a single space between the elements. Any additional
	" required spacing (e.g., for brackets) is represented in label itself.
	let d = a:e_pair[0][1] - a:e_pair[1][0] + 2
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

" Convert tree to a list of nodes in reverse breadth-first traversal order.
" E.g.: TAIL: 0 1.1 1.2 1.3 1.1.1 1.1.2 1.2.1 1.2.2 1.3.1 1.3.2 :HEAD
" TODO: Any advantage to using my lispy lists in lieu of Vim List for bflist?
fu! s:Tree_to_bflist(tree)
	let [fifo, bflst] = [[a:tree], []]
	while !empty(fifo)
		let t = remove(fifo, 0)
		call insert(bflst, t)
		let tc = t.children.fst
		while !empty(tc)
			call add(fifo, tc)
			let tc = tc.next
		endwhile
	endwhile
	return bflst
endfu

fu! s:Design(nodes, tree)
	" Walk the children.
	let bflst = s:Tree_to_bflist(a:tree)
	" trees/extents are accumulator lists for current sibling group.
	" rfifo is result fifo where we push positioned extents/trees for an entire
	" sibling group till they're required by the parent.
	let [trees, extents, rfifo] = [{}, {}, []]
	" Iterate breadth-first list (ordered from lower-right-most element to root
	" working leftward/upward).
	" Note: tp is parent node for the current sibling group.
	let [t, tp] = [bflst, {}]
	for t in bflst
		if tp isnot t.parent
			" Starting new sibling group.
			" Note: On first iteration, we'll get here (since {} isnot {}), but
			" not into subsequent if (since there are no extents).
			if !empty(extents)
				" Process old sibling group (represented by trees and extents).
				let positions = s:Fitlist(extents)
				" Modify list in place to ensure that no child is offset exactly
				" 1 col from parent.
				" Rationale: Prevents annoying visual disturbance in tree.
				call s:Optimize_child_positions(positions)
				" Position the trees/extents.
				let trees = s:Map(function('s:Move_tree_fn'), s:Zip(trees, positions))
				let extents = s:Map(function('s:Move_extent'), s:Zip(extents, positions))
				" Add positioned trees/extents to the fifo for parent to pull.
				call add(rfifo, [extents, trees])
				" Clear out just-processed sibling group.
				let [extents, trees] = [{}, {}]
			endif
			" Record transition to new sibling group.
			let tp = t.parent
		endif
		" Pull descendant results from result fifo (if applicable); otherwise,
		" we're at leaf and must initialize to empty lists.
		" Note: It's actually possible to pull what was pushed this iteration.
		let [cextents, ctrees] = !empty(t.children.fst)
			\ ? remove(rfifo, 0) : [{}, {}]
		" Prepend this node (along with its geometry) to descendant lists.
		" Note: Each element of 'cextents' list is the extent for one of this
		" node's child trees. Merge_list merges these into a single extent, onto
		" which we cons this node's contribution, before consing the whole thing
		" onto the 'extents' list for the current sibling group.
		let geom = a:nodes[t.seq].geom
		let extent = s:Cons(geom.e, s:Merge_list(cextents))
		let tree = {'node': t, 'x': 0, 'geom': geom, 'trees': ctrees}
		" TODO: Better way to handle this now that we're using a Vim list?
		if empty(t.parent)
			" We've hit root. No reason to augment trees/extents...
			" TODO: break instead?
			return [tree, extent]
		endif
		" Augment extents/trees for current sibling group.
		let extents = s:Cons(extent, extents)
		let trees = s:Cons(tree, trees)
	endfor
	" Assumption: Existence of root node precludes getting here.
endfu

fu! s:Design_recursive(nodes, t)
	" Walk the children.
	let [trees, extents] = [{}, {}]
	let [trees_tail, extents_tail] = [trees, extents]
	let tc = a:t.children.fst
	while !empty(tc)
		let [tree, extent] = s:Design(a:nodes, tc)
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
	" Fix geometry on the node.
	let geom = a:nodes[a:t.seq].geom
	let resultextent = s:Cons(geom.e, s:Merge_list(pextents))
	" Note: We'll need relative extent in Build_tree_display.
	let resulttree = {'node': a:t, 'x': 0, 'geom': geom, 'ptrees': ptrees}
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
" may be converted to cleaner breadth-first implementation.
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
fu! s:Build_tree_display(ptree, pextent, detailed)
	" Tree is centered at 0, but we need its left edge at 0. Determine the bias,
	" and while we're at it, the width, both of which will be stored on the geom
	" object we create.
	" Note: Can't really apply the bias in the tree itself since the x values in
	" tree are relative to parent. We *could* store absolute positions in the
	" tree, but I don't really like that.
	let [xmin, xmax] = s:Foldl(function('s:Extent_minmax_fn'), [0, 0], a:pextent)
	let [x, w] = [abs(xmin), xmax - xmin + 1]
	" Breadth-first traversal
	" Fifo elements: [<node>, <absolute-parent_x>, <lvl>]
	let fifo = [[a:ptree, x, 0]]
	" TODO: Make this part of some sort of global config.
	" Break row height into fixed and non-fixed (label) portions (noting that
	" non-fixed portion may be different for root and non-root levels).
	let [fixed_ht, rlbl_ht, nrlbl_ht] = [2, 1, a:detailed ? 2 : 1]
	" Cache rows per level and amount by which root level is shorter.
	let [rows_per_lvl, root_diff] = [fixed_ht + nrlbl_ht, nrlbl_ht - rlbl_ht]
	let lines = s:Make_gridlines()
	" Hash geom info by node id (seq number)
	let nodes = {}
	" Note: lines will be added later.
	" TODO: Decide whether we need to store 'detailed' on this, or whether it's
	" sufficient to rely on geom info.
	let ret = {'x_bias': x, 'width': w, 'nodes': nodes, 'lines': []}
	while !empty(fifo)
		let [ptree, parent_x, lvl] = remove(fifo, 0)
		let [t, gi] = [ptree.node, ptree.geom]
		" Calculate absolute x position in grid.
		let x = parent_x + ptree.x
		" Get index of top (possibly only) row containing label.
		let lrow = lvl * rows_per_lvl - (lvl ? root_diff : 0)
		" Hash node information by seq number.
		" Rationale: Associates nodes with actual canvas location.
		" Note: Geom info is fixed in Design and stored, not only for
		" convenience, but also because under detailed display, the geometry can
		" change with time.
		" Note: +1 converts from 0-based string offsets to 1-based row/col.
		" Design Decision: Adding 'row' key to nodes object as convenience,
		" though it could be derived from lvl.
		" TODO: Decide whether the convenience justifies the denormalization.
		let nodes[t.seq] = {'node': t, 'col': x + 1, 'row': lrow + 1, 'geom': gi }
		" Add this node's children to fifo
		let pts = ptree.trees
		while !empty(pts)
			" Note: Add parent x only, as child x can be calculated therefrom.
			call add(fifo, [pts.el, x, lvl + 1])
			let pts = pts.next
		endwhile
		" Process current node.
		" Build the seq # label (and the 'ago' time label as well, if mode is
		" detailed).
		let i = 0
		while i < len(gi.text)
			call lines.add(lrow + i, x + gi.es[i][0], gi.text[i])
			let i += 1
		endwhile
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
			if !empty(t.prev)
				" Extend horiz header line from previous node, and if appropriate,
				" add upper vertical from parent (which is deferred to child row
				" because it's in the same row as horiz line).
				if t.prev.x < 0
					" l->?
					let text_x = parent_x + t.prev.x + 2
					if t.x < 0
						" l->l
						let text = repeat('_', t.x - t.prev.x)
					elseif t.x > 0
						" l->r
						let text = repeat('_', -t.prev.x - 2)
							\ . '|' . repeat('_', t.x - 2)
						"echo "l->r case: " . t.seq . ": " . text
					else
						" l->c
						let text = repeat('_', -t.prev.x - 2) . '|'
					endif
				elseif t.prev.x > 0
					" r->r
					let text_x = parent_x + t.prev.x - 1
					let text = repeat('_', t.x - t.prev.x)
				else
					" c->r
					let text_x = parent_x
					let text = '|' . repeat('_', t.x - 2)
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

let s:Debug_level = -1
fu! s:Dbg(lvl, fmt, ...)
	if a:lvl > s:Debug_level
		return
	endif
	echo call('printf', extend([a:fmt], a:000))
endfu

" TODO: Consider using json_encode/decode in conjunction with Capitalized
" globals to have marks persisted.
" Note: By default, 'viminfo' doesn't contain `!', which means global variables
" won't be saved for most users unless they change their 'viminfo' setting.
" Thus, I'm not sure it makes sense to persist marks.
let s:marks = {'files': {}}

fu! s:marks.Add(file, ltr, seq) dict
	" Assumption: Caller ensures there's a valid filename.
	let path = fnamemodify(a:file, ':p')
	" Do we have any entry for this file yet? If not, create.
	if has_key(self.files, path)
		let pentry = self.files[path]
	else
		let pentry = {'marks': {}, 'seqs': {}}
		let self.files[path] = pentry
	endif
	" Update MRU timestamp.
	let pentry.ts = localtime()
	" Check for obsolete mapping.
	if has_key(pentry.marks, a:ltr)
		" Remove ltr from list of marks pointing to this seq.
		" Assumption: Item must exist.
		let seq = pentry.marks[a:ltr]
		call remove(pentry.seqs[seq], index(pentry.seqs[seq], a:ltr))
	endif
	" Map ltr->seq
	let pentry.marks[a:ltr] = a:seq
	" Map seq->[ltr,...]
	if !has_key(pentry.seqs, a:seq)
		let pentry.seqs[a:seq] = [a:ltr]
	else
		" Grow the list of ltr's pointing to this seq
		call insert(pentry.seqs[a:seq], a:ltr)
	endif
endfu

fu! s:marks.Remove(file, ltr) dict
	let path = fnamemodify(a:file, ':p')
	if has_key(self.files, path)
		let pentry = self.files[path]
		" Update MRU timestamp.
		let pentry.ts = localtime()
		" Check for obsolete mapping.
		if has_key(pentry.marks, a:ltr)
			" Remove ltr from list of marks pointing to this seq.
			" Assumption: Presence of ltr in marks hash guarantees existence.
			let seq = pentry.marks[a:ltr]
			call remove(pentry.seqs[seq], index(pentry.seqs[seq], a:ltr))
			call remove(pentry.marks, a:ltr)
			if empty(pentry.marks)
				" No reason to keep path's key if it contains no marks.
				call remove(self.files, path)
			endif
		endif
	endif
endfu

fu! s:marks.Get_seq(file, ltr) dict
	let path = fnamemodify(a:file, ':p')
	if has_key(self.files, path)
		let self.files[path].ts = localtime()
		if has_key(self.files[path].marks, a:ltr)
			return self.files[path].marks[a:ltr]
		endif
	endif
	return -1
endfu

fu! s:marks.Get_marks(file, seq) dict
	let path = fnamemodify(a:file, ':p')
	if has_key(self.files, path)
		let self.files[path].ts = localtime()
		if has_key(self.files[path].seqs, a:seq)
			return self.files[path].seqs[a:seq]
		endif
	endif
	return -1
endfu

" Called whenever we've obtained the vim undotree for a buffer.
" TODO: This is only needed if I support persisting marks across sessions.
fu! s:marks.Gc(file, vut)
	let path = fnamemodify(a:file, ':p')
endfu

" Note: These defaults correspond to global settings g:undotree_display_sigfigs
" and g:undotree_display_seconds, respectively.
" TODO: Probably move all such defaults to common location.
let s:DEFAULT_DISPLAY_SIGFIGS = 2
let s:DEFAULT_DISPLAY_SECONDS = 0
let s:TIME_UNITS = [[86400, 'd'], [3600, 'h'], [60, 'm'], [1, 's']]

" TODO: Document the logic...

fu! s:Make_ago(...)
	let me = {
		\ 'now': a:0 ? a:1 : localtime(),
		\ 'times': {},
		\ 'nodes': {}
	\ }
	let me.display_seconds = exists('g:undotree_display_seconds')
		\ && (type(g:undotree_display_seconds) == 0
		\     || type(g:undotree_display_seconds) == 6)
		\ ? !!g:undotree_display_seconds
		\ : s:DEFAULT_DISPLAY_SECONDS
	let me.display_sigfigs = exists('g:undotree_display_sigfigs')
		\ && type(g:undotree_display_sigfigs) == 0
		\ && g:undotree_display_sigfigs > 0
		\ ? min([g:undotree_display_sigfigs, me.display_seconds ? 4 : 3])
		\ : s:DEFAULT_DISPLAY_SIGFIGS
	" Return unfiltered list of components down to least significant nonzero
	" component that *might* be needed.
	fu! me.Decompose_seconds(seconds)
		let ret = []
		let [rem, i, sf] = [a:seconds, 0, 0]
		for [mod, ltr] in s:TIME_UNITS
			" Design Decision: If seconds display is disabled, we keep the
			" seconds component if and only if it's sole nonzero component.
			" Alternative Approach: Never keep seconds if seconds display
			" disabled; in that case, 'now' would be used in lieu of nonzero
			" seconds when only seconds nonzero.
			if ltr == 's' && !self.display_seconds && sf
				" Don't need seconds
				break
			endif
			let [div, rem] = [rem / mod, rem % mod]
			if div
				" Nonzero component
				if !sf && rem < 0
					" Permit 1st comp only to be negative (as indication of
					" future time).
					" Note: Shouldn't happen.
					let rem = -rem
				endif
				let sf += 1
			endif
			call add(ret, div)
			if sf >= self.display_sigfigs
				" We have all the nonzero components we can use.
				break
			endif
			let i += 1
		endfor
		return ret
	endfu
	" Decompose a signed seconds value, hashing it by corresponding seq
	" number, augmenting data structure required (when sigfig > 1) to
	" implement the uniquification logic.
	fu! me.Add(seq, seconds)
		let comps = self.Decompose_seconds(self.now - a:seconds)
		" Hash the list of time components against seq number; only
		" after self.times is fully built will we be able to convert
		" to strings.
		let self.nodes[a:seq] = comps
		" Note: No need for o[] apparatus when sigfigs == 1.
		if self.display_sigfigs > 1
			let o = self.times
			for comp in comps
				if !has_key(o, comp)
					let o[comp] = {'cnt': 1, 'sub': {}}
				else
					let o[comp].cnt += 1
				endif
				let o = o[comp].sub
			endfor
		endif
	endfu
	" Using the nodes hash (if necessary), convert the lists of time components
	" to a single string using logic designed to minimize the string's length
	" without throwing away useful information.
	" Logic: Pre-processing in Add method discarded any nonzero components in
	" excess of display_sigfigs; additionally, the pre-processing discards
	" seconds component if display_seconds is false *unless* seconds component
	" is the sole nonzero component, in which case it is retained. The Finalize
	" method keeps up to display_sigfigs components, but once it has 1 nonzero
	" component, it will retain additional components only if they contribute to
	" the 'uniqueness' of the displayed time: e.g., if display_sigfigs == 2 and
	" the nonzero components are 3d,5h, the 5h would be discarded if there are
	" no other nodes with time of 3d,5h,... Additionally, the 5h would be
	" discarded if all nodes with '3d,5h' are exactly '3d,5h' (i.e., have
	" insignificant minute and second components), since in that case, the '5h'
	" is not helping to differentiate.
	fu! me.Finalize()
		for [seq, comps] in items(self.nodes)
			let [msi, lsi] = [-1, -1]
			let [s, i] = ['', 0]
			let o = self.times
			for comp in comps
				if comp
					if msi < 0
						let msi = i
					endif
					let lsi = i
				endif
				" Caveat: Special multi-level hash is unnecessary when
				" display_sigfigs == 1; don't assume it exists in that case.
				if msi >= 0 && (self.display_sigfigs == 1 || o[comp].cnt == 1)
					break
				endif
				if self.display_sigfigs > 1
					" Keep descending...
					let o = o[comp].sub
				endif
				let i += 1
			endfor
			if msi < 0
				" No sig figs.
				let s = 'now'
			else
				" Build the string of components.
				let s = ''
				for i in range(msi, lsi)
					" Use only nonzero components.
					if comps[i]
						let comp = comps[i]
						if comp < 0 && empty(s)
							" Shouldn't happen often, but if 1st component is
							" negative, it means future time, which we'll
							" indicate with a leading `+'.
							let [s, comp] = ['+', abs(comp)]
						endif
						if !empty(s) | let s .= ',' | endif
						let s .= comp . s:TIME_UNITS[i][1]
					endif
				endfor
			endif
			" Replace the component list with corresponding string.
			" TODO: Alternatively, could build in list and replace the entire
			" thing after loop.
			let self.nodes[seq] = s
		endfor
		" Return the hash.
		" TODO: Any reason to support Get_nodes() for subsequent access?
		return self.nodes
	endfu
	fu! me.Get_nodes()
		return self.nodes
	endfu
	return me
endfu

" Inputs:
" patt:        the pattern to match
" pos:         constraint pos (1-based [row, col])
" [len]:       constraint length
"              Design Decision: Don't support multiline patterns: no use-case.
" [anchor(s)]: optional bool or [bool, bool] indicating whether or not patt is
"              anchored at edge(s). If List provided, begin/end anchor are
"              specified separately. (Defaults to unanchored.)
fu! s:Make_regex(patt, pos, ...)
	if a:0
		let len = a:1
		let anchors = a:0 > 1
			\ ? type(a:2) == 3 ? a:2 : repeat([!!a:2], 2) : [0, 0]
		" Range
		let re = '\%' . a:pos[0] . 'l\&\%('
			\ . '\%' . (anchors[0] ? a:pos[1] : '>' . (a:pos[1] - 1)) . 'c\&'
			\ . '\%(' . a:patt . '\)'
			\ . '\%' . (anchors[1]
				\ ? (a:pos[1] + len)
				\ : '<' . (a:pos[1] + len + 1))
			\ . 'c\)'
	else
		" Exact position
		let re = '\%' . a:pos[0] . 'l\&\%' . a:pos[1] . 'c\&' . a:patt
	endif
	" TODO: Consider optimal way to apply constraint: e.g., is it faster to
	" check the patt or the constraint?
	return re
endfu

" Inputs:
" ids: list of matchadd ids for the level being built.
" gnode: child geom node
" detailed: nonzero if detailed node display is desired
" TODO: Let this be based entirely on lvl, with the regexes pulled from geom
" cache where they were stored by Build_tree_display.
fu! s:Update_syn_tree_node(ids, gnode, detailed)
	let [node, gi, row, col] =
		\ [a:gnode.node, a:gnode.geom, a:gnode.row, a:gnode.col]
	" Loop over the 1 or 2 rows of node text beginning at row.
	let i = 0
	while i < len(gi.es)
		let re = s:Make_regex('.*', [row + i, col + gi.es[i][0]], gi.w[i])
		" TODO: Don't hardcode the priorities like this.
		" Note: Priority 10 is default. It will override hlsearch and such, but
		" should be lowest of the undo-tree groups.
		call add(a:ids, matchadd('undo_redo_path', re, 10))
		let i += 1
	endwhile
	if !node.lvl
		" Nothing else to do for root.
		return
	endif

	" Vertical or diagonal up to horizontal header bar
	if node.x < 0
		let [off, text] = [1, '/']
	elseif node.x > 0
		" Escape for regex
		let [off, text] = [-1, '\\']
	else
		let [off, text] = [0, '|']
	endif
	let re = s:Make_regex(text, [row - 1, col + off])
	call add(a:ids, matchadd('undo_redo_path', re, 10))

	" Horizontal header bar
	" Note: repeat() will produce empty string for negative count, and this
	" will happen, for instance, when child is unit distance from parent.
	if node.x < 0
		let [off, text] = [2, repeat('_', -node.x - 2)]
	elseif node.x > 0
		let [off, text] = [-node.x + 1, repeat('_', node.x - 2)]
	else
		" Child directly under parent => no horizontal
		let [off, text] = [0, '']
	endif
	if !empty(text)
		" We have a piece of horizontal to display.
		let re = s:Make_regex(text, [row - 2, col + off])
		call add(a:ids, matchadd('undo_redo_path', re, 10))
	endif

	" Vertical to parent
	let re = s:Make_regex('|', [row - 2, col - node.x])
	call add(a:ids, matchadd('undo_redo_path', re, 10))

endfu

" Return nonzero iff tree needs to be updated.
" Note: No reason to introduce distinct dirty flag, since Clear() sets seq to
" -1, and Update() sets it to valid seq number.
fu! s:Is_syn_dirty() dict
	return self.seq < 0
endfu

fu! s:Clear_syn_tree() dict
	let self.ids = []
	let self.seq = -1
endfu

" Add or erase brackets around input node from geom.nodes dictionary.
" gnode: {'node': {}, 'col': <colnr>, 'row': <rownr>}
" TODO: Make more generic by changing 'erase' flag to an arg that indicates the
" wrap char desired.
fu! s:Bracket_node(gnode, erase, detailed)
	let [tnode, row, col, gi] =
		\ [a:gnode.node, a:gnode.row, a:gnode.col, a:gnode.geom]
	" TODO: For now, just bracket the seq # row (always first when more than
	" one); consider whether we should do something for the 'ago' time row as
	" well.
	let [scol, ecol] = [col + gi.es[0][0], col + gi.es[0][1]]
	"echomsg "gi: " . string(gi) . ", scol=" . scol . ", ecol=" . ecol
	" TODO: Make change manually? Or with setline()?
	setl modifiable
	let s = getline(row)
	call setline(row,
		\ strpart(s, 0, scol - 1) .
		\ (a:erase ? ' ' : '[') .
		\ strpart(s, scol, gi.w[0] - 2) .
		\ (a:erase ? ' ' : ']') .
		\ strpart(s, ecol))
	setl nomodifiable
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
	while !empty(t)
		call add(self.ids, [])
		let gnode = self.geom.nodes[t.seq]
		call s:Update_syn_tree_node(self.ids[-1], gnode, self.detailed)
		let t = t.children.cur
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

fu! s:Make_syn_tree(geom, detailed)
	let me = {
		\ 'ids': [],
		\ 'seq': -1,
		\ 'Clear': function('s:Clear_syn_tree'),
		\ 'Is_dirty': function('s:Is_syn_dirty'),
		\ 'Update': function('s:Update_syn'),
		\ 'geom': a:geom,
		\ 'detailed': a:detailed
		\ }

	return me
endfu

" For convenience, ensure these script-locals always exist.
let [s:bufnr, s:winid] = [-1, -1]
let [s:undo_bufnr, s:undo_winid] = [-1, -1]
let s:diff_bufnr = -1
let s:undo_cache = {}
let s:actions = {}
let s:mark_seq = -1

" For use by the override mechanism.
let s:opt_overrides = {}

" Option configuration
let s:opt_cfg = {
	\ 'showdiff': {'type': 6, 'default': v:true},
	\ 'diffheight': {'type': 0, 'min': 2, 'default': 10}
\ }

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
fu! s:Unconfigure_treebuf()
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
		au! undo_tree_treebuf
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

fu! s:Unconfigure_diffbuf()
endfu

fu! s:Create_autocmds_in_treebuf()
	" Create autocmds that will permit us to clean up when child buf deleted
	aug undo_tree_treebuf
		au!
		au BufDelete <buffer> call s:Treebuf_BufDelete(expand("<abuf>"))
		au BufEnter <buffer> call s:Treebuf_BufEnter()
		au BufLeave <buffer> call s:Treebuf_BufLeave()
		"au TextChanged <buffer> call s:Treebuf_TextChanged()
		"au TextChangedI <buffer> call s:Treebuf_TextChanged()
		au BufWinLeave <buffer> call s:Treebuf_BufWinLeave(expand("<abuf>"))
		au WinEnter * call s:WinEnter()
		au WinLeave * call s:WinLeave()
	augroup END
endfu

fu! s:Create_autocmds_in_diffbuf()
	" Create autocmds that will permit us to clean up when child buf deleted
	aug undo_tree_diffbuf
		au!
		au BufDelete <buffer> call s:Diffbuf_BufDelete(expand("<abuf>"))
		au BufEnter <buffer> call s:Diffbuf_BufEnter()
		au BufLeave <buffer> call s:Diffbuf_BufLeave()
		au BufWinLeave <buffer> call s:Diffbuf_BufWinLeave(expand("<abuf>"))
	augroup END
endfu

fu! s:Create_mappings_in_diffbuf()
	" Create buffer-local mapping(s)
	" TODO: Any?
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

" TODO: Consider implementing Get_parent_path accessor to avoid passing hte
" parent path down the call chain like this.
fu! s:Create_mark_mappings_in_treebuf(parent_path)
	" Create maps for all possible lowercase marks.
	for ltr in map(range(char2nr('a'), char2nr('z')), 'nr2char(v:val)')
		exe printf('nnoremap <silent> <nowait> <buffer> m%s'
			\ . ' :<C-U>call <SID>Set_mark("%s", "%s")<CR>',
			\ ltr, escape(a:parent_path, '"\'), ltr)
		exe printf('nnoremap <silent> <nowait> <buffer> ''%s'
			\ . ' :<C-U>call <SID>Goto_mark("%s", "%s")<CR>',
			\ ltr, escape(a:parent_path, '"\'), ltr)
	endfor
endfu

fu! s:Create_mappings_in_treebuf(parent_path)
	" Create buffer-local mapping(s)
	" To refresh the tree forcibly
	nnoremap <silent> <nowait> <buffer> R :call <SID>Refresh_treebuf()<CR>

	" Moving up/down and changing undo/redo path through tree.
	" Design Decision: No reason to avoid using regular Vim motion commands.
	" Rationale: Cursor movement is highly constrained.
	nnoremap <silent> <nowait> <buffer> k  :<C-U>call <SID>Move_in_tree('up')<CR>
	nnoremap <silent> <nowait> <buffer> j  :<C-U>call <SID>Move_in_tree('down')<CR>
	nnoremap <silent> <nowait> <buffer> h  :<C-U>call <SID>Move_in_tree('left')<CR>
	nnoremap <silent> <nowait> <buffer> l  :<C-U>call <SID>Move_in_tree('right')<CR>
	nnoremap <silent> <nowait> <buffer> C  :<C-U>call <SID>Center_tree(1, 0)<CR>
	nnoremap <silent> <nowait> <buffer> G  :<C-U>call <SID>Goto_node_in_tree(v:count)<CR>
	" Note: gg works like G except that when no count is supplied, it defaults
	" to leaf node, not root.
	nnoremap <silent> <nowait> <buffer> gg
		\ :<C-U>call <SID>Goto_node_in_tree(v:count ? v:count : -1)<CR>
	nnoremap <silent> <nowait> <buffer> X  :<C-U>call <SID>Toggle_display_mode_handler()<CR>
	nnoremap <silent> <nowait> <buffer> q  :<C-U>call <SID>Close_undotree()<CR>
	nnoremap <silent> <nowait> <buffer> D  :<C-U>call <SID>Toggle_diff()<CR>
	call s:Create_mark_mappings_in_treebuf(a:parent_path)
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

fu! s:Create_syntax_in_treebuf()
	" Design Decision: Could do this once up front, but re-doing it is cheap,
	" and allows changes to g:undotree_path_hlgroup to be taken into account.
	exe 'hi undo_redo_path gui=bold cterm=bold term=bold'
		\ .	s:Get_undoredo_path_color_attrs()
endfu

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
	if has_key(s:opt_overrides, a:name)
		exe 'let &' . a:name ' = s:opt_overrides[a:name]'
		call remove(s:opt_overrides, a:name)
	endif
endfu

" Rationale: The reason for refusing to save an already-overridden value is that
" if we somehow managed to do 2 consecutive overrides from this script, we might
" inadvertently save our own setting for subsequent restoration. This would be
" particularly bad if the option were 'guicursor'. Refusing to save an override
" ensures this can't happen (at the cost of a bit of complexity).
" TODO: Doing the save only in BufEnter (and in Configure_treebuf when we're sure
" the BufEnter has been skipped) could also prevent the problem scenario, and
" would obviate the need for the more complex override mechanism. Consider
" whether this mechanism is justified.
" TODO: Consider adding a 'local' flag parameter. (May not be necessary, given
" that we typically wouldn't need to 'override' local params.)
fu! s:Override_opt(name, value)
	"let setter = s:Get_opt_setter(a:name)
	if !has_key(s:opt_overrides, a:name)
		" Save old value.
		exe 'let s:opt_overrides[a:name] = &' . a:name
	endif
	" Override.
	exe 'let &' . a:name . ' = a:value'
endfu

fu! s:Configure_cursor_in_treebuf()
	if has('gui')
		call s:Override_opt('guicursor', 'n-v:block-NONE')
	else
		call s:Override_opt('t_ve', '')
	endif
endfu

" Note: Redundant calls are safe.
fu! s:Unconfigure_cursor_in_treebuf()
	call s:Restore_opt(has('gui') ? 'guicursor' : 't_ve')
endfu

" TODO: Perhaps make it so stuff isn't redone unnecessarily (though all of this
" should be safe to redo).
" TODO: Make sure child has all requisite autocmds and such.
fu! s:Configure_treebuf(parent_path)
	call s:Create_autocmds_in_treebuf()
	call s:Create_mappings_in_treebuf(a:parent_path)
	call s:Create_syntax_in_treebuf()
	" Note: This will also be called in child BufEnter, but that call may have
	" been skipped due to :noauto.
	call s:Configure_cursor_in_treebuf()
	" TODO: Decide whether separate function should be used for options.
	" Rationale: Tree display falls apart if 'wrap' on
	setl nowrap
endfu

fu! s:In_treebuf()
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

fu! s:Hide_buffer(bnr)
	let wid_orig = win_getid()
	for wid in win_findbuf(a:bnr)
		let [tnr, wnr] = win_id2tabwin(wid)
		noauto call win_gotoid(wid)
		" Attempt to hide the child, noting that command will fail (harmlessly)
		" if it's the last window.
		" Caveat: 'noauto' modifier needed to prevent BufWinLeave firing for the
		" child.
		silent! noauto hide
	endfor
	" Return to starting window.
	noauto call win_gotoid(wid_orig)
endfu

fu! s:Orphan_treebuf()
	call s:Unconfigure_parent()
	" TODO: Determine whether there could be any reason to call Delete_children
	" when s:undo_winid has already been cleared (-1). I'm thinking not.
	if s:undo_winid > 0
		" Also unconfigure child so we don't need to worry about its autocmds
		" triggering again.
		" Rationale: :bd doesn't delete <buffer> autocmds.
		call s:Unconfigure_treebuf()
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
	call s:Orphan_treebuf()
endfu

" Detect when parent buffer has departed its original window.
fu! s:BufEnter()
	if s:winid == win_getid() && s:bufnr != bufnr('%')
		" Parent buffer is leaving original window!
		call s:Orphan_treebuf()
	endif
endfu

" Child buffer is being deleted, or perhaps just hidden/unloaded.
" Note: BufDelete implies BufWinLeave (though the converse is not true), so
" there is some harmless redundancy.
fu! s:Treebuf_BufWinLeave(bnr)
	call s:Unconfigure_treebuf()
endfu

" Note: User shouldn't typically delete the child manually, but if he does, we
" will lose the maps in the child buffer, so we need to ensure that the child
" goes through full reconfiguration next time it's needed.
fu! s:Treebuf_BufDelete(bnr)
	call s:Unconfigure_treebuf()
endfu

" Each time child buf is entered in its special window, we need to check to see
" whether it's in need of refresh.
" Called When: Moving to child buf's special window.
" Assumption: Since child is unconfigured when child buffer first leaves special
" window, a BufEnter in the special child window should always imply window
" change, rather than (e.g.) :buffer command.
fu! s:Treebuf_BufEnter()
	" Ignore BufEnter in non-special window.
	if s:In_treebuf()
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
			call s:Center_tree(0, 1)
			call s:Configure_cursor_in_treebuf()
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
fu! s:Treebuf_BufLeave()
	if s:undo_winid == win_getid()
		call s:Unconfigure_cursor_in_treebuf()
	endif
endfu

" In case user changes child buffer contents.
" TODO: Probably remove this, as I'm thinking we shouldn't protect against it.
fu! s:Treebuf_TextChanged()
endfu

fu! s:Diffbuf_BufDelete()
endfu
fu! s:Diffbuf_BufEnter()
endfu
fu! s:Diffbuf_BufLeave()
endfu
fu! s:Diffbuf_BufBufWinLeave()
endfu

" Note: WinEnter/Leave used to determine when cursor needs to be shown/hidden.
" Important Note: If child appears in multiple windows, only one gets special
" cursor treatment.
fu! s:WinEnter()
	if s:undo_winid == win_getid()
		call s:Configure_cursor_in_treebuf()
	endif
endfu

fu! s:WinLeave()
	if s:undo_winid == win_getid()
		call s:Unconfigure_cursor_in_treebuf()
	endif
endfu

" Calculate child size in the variable direction, taking into account the input
" splitinfo (applicable component only) and available space (in free direction).
" Assumption: undo cache is valid and parent is established
fu! s:Calc_child_size(splitinfo)
	let si = a:splitinfo
	let geom = s:undo_cache.geom
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
fu! s:Position_treebuf(splithow)
	let geom = s:undo_cache.geom
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
	let cmd = ' +setl\ buftype=nofile\ bufhidden=hide\ noswapfile\ nomodifiable '
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
	" TODO: Perhaps refactor this to use Hide_buffer...
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

fu! s:Option_get(name)
	let cfg = s:opt_cfg[a:name]
	if !exists('g:undotree_' . a:name)
		" User didn't override.
		return cfg.default
	endif
	" User has set.
	let v = g:undotree_{a:name}
	let vt = type(v)
	if cfg.type == 6 && (vt == 0 && (v == 0 || v == 1))
		\ || cfg.type == vt
		" Correct type
		if vt == 0 || vt == 5
			" Check min/max constraints if applicable.
			if has_key(cfg, 'max') && v > cfg.max
				echomsg printf(
					\ "Warning: Provided %s option value of %d is greater than max:"
					\." setting to %d",
					\ a:name, v, cfg.max)
				v = cfg.max
			elseif has_key(cfg, 'min') && v < cfg.min
				echomsg printf(
					\ "Warning: Provided %s option value of %d is less than min:"
					\." setting to %d",
					\ a:name, v, cfg.min)
				v = cfg.min
			endif
		endif
	else
		" Wrong type!
		echomsg printf(
			\ "Warning: Ignoring value for option '%s': must be Vim type %d",
			\ a:name, cfg.type)
		v = cfg.default
	endif
	return v
endfu

" Pick the best parent window number.
fu! s:Set_parent()
	if s:bufnr < 0 || bufnr(s:bufnr) < 0
		" No valid parent buffer exists.
		let s:winid = -1
		return -1
	endif
	" Parent buffer exists. Is it in a window in current tab?
	let this_tnr = tabpagenr()
	if s:winid > 0
		let [tnr, wnr] = win_id2tabwin(s:winid)
		if wnr > 0 && tnr == this_tnr
			return wnr
		endif
	endif
	" Initial window doesn't exist any longer. Find one in current tab that
	" contains the buffer. If more than one, pick tallest.
	let [best_wnr, max_ht] = [-1, -1]
	for winid in win_findbuf(s:bufnr)
		let [tnr, wnr] = win_id2tabwin(winid)
		let ht = winheight(wnr)
		if this_tnr == tnr && ht > max_ht
			let best_wnr = wnr
		endif
	endfor
	let s:winid = best_wnr > 0 ? win_getid(best_wnr) : -1
	" Could still be -1
	return best_wnr
endfu

fu! s:Goto_parent()
	" Assumption: Parent is visible.
	" Rationale: BufWinLeave event...
	noauto call win_gotoid(s:winid)
endfu

fu! s:Goto_diffbuf()
	" Assumption: The buffer's existence has been validated.
	exe 'noauto ' . bufwinnr(s:diff_bufnr) . 'wincmd w'
endfu

fu! s:Goto_treebuf()
	" Assumption: The buffer's existence has been validated.
	exe 'noauto ' . bufwinnr(s:undo_bufnr) . 'wincmd w'
endfu

" Set 'wfh' and 'wfw' for all windows in current tab page, except for any whose
" window numbers are input. Return a pair of lists containing window ids, which
" can be passed to Unfreeze_windows() to unfreeze only those windows that need
" it (and only in the necessary directions):
" Note: Intentionally using window ids rather than nrs to permit windows to be
" closed between Freeze and Unfreeze.
" Return Format: [[wfw-1, wfw-2, ...], [wfh-1, wfh-2, ...]]
fu! s:Freeze_windows(...)
	let ret = [[], []]
	let skip_wids = a:000
	for wnr in range(1, winnr('$'))
		let wid = win_getid(wnr)
		if index(skip_wids, wid) < 0
			" Not skipping this window.
			for wh in ['w', 'h']
				if !getwinvar(wnr, '&wf' . wh)
					" Freeze and record the fact that we've done so.
					call setwinvar(wnr, '&wf' . wh, 1)
					call add(ret[wh == 'w' ? 0 : 1], wid)
				endif
			endfor
		endif
	endfor
	return ret
endfu

" Unfreeze windows frozen by Freeze_windows.
" Input: Takes the pair of lists (0=width 1=height) returned by Freeze_windows.
fu! s:Unfreeze_windows(wid_lists)
	let whs = ['w', 'h']
	for wids in a:wid_lists
		" Handle one of the two dimensions.
		let wh = remove(whs, 0)
		for wid in wids
			let wnr = win_id2win(wid)
			" Make sure window hasn't been closed since freeze.
			if wnr > 0
				" Unfreeze
				call setwinvar(wnr, '&wf' . wh, 0)
			endif
		endfor
	endfor
endfu

" Assumption: Parent exists in window in current tab.
fu! s:Position_diffbuf()
	call s:Goto_parent()
	" Save viewport so we can restore after opening (or re-opening) child.
	" Rationale: Opening child window (e.g.) can effectively scroll the parent.
	let p_wsv = winsaveview()

	" Note: This may clear the bufnr; its purpose is to ensure that the bufnr is
	" still valid (i.e., buffer hasn't been wiped).
	let s:diff_bufnr = s:diff_bufnr > 0 ? bufnr(s:diff_bufnr) : -1
	" Note: 'silent' prevents annoying and misleading [new file] msg.
	let cmd = ' +setl\ buftype=nofile\ bufhidden=hide\ noswapfile\ nomodifiable '
	let size = s:Option_get('diffheight')
	try 
		" Make sure only the parent window's size is affected by the split.
		let ea_save = &ea | set noea
		if s:diff_bufnr > 0
			" Re-using child buffer.
			" Note: bufwinnr() ignores other tabs.
			let wnr = bufwinnr(s:diff_bufnr)
			if wnr > 0
				" Buf is in a window; save its viewport for restore.
				" Design Decision: User shouldn't be opening the special bufs
				" manually; don't bother protecting against it.
				exe 'noauto ' . wnr . 'wincmd w'
				let c_wsv = winsaveview()
				" Return to parent.
				noauto wincmd p
				" Make sure we end up with the diff displayed in only 1 window.
				" TODO: I'm thinking ok to generate BufWinLeave here, as that
				" should be significant only for parent under redesign.
				call s:Hide_buffer(s:diff_bufnr)
			endif
			" Note: 'noauto' modifier prevents premature attempt to refresh.
			exe 'silent noauto below ' . s:diff_bufnr . 'sb' . cmd
			" IMPORTANT TODO: Set its size (since sb doesn't accept one...
			" !!!!! UNDER CONSTRUCTION !!!!!
			" Use Freeze_windows mechanism to ensure that the additional height
			" is given to parent!!!
			exe 'wincmd ' . size . '_'
			if exists('l:c_wsv')
				" Buffer was already displayed; restore its viewport.
				call winrestview(c_wsv)
			endif
		else
			" Create new window containing scratch buffer.
			" Caveat: Wanted to wrap name in [...], but that makes Vim think it's
			" directory.
			exe 'silent below ' . size . 'new ' . cmd . expand('%') . '.diff'
			let s:diff_bufnr = bufnr('%')
		endif
	finally
		" TODO: Need a workaround for fact that simply *setting* ea causes all
		" windows to be made same size!!!! Maybe view restore?
		" Idea: Use wincmd j to check whether diff win is directly below parent,
		" and if so, add heights of both parent and diff win to get new parent
		" height. Alternatively, temporarily set 'winfixheight' in all but
		" parent.
		if ea_save
			" TODO: Should we attempt to protect with try/catch? If so, how?
			let freeze_wids = s:Freeze_windows()
			let &ea = ea_save
			call s:Unfreeze_windows(freeze_wids)
		endif
	endtry
	" Make sur the parent's viewport hasn't changed.
	noauto wincmd p
	call winrestview(p_wsv)
	" Back to newly-positioned diff buf.
	noauto wincmd p

endfu

fu! s:Configure_diffbuf()
	call s:Create_autocmds_in_diffbuf()
	call s:Create_mappings_in_diffbuf()
	" TODO: Decide whether separate function should be used for options.
	" Rationale: Tree display falls apart if 'wrap' on
	setf diff
endfu

" Make sure an undo_buf exists, is visible, and is properly configured, creating
" if necessary.
" Assumption: In parent buffer
fu! s:Prepare_treebuf(splithow)
	" Before creating/moving to child, save parent's path to obviate need for
	" Configure_treebuf to switch back to parent's buffer to get it.
	let p_path = expand('%:p')
	call s:Position_treebuf(a:splithow)
	" Note: We call this even when buf already existed, to ensure it's got all
	" the mappings and autocmds we'll need. (It's unlikely, but possible, that
	" someone deleted maps/autocmds, for instance.)
	call s:Configure_treebuf(p_path)
endfu

" Return wnr (always truthy) or 0 if diff buf not visible in curent tab.
fu! s:Is_diff_visible()
	let wnr = s:diff_bufnr > 0 ? bufwinnr(s:diff_bufnr) : -1
	return wnr > 0 ? wnr : 0
endfu

fu! s:Prepare_diffbuf()
	call s:Position_diffbuf()
	call s:Configure_diffbuf()
endfu

fu! s:Compare_states(seqs, cur_seq, ...)
	let tgt_seq = a:0 ? a:1 : -1
	" Determine which seq number to start with to minimize state switches.
	" Logic:
	"   If current state is one of the 2 we need, do it first.
	"   Else if target state is one of the 2 we need, do it last.
	let sidx = index(a:seqs, a:cur_seq)
	if sidx < 0
		" Current state isn't one of the 2 we need.
		let sidx = index(a:seqs, tgt_seq)
		if sidx >= 0
			" Do target state last.
			let sidx = !sidx
		endif
	endif
	if sidx < 0
		" No need to change default order.
		let sidx = 0
	endif
	let fs = ['', '']
	try
		" TODO: Consider whether to disable folding temporarily as well.
		let wsv = winsaveview()
		" Note: Docs on winsaveview() suggest disabling folding temporarily to
		" prevent folds being opened while moving around.
		" Rationale: Fold information is not returned by winsaveview(); thus,
		" if folding were enabled while moving around, folding could change as
		" a side-effect of the movement.
		let fen = &foldenable
		set nofen
		let i = 0
		while i < 2 
			let i_cmp = (i + sidx) % 2
			silent exe 'undo ' . a:seqs[i_cmp]
			let fs[i_cmp] = tempname()
			" Write this undo state to temporary file.
			exe 'w ' . fs[i_cmp]
			let i += 1
		endwhile
		" We now have 2 temporary files for comparison.
		" TODO: Eventually, support diff options, but for now just do default.
		let res = systemlist(printf("diff %s %s", fs[0], fs[1]))
	finally
		" Move to the target undo state if provided, else return to current.
		silent exe 'undo ' . (tgt_seq >= 0 ? tgt_seq : a:cur_seq)
		" Restore folds and view setting.
		" Note: First restore 'fen' to setting it had when view was saved.
		let &fen = fen
		call winrestview(wsv)
		" Don't leave temp files lying around.
		for f in fs
			call delete(f)
		endfor
	endtry
	" Return diff as list of lines.
	return res
endfu

" TODO: Have this used elsewhere...
fu! s:Put_buf_contents(lines)
	try
		setl modifiable
		" Replace undo buffer's contents.
		silent %d
		call append(0, a:lines)
		" Append adds a final blank line, which serves no purpose, and which we
		" don't want to impact window sizing, so delete it.
		$d
		" Caveat: The preceding :$d can shift the viewport.
		norm! gg
	finally
		setl nomodifiable
	endtry
endfu

fu! s:Update_diff()
	" Goto parent to compute the diff
	call s:Goto_parent()
	let t = s:undo_cache.tree.cur
	let seq2 = t.seq
	" TODO: Implement marking of seq.
	let seq1 = s:mark_seq < 0
		\ ? empty(t.parent) ? -1 : t.parent.seq
		\ : s:mark_seq

	" Perform the comparison.
	" TODO: What to do if nothing to compare? (Currently, can happen only if
	" seq2 is root.)
	let lines = seq1 >= 0
		\ ? s:Compare_states([seq1, seq2], seq1, seq2)
		\ : []
	call s:Goto_diffbuf()
	call s:Put_buf_contents(lines)
endfu

fu! s:Describe_node(...)
	" If user didn't pass a seq number, describe current node.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	let t = s:undo_cache.geom.nodes[seq].node
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
" Input(s):
"   limit  zero iff it's ok to scroll end of buffer above bottom of window.
"   [seq]  (optional) seq number of node to center (default current node)
" Design Decision: Don't try to prevent a shift that Vim (harmlessly) disallows:
" e.g., trying to center the root node will most likely fail (unless window is
" very short), but so what? Vim itself ensures that we shift only as far as
" possible and no further. The shift we must limit (when 'limit' is set) is the
" one that Vim allows, but which we may wish to prevent in certain scenarios:
" namely, the shift that would pull empty lines into view. When 'limit' input is
" set, we not only limit such shifts, but actually attempt, if possible, to
" prevent empty lines/cols altogether.
" Rationale: There are scenarios in which the desire for centering is tempered
" by a desire to show as much of the tree as possible: in such cases, we bring
" the node towards the center only until further such shifting would diminish
" the visible portion of the tree.
" TODO: Any advantage to having this in separate function? If not, subsume into
" Center_tree.
fu! s:Calculate_tree_node_offset(limit, ...)
	let ret = {'visible': 1}
	" If user didn't pass a seq number, center on current.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	" Determine current horiz/vert view scroll amounts.
	let [vscroll, hscroll] = [line('.') - winline(), col('.') - wincol()]
	let [wh, ww] = [winheight(0), winwidth(0)]
	" Get the geom node corresponding to node.
	let gnode = s:undo_cache.geom.nodes[seq]
	let [gi, t] = [gnode.geom, gnode.node]
	" Calculate center point (which, may be in the middle of a sequence of label
	" rows).
	let nrows = len(gi.text)
	let [row1, row2] = [gnode.row, gnode.row + nrows - 1]
	let [row, col] = [(row1 + row2) / 2, gnode.col]
	" Calculate 1-based offset from edge of window to points of interest.
	let [row_rel, row1_rel, row2_rel, col_rel] =
		\ [row - vscroll, row1 - vscroll, row2 - vscroll, col - hscroll]
	" Check edges of node to see whether any part is not visible.
	" Note: For check with bottom of window, use bottom of node (whose row may
	" differ from row of top), and for check with edges, use the merged extent
	" (which may take multiple rows into account).
	if row1_rel < 1 || row2_rel > wh ||
		\ col_rel + gi.e[0] < 1 || col_rel + gi.e[1] > ww
		let ret.visible = 0
	endif
	" Calculate delta: pos delta => scroll view right/down to center.
	let ret.dy = row_rel - (wh + 1) / 2
	let ret.dx = col_rel - (ww + 1) / 2
	if a:limit
		" Minimize the number of empty lines/cols at edge of window.
		" If buffer is smaller than window, of course, we won't be able to avoid
		" empty lines/cols, but the 'limit' flag requests fewer empties even at
		" the cost of not centering.
		let ret.dy = min([ret.dy, line('$') - vscroll - wh])
		let ret.dx = min([ret.dx, col('$') - hscroll - ww])
	endif
	return ret
endfu

" If 'force' flag is true or requested node is even partially outside view,
" center tree on current node (default) or node whose seq is input.
" If 'limit' flag is true, minimize the number of completely empty lines/cols at
" edge of window (even at cost of centering).
" Assumption: In child buffer with consistent cache
fu! s:Center_tree(force, limit, ...)
	" If user didn't pass a seq number, center on current.
	let seq = a:0 ? a:1 : s:undo_cache.tree.cur.seq 
	let oi = s:Calculate_tree_node_offset(a:limit, seq)
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

" Goto (and center on) the node whose seq number is provided, or leaf node if
" input seq == -1.
fu! s:Goto_node_in_tree(seq)
	if a:seq < 0
		" Default to leaf node at end of current redo path.
		let node = s:undo_cache.tree.cur.Get_leaf()
	elseif !a:seq
		" Root node
		let node = s:undo_cache.tree
	else
		" Get the node, bearing in mind that there's no guarantee it exists.
		let geom = s:undo_cache.geom
		let gnode = has_key(geom.nodes, a:seq) ? geom.nodes[a:seq] : {}
		if empty(gnode)
			echomsg "Warning: Node " . a:seq . " does not exist."
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
	call s:Center_tree(1, 1, node.seq)
	" Update display.
	call s:undo_cache.syn.Update(node)
	" Output summary of current change.
	call s:Describe_node()
	if s:Is_diff_visible()
		call s:Update_diff()
	endif

endfu

fu! s:Set_mark(parent_path, ltr)
	" TODO: Functionize this test, as it's used in multiple places.
	if s:undo_bufnr < 0 || s:undo_winid != win_getid()
		" Ignore maps in non-special child window.
		" TODO: If we stick with this approach, should probably use normal! to
		" let the key sequence have its normal effect.
		return
	endif
	if s:bufnr < 0 || s:winid < 0 || !win_id2win(s:winid)
		" TODO: Can this happen?
		echomsg "Warning: Cannot traverse undo tree for inactive parent buffer"
	endif
	call s:marks.Add(a:parent_path, a:ltr, s:undo_cache.tree.cur.seq)
endfu

fu! s:Goto_mark(parent_path, ltr)
	" TODO: Functionize this test, as it's used in multiple places.
	if s:undo_bufnr < 0 || s:undo_winid != win_getid()
		" Ignore maps in non-special child window.
		" TODO: If we stick with this approach, should probably use normal! to
		" let the key sequence have its normal effect.
		return
	endif
	if s:bufnr < 0 || s:winid < 0 || !win_id2win(s:winid)
		" TODO: Can this happen?
		echomsg "Warning: Cannot traverse undo tree for inactive parent buffer"
	endif
	let seq = s:marks.Get_seq(a:parent_path, a:ltr)
	if seq >= 0
		" TODO: Get rid of defaults_to_root arg.
		call s:Goto_node_in_tree(seq)
	endif
endfu

" Called From: child buffer maps for moving in tree.
" Assumption: Can be invoked only when fresh undo data structures exist.
" Rationale: Invoked from undo buffer mappings, and freshness is checked in
" BufEnter. Hmm... What if user executes a :bd or something. Well, in that case,
" parent's BufDelete would fire, clearing data structures.
fu! s:Move_in_tree(dir) " entry
	" TODO: Maybe get rid of the "specialness" test.
	if s:undo_bufnr < 0 || s:undo_winid != win_getid()
		" Ignore maps in non-special child window.
		" TODO: If we stick with this approach, should probably use normal! to
		" let the key sequence have its normal effect.
		" TODO: Consider using feedkeys() (with no remap "n" in mode arg).
		" Another possibility would be to disable the mapping that got us here
		" just long enough to run feedkeys() with mapping enabled (in case user
		" has j, k, etc. remapped). This is a lot of trouble, which could be
		" obviated if I didn't insist on the "special" window approach... Of
		" course, this would mean reworking syntax a bit to make it
		" buffer-specific.
		return
	endif
	if s:bufnr < 0 || s:winid < 0 || !win_id2win(s:winid)
		" TODO: Can this happen?
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
	call s:Center_tree(0, 1)
	" Output summary of current change.
	call s:Describe_node()
	" Update the diff buffer if applicable.
	" Note: If it's not visible, don't resurrect; assume user wanted it closed.
	if s:Is_diff_visible()
		call s:Update_diff()
	endif
	call s:Goto_treebuf()
endfu

fu! s:Toggle_display_mode_handler()
	if !s:In_treebuf()
		" Ignore invocation from non-special child window.
		return
	endif
	call s:undo_cache.Toggle_display_mode()
	call s:Refresh_undo_window()
endfu

" Invoked from child's <buffer> map.
" Assumption: Can't get here unless we're in true child buffer (though we will
" need to check to be sure we're in special window).
fu! s:Refresh_treebuf() " entry
	if !s:In_treebuf()
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
		call s:Orphan_treebuf()
		return
	endif
	" There's a visible and active parent. Forcibly refresh
	call s:Refresh_cache(1)
	" TODO: Decide whether to force the window refresh, or only the cache
	" refresh: i.e., if cache was up to date, perhaps we should pass 0 here...
	" Note: Has implications for centering tree...
	call s:Refresh_undo_window()
	call s:Center_tree(1, 1)
endfu

" TODO: Consider calling this Build_tree_display_pre, as it builds a portion of
" the objects in the node hash and passes it along for use in both Design() and
" Build_tree_display proper.
fu! s:Build_nodes_hash(tree, detailed)
	if a:detailed
		let ago = s:Make_ago()
	endif
	" Format of hash values:
	" node: {}, geom: {text: '', e: [], w: 0}}
	" Note: col/row will be added later, during tree build.
	let nodes = {}
	" Breadth-first traversal
	" Fifo elements: <node>
	let fifo = [a:tree]
	while !empty(fifo)
		let t = remove(fifo, 0)
		" Add this node's children to fifo
		let tc = t.children.fst
		while !empty(tc)
			call add(fifo, tc)
			let tc = tc.next
		endwhile
		" Process current node.
		if a:detailed && t.seq
			call ago.Add(t.seq, t.time)
			" Can't finalize the node geom yet.
			let gi = {}
		else
			" Note: Root is handled here, regardless of display mode.
			let gi = s:Make_node_geom(t, 0)
		endif
		let nodes[t.seq] = {'node': t, 'geom': gi}
	endwhile
	if a:detailed
		" Iterate the hash, building and hashing node geoms.
		for [seq, ago_str] in items(ago.Finalize())
			let nodes[seq].geom = s:Make_node_geom(nodes[seq].node, a:detailed, ago_str)
		endfor
	endif
	return nodes
endfu

fu! s:Dbg_build(uc)
	let a:uc.dirty = 1
	let nodes = s:Build_nodes_hash(a:uc.tree, a:uc.detailed)
	let [ptree, pextent] = s:Design(nodes, a:uc.tree)
	let a:uc.geom = s:Build_tree_display(ptree, pextent, a:uc.detailed)
	let a:uc.syn = s:Make_syn_tree(a:uc.geom, a:uc.detailed)
endfu
fu! s:Make_undo_cache(tree, detailed)
	let me = {
		\ 'dirty': 1,
		\ 'tree': a:tree,
		\ 'detailed': a:detailed
	\}
	fu! me.Build()
		let self.dirty = 1
		let nodes = s:Build_nodes_hash(self.tree, self.detailed)
		let [ptree, pextent] = s:Design(nodes, self.tree)
		let self.geom = s:Build_tree_display(ptree, pextent, self.detailed)
		let self.syn = s:Make_syn_tree(self.geom, self.detailed)
	endfu
	" Assumption: Called from child buffer
	fu! me.Toggle_display_mode()
		" Caveat: Need to clear old syntax before releasing the object.
		call self.syn.Clear()
		let self.detailed = !self.detailed
		call self.Build()
	endfu
	"call me.Build()
	call s:Dbg_build(me)
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
			\ ? s:undo_cache.detailed
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
	call s:Prepare_treebuf(splithow)
	" Note: Syntax is currently associated with child window, which always
	" changes, due to the way we do child positioning.
	" Note: Call to clearmatches in Clear is harmless but pointless in this
	" case.
	call s:undo_cache.syn.Clear()
	if s:Option_get('showdiff')
		call s:Prepare_diffbuf()
	endif
	" Everything should be in order now with the 2 windows.
	call s:Refresh_undo_window()
	" Forcibly center current node in limited mode.
	" Rationale: Centering is good, but should not prevent our seeing as much of
	" the tree as possible. (Note that this is especially important when we've
	" sized the child window large enough to display the entire tree.)
	call s:Center_tree(1, 1)
endfu

" Design Decision: Don't try to keep anything cached.
" Rationale: Simplest/safest approach
" TODO: Also give command and/or mapping to accomplish this.
" Rationale: Ensures user can always get back to known state.
fu! s:Close_undotree() " entry
endfu

let S_Freeze = function('s:Freeze_windows')
let S_Unfreeze = function('s:Unfreeze_windows')

fu! s:Toggle_diff()
	if s:Is_diff_visible()
		try 
			" Make sure only the parent window's size is affected by the split.
			let ea_save = &ea | set noea
			" Freeze all but parent, to ensure it gets the freed up space.
			let freeze_wids = s:Freeze_windows(s:winid)
			call s:Goto_treebuf()
			call s:Hide_buffer(s:diff_bufnr)
			" TODO: Do we need to save/restore parent's view?
			call s:Unfreeze_windows(freeze_wids)
		finally
			" Caveat: Setting 'ea' will resize windows if we don't prevent.
			let freeze_wids = s:Freeze_windows()
			let &ea = ea_save
			call s:Unfreeze_windows(freeze_wids)
		endtry
	else
		call s:Prepare_diffbuf()
		call s:Update_diff()
		call s:Goto_treebuf()
	endif

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
	" TODO: Consider refactoring flags: here are the things that could
	" potentially be invalid:
	" -cache itself has been changed, which means buffer contents are invalid
	" -syntax could have been invalidated by \u making a new parent win special.
	" -buffer contents could have been invalidated by user modification
	"  (shouldn't happen - don't guard against this)
	if s:undo_cache.dirty
		call s:Put_buf_contents(s:undo_cache.geom.lines)
		let s:undo_cache.dirty = 0
		if s:Is_diff_visible()
			call s:Update_diff()
			call s:Goto_treebuf()
		endif
	endif
	" Note: Even if cache wasn't refreshed, syntax could have been invalidated
	" because \u made a different parent window special.
	if s:undo_cache.syn.Is_dirty()
		" Highlight from scratch.
		call clearmatches()
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

" vim:ts=4:sw=4:tw=80:fdm=marker:fmr=fu!,endfu
