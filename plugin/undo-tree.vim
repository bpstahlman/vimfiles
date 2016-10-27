
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
" TODO: Consider pulling everything but 'next' under an 'el' property so that
" these elements can be used with the list functions further down. Hmm... Not
" yet clear whether structural members like children and parent would also need
" to be in el.
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
	" TODO: Make min separation (4) configurable? At least, don't hard-code.
	" Note: For now, you need screen positions (to get space separation) and 2
	" parens.
	let d = a:e_pair[0][1] - a:e_pair[1][0] + 4
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
	let w = len(a:t.seq)
	let e = [-w/2, w/2 + w%2]
	let resultextent = S_Cons(e, s:Merge_list(pextents))
	let resulttree = {'node': a:t, 'x': 0, 'ptrees': ptrees}
	return [resulttree, resultextent]
endfu

fu! s:Treegrid_add_node(lvl, x, text)
	let rows_per_lvl = 3
	let sr = lvl * rows_per_lvl
	" Make sure we have sufficient rows.
	if sr >= len(grid)
		call extend(grid, repeat([[]], rows_per_lvl))
	endif
	" UNDER CONSTRUCTION - May abandon...
	
endfu

fu! S_Make_tree_grid(tree)
	let me = {
		\ 'w': 0, 'h': 0
		\ 'add': function('s:Treegrid_add'),
		\ 'grid': []
	\ }

endfu

let xs = S_Cons("baz", {})
let xs = S_Cons("bar", xs)
let xs = S_Cons("foo", xs)
let xs_upper = S_Map(function('toupper'), xs)
let xs_rev = S_Reverse(xs)
let xs_zipped = S_Zip(xs_upper, xs_rev)
fu! Swap_tuple_fn(x)
	return [a:x[1], a:x[0]]
endfu
let xs_zswapped = S_Map(function('Swap_tuple_fn'), xs_zipped)
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


" Note: Input tree is the undo-tree object. From that, build one that looks like
" this:
" {'node': <ut-node>, 'x': <position relative to parent>, 'children': <array of these nodes>}
" TODO: Convert arrays used in these methods from Vim lists to singly-linked
" lists. (Perhaps wait till I've tested basic algorithm.)
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
	let lines = []
	" Tree is centered at 0, but we need its left edge at 0. Determine the bias.
	let x = abs(S_Foldl(function('s:Extent_min_fn'), 0, a:extent))
	" Breadth-first traversal
	let fifo = [[a:tree, x, 0]]
	let lvl_prev = -1
	let rows_per_lvl = 3
	while !empty(fifo)
		let [t, parent_x, x, lvl] = remove(fifo, 0)
		if lvl_prev != lvl
			" Get index of row containing the label.
			let lrow = lvl * rows_per_lvl + 2
			" Add additional lines.
			call extend(lines, repeat([''], rows_per_lvl))
		endif
		" Add this node's children
		let tc = t.children.fst
		while !empty(tc)
			call add(fifo, [tc, x, x + tc.el.x, lvl + 1])
			let tc = tc.next
		endwhile
		" Process current node.
		" Horizontal header lines
		let s = ''
		" Note: Positive poff means child to left of parent.
		let poff = parent_x - x
		" Place the label, leaving space for a [...].
		let text = ' ' . t.seq . ' '
		" TODO: Think through rounding/truncating...
		let text_x = len(text) - text / 2
		call gridlines.add(lrow, text_x, text)
		if poff == 1
			" Special case.
			let [off, text] = [0, '/']
		elseif poff == -1
			let [off, text] = [0, '\']
		elseif poff > 0
			let [off, text] = [1, '/']
		elseif poff < 0
			let [off, text] = [-1, '\']
		endif
		call gridlines.add(lrow - 1, x + off, text)
		" Extend horizontal header line from previous node if applicable.

		" TODO: Harmonize the case of node_prev and no node_prev.
		" For one thing, have a test that causes the line to be padded up to the
		" first child of a level.
		if !empty(node_prev)
			" Are we within 1 unit of the parent's x?
		else
			let [pad1, pad2] = [x, x]
		endif
		let len = len(lines[row])
		let x1 = x - len(t.seq) / 2
		let lines[row] .= repeat(' ', x1 - len)
		let lines[row] .= t.seq

		let [t_prev, x_prev, lvl_prev] = [t, x, lvl]
	endwhile

	return lines
endfu
" Convert tree/extent pair built by Design to a list of lines.
fu! s:Build_tree_display_old(tree, extent)
	let lines = []
	" Tree is centered at 0, but we need its left edge at 0. Determine the bias.
	let x = abs(S_Foldl(function('s:Extent_min_fn'), 0, a:extent))
	" Breadth-first traversal
	let fifo = [[a:tree, x, 0]]
	let [lvl_prev, node_prev] = [-1, {}]
	let rows_per_lvl = 4
	while !empty(fifo)
		let [parent, node, parent_x, x, lvl] = remove(fifo, 0)
		if lvl_prev != lvl
			" Start of group of rows corresponding to this level.
			let row = lvl * rows_per_lvl
		endif
		" Add this node's children
		let tc = node.ptrees
		while !empty(tc)
			call add(fifo, [node, tc.el, x, x + tc.el.x, lvl + 1])
			let tc = tc.next
		endwhile
		" Process current node.
		" TODO: Testing shows I need more spacing.
		let node = node.node
		if len(lines) <= row
			" Add 2 more lines. Note that this is junk code.
			" TODO: Rewrite this function.
			call extend(lines, ['', ''])
		endif
		" Horizontal header lines
		let s = ''
		let poff = parent_x - x
		" TODO: Harmonize the case of node_prev and no node_prev.
		" For one thing, have a test that causes the line to be padded up to the
		" first child of a level.
		if !empty(node_prev)
			" Are we within 1 unit of the parent's x?
			if poff == 1
				let l1 = repeat("-", x - x_prev) . "v"
				let l2 = '/'
			elseif poff == 0
				let l1 = repeat("-", poff - 1) . "v"
				let l2 = '|'
			elseif poff == -1
				let l1 = repeat("-", x - x_prev - 2) . "v-"
				let l2 = '\'
			else
				" Take into account whether we're crossing parent.
				if x > parent_x && x_prev < parent_x
					let l1 = repeat("-", parent_x - x_prev - 1) . "v"
						\ . repeat("-", x - parent_x - 1) . "+"
				else
					let l1 = repeat("-", x - x_prev - 1) . "+"
				endif
			endif
		else
			let [pad1, pad2] = [x, x]
			if poff == 1
				let l1 = "v"
				let l2 = '/'
				let pad2 -= 1
			elseif poff == 0
				let l1 = "v"
				let l2 = '|'
			elseif poff == -1
				let l1 = "v"
				let l2 = '\'
				let pad2 += 1
			else
				let l1 = "+"
				let l2 = '|'
			endif
		endif
		let len = len(lines[row])
		let x1 = x - len(node.seq) / 2
		let lines[row] .= repeat(' ', x1 - len)
		let lines[row] .= node.seq

		let [t_prev, x_prev, lvl_prev] = [node, x, lvl]
	endwhile

	return lines
endfu

fu! s:Refresh_undo_tree()
	let b:undo_tree = Make_undo_tree()
	" TODO: Thinking I may no longer need tree returned, now that positions are
	" stored on b:undo_tree.
	let [tree, extent] = s:Design(b:undo_tree)
	let tree_lines = s:Build_tree_display(b:undo_tree, extent)
	let g:tree_lines = tree_lines
	let g:tree = tree
	let g:extent = extent
	echo "tree:"
	echo tree
	echo "extent:"
	echo extent
endfu

nmap <F7> :call <SID>Refresh_undo_tree()<CR>
nmap <F8> :call <SID>Display_undo_tree(b:undo_tree)<CR>
nmap <C-Up> :call b:undo_tree.up()<CR>
nmap <C-Down> :call b:undo_tree.down()<CR>
nmap <C-Left> :call b:undo_tree.left()<CR>
nmap <C-Right> :call b:undo_tree.right()<CR>

nmap <F9> :echo string(<SID>Show_undo_tree())<CR>
" vim:ts=4:sw=4:tw=80

