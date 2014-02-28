" my filetype file
if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect

" EcuTester script (Hinkle script)
au! BufRead,BufNewFile *.ets		setfiletype ets
" BCS Story Script
au! BufRead,BufNewFile *.bcss		setfiletype bcss
" Brett Stahlman's journal file
" Do some configuration for journal before txtfmt plugins are
" loaded.
au  BufRead,BufNewFile *.jnl		call Jnl_configure_txtfmt()
au  BufRead,BufNewFile *.jnl		setfiletype jnl.txtfmt
" Allow Txtfmt regions within C source files
" Note: If I ever decide to embed Txtfmt regions in my C
" comments, uncomment the block of lines between 'Txtfmt
" BEGIN' and 'Txtfmt END'
" Txtfmt BEGIN
""""au! BufRead,BufNewFile *.c setf c
""""au  BufRead,BufNewFile *.c let b:txtfmtTokrange = '180S' | let b:txtfmtNested = 1
""""" IMPORTANT NOTE: The following 2 lines are a workaround for a
""""" Vim bug; specifically, I should be able simply to set
""""" filetype to c.txtfmt; however, the distributed Vim indent
""""" script doesn't know about the dot-separated name syntax;
""""" hence, if I do it that way, cindent will be turned off...
""""" TODO: Explore possibility of simply setting cindent after
""""" the :setfiletype...
""""" TODO: This is fixed in Vim 7.2 - Perhaps use dot-separated
""""" syntax
""""au BufRead,BufNewFile *.c runtime syntax/txtfmt.vim
""""au BufRead,BufNewFile *.c runtime ftplugin/txtfmt.vim
" Txtfmt END

augroup END
