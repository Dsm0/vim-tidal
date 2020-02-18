" Insert or delete brackets, parens, quotes in pairs.
" Maintainer:	JiangMiao <jiangfriend@gmail.com>
" Contributor: camthompson
" Last Change:  2019-02-02
" Version: 2.0.0
" Homepage: http://www.vim.org/scripts/script.php?script_id=3599
" Repository: https://github.com/jiangmiao/auto-pairs
" License: MIT

if exists('g:AutoPairsLoaded') || &cp
  finish
end
let g:AutoPairsLoaded = 1

if !exists('g:AutoPairs')
  let g:AutoPairs = {'(':')', '[':']', '{':'}',"'":"'",'"':'"', '```':'```', '"""':'"""', "'''":"'''", "`":"`"}
end

" default pairs base on filetype
func! AutoPairsDefaultPairs()
  if exists('b:autopairs_defaultpairs')
    return b:autopairs_defaultpairs
  end
  let r = copy(g:AutoPairs)
  let allPairs = {
        \ 'vim': {'\v^\s*\zs"': ''},
        \ 'rust': {'\w\zs<': '>', '&\zs''': ''},
        \ 'php': {'<?': '?>//k]', '<?php': '?>//k]'}
        \ }
  for [filetype, pairs] in items(allPairs)
    if &filetype == filetype
      for [open, close] in items(pairs)
        let r[open] = close
      endfor
    end
  endfor
  let b:autopairs_defaultpairs = r
  return r
endf

if !exists('g:AutoPairsMapBS')
  let g:AutoPairsMapBS = 1
end

" Map <C-h> as the same BS
if !exists('g:AutoPairsMapCh')
  let g:AutoPairsMapCh = 1
end

if !exists('g:AutoPairsMapCR')
  let g:AutoPairsMapCR = 1
end

if !exists('g:AutoPairsWildClosedPair')
  let g:AutoPairsWildClosedPair = ''
end

if !exists('g:AutoPairsMapSpace')
  let g:AutoPairsMapSpace = 1
end

if !exists('g:AutoPairsCenterLine')
  let g:AutoPairsCenterLine = 1
end

if !exists('g:AutoPairsShortcutToggle')
  let g:AutoPairsShortcutToggle = '<M-p>'
end

if !exists('g:AutoPairsShortcutFastWrap')
  let g:AutoPairsShortcutFastWrap = '<M-e>'
end

if !exists('g:AutoPairsMoveCharacter')
  let g:AutoPairsMoveCharacter = "()[]{}\"'"
end

if !exists('g:AutoPairsShortcutJump')
  let g:AutoPairsShortcutJump = '<M-n>'
endif

" Fly mode will for closed pair to jump to closed pair instead of insert.
" also support AutoPairsBackInsert to insert pairs where jumped.
if !exists('g:AutoPairsFlyMode')
  let g:AutoPairsFlyMode = 0
endif

" When skipping the closed pair, look at the current and
" next line as well.
if !exists('g:AutoPairsMultilineClose')
  let g:AutoPairsMultilineClose = 1
endif

" Work with Fly Mode, insert pair where jumped
if !exists('g:AutoPairsShortcutBackInsert')
  let g:AutoPairsShortcutBackInsert = '<M-b>'
endif

if !exists('g:AutoPairsSmartQuotes')
  let g:AutoPairsSmartQuotes = 1
endif

" 7.4.849 support <C-G>U to avoid breaking '.'
" Issue talk: https://github.com/jiangmiao/auto-pairs/issues/3
" Vim note: https://github.com/vim/vim/releases/tag/v7.4.849
if v:version > 704 || v:version == 704 && has("patch849")
  let s:Go = "\<C-G>U"
else
  let s:Go = ""
endif

let s:Left = s:Go."\<LEFT>"
let s:Right = s:Go."\<RIGHT>"




" unicode len
func! s:ulen(s)
  return len(split(a:s, '\zs'))
endf

func! s:left(s)
  return repeat(s:Left, s:ulen(a:s))
endf

func! s:right(s)
  return repeat(s:Right, s:ulen(a:s))
endf

func! s:delete(s)
  return repeat("\<DEL>", s:ulen(a:s))
endf

func! s:backspace(s)
  return repeat("\<BS>", s:ulen(a:s))
endf

func! s:getline()
  let line = getline('.')
  let pos = col('.') - 1
  let before = strpart(line, 0, pos)
  let after = strpart(line, pos)
  let afterline = after
  if g:AutoPairsMultilineClose
    let n = line('$')
    let i = line('.')+1
    while i <= n
      let line = getline(i)
      let after = after.' '.line
      if !(line =~ '\v^\s*$')
        break
      end
      let i = i+1
    endwhile
  end
  return [before, after, afterline]
endf

" split text to two part
" returns [orig, text_before_open, open]
func! s:matchend(text, open)
    let m = matchstr(a:text, '\V'.a:open.'\v$')
    if m == ""
      return []
    end
    return [a:text, strpart(a:text, 0, len(a:text)-len(m)), m]
endf

" returns [orig, close, text_after_close]
func! s:matchbegin(text, close)
    let m = matchstr(a:text, '^\V'.a:close)
    if m == ""
      return []
    end
    return [a:text, m, strpart(a:text, len(m), len(a:text)-len(m))]
endf

" add or delete pairs base on g:AutoPairs
" AutoPairsDefine(addPairs:dict[, removeOpenPairList:list])
"
" eg:
"   au FileType html let b:AutoPairs = AutoPairsDefine({'<!--' : '-->'}, ['{'])
"   add <!-- --> pair and remove '{' for html file
func! AutoPairsDefine(pairs, ...)
  let r = AutoPairsDefaultPairs()
  if a:0 > 0
    for open in a:1
      unlet r[open]
    endfor
  end
  for [open, close] in items(a:pairs)
    let r[open] = close
  endfor
  return r
endf

func! AutoPairsInsert(key)
  if !b:autopairs_enabled
    return a:key
  end

  let b:autopairs_saved_pair = [a:key, getpos('.')]

  let [before, after, afterline] = s:getline()

  " Ignore auto close if prev character is \
  if before[-1:-1] == '\'
    return a:key
  end

  " check open pairs
  for [open, close, opt] in b:AutoPairsList
    let ms = s:matchend(before.a:key, open)
    let m = matchstr(afterline, '^\v\s*\zs\V'.close)
    if len(ms) > 0
      " process the open pair
      
      " remove inserted pair
      " eg: if the pairs include < > and  <!-- --> 
      " when <!-- is detected the inserted pair < > should be clean up 
      let target = ms[1]
      let openPair = ms[2]
      if len(openPair) == 1 && m == openPair
        break
      end
      let bs = ''
      let del = ''
      while len(before) > len(target)
        let found = 0
        " delete pair
        for [o, c, opt] in b:AutoPairsList
          let os = s:matchend(before, o)
          if len(os) && len(os[1]) < len(target)
            " any text before openPair should not be deleted
            continue
          end
          let cs = s:matchbegin(afterline, c)
          if len(os) && len(cs)
            let found = 1
            let before = os[1]
            let afterline = cs[2]
            let bs = bs.s:backspace(os[2])
            let del = del.s:delete(cs[1])
            break
          end
        endfor
        if !found
          " delete charactor
          let ms = s:matchend(before, '\v.')
          if len(ms)
            let before = ms[1]
            let bs = bs.s:backspace(ms[2])
          end
        end
      endwhile
      return bs.del.openPair.close.s:left(close)
    end
  endfor

  " check close pairs
  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end
    if a:key == g:AutoPairsWildClosedPair || opt['mapclose'] && opt['key'] == a:key
      " the close pair is in the same line
      let m = matchstr(afterline, '^\v\s*\V'.close)
      if m != ''
        if before =~ '\V'.open.'\v\s*$' && m[0] =~ '\v\s'
          " remove the space we inserted if the text in pairs is blank
          return "\<DEL>".s:right(m[1:])
        else
          return s:right(m)
        end
      end
      let m = matchstr(after, '^\v\s*\zs\V'.close)
      if m != ''
        if a:key == g:AutoPairsWildClosedPair || opt['multiline']
          if b:autopairs_return_pos == line('.') && getline('.') =~ '\v^\s*$'
            normal! ddk$
          end
          call search(m, 'We')
          return "\<Right>"
        else
          break
        end
      end
    end
  endfor


  " Fly Mode, and the key is closed-pairs, search closed-pair and jump
  if g:AutoPairsFlyMode &&  a:key =~ '\v[\}\]\)]'
    if search(a:key, 'We')
      return "\<Right>"
    endif
  endif

  return a:key
endf

func! AutoPairsDelete()
  if !b:autopairs_enabled
    return "\<BS>"
  end

  let [before, after, ig] = s:getline()
  for [open, close, opt] in b:AutoPairsList
    let b = matchstr(before, '\V'.open.'\v\s?$')
    let a = matchstr(after, '^\v\s*\V'.close)
    if b != '' && a != ''
      if b[-1:-1] == ' '
        if a[0] == ' '
          return "\<BS>\<DELETE>"
        else
          return "\<BS>"
        end
      end
      return s:backspace(b).s:delete(a)
    end
  endfor

  return "\<BS>"
  " delete the pair foo[]| <BS> to foo
  for [open, close, opt] in b:AutoPairsList
    let m = s:matchend(before, '\V'.open.'\v\s*'.'\V'.close.'\v$')
    if len(m) > 0
      return s:backspace(m[2])
    end
  endfor
  return "\<BS>"
endf


" Fast wrap the word in brackets
func! AutoPairsFastWrap()
  let c = @"
  normal! x
  let [before, after, ig] = s:getline()
  if after[0] =~ '\v[\{\[\(\<]'
    normal! %
    normal! p
  else
    for [open, close, opt] in b:AutoPairsList
      if close == ''
        continue
      end
      if after =~ '^\s*\V'.open
        call search(close, 'We')
        normal! p
        let @" = c
        return ""
      end
    endfor
    if after[1:1] =~ '\v\w'
      normal! e
      normal! p
    else
      normal! p
    end
  end
  let @" = c
  return ""
endf

func! AutoPairsJump()
  call search('["\]'')}]','W')
endf

func! AutoPairsMoveCharacter(key)
  let c = getline(".")[col(".")-1]
  let escaped_key = substitute(a:key, "'", "''", 'g')
  return "\<DEL>\<ESC>:call search("."'".escaped_key."'".")\<CR>a".c."\<LEFT>"
endf

func! AutoPairsBackInsert()
  let pair = b:autopairs_saved_pair[0]
  let pos  = b:autopairs_saved_pair[1]
  call setpos('.', pos)
  return pair
endf

func! AutoPairsReturn()
  if b:autopairs_enabled == 0
    return ''
  end
  let b:autopairs_return_pos = 0
  let before = getline(line('.')-1)
  let [ig, ig, afterline] = s:getline()
  let cmd = ''
  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end

    if before =~ '\V'.open.'\v\s*$' && afterline =~ '^\s*\V'.close
      let b:autopairs_return_pos = line('.')
      if g:AutoPairsCenterLine && winline() * 3 >= winheight(0) * 2
        " Recenter before adding new line to avoid replacing line content
        let cmd = "zz"
      end

      " If equalprg has been set, then avoid call =
      " https://github.com/jiangmiao/auto-pairs/issues/24
      if &equalprg != ''
        return "\<ESC>".cmd."O"
      endif

      " conflict with javascript and coffee
      " javascript   need   indent new line
      " coffeescript forbid indent new line
      if &filetype == 'coffeescript' || &filetype == 'coffee'
        return "\<ESC>".cmd."k==o"
      else
        return "\<ESC>".cmd."=ko"
      endif
    end
  endfor
  return ''
endf

func! AutoPairsSpace()
  if !b:autopairs_enabled
    return "\<SPACE>"
  end

  let [before, after, ig] = s:getline()

  for [open, close, opt] in b:AutoPairsList
    if close == ''
      continue
    end
    if before =~ '\V'.open.'\v$' && after =~ '^\V'.close
      if close =~ '\v^[''"`]$'
        return "\<SPACE>"
      else
        return "\<SPACE>\<SPACE>".s:Left
      end
    end
  endfor
  return "\<SPACE>"
endf

func! AutoPairsMap(key)
  " | is special key which separate map command from text
  let key = a:key
  if key == '|'
    let key = '<BAR>'
  end
  let escaped_key = substitute(key, "'", "''", 'g')
  " use expr will cause search() doesn't work
  execute 'inoremap <buffer> <silent> '.key." <C-R>=AutoPairsInsert('".escaped_key."')<CR>"
endf

func! AutoPairsToggle()
  if b:autopairs_enabled
    let b:autopairs_enabled = 0
    echo 'AutoPairs Disabled.'
  else
    let b:autopairs_enabled = 1
    echo 'AutoPairs Enabled.'
  end
  return ''
endf

func! s:sortByLength(i1, i2)
  return len(a:i2[0])-len(a:i1[0])
endf

func! AutoPairsInit()
  let b:autopairs_loaded  = 1
  if !exists('b:autopairs_enabled')
    let b:autopairs_enabled = 1
  end

  if !exists('b:AutoPairs')
    let b:AutoPairs = AutoPairsDefaultPairs()
  end

  if !exists('b:AutoPairsMoveCharacter')
    let b:AutoPairsMoveCharacter = g:AutoPairsMoveCharacter
  end

  let b:autopairs_return_pos = 0
  let b:autopairs_saved_pair = [0, 0]
  let b:AutoPairsList = []

  " buffer level map pairs keys
  " n - do not map the first charactor of closed pair to close key
  " m - close key jumps through multi line
  " s - close key jumps only in the same line
  for [open, close] in items(b:AutoPairs)
    let o = open[-1:-1]
    let c = close[0]
    let opt = {'mapclose': 1, 'multiline':1}
    let opt['key'] = c
    if o == c
      let opt['multiline'] = 0
    end
    let m = matchlist(close, '\v(.*)//(.*)$')
    if len(m) > 0 
      if m[2] =~ 'n'
        let opt['mapclose'] = 0
      end
      if m[2] =~ 'm'
        let opt['multiline'] = 1
      end
      if m[2] =~ 's'
        let opt['multiline'] = 0
      end
      let ks = matchlist(m[2], '\vk(.)')
      if len(ks) > 0
        let opt['key'] = ks[1]
        let c = opt['key']
      end
      let close = m[1]
    end
    call AutoPairsMap(o)
    if o != c && c != '' && opt['mapclose']
      call AutoPairsMap(c)
    end
    let b:AutoPairsList += [[open, close, opt]]
  endfor

  " sort pairs by length, longer pair should have higher priority
  let b:AutoPairsList = sort(b:AutoPairsList, "s:sortByLength")

  for item in b:AutoPairsList
    let [open, close, opt] = item
    if open == "'" && open == close
      let item[0] = '\v(^|\W)\zs'''
    end
  endfor


  for key in split(b:AutoPairsMoveCharacter, '\s*')
    let escaped_key = substitute(key, "'", "''", 'g')
    execute 'inoremap <silent> <buffer> <M-'.key."> <C-R>=AutoPairsMoveCharacter('".escaped_key."')<CR>"
  endfor

  " Still use <buffer> level mapping for <BS> <SPACE>
  if g:AutoPairsMapBS
    " Use <C-R> instead of <expr> for issue #14 sometimes press BS output strange words
    execute 'inoremap <buffer> <silent> <BS> <C-R>=AutoPairsDelete()<CR>'
  end

  if g:AutoPairsMapCh
    execute 'inoremap <buffer> <silent> <C-h> <C-R>=AutoPairsDelete()<CR>'
  endif

  if g:AutoPairsMapSpace
    " Try to respect abbreviations on a <SPACE>
    let do_abbrev = ""
    if v:version == 703 && has("patch489") || v:version > 703
      let do_abbrev = "<C-]>"
    endif
    execute 'inoremap <buffer> <silent> <SPACE> '.do_abbrev.'<C-R>=AutoPairsSpace()<CR>'
  end

  if g:AutoPairsShortcutFastWrap != ''
    execute 'inoremap <buffer> <silent> '.g:AutoPairsShortcutFastWrap.' <C-R>=AutoPairsFastWrap()<CR>'
  end

  if g:AutoPairsShortcutBackInsert != ''
    execute 'inoremap <buffer> <silent> '.g:AutoPairsShortcutBackInsert.' <C-R>=AutoPairsBackInsert()<CR>'
  end

  if g:AutoPairsShortcutToggle != ''
    " use <expr> to ensure showing the status when toggle
    execute 'inoremap <buffer> <silent> <expr> '.g:AutoPairsShortcutToggle.' AutoPairsToggle()'
    execute 'noremap <buffer> <silent> '.g:AutoPairsShortcutToggle.' :call AutoPairsToggle()<CR>'
  end

  if g:AutoPairsShortcutJump != ''
    execute 'inoremap <buffer> <silent> ' . g:AutoPairsShortcutJump. ' <ESC>:call AutoPairsJump()<CR>a'
    execute 'noremap <buffer> <silent> ' . g:AutoPairsShortcutJump. ' :call AutoPairsJump()<CR>'
  end

  if &keymap != ''
    let l:imsearch = &imsearch
    let l:iminsert = &iminsert
    let l:imdisable = &imdisable
    execute 'setlocal keymap=' . &keymap
    execute 'setlocal imsearch=' . l:imsearch
    execute 'setlocal iminsert=' . l:iminsert
    if l:imdisable
      execute 'setlocal imdisable'
    else
      execute 'setlocal noimdisable'
    end
  end

endf

func! s:ExpandMap(map)
  let map = a:map
  let map = substitute(map, '\(<Plug>\w\+\)', '\=maparg(submatch(1), "i")', 'g')
  let map = substitute(map, '\(<Plug>([^)]*)\)', '\=maparg(submatch(1), "i")', 'g')
  return map
endf

func! AutoPairsTryInit()
  if exists('b:autopairs_loaded')
    return
  end

  " for auto-pairs starts with 'a', so the priority is higher than supertab and vim-endwise
  "
  " vim-endwise doesn't support <Plug>AutoPairsReturn
  " when use <Plug>AutoPairsReturn will cause <Plug> isn't expanded
  "
  " supertab doesn't support <SID>AutoPairsReturn
  " when use <SID>AutoPairsReturn  will cause Duplicated <CR>
  "
  " and when load after vim-endwise will cause unexpected endwise inserted.
  " so always load AutoPairs at last

  " Buffer level keys mapping
  " comptible with other plugin
  if g:AutoPairsMapCR
    if v:version == 703 && has('patch32') || v:version > 703
      " VIM 7.3 supports advancer maparg which could get <expr> info
      " then auto-pairs could remap <CR> in any case.
      let info = maparg('<CR>', 'i', 0, 1)
      if empty(info)
        let old_cr = '<CR>'
        let is_expr = 0
      else
        let old_cr = info['rhs']
        let old_cr = s:ExpandMap(old_cr)
        let old_cr = substitute(old_cr, '<SID>', '<SNR>' . info['sid'] . '_', 'g')
        let is_expr = info['expr']
        let wrapper_name = '<SID>AutoPairsOldCRWrapper73'
      endif
    else
      " VIM version less than 7.3
      " the mapping's <expr> info is lost, so guess it is expr or not, it's
      " not accurate.
      let old_cr = maparg('<CR>', 'i')
      if old_cr == ''
        let old_cr = '<CR>'
        let is_expr = 0
      else
        let old_cr = s:ExpandMap(old_cr)
        " old_cr contain (, I guess the old cr is in expr mode
        let is_expr = old_cr =~ '\V(' && toupper(old_cr) !~ '\V<C-R>'

        " The old_cr start with " it must be in expr mode
        let is_expr = is_expr || old_cr =~ '\v^"'
        let wrapper_name = '<SID>AutoPairsOldCRWrapper'
      end
    end

    if old_cr !~ 'AutoPairsReturn'
      if is_expr
        " remap <expr> to `name` to avoid mix expr and non-expr mode
        execute 'inoremap <buffer> <expr> <script> '. wrapper_name . ' ' . old_cr
        let old_cr = wrapper_name
      end
      " Always silent mapping
      execute 'inoremap <script> <buffer> <silent> <CR> '.old_cr.'<SID>AutoPairsReturn'
    end
  endif
  call AutoPairsInit()
endf

" Always silent the command
inoremap <silent> <SID>AutoPairsReturn <C-R>=AutoPairsReturn()<CR>
imap <script> <Plug>AutoPairsReturn <SID>AutoPairsReturn


au BufEnter * :call AutoPairsTryInit()
if exists("g:loaded_tidal") || &cp || v:version < 700
  finish
endif
let g:loaded_tidal = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tmux
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:TmuxSend(config, text)
  let l:prefix = "tmux -L " . shellescape(a:config["socket_name"])
  " use STDIN unless configured to use a file
  if !exists("g:tidal_paste_file")
    call system(l:prefix . " load-buffer -", a:text)
  else
    call s:WritePasteFile(a:text)
    call system(l:prefix . " load-buffer " . g:tidal_paste_file)
  end
  call system(l:prefix . " paste-buffer -d -t " . shellescape(a:config["target_pane"]))
endfunction


function! s:TmuxPaneNames(A,L,P)
  let format = '#{pane_id} #{session_name}:#{window_index}.#{pane_index} #{window_name}#{?window_active, (active),}'
  return system("tmux -L " . shellescape(b:tidal_config['socket_name']) . " list-panes -a -F " . shellescape(format))
endfunction

function! s:TmuxConfig() abort
  if !exists("b:tidal_config")
    let b:tidal_config = {"socket_name": "default", "target_pane": ":"}
  end

  let b:tidal_config["socket_name"] = input("tmux socket name: ", b:tidal_config["socket_name"])
  let b:tidal_config["sclang_pane"] = input("tmux target pane: ", b:tidal_config["sclang_pane"], "custom,<SNR>" . s:SID() . "_TmuxPaneNames")
  if b:tidal_config["sclang_pane"] =~ '\s\+'
    let b:tidal_config["sclang_pane"] = split(b:tidal_config["sclang_pane"])[0]
  endif

  let b:tidal_config["target_pane"] = input("tmux target pane: ", b:tidal_config["target_pane"], "custom,<SNR>" . s:SID() . "_TmuxPaneNames")
  if b:tidal_config["target_pane"] =~ '\s\+'
    let b:tidal_config["target_pane"] = split(b:tidal_config["target_pane"])[0]
  endif
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Terminal
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let s:tidal_term = -1
let s:sclang_term = -1

function! s:TerminalOpen()
  if !has('nvim')
    echom "'terminal' target currently supported on NeoVim only. Use 'tmux'"
    return
  endif

  if s:tidal_term != -1
    return
  endif

  split term://tidal

  let s:tidal_term = b:terminal_job_id

  " Give tidal a moment to start up so the command doesn't show up at the top
  " unaesthetically.
  " But this isn't very robust.
  sleep 500m

  " Make terminal scroll to follow output
  :exe "normal G"

  " Make small & on the bottom.
  :exe "normal \<c-w>J"
  :exe "normal \<c-w>\<c-w>"
  :exe "normal \<c-w>_"
  :exe "normal \<c-w>10-"
endfunction

function! s:TerminalSend(config, text)
  call s:TerminalOpen()
  call jobsend(s:tidal_term, a:text)
endfunction

" These two are unnecessary AFAIK.
function! s:TerminalPaneNames(A,L,P)
endfunction
function! s:TerminalConfig() abort
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Helpers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun

function! s:WritePasteFile(text)
  " could check exists("*writefile")
  call system("cat > " . g:tidal_paste_file, a:text)
endfunction

function! s:_EscapeText(text)
  if exists("&filetype")
    let custom_escape = "_EscapeText_" . substitute(&filetype, "[.]", "_", "g")
    if exists("*" . custom_escape)
      let result = call(custom_escape, [a:text])
    end
  end

  " use a:text if the ftplugin didn't kick in
  if !exists("result")
    let result = a:text
  end

  " return an array, regardless
  if type(result) == type("")
    return [result]
  else
    return result
  end
endfunction

function! s:TidalGetConfig()
  if !exists("b:tidal_config")
    if exists("g:tidal_default_config")
      let b:tidal_config = g:tidal_default_config
    else
      call s:TidalDispatch('Config')
    end
  end
endfunction

function! s:TidalFlashVisualSelection()
  " Redraw to show current visual selection, and sleep
  redraw
  execute "sleep " . g:tidal_flash_duration . " m"
  " Then leave visual mode
  silent exe "normal! vv"
endfunction

function! s:TidalSendOp(type, ...) abort
  call s:TidalGetConfig()

  let sel_save = &selection
  let &selection = "inclusive"
  let rv = getreg('"')
  let rt = getregtype('"')

  if a:0  " Invoked from Visual mode, use '< and '> marks.
    silent exe "normal! `<" . a:type . '`>y'
  elseif a:type == 'line'
    silent exe "normal! '[V']y"
  elseif a:type == 'block'
    silent exe "normal! `[\<C-V>`]\y"
  else
    silent exe "normal! `[v`]y"
  endif

  call setreg('"', @", 'V')
  call s:TidalSend(@")

  " Flash selection
  if a:type == 'line'
    silent exe "normal! '[V']"
    call s:TidalFlashVisualSelection()
  endif

  let &selection = sel_save
  call setreg('"', rv, rt)

  call s:TidalRestoreCurPos()
endfunction

" This is really quick and dirty.
" Ideally, I'd have a more general function
" I'll come back when I know more vimscript
"""""""""""""""""""""""""""""""""
function! s:SclangSendOp(type, ...) abort
  call s:TidalGetConfig()

  let sel_save = &selection
  let &selection = "inclusive"
  let rv = getreg('"')
  let rt = getregtype('"')

  if a:0  " Invoked from Visual mode, use '< and '> marks.
    silent exe "normal! `<" . a:type . '`>y'
  elseif a:type == 'line'
    silent exe "normal! '[V']y"
  elseif a:type == 'block'
    silent exe "normal! `[\<C-V>`]\y"
  else
    silent exe "normal! `[v`]y"
  endif

  call setreg('"', @", 'V')
  call s:SclangSend(@")

  " Flash selection
  if a:type == 'line'
    silent exe "normal! '[V']"
    call s:TidalFlashVisualSelection()
  endif

  let &selection = sel_save
  call setreg('"', rv, rt)

  call s:TidalRestoreCurPos()
endfunction

function! s:SclangSendLines(count) abort
  call s:TidalGetConfig()

  let rv = getreg('"')
  let rt = getregtype('"')

  silent execute "normal! " . a:count . "yy"

  call s:SclangSend(@")
  call setreg('"', rv, rt)

  " Flash lines
  silent execute "normal! V"
  if a:count > 1
    silent execute "normal! " . (a:count - 1) . "\<Down>"
  endif
  call s:TidalFlashVisualSelection()
endfunction



function! s:TidalSendRange() range abort
  call s:TidalGetConfig()

  let rv = getreg('"')
  let rt = getregtype('"')
  silent execute a:firstline . ',' . a:lastline . 'yank'
  call s:TidalSend(@")
  call setreg('"', rv, rt)
endfunction

function! s:TidalSendLines(count) abort
  call s:TidalGetConfig()

  let rv = getreg('"')
  let rt = getregtype('"')

  silent execute "normal! " . a:count . "yy"

  call s:TidalSend(@")
  call setreg('"', rv, rt)

  " Flash lines
  silent execute "normal! V"
  if a:count > 1
    silent execute "normal! " . (a:count - 1) . "\<Down>"
  endif
  call s:TidalFlashVisualSelection()
endfunction

function! s:TidalStoreCurPos()
  if g:tidal_preserve_curpos == 1
    if exists("*getcurpos")
      let s:cur = getcurpos()
    else
      let s:cur = getpos('.')
    endif
  endif
endfunction

function! s:TidalRestoreCurPos()
  if g:tidal_preserve_curpos == 1
    call setpos('.', s:cur)
  endif
endfunction

let s:parent_path = fnamemodify(expand("<sfile>"), ":p:h:s?/plugin??")

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"REALLY IMPORTANT FUNCTIONS \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
"
function! s:TidalSend(text)
  call s:TidalGetConfig()

  let pieces = s:_EscapeText(a:text)
  for piece in pieces
    call s:TidalDispatch('Send', b:tidal_config, piece)
  endfor
endfunction


function SclangSend(text)
  call s:TidalGetConfig()

  let pieces = s:_EscapeText(a:text)
  for piece in pieces
    call s:SclangDispatch('Send', b:tidal_config, piece)
  endfor
endfunction

"
function! s:TidalConfig() abort
  call inputsave()
  call s:TidalDispatch('Config')
  call inputrestore()
endfunction

" delegation
function! s:TidalDispatch(name, ...)
  let target = substitute(tolower(g:tidal_target), '\(.\)', '\u\1', '') " Capitalize
  return call("s:" . target . a:name, a:000)
endfunction

function! s:SclangDispatch(name, ...)
  let target = substitute(tolower(g:sclang_target), '\(.\)', '\u\1', '') " Capitalize
  return call("s:" . target . a:name, a:000)
endfunction


"REALLY IMPORTANT FUNCTIONS ^^^^^^^^^^^6
"
"
"
"MY revision::::
function! s:PaneSend(text)
  call s:TidalGetConfig()

  let pieces = s:_EscapeText(a:text)
  for piece in pieces
    call s:PaneDispatch('Send', b:tidal_config, piece)
  endfor
endfunction


function PaneSend(text)
  call s:TidalGetConfig()

  let pieces = s:_EscapeText(a:text)
  for piece in pieces
    call s:PaneDispatch('Send', b:tidal_config, piece)
  endfor
endfunction

"
function! s:Config() abort
  call inputsave()
  call s:PaneDispatch('Config')
  call inputrestore()
endfunction

" delegation
function! s:PaneDispatch(name, ...)
  let target = substitute(tolower(g:tidal_target), '\(.\)', '\u\1', '') " Capitalize
  return call("s:" . target . a:name, a:000)
endfunction

""""

function! s:TidalHush()
  execute 'TidalSend1 hush'
endfunction

function! s:TidalSilence(stream)
  silent execute 'TidalSend1 d' . a:stream . ' silence'
endfunction

function! s:TidalPlay(stream)
  let res = search('^\s*d' . a:stream)
  if res > 0
    silent execute "normal! vip:TidalSend\<cr>"
    silent execute "normal! vip"
    call s:TidalFlashVisualSelection()
  else
    echo "d" . a:stream . " was not found"
  endif
endfunction

function! s:TidalGenerateCompletions(path)
  let l:exe = s:parent_path . "/bin/generate-completions"
  let l:output_path = s:parent_path . "/.dirt-samples"

  if !empty(a:path)
    let l:sample_path = a:path
  else
    if has('macunix')
      let l:sample_path = "~/Library/Application Support/SuperCollider/downloaded-quarks/Dirt-Samples"
    elseif has('unix')
      let l:sample_path = "~/.local/share/SuperCollider/downloaded-quarks/Dirt-Samples"
    endif
  endif
  " generate completion file
  silent execute '!' . l:exe shellescape(expand(l:sample_path)) shellescape(expand(l:output_path))
  echo "Generated dictionary of dirt-samples"
  " setup completion
  let &l:dictionary .= ',' . l:output_path
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setup key bindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

command -bar -nargs=0 TidalConfig call s:TidalConfig()
command -range -bar -nargs=0 TidalSend <line1>,<line2>call s:TidalSendRange()

command -range -bar -nargs=0 SclangSend <line1>,<line2>call s:SclangSendRange()

command -nargs=+ TidalSend1 call s:TidalSend(<q-args> . "\r")

command! -nargs=0 TidalHush call s:TidalHush()
command! -nargs=1 TidalSilence call s:TidalSilence(<args>)
command! -nargs=1 TidalPlay call s:TidalPlay(<args>)
command! -nargs=? TidalGenerateCompletions call s:TidalGenerateCompletions(<q-args>)

noremap <SID>Operator :<c-u>call <SID>TidalStoreCurPos()<cr>:set opfunc=<SID>TidalSendOp<cr>g@

noremap <unique> <script> <silent> <Plug>TidalRegionSend :<c-u>call <SID>TidalSendOp(visualmode(), 1)<cr>
noremap <unique> <script> <silent> <Plug>TidalLineSend :<c-u>call <SID>TidalSendLines(v:count1)<cr>

noremap <unique> <script> <silent> <Plug>SclangRegionSend :<c-u>call <SID>SclangSendOp(visualmode(), 1)<cr>
noremap <unique> <script> <silent> <Plug>SclangLineSend :<c-u>call <SID>SclangSendLines(v:count1)<cr>

noremap <unique> <script> <silent> <Plug>TidalHush:<c-h>call <SID>TidalHush<cr>
noremap <unique> <script> <silent> <Plug>TidalMotionSend <SID>Operator
noremap <unique> <script> <silent> <Plug>TidalParagraphSend <SID>Operatorip
noremap <unique> <script> <silent> <Plug>TidalConfig :<c-u>TidalConfig<cr>

""
" Default options
""

if !exists("g:tidal_target")
  let g:tidal_target = "tmux"
endif

if !exists("g:tidal_paste_file")
  let g:tidal_paste_file = tempname()
endif

if !exists("g:tidal_default_config")
  let g:tidal_default_config = { "socket_name": "default", "target_pane": "tidal:0.2" , "sclang_pane": "tidal:0.0"}
endif

if !exists("g:tidal_preserve_curpos")
  let g:tidal_preserve_curpos = 1
end

if !exists("g:tidal_flash_duration")
  let g:tidal_flash_duration = 2
end

if filereadable(s:parent_path . "/.dirt-samples")
  let &l:dictionary .= ',' . s:parent_path . "/.dirt-samples"
endif
"file copied from 
"https://github.com/supercollider/scvim/blob/master/syntax/supercollider_operators.vim

" Copyright 2007 Alex Norman
" This file is part of SCVIM.
"
" SCVIM is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" SCVIM is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with SCVIM.  If not, see <http://www.gnu.org/licenses/>.
"
" Vim syntax file
" Language:	supercollider	
" Maintainer: Stephen Lumenta <stephen.lumenta@gmail.com>
" Version:	0.2
" Last change:	2012-03-31

syn clear

syn match	scAoperator	"{"
syn match	scAoperator	"}"

"syn	match	scVariable	"\%(var.*\)\@<=\(\l\w*\)" "lowercase followed by wordchar
syn	match	scGlobvariable	"\~\l\w*" "~ followed by lowercase followed by wordchar
syn	match scVar "\s*var\s"
syn	match scVar "\s*classvar\s"
syn	match scArg "\s*arg\s"

" symbols, strings, numbers
syn match	scSymbol "\v(\w|\\)@<!\'.{-}(\\)@<!\'" "\{-} is a non greedy version of *
syn match	scSymbol "\v\$@<!\\\w\w*"
syn match	scSymbol "\\\\"
syn match	scSymbol "\w\+:"

syn region  scString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn match	scChar	"\$\\\?."

syn match scInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*\>"								display
syn match scInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)\>"						display
syn match scInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*\>"								display
syn match scInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*\>"								display
syn match scFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*\>"					display
syn match scFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)\>"	display
syn match scInfinity "inf"	

" keywords
syn match   scControl	"\<\%(break\|rescue\|return\)\>[?!]\@!"
syn match   scKeyword	"\<\%(super\|this\|new\|yield\)\>[?!]\@!"
syn match   scBoolean	"\<\%(true\|false\)\>[?!]\@!"
syn match   scControl "\<\%(case\|begin\|do\|forBy\|loop\|if\|while\|else\)\>[?!]\@!"

" scsynth
syn match scArate "\v\.@<=ar(\w)@!"
syn match scKrate "\v\.@<=kr(\w)@!"

" operators
syn keyword  scUnaryoperator  neg reciprocal abs floor ceil frac sign squared cubed sqrt exp midicps cpsmidi midiratio ratiomidi dbamp ampdb octcps cpsoct log log2 log10 sin cos tan asin acos atan sinh cosh tanh distort softclip isPositive isNegative isStrictlyPositive
syn keyword  scBinaryoperator  min max round trunc atan2 hypot hypotApx ring1 ring2 ring3 ring4 sumsqr difsqr sqrsum sqrdif absdif thresh amclip scaleneg clip2 wrap2 fold2 excess + - *

syn match scBinaryoperator "+"
syn match scBinaryoperator "-"
syn match scBinaryoperator "*"
syn match scBinaryoperator "/"
syn match scBinaryoperator "%"
syn match scBinaryoperator "\*\*"
syn match scBinaryoperator "<"
syn match scBinaryoperator "<="
syn match scBinaryoperator ">"
syn match scBinaryoperator "<>"
syn match scBinaryoperator ">="
syn match scBinaryoperator "="
syn match scBinaryoperator "=="
syn match scBinaryoperator "==="
syn match scBinaryoperator "!="
syn match scBinaryoperator "!=="
syn match scBinaryoperator "&"
syn match scBinaryoperator "|"
syn match scBinaryoperator "<!"
syn match scBinaryoperator "?"
syn match scBinaryoperator "??"
syn match scBinaryoperator "!?"
syn match scBinaryoperator "!"
syn match scBinaryoperator "#"
syn match scBinaryoperator "_"
syn match scBinaryoperator "\.\."
syn match scBinaryoperator "\.\.\."
syn match scBinaryoperator "`"
syn match scBinaryoperator ":"

" comments
syn keyword scCommentTodo   TODO FIXME XXX TBD contained
syn match   scLineComment   "\/\/.*" contains=@Spell,scCommentTodo
syn region  scComment	      start="/\*"  end="\*/" contains=@Spell,scCommentTodo


" object syntax file is regenerated on startup
runtime! syntax/supercollider_objects.vim

"""""""""""""""""""""""""""""""""""""""""
" linkage

hi def link scObject Identifier
hi def link scBinaryoperator Special
hi def link scUnaryoperator Special
hi def link scAoperator Statement
hi def link scArate Statement
hi def link scKrate Statement
hi def link scSymbol Constant
hi def link scString String
hi def link scChar String
hi def link scInteger Number
hi def link scInfinity Number
hi def link scFloat Float
hi def link scGlobVariable Define
hi def link scComment		Comment
hi def link scLineComment		Comment
hi def link scCommentTodo		Todo
hi def link scVar Type
hi def link scArg Type
hi def link scControl Statement
hi def link scKeyword Keyword
hi def link scBoolean Boolean

let b:current_syntax = "supercollider"
"file copied from 
"https://github.com/supercollider/scvim/blob/master/syntax/supercollider.vim

"Copyright 2007 Alex Norman
"
"This file is part of SCVIM.
"
"SCVIM is free software: you can redistribute it and/or modify
"it under the terms of the GNU General Public License as published by
"the Free Software Foundation, either version 3 of the License, or
"(at your option) any later version.
"
"SCVIM is distributed in the hope that it will be useful,
"but WITHOUT ANY WARRANTY; without even the implied warranty of
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"GNU General Public License for more details.
"
"You should have received a copy of the GNU General Public License
"along with SCVIM.  If not, see <http://www.gnu.org/licenses/>.

syn keyword  scUnaryoperator  neg reciprocal abs floor ceil frac sign squared cubed sqrt exp midicps cpsmidi midiratio ratiomidi dbamp ampdb octcps cpsoct log log2 log10 sin cos tan asin acos atan sinh cosh tanh distort softclip isPositive isNegative isStrictlyPositive

syn keyword  scBinaryoperator  min max round trunc atan2 hypot hypotApx ring1 ring2 ring3 ring4 sumsqr difsqr sqrsum sqrdif absdif thresh amclip scaleneg clip2 wrap2 fold2 excess + - *

syn match scBinaryoperator "+"
syn match scBinaryoperator "-"
syn match scBinaryoperator "*"
syn match scBinaryoperator "/"
syn match scBinaryoperator "%"
syn match scBinaryoperator "\*\*"
syn match scBinaryoperator "<"
syn match scBinaryoperator "<="
syn match scBinaryoperator ">"
syn match scBinaryoperator "<>"
syn match scBinaryoperator ">="
syn match scBinaryoperator "="
syn match scBinaryoperator "=="
syn match scBinaryoperator "==="
syn match scBinaryoperator "!="
syn match scBinaryoperator "!=="
syn match scBinaryoperator "&"
syn match scBinaryoperator "|"
syn match scBinaryoperator "<!"
syn match scBinaryoperator "?"
syn match scBinaryoperator "??"
syn match scBinaryoperator "!?"
syn match scBinaryoperator "!"
syn match scBinaryoperator "#"
syn match scBinaryoperator "_"
syn match scBinaryoperator "\.\."
syn match scBinaryoperator "\.\.\."
syn match scBinaryoperator "`"
syn match scBinaryoperator ":"
" syntax highlighting for tidal/haskell
"
" Very minor modifications from syntax file of
" https://github.com/neovimhaskell/haskell-vim
"
" Heavily modified version of the haskell syntax
" highlighter to support haskell.
"
" author: raichoo (raichoo@googlemail.com)

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif
if get(g:, 'haskell_backpack', 0)
  syn keyword haskellBackpackStructure unit signature
  syn keyword haskellBackpackDependency dependency
endif

syn spell notoplevel
syn match haskellRecordField contained containedin=haskellBlock
  \ "[_a-z][a-zA-Z0-9_']*\(,\s*[_a-z][a-zA-Z0-9_']*\)*\_s\+::\_s"
  \ contains=
  \ haskellIdentifier,
  \ haskellOperators,
  \ haskellSeparator,
  \ haskellParens
syn match haskellTypeSig
  \ "^\s*\(where\s\+\|let\s\+\|default\s\+\)\?[_a-z][a-zA-Z0-9_']*#\?\(,\s*[_a-z][a-zA-Z0-9_']*#\?\)*\_s\+::\_s"
  \ contains=
  \ haskellWhere,
  \ haskellLet,
  \ haskellDefault,
  \ haskellIdentifier,
  \ haskellOperators,
  \ haskellSeparator,
  \ haskellParens
syn keyword haskellWhere where
syn keyword haskellLet let
syn match HaskellDerive "\<deriving\>\(\s\+\<\(anyclass\|instance\|newtype\|stock\)\>\)\?"
syn keyword haskellDeclKeyword module class instance newtype in
syn match haskellDecl "\<\(type\|data\)\>\s\+\(\<family\>\)\?"
syn keyword haskellDefault default
syn keyword haskellImportKeywords import qualified safe as hiding contained
syn keyword haskellForeignKeywords foreign export import ccall safe unsafe interruptible capi prim contained
syn region haskellForeignImport start="\<foreign\>" end="\_s\+::\s" keepend
  \ contains=
  \ haskellString,
  \ haskellOperators,
  \ haskellForeignKeywords,
  \ haskellIdentifier
syn match haskellImport "^\s*\<import\>\s\+\(\<safe\>\s\+\)\?\(\<qualified\>\s\+\)\?.\+\(\s\+\<as\>\s\+.\+\)\?\(\s\+\<hiding\>\)\?"
  \ contains=
  \ haskellParens,
  \ haskellOperators,
  \ haskellImportKeywords,
  \ haskellType,
  \ haskellLineComment,
  \ haskellBlockComment,
  \ haskellString,
  \ haskellPragma
syn keyword haskellKeyword do case of
if get(g:, 'haskell_enable_static_pointers', 0)
  syn keyword haskellStatic static
endif
syn keyword haskellConditional if then else
syn match haskellNumber "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>\|\<0[bB][10]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match haskellSeparator  "[,;]"
syn region haskellParens matchgroup=haskellDelimiter start="(" end=")" contains=TOP,haskellTypeSig,@Spell
syn region haskellBrackets matchgroup=haskellDelimiter start="\[" end="]" contains=TOP,haskellTypeSig,@Spell
syn region haskellBlock matchgroup=haskellDelimiter start="{" end="}" contains=TOP,@Spell
syn keyword haskellInfix infix infixl infixr
syn keyword haskellBottom undefined error
syn match haskellOperators "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"
syn match haskellQuote "\<'\+" contained
syn match haskellQuotedType "[A-Z][a-zA-Z0-9_']*\>" contained
syn region haskellQuoted start="\<'\+" end="\>"
  \ contains=
  \ haskellType,
  \ haskellQuote,
  \ haskellQuotedType,
  \ haskellSeparator,
  \ haskellParens,
  \ haskellOperators,
  \ haskellIdentifier
syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
  \ contains=
  \ haskellTodo,
  \ @Spell
syn match haskellBacktick "`[A-Za-z_][A-Za-z0-9_\.']*#\?`"
syn region haskellString start=+"+ skip=+\\\\\|\\"+ end=+"+
  \ contains=@Spell
syn match haskellIdentifier "[_a-z][a-zA-z0-9_']*" contained
syn match haskellChar "\<'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'\>"
syn match haskellType "\<[A-Z][a-zA-Z0-9_']*\>"
syn region haskellBlockComment start="{-" end="-}"
  \ contains=
  \ haskellBlockComment,
  \ haskellTodo,
  \ @Spell
syn region haskellPragma start="{-#" end="#-}"
syn region haskellLiquid start="{-@" end="@-}"
"syn match haskellPreProc "^#.*$"
syn keyword haskellTodo TODO FIXME contained
" Treat a shebang line at the start of the file as a comment
syn match haskellShebang "\%^#!.*$"
if !get(g:, 'haskell_disable_TH', 0)
    syn match haskellQuasiQuoted "." containedin=haskellQuasiQuote contained
    syn region haskellQuasiQuote matchgroup=haskellTH start="\[[_a-zA-Z][a-zA-z0-9._']*|" end="|\]"
    syn region haskellTHBlock matchgroup=haskellTH start="\[\(d\|t\|p\)\?|" end="|]" contains=TOP
    syn region haskellTHDoubleBlock matchgroup=haskellTH start="\[||" end="||]" contains=TOP
endif
if get(g:, 'haskell_enable_typeroles', 0)
  syn keyword haskellTypeRoles phantom representational nominal contained
  syn region haskellTypeRoleBlock matchgroup=haskellTypeRoles start="type\s\+role" end="$" keepend
    \ contains=
    \ haskellType,
    \ haskellTypeRoles
endif
if get(g:, 'haskell_enable_quantification', 0)
  syn keyword haskellForall forall
endif
if get(g:, 'haskell_enable_recursivedo', 0)
  syn keyword haskellRecursiveDo mdo rec
endif
if get(g:, 'haskell_enable_arrowsyntax', 0)
  syn keyword haskellArrowSyntax proc
endif
if get(g:, 'haskell_enable_pattern_synonyms', 0)
  syn keyword haskellPatternKeyword pattern
endif

highlight def link haskellBottom Macro
highlight def link haskellTH Boolean
highlight def link haskellIdentifier Identifier
highlight def link haskellForeignKeywords Structure
highlight def link haskellKeyword Keyword
highlight def link haskellDefault Keyword
highlight def link haskellConditional Conditional
highlight def link haskellNumber Number
highlight def link haskellFloat Float
highlight def link haskellSeparator Delimiter
highlight def link haskellDelimiter Delimiter
highlight def link haskellInfix Keyword
highlight def link haskellOperators Operator
highlight def link haskellQuote Operator
highlight def link haskellShebang Comment
highlight def link haskellLineComment Comment
highlight def link haskellBlockComment Comment
highlight def link haskellPragma SpecialComment
highlight def link haskellLiquid SpecialComment
highlight def link haskellString String
highlight def link haskellChar String
highlight def link haskellBacktick Operator
highlight def link haskellQuasiQuoted String
highlight def link haskellTodo Todo
"highlight def link haskellPreProc PreProc
highlight def link haskellAssocType Type
highlight def link haskellQuotedType Type
highlight def link haskellType Type
highlight def link haskellImportKeywords Include
if get(g:, 'haskell_classic_highlighting', 0)
  highlight def link haskellDeclKeyword Keyword
  highlight def link HaskellDerive Keyword
  highlight def link haskellDecl Keyword
  highlight def link haskellWhere Keyword
  highlight def link haskellLet Keyword
else
  highlight def link haskellDeclKeyword Structure
  highlight def link HaskellDerive Structure
  highlight def link haskellDecl Structure
  highlight def link haskellWhere Structure
  highlight def link haskellLet Structure
endif

if get(g:, 'haskell_enable_quantification', 0)
  highlight def link haskellForall Operator
endif
if get(g:, 'haskell_enable_recursivedo', 0)
  highlight def link haskellRecursiveDo Keyword
endif
if get(g:, 'haskell_enable_arrowsyntax', 0)
  highlight def link haskellArrowSyntax Keyword
endif
if get(g:, 'haskell_enable_static_pointers', 0)
  highlight def link haskellStatic Keyword
endif
if get(g:, 'haskell_classic_highlighting', 0)
  if get(g:, 'haskell_enable_pattern_synonyms', 0)
    highlight def link haskellPatternKeyword Keyword
  endif
  if get(g:, 'haskell_enable_typeroles', 0)
    highlight def link haskellTypeRoles Keyword
  endif
else
  if get(g:, 'haskell_enable_pattern_synonyms', 0)
    highlight def link haskellPatternKeyword Structure
  endif
  if get(g:, 'haskell_enable_typeroles', 0)
    highlight def link haskellTypeRoles Structure
  endif
endif

if get(g:, 'haskell_backpack', 0)
  highlight def link haskellBackpackStructure Structure
  highlight def link haskellBackpackDependency Include
endif

let b:current_syntax = "tidal"
autocmd BufRead,BufNewFile *.tidal setfiletype tidal
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let s:not_prefixable_keywords = [ "import", "data", "instance", "class", "{-#", "type", "case", "do", "let", "default", "foreign", "--"]

" guess correct number of spaces to indent
" (tabs are not allowed)
function! Get_indent_string()
    return repeat(" ", 4)
endfunction

" replace tabs by spaces
function! Tab_to_spaces(text)
    return substitute(a:text, "	", Get_indent_string(), "g")
endfunction

" Wrap in :{ :} if there's more than one line
function! Wrap_if_multi(lines)
    if len(a:lines) > 1
        return [":{"] + a:lines + [":}"]
    else
        return a:lines
    endif
endfunction

" change string into array of lines
function! Lines(text)
    return split(a:text, "\n")
endfunction

" change lines back into text
function! Unlines(lines)
    return join(a:lines, "\n") . "\n"
endfunction

" vim slime handler
function! _EscapeText_tidal(text)
    let l:lines = Lines(Tab_to_spaces(a:text))
    let l:lines = Wrap_if_multi(l:lines)
    return Unlines(l:lines)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"I've found bindings work more consistantly when defined inside the .vimrc
"currently, all key bindings have been moved to the .vimrc inside the config/ folder

"if !exists("g:tidal_no_mappings") || !g:tidal_no_mappings
"  if !hasmapto('<Plug>TidalConfig', 'n')
"    nmap <buffer> <localleader>c <Plug>TidalConfig
"  endif
"
"  if !hasmapto('<Plug>TidalRegionSend', 'x')
"    xmap <buffer> <localleader>s  <Plug>TidalRegionSend
"    xmap <buffer> <c-l> <Plug>TidalRegionSend
"  endif
"
"  if !hasmapto('<Plug>TidalLineSend', 'n')
"    nmap <buffer> <localleader>s  <Plug>TidalLineSend
"    nmap <buffer> <c-k> <Plug>TidalLineSend
"  endif
"
"  if !hasmapto('<Plug>TidalParagraphSend', 'n')
"    nmap <buffer> <localleader>ss <Plug>TidalParagraphSend
"    nmap <buffer> <c-l> <Plug>TidalParagraphSend
"  endif
"
"  if !hasmapto('<Plug>TidalHush', 'n')
"    " no idea why this bind isn't working, 
"    " currently, the binding is defined in the ,vimrc
"    nmap <buffer> <m-h> <Plug>TidalHush
"  endif
"
"  let i = 1
"  while i <= 9
"    execute 'nnoremap <buffer> <localleader>'.i.'  :TidalSilence '.i.'<cr>'
"    execute 'nnoremap <buffer> <c-'.i.'>  :TidalSilence '.i.'<cr>'
"    execute 'nnoremap <buffer> <localleader>s'.i.' :TidalPlay '.i.'<cr>'
"    let i += 1
"  endwhile
"
"endif
"" General
set number	"" Show line numbers
set linebreak	"" Break lines at word (requires Wrap lines)
set showbreak=+++	"" Wrap-broken line prefix
set textwidth=100	"" Line wrap (number of cols)
set showmatch	"" Highlight matching brace
set visualbell	"" Use visual bell (no beeping)

set hlsearch	"" Highlight all search results
set smartcase	"" Enable smart-case search
set ignorecase	"" Always case-insensitive
set incsearch	"" Searches for strings incrementally

set autoindent	"" Auto-indent new lines
set shiftwidth=2	"" Number of auto-indent spaces
set smartindent	"" Enable smart-indent
set smarttab	"" Enable smart-tabs
set softtabstop=2	"" Number of spaces per Tab

"" Advanced
set ruler	"" Show row and column ruler information

set undolevels=1000	"" Number of undo levels
set backspace=indent,eol,start	"" Backspace behaviour

"" Generated by VimConfig.com

"call plug#begin()

"Plug 'Dsm0/vim-tidal'
"Plug 'jiangmiao/auto-pairs'
"Plug 'supercollider/scvim'

"call plug#end()

augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
augroup END

" SClang stuff


" tidalvim binds
let g:tidal_no_mappings = 1

nmap <buffer> <localleader>c <Plug>TidalConfig

xmap <buffer> <localleader>s  <Plug>TidalRegionSend
xmap <buffer> <c-l> <Plug>TidalRegionSend

nmap <buffer> <localleader>ss <Plug>TidalParagraphSend
nmap <buffer> <c-l> <Plug>TidalParagraphSend

nmap <buffer> <localleader>s  <Plug>TidalLineSend
nmap <buffer> <c-k> <Plug>TidalLineSend

"xmap <buffer> <localleader>s  <Plug>SclangRegionSend
"xmap <buffer> <c-L>           <Plug>SclangRegionSend

"nmap <buffer> <localleader>s  <Plug>SclangLineSend
"nmap <buffer> <c-l> 	      <Plug>SclangLineSend


map <m-h> :TidalHush <Return>

let i = 1
while i <= 9
  execute 'nnoremap <buffer> <localleader>'.i.'  :TidalSilence '.i.'<cr>'
  execute 'nnoremap <buffer> <m-'.i.'>  :TidalSilence '.i.'<cr>'
  execute 'nnoremap <buffer> <localleader>s'.i.' :TidalPlay '.i.'<cr>'
  let i += 1
endwhile

" When the filetype is FILETYPE then make AutoPairs only match for parenthesis
" au Filetype tidal let b:AutoPairs = {"(": ")"}

if !exists('g:AutoPairsLoaded') || &cp
  echo "AHAHAHAHHHAHAH"
  finish
end

let g:AutoPairsFlyMode = 1
echo 


set nocp

set noswapfile "feel free to change

