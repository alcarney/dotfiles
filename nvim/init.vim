" -- init.vim
"
"  Base settings and defaults that apply everywhere
set path+=**

" -- tabs and spaces
"
" tabstop:     Tabs = N spaces
" expandtab:   Insert spaces when we use the <tab> key
" shiftwidth:  How many spaces do indentation commands use?
" softtabstop: Treat N spaces as a single tab character
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=4

" -- text formatting
"
" textwidth:     Lines should be N characters long
" formatoptions: Controls how automatic formatting works
"   c              Automatically wrap comments according to `textwidth`
"   o              Automatically insert comment chars on 'o' (normal mode)
"   r              Automatically insert comment chars on Enter (insert mode)
"   q              Allow formatting with 'gq'
"   j              Automatically remove comment chars on 'j' (normal mode)
set textwidth=79
set formatoptions=corqj

" -- appearance
"
" list:          Display whitespace chars according to `listchars`
" title:         Use the current filename as window title
" mouse:         Which modes is the mouse active?
" nowrap:        Disable line wrapping
" number:        Show line numbers
" listchars:     Render the listed whitespace chars as...
" background:    Light or dark colorscheme?
" noshowmode:    Hide the default mode indicator e.g. '-- INSERT --'
" cursorline:    Highlight the current line
" nofoldenable:  Disable folding
" laststatus:    Only show a single, full-width statusline
" signcolumn:    Where to show git signs, warning/error markers etc.
" showtabline:   Only show tabline when there's more than one tab
" colorcolumn:   Highlight column at textwidth + 1
" termguicolors: Enable 24bit color in the terminal
set list
set title
set nowrap
set number
set mouse=nv
set noshowmode
set cursorline
set nofoldenable
set laststatus=3
set showtabline=1
set colorcolumn=+1
set termguicolors
set background=dark
set signcolumn=yes
set listchars=tab:».,trail:·,extends:→,precedes:←

" -- custom tabline
"
" I like to keep tabs focused on a particular project by using :tcd to set the
" woking directory for the tab to the project's root.
"
" This tabline function simply displays the working directory for the
" corresponding tab.
function Tabline()
    let s = ''

    for i in range(tabpagenr('$'))
        " highlight just the selected tab
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        let s .= ' %{getcwd(-1,' . (i + 1) . ')} '
    endfor

    " fill the remaining space with the fill color
    let s .= '%#TabLineFill#'

    return s
endfunction

set tabline=%!Tabline()


colorscheme everforest

" -- keymaps
"
" mapleader:      Prefix key to use for <leader> mappings
" maplocalleader: Prefix key to use for <localleader> mappings
let mapleader = ' '
let maplocalleader = ','

" -- buffers
"
" hidden: Permit modified buffers to be sent to the background
set hidden

" -- windows
"
" splitbelow: Open horizontal splits below
set splitbelow

nnoremap <c-h>     <c-w><c-h>
nnoremap <c-j>     <c-w><c-j>
nnoremap <c-k>     <c-w><c-k>
nnoremap <c-l>     <c-w><c-l>

" -- centered movement
"
" Tweak some of the standard movement commands to also re-center the buffer.
nnoremap n     nzz
nnoremap N     Nzz
nnoremap G     Gzz
nnoremap <c-i> <c-i>zz
nnoremap <c-o> <c-o>zz

" -- editing
"
" Make alt+j/alt+k behave like alt+up/alt+down in VSCode.
inoremap <a-j>      <esc>:move .+1<cr>==gi
inoremap <a-down>   <esc>:move .+1<cr>==gi
inoremap <a-k>      <esc>:move .-2<cr>==gi
inoremap <a-up>     <esc>:move .-2<cr>==gi

xnoremap <a-j>      :move '>+1<cr>gv
xnoremap <a-down>   :move '>+1<cr>gv
xnoremap <a-k>      :move '<-2<cr>gv
xnoremap <a-up>     :move '<-2<cr>gv

" keep selection when indenting
xnoremap >   >gv
xnoremap <   <gv

"nnoremap! <C-BS> <C-W>   this doesn't work for some reason...

" -- searching
"
" incsearch:    Incrementally higlight search matches
" inccommand:   Preview :s/../../ commands in a new split
" noignorecase: Case sensitive searches by default.
"               Use '\c' anywhere in the search pattern to disable it.
set incsearch
set noignorecase
set inccommand=split

" open results of the most recent search in a location list
nnoremap <leader>/  :silent! lvimgrep /<c-r>//j %<cr>:lwindow<cr>

" reset search highlights
nnoremap <localleader>l      <Cmd>nohlsearch<Bar>diffupdate<CR><C-L>

" -- netrw - the built in file browser
"
" netrw_banner=0:    Hide the banner
" netrw_listhide:    Which files should be excluded?
" netrw_liststyle=3  Display files in a tree.
let g:netrw_banner=0
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()


" ---------------------------------- Auto Commands ---------------------------
"
" - Trim trailing whitespace on save
augroup general
    autocmd!
    autocmd BufWritePre * %s/\s\+$//e
augroup END
