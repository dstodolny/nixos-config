{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.vim;
in
{
  options = {
    profiles.vim = {
      enable = mkEnableOption "Enable vim profile";
    };
  };
  config = mkIf cfg.enable {
    home.file = {
      ".config/nvim".source = ../../assets/nvim;
    };
    programs = {
      fzf = {
        enable = true;
      };
      neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
        plugins = with pkgs.vimPlugins; [
          auto-pairs
          fzf-vim
          fzfWrapper
          vim-airline
          vim-commentary
          vim-ledger
          vim-nix
          vim-polyglot
          vim-repeat
          vim-surround
        ];
        extraConfig = ''
          syntax on
          filetype plugin indent on

          colorscheme gotham
          set termguicolors
          let mapleader=','

          set autowrite
          set clipboard=unnamed
          set expandtab
          set hidden
          set ignorecase
          set mouse=a
          set noshowmode
          set nowritebackup
          set number
          set relativenumber
          set scrolloff=8
          set smartcase
          set smartindent
          set splitbelow
          set splitright
          set shiftwidth=2
          set softtabstop=2
          set tabstop=2
          set undofile
          set wrap

          let g:airline_theme='gotham'
          let g:AutoPairsMultilineClose=0
          let g:polyglot_disabled=['org']

          command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!{.git,node_modules,build,dist,*/node_modules}/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

          function! CloseWindowOrKillBuffer()
            let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))
            if matchstr(expand("%"), 'NERD') == 'NERD'
              wincmd c
              return
            endif
            if number_of_windows_to_this_buffer > 1
              wincmd c
            else
              bdelete
            endif
          endfunction

          autocmd BufWritePre * %s/\s\+$//e

          nmap 0 ^
          imap jk <esc>
          imap kj <esc>
          nmap k gk
          nmap j gj
          nnoremap <leader><leader> <c-^>
          nnoremap <C-j> <C-w>j
          nnoremap <C-k> <C-w>k
          nnoremap <C-h> <C-w>h
          nnoremap <C-l> <C-w>l
          nmap <silent> // :nohlsearch<cr>
          nnoremap <silent> Q :call CloseWindowOrKillBuffer()<CR>
          map <C-P> :FZF<CR>
          map <C-T> :Buffers<CR>
          map <leader>f :Find<CR>
        '';
      };
    };
  };
}
