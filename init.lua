-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	"neovim/nvim-lspconfig",
	{
		"nvim-telescope/telescope.nvim",
		dependencies = { "nvim-lua/plenary.nvim" }
	},
	"avm99963/vim-jjdescription",
})

require("lspconfig").gopls.setup{}

-- Enable diagnostics
vim.diagnostic.config({ virtual_text = true })

-- Enable line numbers
vim.opt.number = true

-- Colors
--
-- The colors I want are actually the "light" background of "vim" with GUI
-- colors disabled. This is despite having a dark background terminal.
vim.cmd[[colorscheme vim]]
vim.opt.bg = "light"
vim.opt.termguicolors = false
-- Fixup bad default highlight yellow.
vim.cmd[[hi Search term=reverse ctermbg=3 guibg=Yellow]]
-- Fixup bad default "Visual" (used for Telescope selection)
vim.cmd[[hi Visual ctermfg=0 guifg=Black]]
-- Fixup bad default bad spelling
vim.cmd[[hi SpellBad ctermfg=0 guifg=Black]]
-- Fixup bad default bad diff change
vim.cmd[[hi DiffChange ctermfg=0 guifg=Black]]
vim.cmd[[hi DiffText ctermfg=0 guifg=Black]]
-- Fixup bad default diagnostic column background
vim.cmd[[hi SignColumn ctermbg=none guibg=none]]
-- Fixup bad default fold column background
vim.cmd[[hi FoldColumn ctermbg=none guibg=none]]

-- Always display sign column (diagnostics)
vim.cmd[[set signcolumn=yes]]

-- Display tabs
vim.opt.list = true
vim.opt.listchars = {
	tab = '».'
}

-- Window switching
vim.keymap.set('n', '<c-h>', '<c-w>h')
vim.keymap.set('n', '<c-j>', '<c-w>j')
vim.keymap.set('n', '<c-k>', '<c-w>k')
vim.keymap.set('n', '<c-l>', '<c-w>l')

-- Open newline without entering insert mode
vim.keymap.set('n', 'go', 'o<esc>')
vim.keymap.set('n', 'gO', 'O<esc>')

-- Clear highlight on enter
vim.keymap.set('n', '<cr>', ':noh<cr><cr>')

-- Undofile
vim.cmd[[set undofile]]
vim.cmd[[au BufWritePre /tmp/* setlocal noundofile]]

-- No mouse
vim.cmd[[set mouse=]]

-- Highlight trailing whitespace
vim.cmd[[highlight ExtraWhitespace ctermbg=red guibg=red]]
vim.cmd[[autocmd BufWinEnter * match ExtraWhitespace /\s\+$/]]

-- Telescope
local builtin = require("telescope.builtin")
vim.keymap.set('n', 'gf', builtin.find_files, {})

-- Use telescope versions of :help lsp-defaults

-- References
vim.keymap.set('n', 'grr', function()
	-- Open in normal mode.
	return builtin.lsp_references({on_complete = { function()
		vim.cmd[[stopinsert]]
	end }})
end, {})
-- Implementations
vim.keymap.set('n', 'gri', function()
	-- Open in normal mode.
	return builtin.lsp_implementations({on_complete = { function()
		vim.cmd[[stopinsert]]
	end }})
end, {})
-- Symbol search
vim.keymap.set('n', 'grs', builtin.lsp_dynamic_workspace_symbols, {})
-- Outline
vim.keymap.set('n', 'gro', builtin.lsp_document_symbols, {})

vim.keymap.set('n', 'grh', vim.lsp.buf.signature_help)
