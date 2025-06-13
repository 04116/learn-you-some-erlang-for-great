-- =============================================================================
-- Neovim Configuration for Erlang Development
-- =============================================================================
-- This configuration provides:
-- - Tab-only indentation (no spaces)
-- - Auto-formatting on save
-- - Erlang-specific syntax highlighting and features
-- - LSP integration with erlang_ls
-- =============================================================================

-- Tab Configuration (Tabs Only, No Spaces)
vim.opt.expandtab = false      -- Use tabs, not spaces
vim.opt.tabstop = 4           -- Tab width = 4 characters
vim.opt.shiftwidth = 4        -- Indentation width = 4
vim.opt.softtabstop = 4       -- Soft tab = 4
vim.opt.smartindent = true    -- Smart auto-indenting
vim.opt.autoindent = true     -- Copy indent from current line

-- Show tabs and trailing spaces
vim.opt.list = true
vim.opt.listchars = {
	tab = '→ ',              -- Show tabs as →
	trail = '·',             -- Show trailing spaces as ·
	extends = '⟩',           -- Show line continuation
	precedes = '⟨'           -- Show line continuation
}

-- File-specific settings for Erlang
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'erlang', 'erl'},
	callback = function()
		-- Ensure tabs are used for Erlang files
		vim.opt_local.expandtab = false
		vim.opt_local.tabstop = 4
		vim.opt_local.shiftwidth = 4
		vim.opt_local.softtabstop = 4
		
		-- Erlang-specific settings
		vim.opt_local.textwidth = 80
		vim.opt_local.colorcolumn = '80'
		vim.opt_local.wrap = false
		
		-- Show function signatures
		vim.opt_local.foldmethod = 'syntax'
		vim.opt_local.foldlevel = 99
	end
})

-- Auto-format on save for Erlang files
vim.api.nvim_create_autocmd('BufWritePre', {
	pattern = {'*.erl', '*.hrl', '*.escript'},
	callback = function()
		-- Format with erlang_ls if available, otherwise use basic formatting
		local has_lsp = false
		for _, client in pairs(vim.lsp.get_active_clients()) do
			if client.name == 'erlangls' then
				has_lsp = true
				vim.lsp.buf.format({ async = false })
				break
			end
		end
		
		if not has_lsp then
			-- Basic formatting: convert spaces to tabs
			local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
			for i, line in ipairs(lines) do
				-- Convert leading spaces to tabs (assuming 4 spaces = 1 tab)
				local leading_spaces = string.match(line, '^( *)')
				if leading_spaces and #leading_spaces > 0 then
					local num_tabs = math.floor(#leading_spaces / 4)
					local remaining_spaces = #leading_spaces % 4
					local new_line = string.rep('\t', num_tabs) .. string.rep(' ', remaining_spaces) .. string.sub(line, #leading_spaces + 1)
					lines[i] = new_line
				end
			end
			vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
		end
	end
})

-- Erlang LSP Configuration (erlang_ls)
local lspconfig = require('lspconfig')

lspconfig.erlangls.setup({
	cmd = { 'erlang_ls' },
	filetypes = { 'erlang' },
	root_dir = lspconfig.util.root_pattern('rebar.config', 'erlang.mk', '.git'),
	settings = {
		erlangls = {
			-- Enable formatting
			formatting = {
				enabled = true,
			},
			-- Enable diagnostics
			diagnostics = {
				enabled = true,
			},
			-- Code completion settings
			completion = {
				enabled = true,
			}
		}
	},
	on_attach = function(client, bufnr)
		-- Enable formatting
		if client.server_capabilities.documentFormattingProvider then
			vim.api.nvim_buf_set_option(bufnr, 'formatexpr', 'v:lua.vim.lsp.formatexpr()')
		end
		
		-- Key mappings for LSP
		local opts = { noremap = true, silent = true, buffer = bufnr }
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
		vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
		vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
		vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
		vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
		vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, opts)
		vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
		vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format({ async = true }) end, opts)
	end
})

-- Erlang-specific key mappings
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'erlang', 'erl'},
	callback = function()
		local opts = { noremap = true, silent = true, buffer = true }
		
		-- Compile current file
		vim.keymap.set('n', '<F5>', ':!rebar3 compile<CR>', opts)
		
		-- Run tests
		vim.keymap.set('n', '<F6>', ':!rebar3 eunit<CR>', opts)
		
		-- Start shell
		vim.keymap.set('n', '<F7>', ':!rebar3 shell<CR>', opts)
		
		-- Format buffer manually
		vim.keymap.set('n', '<leader>f', function()
			vim.lsp.buf.format({ async = false })
		end, opts)
	end
})

-- Additional Erlang syntax highlighting
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'erlang', 'erl'},
	callback = function()
		-- Enhanced syntax highlighting for Erlang
		vim.cmd([[
			syntax match erlangAtom /\v<[a-z][a-zA-Z0-9_]*>/
			syntax match erlangVariable /\v<[A-Z][a-zA-Z0-9_]*>/
			syntax match erlangModule /\v^-module\(\zs[^)]+\ze\)/
			syntax match erlangExport /\v^-export\(\[\zs[^\]]+\ze\]\)/
			syntax match erlangSpec /\v^-spec\s+\zs[^(]+\ze\(/
			
			highlight link erlangAtom Constant
			highlight link erlangVariable Identifier
			highlight link erlangModule Type
			highlight link erlangExport Function
			highlight link erlangSpec Function
		]])
	end
})

-- Auto-completion configuration
local cmp = require('cmp')
cmp.setup({
	snippet = {
		expand = function(args)
			require('luasnip').lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert({
		['<C-d>'] = cmp.mapping.scroll_docs(-4),
		['<C-f>'] = cmp.mapping.scroll_docs(4),
		['<C-Space>'] = cmp.mapping.complete(),
		['<C-e>'] = cmp.mapping.close(),
		['<CR>'] = cmp.mapping.confirm({ select = true }),
		['<Tab>'] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end, { 'i', 's' }),
	}),
	sources = cmp.config.sources({
		{ name = 'nvim_lsp' },
		{ name = 'luasnip' },
	}, {
		{ name = 'buffer' },
		{ name = 'path' },
	})
})

-- Prevent accidental space-to-tab conversion in comments and strings
vim.api.nvim_create_autocmd('InsertCharPre', {
	pattern = {'*.erl', '*.hrl', '*.escript'},
	callback = function()
		-- Only convert spaces to tabs if we're not in a comment or string
		local char = vim.v.char
		if char == ' ' then
			local line = vim.api.nvim_get_current_line()
			local col = vim.fn.col('.')
			local before_cursor = string.sub(line, 1, col - 1)
			
			-- Check if we're at the beginning of the line (indentation)
			if string.match(before_cursor, '^%s*$') then
				-- We're in indentation area, this should be a tab
				vim.v.char = '\t'
			end
		end
	end
})

-- Status line configuration for Erlang
vim.api.nvim_create_autocmd('FileType', {
	pattern = {'erlang', 'erl'},
	callback = function()
		-- Show current function in status line
		vim.opt_local.statusline = '%f %m%r%h%w [%{&ff}] [%Y] [%04l,%04v] [%p%%] [LEN=%L] {%{ErlangCurrentFunction()}}'
	end
})

-- Function to get current Erlang function name
vim.cmd([[
function! ErlangCurrentFunction()
	let save_cursor = getpos(".")
	let func_line = search('^\s*\w\+\s*(', 'bnW')
	if func_line > 0
		let func_name = matchstr(getline(func_line), '^\s*\zs\w\+\ze\s*(')
		call setpos('.', save_cursor)
		return func_name
	endif
	call setpos('.', save_cursor)
	return ''
endfunction
]])

-- Project-specific settings
vim.api.nvim_create_autocmd('BufEnter', {
	pattern = {'*/cookie_crud/*'},
	callback = function()
		-- Set working directory to project root
		vim.cmd('cd %:p:h')
		while vim.fn.findfile('rebar.config', '.;') == '' and vim.fn.getcwd() != '/' do
			vim.cmd('cd ..')
		end
		
		-- Project-specific key mappings
		local opts = { noremap = true, silent = true }
		vim.keymap.set('n', '<leader>cc', ':!make compile<CR>', opts)
		vim.keymap.set('n', '<leader>ct', ':!make test<CR>', opts)
		vim.keymap.set('n', '<leader>cs', ':!make cluster-start<CR>', opts)
		vim.keymap.set('n', '<leader>cS', ':!make cluster-stop<CR>', opts)
		vim.keymap.set('n', '<leader>cT', ':!make cluster-test<CR>', opts)
	end
})