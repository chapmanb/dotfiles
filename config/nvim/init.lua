---@diagnostic disable: missing-fields

-- INFO: introduction
-- this is a minimal neovim configuration written in lua. this is not meant to
-- be a distribution, but rather a template for you to build upon and/or a
-- reference for how to configure neovim using lua in the latest version.
--
-- TUTOR:
-- if you're completely new to neovim and/or vim, consider going through
-- `:Tutor` inside neovim to get a basic idea of how it works.
--     if you don't know what this means, type the following:
--       - <escape key>
--       - :
--       - Tutor
--       - <enter key>
--
-- LUA:
-- some level of familiarity with lua/programming languages are also expected.
-- if you're new to lua, consider going through the official reference:
--    https://www.lua.org/manual
-- or a more friendly tutorial like:
--    https://learnxinyminutes.com/docs/lua/
-- you can also check out `:h lua-guide` inside neovim for a neovim-specific
-- lua guide.
--
-- DEPENDENCIES:
-- this configuration assumes you have the following tools installed on your
-- system:
--    `git` - for vim builtin package manager. (see `:h vim.pack`)
--    `ripgrep` - for fuzzy finding
--    `tree-sitter` + a c compiler - to build the treesitter parsers
--      (`brew install tree-sitter-cli`; the npm build needs a newer glibc than
--       ubuntu 20.04 has)
--    clipboard tool: xclip/xsel/win32yank - for clipboard sharing between OS and neovim (see `h: clipboard-tool`)
--    a nerdfont (ensure the terminal running neovim is using it)
-- run `:checkhealth` inside neovim to see if your system is missing anything.
--
-- MINIMAL:
-- to say that something is 'minimal' you have to define what variable you're
-- minimizing. this configuration minimizes for lines of code and concepts.
-- to some, this configuration may have too many plugins. for example, using
-- mason.nvim to manage lsp servers will be an unnecessary dependency if the
-- user is already familiar with lsps and is comfortable managing them through
-- their OS package manager. but to someone that isn't familiar with lsp servers
-- this approach wouldn't cover everything needed to have the 'minimum' necessary
-- for lsp + completion + fuzzy finding. to some, fuzzy finding is also a bloated
-- dependency.
-- this configuration is only a starting point/reference. it is expected that
-- the user will change the configuration to suit their needs.


-- INFO: options
-- these change the default neovim behaviours using the 'vim.opt' API.
-- see `:h vim.opt` for more details.
-- run `:h '{option_name}'` to see what they do and what values they can take.
-- for example, `:h 'number'` for `vim.opt.number`.

-- set <space> as the leader key
-- must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- enable true color support
vim.opt.termguicolors = true

-- make line numbers default
vim.opt.number = true
vim.opt.relativenumber = true

-- enable mouse mode, can be useful for resizing splits
vim.opt.mouse = "a"

-- sync clipboard between OS and neovim.
--  remove this option if you want your OS clipboard to remain independent.
--  see `:help 'clipboard'`
vim.opt.clipboard = "unnamedplus"

-- save undo history
vim.opt.undofile = true

-- keep signcolumn on by default
vim.opt.signcolumn = "yes"

-- sets how neovim will display certain whitespace characters in the editor.
--  see `:help 'list'`
--  and `:help 'listchars'`
vim.opt.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣", }

-- enable live preview of substitutions
vim.opt.inccommand = "split"

-- show which line your cursor is on
vim.opt.cursorline = true

-- set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true

-- enable break indent
vim.opt.breakindent = true

-- enable line wrapping
vim.opt.wrap = true

-- folding, driven by treesitter. `vim.treesitter.foldexpr()` falls back to "no
-- fold" for buffers without a parser, so this is safe to set globally.
-- use `za` to toggle a fold, `zR`/`zM` to open/close them all.
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldtext = ""       -- keep syntax highlighting on the folded line
vim.opt.foldlevelstart = 99 -- open files with everything unfolded

-- formatting
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.textwidth = 80

vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = " ",
      [vim.diagnostic.severity.WARN] = " ",
      [vim.diagnostic.severity.INFO] = " ",
      [vim.diagnostic.severity.HINT] = " ",
    },
  },
  virtual_text = true, -- show inline diagnostics
})

-- clear search highlights with <Esc>
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- INFO: colorscheme
vim.cmd.colorscheme("catppuccin")

-- INFO: plugins
-- we install plugins with neovim's builtin package manager: vim.pack
-- and then enable/configure them by calling their setup functions.
--
-- (see `:h vim.pack` for more details on how it works)
-- you can press `gx` on any of the plugin urls below to open them in your
-- browser and check out their documentation and functionality.
-- alternatively, you can run `:h {plugin-name}` to read their documentation.
--
-- plugins are then loaded and configured with a call to `setup` functions
-- provided by each plugin. this is not a rule of neovim but rather a convention
-- followed by the community.
-- these setup calls take a table as an agument and their expected contents can
-- vary wildly. refer to each plugin's documentation for details.

-- INFO: formatting and syntax highlighting
vim.pack.add({
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/nvim-treesitter/nvim-treesitter-context", -- sticky parents
}, { confirm = false })

local treesitter = require("nvim-treesitter")

treesitter.setup({})

-- refreshing parsers fetches the parser registry over the network, so do it on
-- demand with `:TSUpdateAll` rather than on every startup
vim.api.nvim_create_user_command("TSUpdateAll", function()
  treesitter.update(nil, { summary = true })
end, { desc = "update all installed treesitter parsers", })

-- parsers to keep installed. this branch of nvim-treesitter has no
-- `ensure_installed`/`auto_install` options: parsers are requested by calling
-- `install()`, which is a no-op for parsers that are already present.
treesitter.install({
  "bash", "c", "css", "diff", "html", "json", "lua", "luadoc", "markdown",
  "markdown_inline", "python", "query", "regex", "rust", "toml", "vim",
  "vimdoc", "xml", "yaml",
})

-- neovim only starts treesitter automatically for the few parsers it ships
-- with, so highlighting, indentation and folding have to be turned on per
-- buffer. parsers that aren't installed yet are fetched in the background and
-- the buffer is upgraded once they land.
local function start_treesitter(buf)
  if not vim.api.nvim_buf_is_valid(buf) then
    return
  end

  -- report failures instead of quietly falling back to legacy syntax, which
  -- looks almost identical and hides the fact that treesitter never started
  local ok, err = pcall(vim.treesitter.start, buf)
  if not ok then
    vim.notify("treesitter: " .. tostring(err), vim.log.levels.WARN)
    return
  end

  vim.bo[buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"

  -- nvim evaluates 'foldexpr' when the window first draws the buffer. if the
  -- parser only shows up afterwards (a late `:setfiletype`, or an on demand
  -- parser install) it has already cached "no folds here" and nothing tells it
  -- to try again, so `zc` reports E490. re-setting 'foldmethod' invalidates
  -- those stale folds and they get recomputed against the tree.
  for _, win in ipairs(vim.fn.win_findbuf(buf)) do
    if vim.api.nvim_get_option_value("foldmethod", { win = win, }) == "expr" then
      vim.api.nvim_set_option_value("foldmethod", "expr", { win = win, })
    end
  end
end

-- `get_available()` rebuilds the parser table, so look it up once
local available_parsers

vim.api.nvim_create_autocmd("FileType", {
  desc = "enable treesitter highlighting, indentation and folding",
  callback = function(args)
    -- filetypes like xsd/xslt/svg are all parsed by the xml parser
    local lang = vim.treesitter.language.get_lang(args.match)
    if not lang then
      return
    end

    available_parsers = available_parsers or treesitter.get_available()
    if not vim.list_contains(available_parsers, lang) then
      return
    end

    if vim.list_contains(treesitter.get_installed("parsers"), lang) then
      start_treesitter(args.buf)
    else
      treesitter.install({ lang }):await(vim.schedule_wrap(function(err)
        if not err then start_treesitter(args.buf) end
      end))
    end
  end,
})

-- keep the enclosing nodes (xml elements, functions, ...) pinned to the top of
-- the window so you can see where you are inside deeply nested markup.
require("treesitter-context").setup({
  max_lines = 6,           -- never let the context eat more than 6 lines
  multiline_threshold = 1, -- show only the first line of a multiline open tag
  trim_scope = "inner",    -- drop the innermost lines when over `max_lines`
})

vim.keymap.set("n", "[C", function() require("treesitter-context").go_to_context() end,
  { desc = "Jump to enclosing [C]ontext", })

-- INFO: completion engine
vim.pack.add({ "https://github.com/saghen/blink.cmp" }, { confirm = false })

require("blink.cmp").setup({
  completion = {
    documentation = {
      auto_show = true,
    },
  },

  keymap = { preset = 'super-tab' },

  fuzzy = {
    implementation = "lua",
  },
})

-- INFO: lsp server installation and configuration

-- lsp servers we want to use and their configuration
-- see `:h lspconfig-all` for available servers and their settings
local lsp_servers = {
  lua_ls = {
    -- https://luals.github.io/wiki/settings/ | `:h nvim_get_runtime_file`
    Lua = { workspace = { library = vim.api.nvim_get_runtime_file("lua", true) }, },
  },
  clangd = {},
  rust_analyzer = {},
  -- gopls = {},

  -- xml/xsd/xslt/svg. gives document symbols (so the outline pickers below
  -- work), schema-aware completion, validation and tag renaming.
  -- https://github.com/redhat-developer/vscode-xml/blob/main/docs/Preferences.md
  lemminx = {
    xml = {
      format = { splitAttributes = false, joinCDATALines = false, },
      validation = { enabled = true, noGrammar = "hint", },
      symbols = { maxItemsComputed = 10000, }, -- outlines of big documents
      completion = { autoCloseTags = true, },
    },
  },
}

vim.pack.add({
  "https://github.com/neovim/nvim-lspconfig", -- default configs for lsps

  -- NOTE: if you'd rather install the lsps through your OS package manager you
  -- can delete the next three mason-related lines and their setup calls below.
  -- see `:h lsp-quickstart` for more details.
  "https://github.com/mason-org/mason.nvim",                     -- package manager
  "https://github.com/mason-org/mason-lspconfig.nvim",           -- lspconfig bridge
  "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim" -- auto installer
}, { confirm = false })

require("mason").setup()
require("mason-lspconfig").setup()
require("mason-tool-installer").setup({
  -- lemminx is handled separately below: the binary mason installs is a native
  -- build that needs a newer glibc than ubuntu 20.04 ships
  ensure_installed = vim.tbl_filter(
    function(server) return server ~= "lemminx" end,
    vim.tbl_keys(lsp_servers)
  ),
})

-- configure each lsp server on the table
-- to check what clients are attached to the current buffer, use
-- `:checkhealth vim.lsp`. to view default lsp keybindings, use `:h lsp-defaults`.
for server, config in pairs(lsp_servers) do
  vim.lsp.config(server, {
    settings = config,

    -- only create the keymaps if the server attaches successfully
    on_attach = function(_, bufnr)
      vim.keymap.set("n", "grd", vim.lsp.buf.definition,
        { buffer = bufnr, desc = "vim.lsp.buf.definition()", })

      vim.keymap.set("n", "grf", vim.lsp.buf.format,
        { buffer = bufnr, desc = "vim.lsp.buf.format()", })
    end,
  })
end

-- lemminx is distributed as a native binary that needs a newer glibc than
-- ubuntu 20.04 has, so prefer the java build when it is available. get the jar
-- from the platform independent vsix at
--   https://github.com/redhat-developer/vscode-xml/releases
-- and copy `extension/server/org.eclipse.lemminx-*-uber.jar` to the path below.
local lemminx_jar = vim.fs.joinpath(vim.fn.stdpath("data"), "lemminx", "lemminx-uber.jar")

-- mason-lspconfig only enables the servers it installed itself, so lemminx has
-- to be turned on by hand
if vim.uv.fs_stat(lemminx_jar) then
  vim.lsp.config("lemminx", { cmd = { "java", "-jar", lemminx_jar, }, })
  vim.lsp.enable("lemminx")
end

-- INFO: fuzzy finder
vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",        -- library dependency
  "https://github.com/nvim-tree/nvim-web-devicons",  -- icons (nerd font)
  "https://github.com/nvim-telescope/telescope.nvim" -- the fuzzy finder
}, { confirm = false })

require("telescope").setup({})

local pickers = require("telescope.builtin")

vim.keymap.set("n", "<leader>sp", pickers.builtin, { desc = "[S]earch Builtin [P]ickers", })
vim.keymap.set("n", "<leader>sb", pickers.buffers, { desc = "[S]earch [B]uffers", })
vim.keymap.set("n", "<leader>sf", pickers.find_files, { desc = "[S]earch [F]iles", })
vim.keymap.set("n", "<leader>sw", pickers.grep_string, { desc = "[S]earch Current [W]ord", })
vim.keymap.set("n", "<leader>sg", pickers.live_grep, { desc = "[S]earch by [G]rep", })
vim.keymap.set("n", "<leader>sr", pickers.resume, { desc = "[S]earch [R]esume", })

vim.keymap.set("n", "<leader>sh", pickers.help_tags, { desc = "[S]earch [H]elp", })
vim.keymap.set("n", "<leader>sm", pickers.man_pages, { desc = "[S]earch [M]anuals", })

-- document outline. for xml this comes from lemminx and lists the element
-- tree, which makes it the fastest way to jump around a large document.
vim.keymap.set("n", "<leader>ss", pickers.lsp_document_symbols, { desc = "[S]earch Document [S]ymbols", })
vim.keymap.set("n", "<leader>sS", pickers.lsp_dynamic_workspace_symbols, { desc = "[S]earch Workspace [S]ymbols", })
vim.keymap.set("n", "<leader>st", pickers.treesitter, { desc = "[S]earch [T]reesitter Symbols", })

-- INFO: markup (xml/html) structural navigation

-- nvim knows the common xml extensions but there are a lot of xml dialects it
-- doesn't. fall back to the declaration in the file itself. the negative
-- priority means this is only consulted after every filename and extension
-- rule has already missed.
vim.filetype.add({
  pattern = {
    [".*"] = {
      priority = -math.huge,
      function(_, bufnr)
        for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, 5, false)) do
          if line:match("^%s*<%?xml%s") then
            return "xml"
          elseif line:match("%S") then
            return nil -- only the first non-blank line can carry the declaration
          end
        end
      end,
    },
  },
})

-- neovim already covers a lot of this out of the box: `%` jumps between an
-- opening and closing tag (see `:h matchit`), and `at`/`it` are textobjects for
-- a tag and its contents (`dat`, `vit`, `cit`, ... see `:h tag-blocks`). they
-- work with namespaced tags such as `<a:child>`.
-- what's missing is moving between elements without touching the text, which
-- is what the motions below add on top of the treesitter tree.

--- walk up from `node` until we hit a node of type `wanted`
---@param node TSNode?
---@param wanted string
---@return TSNode?
local function enclosing_node(node, wanted)
  while node and node:type() ~= wanted do
    node = node:parent()
  end
  return node
end

--- move the cursor to the start of `node`, if there is one
---@param node TSNode?
local function jump_to_node(node)
  if node then
    vim.cmd("normal! m'") -- leave a jumplist entry so <C-o> comes back here
    local row, col = node:start()
    vim.api.nvim_win_set_cursor(0, { row + 1, col })
  end
end

--- build a normal mode motion over nodes of type `wanted`
---@param wanted string node type to move between, e.g. "element"
---@param enter fun(node: TSNode): TSNode? how to leave the current node
---@param advance? fun(node: TSNode): TSNode? how to keep going, defaults to `enter`
local function node_motion(wanted, enter, advance)
  advance = advance or enter

  return function()
    local node = enclosing_node(vim.treesitter.get_node(), wanted)
    if not node then
      return
    end

    -- we can land on nodes of the wrong type (text, comments, ...), so keep
    -- going until we find a real one
    local target = enter(node)
    while target and target:type() ~= wanted do
      target = advance(target)
    end

    jump_to_node(target)
  end
end

local function next_sibling(node) return node:next_named_sibling() end
local function prev_sibling(node) return node:prev_named_sibling() end

--- the first thing nested inside an element. the xml grammar puts everything
--- between the tags in a `content` node, html nests the children directly.
---@param node TSNode
---@return TSNode?
local function first_child(node)
  for child in node:iter_children() do
    if child:type() == "content" then
      return child:named_child(0)
    elseif child:named() and child:type() ~= "STag" then
      return child
    end
  end
end

--- build a motion to the opening or closing tag of the element around the
--- cursor. `%` can do this too, but only when the cursor is on the tag name:
--- from the `<` it matches the angle brackets of the same tag instead.
---@param types string[] node types to look for, xml and html name these differently
local function tag_motion(types)
  return function()
    local node = enclosing_node(vim.treesitter.get_node(), "element")
    if not node then
      return
    end

    for child in node:iter_children() do
      if vim.list_contains(types, child:type()) then
        jump_to_node(child)
        return
      end
    end
  end
end

--- the tag name of an element, i.e. the first name in its opening tag
---@param node TSNode element node
---@return TSNode?
local function tag_name(node)
  local tag = node:named_child(0)
  return tag and tag:named_child(0)
end

--- the chain of elements enclosing the cursor, as an xpath-ish string
---@return string
local function element_path()
  local names = {}
  local node = enclosing_node(vim.treesitter.get_node(), "element")

  while node do
    local name = tag_name(node)
    if name then
      table.insert(names, 1, vim.treesitter.get_node_text(name, 0))
    end
    node = enclosing_node(node:parent(), "element")
  end

  return "/" .. table.concat(names, "/")
end

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "xml", "xsd", "xslt", "svg", "html" },
  desc = "element-wise motions for markup buffers",
  callback = function(args)
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = args.buf, desc = desc, })
    end

    -- lowercase stays at the current depth, uppercase changes it
    map("]e", node_motion("element", next_sibling), "Next sibling [e]lement")
    map("[e", node_motion("element", prev_sibling), "Previous sibling [e]lement")
    map("]E", node_motion("element", first_child, next_sibling), "First child [E]lement")
    map("[E", node_motion("element", function(n) return n:parent() end), "Parent [E]lement")

    -- jump between the tags of the current element from anywhere inside it
    map("]t", tag_motion({ "ETag", "end_tag", }), "End [t]ag of this element")
    map("[t", tag_motion({ "STag", "start_tag", }), "Start [t]ag of this element")

    map("<leader>xp", function() vim.notify(element_path()) end, "[X]ml element [p]ath")
  end,
})

-- INFO: keybinding helper
vim.pack.add({ "https://github.com/folke/which-key.nvim" }, { confirm = false })

require("which-key").setup({
  spec = {
    { "<leader>s", group = "[S]earch", icon = { icon = "", color = "green", }, },
    { "<leader>x", group = "[X]ml", icon = { icon = "󰗀", color = "orange", }, },
  }
})

-- uncomment to enable automatic plugin updates
-- vim.pack.update()
