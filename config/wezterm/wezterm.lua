-- WezTerm config tuned for opencode + tmux on Ubuntu 20.04
local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Font: match GNOME Terminal's system monospace (Ubuntu Mono 13)
config.font = wezterm.font_with_fallback { 'Ubuntu Mono', 'DejaVu Sans Mono' }
config.font_size = 13.0

-- True color / TUI rendering
config.color_scheme = 'Catppuccin Mocha'

-- Scrollback
config.scrollback_lines = 50000
config.enable_scroll_bar = true

-- Performance
config.front_end = 'WebGpu'
config.max_fps = 120

-- Minimal chrome
config.hide_tab_bar_if_only_one_tab = true
config.use_fancy_tab_bar = false
config.audible_bell = 'Disabled'

-- Clean scrolling: forward the wheel to full-screen apps (opencode) so their
-- own scrollback is used; otherwise scroll WezTerm's buffer smoothly.
config.mouse_bindings = {
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = 'NONE',
    action = wezterm.action.ScrollByLine(-3),
  },
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = 'NONE',
    action = wezterm.action.ScrollByLine(3),
  },
}

return config
