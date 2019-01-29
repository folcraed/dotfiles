---------------------------
-- Solarized awesome theme --
---------------------------

local theme = {}
profileConfigPath = "/home/rob/Dropbox/Settings/dotfiles/awesome/"

-- Setting for the Collision keybindings lib
-- theme.collision_resize_width = 10
-- theme.Collision_shape_width = 20
-- theme.collision_shape_height = 20
-- theme.collision_focus_padding = 5

-- Theme fonts
theme.font          = "Roboto Mono for Powerline 11, FontAwesome 11"
-- theme.taglist_font  = "Xirod 11"

-- Theme coloring
theme.bg_normal     = "#002b36"
theme.bg_focus      = "#586e75"
theme.bg_urgent     = "#dc322f"
theme.bg_alternate  = "#dc322f"
theme.bg_minimize   = "#93a1a1"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#839496"
theme.fg_focus      = "#002b36"
theme.fg_urgent     = "#fdf6e3"
theme.fg_minimize   = "#002b36"

theme.useless_gap   = 5
theme.border_width  = 1
theme.border_normal = "#002b36"
theme.border_focus  = "#93a1a1"
theme.border_marked = "#dc322f"

-- Display the taglist highlights
theme.taglist_squares_sel   = profileConfigPath.."themes/Solarized/taglist/linefw.png"
theme.taglist_squares_unsel = profileConfigPath.."themes/Solarized/taglist/linew.png"

theme.menu_submenu_icon = "/home/rob/.config/awesome/themes/Solarized/submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- Titlebar close
theme.titlebar_close_button_normal = profileConfigPath.."themes/Solarized/titlebar/close_normal.png"
theme.titlebar_close_button_normal_hover = profileConfigPath.."themes/Solarized/titlebar/close_normal_hover.png"
theme.titlebar_close_button_normal_press = profileConfigPath.."themes/Solarized/titlebar/close_normal_press.png"
theme.titlebar_close_button_focus  = profileConfigPath.."themes/Solarized/titlebar/close_focus.png"
theme.titlebar_close_button_focus_hover = profileConfigPath.."themes/Solarized/titlebar/close_focus_hover.png"
theme.titlebar_close_button_focus_press = profileConfigPath.."themes/Solarized/titlebar/close_focus_press.png"

-- Titlebar minimize
theme.titlebar_minimize_button_normal = profileConfigPath.."themes/Solarized/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_normal_hover = profileConfigPath.."themes/Solarized/titlebar/minimize_normal_hover.png"
theme.titlebar_minimize_button_normal_press = profileConfigPath.."themes/Solarized/titlebar/minimize_normal_press.png"
theme.titlebar_minimize_button_focus  = profileConfigPath.."themes/Solarized/titlebar/minimize_focus.png"
theme.titlebar_minimize_button_focus_hover = profileConfigPath.."themes/Solarized/titlebar/minimize_focus_hover.png"
theme.titlebar_minimize_button_focus_press = profileConfigPath.."themes/Solarized/titlebar/minimize_focus_press.png"

-- Titlebar maximize inactive
theme.titlebar_maximized_button_normal_inactive = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_inactive_hover = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_inactive_hover.png"
theme.titlebar_maximized_button_normal_inactive_press = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_inactive_press.png"
theme.titlebar_maximized_button_focus_inactive  = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_inactive_hover = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_inactive_hover.png"
theme.titlebar_maximized_button_focus_inactive_press = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_inactive_press.png"

-- Titlebar maximize active
theme.titlebar_maximized_button_normal_active = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_active_hover = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_active_hover.png"
theme.titlebar_maximized_button_normal_active_press = profileConfigPath.."themes/Solarized/titlebar/maximized_normal_active_press.png"
theme.titlebar_maximized_button_focus_active  = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_active_hover = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_active_hover.png"
theme.titlebar_maximized_button_focus_active_press = profileConfigPath.."themes/Solarized/titlebar/maximized_focus_active_press.png"

-- Wallpaper
theme.wallpaper = "/home/rob/Dropbox/Photos/Backgrounds/sailingship-Solarized.jpg"

-- Layout icons
theme.layout_fairh = profileConfigPath.."themes/Solarized/layouts/fairh.png"
theme.layout_fairv = profileConfigPath.."themes/Solarized/layouts/fairv.png"
theme.layout_floating  = profileConfigPath.."themes/Solarized/layouts/floating.png"
theme.layout_tilebottom = profileConfigPath.."themes/Solarized/layouts/tilebottom.png"
theme.layout_tileleft   = profileConfigPath.."themes/Solarized/layouts/tileleft.png"
theme.layout_tile = profileConfigPath.."themes/Solarized/layouts/tile.png"
theme.layout_tiletop = profileConfigPath.."themes/Solarized/layouts/tiletop.png"
theme.awesome_icon = profileConfigPath.."themes/Solarized/awesome16.png"

-- Using default icons from system for application menu, etc.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
