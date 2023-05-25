---------------------------
-- Material awesome theme --
---------------------------

local theme = {}
profileConfigPath = "/home/rob/Dropbox/Settings/dotfiles/awesome/"

-- Setting for the Collision keybindings lib
theme.collision_resize_width = 10
theme.Collision_shape_width = 20
theme.collision_shape_height = 20
theme.collision_focus_padding = 5

-- Theme fonts
theme.font          = "Roboto Mono for Powerline 10, FontAwesome 10"
theme.taglist_font  = "Xirod 11"

-- Theme coloring
theme.bg_normal     = "#222D32"
theme.bg_focus      = "#222D32"
theme.bg_urgent     = "#ff0000"
theme.bg_alternate  = "#bf616a"
theme.bg_minimize   = "#839496"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#657b83"
theme.fg_focus      = "#e4bf7a"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#002b36"

theme.useless_gap   = 5
theme.border_width  = 1
theme.border_normal = "#002b36"
theme.border_focus  = "#586e75"
theme.border_marked = "#91231c"

-- Display the taglist squares
theme.taglist_squares_sel   = profileConfigPath.."themes/Onedark/taglist/linefw.png"
theme.taglist_squares_unsel = profileConfigPath.."themes/Onedark/taglist/linew.png"

theme.menu_submenu_icon = profileConfigPath.."themes/Material/submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- Titlebar close
theme.titlebar_close_button_normal = profileConfigPath.."themes/Onedark/titlebar/close_normal.png"
theme.titlebar_close_button_normal_hover = profileConfigPath.."themes/Onedark/titlebar/close_normal_hover.png"
theme.titlebar_close_button_normal_press = profileConfigPath.."themes/Onedark/titlebar/close_normal_press.png"
theme.titlebar_close_button_focus  = profileConfigPath.."themes/Onedark/titlebar/close_focus.png"
theme.titlebar_close_button_focus_hover = profileConfigPath.."themes/Onedark/titlebar/close_focus_hover.png"
theme.titlebar_close_button_focus_press = profileConfigPath.."themes/Onedark/titlebar/close_focus_press.png"

-- Titlebar minimize
theme.titlebar_minimize_button_normal = profileConfigPath.."themes/Onedark/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_normal_hover = profileConfigPath.."themes/Onedark/titlebar/minimize_normal_hover.png"
theme.titlebar_minimize_button_normal_press = profileConfigPath.."themes/Onedark/titlebar/minimize_normal_press.png"
theme.titlebar_minimize_button_focus  = profileConfigPath.."themes/Onedark/titlebar/minimize_focus.png"
theme.titlebar_minimize_button_focus_hover = profileConfigPath.."themes/Onedark/titlebar/minimize_focus_hover.png"
theme.titlebar_minimize_button_focus_press = profileConfigPath.."themes/Onedark/titlebar/minimize_focus_press.png"

-- Titlebar maximize inactive
theme.titlebar_maximized_button_normal_inactive = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_inactive_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive_hover.png"
theme.titlebar_maximized_button_normal_inactive_press = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive_press.png"
theme.titlebar_maximized_button_focus_inactive  = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_inactive_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive_hover.png"
theme.titlebar_maximized_button_focus_inactive_press = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive_press.png"

-- Titlebar maximize active
theme.titlebar_maximized_button_normal_active = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_active_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_active_hover.png"
theme.titlebar_maximized_button_normal_active_press = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_active_press.png"
theme.titlebar_maximized_button_focus_active  = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_active_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_active_hover.png"
theme.titlebar_maximized_button_focus_active_press = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_active_press.png"

-- Wallpaper
theme.wallpaper = "/home/rob/Dropbox/Photos/Backgrounds/tri-fadeno.jpg"

-- You can use your own layout icons like this:
theme.layout_fairh = profileConfigPath.."themes/Material/layouts/fairhw.png"
theme.layout_fairv = profileConfigPath.."themes/Material/layouts/fairvw.png"
theme.layout_floating  = profileConfigPath.."themes/Material/layouts/floatingw.png"
theme.layout_tilebottom = profileConfigPath.."themes/Material/layouts/tilebottomw.png"
theme.layout_tileleft   = profileConfigPath.."themes/Material/layouts/tileleftw.png"
theme.layout_tile = profileConfigPath.."themes/Material/layouts/tilew.png"
theme.layout_tiletop = profileConfigPath.."themes/Material/layouts/tiletopw.png"
theme.awesome_icon = profileConfigPath.."icons/awesome16.png"

-- Using default icons from the system for application menu, etc.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
