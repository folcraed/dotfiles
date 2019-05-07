---------------------------
-- Onedark awesome theme --
---------------------------

local theme = {}
profileConfigPath = "/home/rob/dotfiles/awesome/"

-- Setting for the Collision keybindings lib
theme.collision_resize_width = 10
theme.Collision_shape_width = 20
theme.collision_shape_height = 20
theme.collision_focus_padding = 5

-- Theme fonts
theme.font          = "Hack 11"
theme.taglist_font  = "Xirod 10"

-- Theme coloring
theme.bg_normal     = "#2b303b"
theme.bg_focus      = "#2b303b"
theme.bg_urgent     = "#bf616a"
theme.bg_alternate  = "#bf616a"
theme.bg_minimize   = "#96b5b4"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#60AEEE"
theme.fg_focus      = "#E4BF7A"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#2b303b"

theme.useless_gap   = 5
theme.border_width  = 1
theme.border_normal = "#2b303b"
theme.border_focus  = "#2b303b"
theme.border_marked = "#bf616a"

-- Display the taglist highlights
theme.taglist_squares_sel   = profileConfigPath.."themes/Onedark/taglist/linefw.png"
theme.taglist_squares_unsel = profileConfigPath.."themes/Onedark/taglist/linew.png"

theme.menu_submenu_icon = "/home/rob/.config/awesome/themes/Onedark/submenu.png"
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
theme.wallpaper = "/home/rob/Pictures/NightSea.jpg"

-- Layout icons
theme.layout_fairh = profileConfigPath.."themes/Onedark/layouts/fairh.png"
theme.layout_fairv = profileConfigPath.."themes/Onedark/layouts/fairv.png"
theme.layout_floating  = profileConfigPath.."themes/Onedark/layouts/floating.png"
theme.layout_tilebottom = profileConfigPath.."themes/Onedark/layouts/tilebottom.png"
theme.layout_tileleft   = profileConfigPath.."themes/Onedark/layouts/tileleft.png"
theme.layout_tile = profileConfigPath.."themes/Onedark/layouts/tile.png"
theme.layout_tiletop = profileConfigPath.."themes/Onedark/layouts/tiletop.png"
theme.awesome_icon = profileConfigPath.."themes/Onedark/awesome16.png"

-- Using default icons from system for application menu, etc.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
