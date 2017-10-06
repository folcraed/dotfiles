---------------------------
-- Onedark awesome theme --
---------------------------

local theme = {}
profileConfigPath = "/home/rob/Dropbox/Settings/dotfiles/awesome/"

theme.font          = "Roboto Mono for Powerline 11, FontAwesome 11"
-- theme.icons         = "/home/rob/.config/awesome/themes/Onedark/icons/"

theme.bg_normal     = "#2b303b"
theme.bg_focus      = "#2b303b"
theme.bg_urgent     = "#bf616a"
theme.bg_minimize   = "#96b5b4"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#81a2be"
theme.fg_focus      = "#97c278"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#2b303b"

theme.useless_gap   = 3
theme.border_width  = 1
theme.border_normal = "#2b303b"
theme.border_focus  = "#2b303b"
theme.border_marked = "#bf616a"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Display the taglist squares
theme.taglist_squares_sel   = profileConfigPath.."themes/Onedark/taglist/linefw.png"
theme.taglist_squares_unsel = profileConfigPath.."themes/Onedark/taglist/linew.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = "/home/rob/.config/awesome/themes/Onedark/submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = profileConfigPath.."themes/Onedark/titlebar/close_normal.png"
theme.titlebar_close_button_normal_hover = profileConfigPath.."themes/Onedark/titlebar/close_normal_hover.png"
theme.titlebar_close_button_normal_press = profileConfigPath.."themes/Onedark/titlebar/close_normal_press.png"
theme.titlebar_close_button_focus  = profileConfigPath.."themes/Onedark/titlebar/close_focus.png"
theme.titlebar_close_button_focus_hover = profileConfigPath.."themes/Onedark/titlebar/close_focus_hover.png"
theme.titlebar_close_button_focus_press = profileConfigPath.."themes/Onedark/titlebar/close_focus_press.png"

theme.titlebar_minimize_button_normal = profileConfigPath.."themes/Onedark/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_normal_hover = profileConfigPath.."themes/Onedark/titlebar/minimize_normal_hover.png"
theme.titlebar_minimize_button_normal_press = profileConfigPath.."themes/Onedark/titlebar/minimize_normal_press.png"
theme.titlebar_minimize_button_focus  = profileConfigPath.."themes/Onedark/titlebar/minimize_focus.png"
theme.titlebar_minimize_button_focus_hover = profileConfigPath.."themes/Onedark/titlebar/minimize_focus_hover.png"
theme.titlebar_minimize_button_focus_press = profileConfigPath.."themes/Onedark/titlebar/minimize_focus_press.png"

theme.titlebar_maximized_button_normal_inactive = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_inactive_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive_hover.png"
theme.titlebar_maximized_button_normal_inactive_press = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive_press.png"
theme.titlebar_maximized_button_focus_inactive  = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_inactive_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive_hover.png"
theme.titlebar_maximized_button_focus_inactive_press = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive_press.png"

theme.titlebar_maximized_button_normal_active = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_normal_active_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_active_hover.png"
theme.titlebar_maximized_button_normal_active_press = profileConfigPath.."themes/Onedark/titlebar/maximized_normal_active_press.png"
theme.titlebar_maximized_button_focus_active  = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_focus_active_hover = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_active_hover.png"
theme.titlebar_maximized_button_focus_active_press = profileConfigPath.."themes/Onedark/titlebar/maximized_focus_active_press.png"

theme.wallpaper = "/home/rob/Dropbox/Photos/Backgrounds/NightSea.jpg"

-- You can use your own layout icons like this:
theme.layout_fairh = profileConfigPath.."themes/Onedark/layouts/fairh.png"
theme.layout_fairv = profileConfigPath.."themes/Onedark/layouts/fairv.png"
theme.layout_floating  = profileConfigPath.."themes/Onedark/layouts/floating.png"
theme.layout_tilebottom = profileConfigPath.."themes/Onedark/layouts/tilebottom.png"
theme.layout_tileleft   = profileConfigPath.."themes/Onedark/layouts/tileleft.png"
theme.layout_tile = profileConfigPath.."themes/Onedark/layouts/tile.png"
theme.layout_tiletop = profileConfigPath.."themes/Onedark/layouts/tiletop.png"
-- theme.layout_cornernw = profileConfigPath.."themes/Onedark/layouts/cornernww.png"
-- theme.layout_cornerne = profileConfigPath.."themes/Onedark/layouts/cornernew.png"
-- theme.layout_cornersw = profileConfigPath.."themes/Onedark/layouts/cornersww.png"
-- theme.layout_cornerse = profileConfigPath.."themes/Onedark/layouts/cornersew.png"

theme.awesome_icon = profileConfigPath.."themes/Onedark/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
