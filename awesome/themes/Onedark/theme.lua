---------------------------
-- Onedark awesome theme --
---------------------------

local theme = {}

theme.font          = "Roboto Mono 11, FontAwesome 11"
-- theme.icons         = "/home/rob/.config/awesome/themes/Onedark/icons/"

theme.bg_normal     = "#2b303b"
theme.bg_focus      = "#2b303b"
theme.bg_urgent     = "#bf616a"
theme.bg_minimize   = "#96b5b4"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#81a2be"
theme.fg_focus      = "#eff1f5"
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
theme.taglist_squares_sel   = "/home/rob/.config/awesome/themes/Onedark/taglist/linefw.png"
theme.taglist_squares_unsel = "/home/rob/.config/awesome/themes/Onedark/taglist/linew.png"

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
theme.titlebar_close_button_normal = "/home/rob/.config/awesome/themes/Onedark/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = "/home/rob/.config/awesome/themes/Onedark/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = "/home/rob/.config/awesome/themes/Onedark/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = "/home/rob/.config/awesome/themes/Onedark/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = "/home/rob/.config/awesome/themes/Onedark/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = "/home/rob/.config/awesome/themes/Onedark/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = "/home/rob/.config/awesome/themes/Onedark/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = "/home/rob/.config/awesome/themes/Onedark/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = "/home/rob/.config/awesome/themes/Onedark/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = "/home/rob/.config/awesome/themes/Onedark/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = "/home/rob/.config/awesome/themes/Onedark/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = "/home/rob/.config/awesome/themes/Onedark/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = "/home/rob/.config/awesome/themes/Onedark/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = "/home/rob/.config/awesome/themes/Onedark/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = "/home/rob/.config/awesome/themes/Onedark/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = "/home/rob/.config/awesome/themes/Onedark/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = "/home/rob/.config/awesome/themes/Onedark/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = "/home/rob/.config/awesome/themes/Onedark/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = "/home/rob/.config/awesome/themes/Onedark/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = "/home/rob/.config/awesome/themes/Onedark/titlebar/maximized_focus_active.png"

theme.wallpaper = "/home/rob/Dropbox/Photos/Backgrounds/tri-fadeno.jpg"

-- You can use your own layout icons like this:
theme.layout_fairh = "/home/rob/.config/awesome/themes/Onedark/layouts/fairhw.png"
theme.layout_fairv = "/home/rob/.config/awesome/themes/Onedark/layouts/fairvw.png"
theme.layout_floating  = "/home/rob/.config/awesome/themes/Onedark/layouts/floatingw.png"
theme.layout_tilebottom = "/home/rob/.config/awesome/themes/Onedark/layouts/tilebottomw.png"
theme.layout_tileleft   = "/home/rob/.config/awesome/themes/Onedark/layouts/tileleftw.png"
theme.layout_tile = "/home/rob/.config/awesome/themes/Onedark/layouts/tilew.png"
theme.layout_tiletop = "/home/rob/.config/awesome/themes/Onedark/layouts/tiletopw.png"
theme.layout_cornernw = "/home/rob/.config/awesome/themes/Onedark/layouts/cornernww.png"
theme.layout_cornerne = "/home/rob/.config/awesome/themes/Onedark/layouts/cornernew.png"
theme.layout_cornersw = "/home/rob/.config/awesome/themes/Onedark/layouts/cornersww.png"
theme.layout_cornerse = "/home/rob/.config/awesome/themes/Onedark/layouts/cornersew.png"

theme.awesome_icon = "/home/rob/.config/awesome/themes/Onedark/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
