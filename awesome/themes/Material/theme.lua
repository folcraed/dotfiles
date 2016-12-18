---------------------------
-- Material awesome theme --
---------------------------

local theme = {}

theme.font          = "Roboto 10, FontAwesome 10"
-- theme.icons         = "/home/rob/.config/awesome/themes/Material/icons/"

theme.bg_normal     = "#2a373e"
theme.bg_focus      = "#455a64"
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#aaaaaa"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.useless_gap   = 3
theme.border_width  = 1
theme.border_normal = "#000000"
theme.border_focus  = "#455a64"
theme.border_marked = "#91231c"

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
theme.taglist_squares_sel   = "/home/rob/.config/awesome/themes/Material/taglist/linefw.png"
theme.taglist_squares_unsel = "/home/rob/.config/awesome/themes/Material/taglist/linew.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = "/home/rob/.config/awesome/themes/Material/submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = "/home/rob/.config/awesome/themes/Material/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = "/home/rob/.config/awesome/themes/Material/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = "/home/rob/.config/awesome/themes/Material/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = "/home/rob/.config/awesome/themes/Material/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = "/home/rob/.config/awesome/themes/Material/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = "/home/rob/.config/awesome/themes/Material/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = "/home/rob/.config/awesome/themes/Material/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = "/home/rob/.config/awesome/themes/Material/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = "/home/rob/.config/awesome/themes/Material/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = "/home/rob/.config/awesome/themes/Material/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = "/home/rob/.config/awesome/themes/Material/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = "/home/rob/.config/awesome/themes/Material/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = "/home/rob/.config/awesome/themes/Material/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = "/home/rob/.config/awesome/themes/Material/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = "/home/rob/.config/awesome/themes/Material/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = "/home/rob/.config/awesome/themes/Material/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = "/home/rob/.config/awesome/themes/Material/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = "/home/rob/.config/awesome/themes/Material/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = "/home/rob/.config/awesome/themes/Material/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = "/home/rob/.config/awesome/themes/Material/titlebar/maximized_focus_active.png"

theme.wallpaper = "/home/rob/Dropbox/Photos/Backgrounds/Material_Design_1.jpg"

-- You can use your own layout icons like this:
theme.layout_fairh = "/home/rob/.config/awesome/themes/Material/layouts/fairhw.png"
theme.layout_fairv = "/home/rob/.config/awesome/themes/Material/layouts/fairvw.png"
theme.layout_floating  = "/home/rob/.config/awesome/themes/Material/layouts/floatingw.png"
theme.layout_magnifier = "/home/rob/.config/awesome/themes/Material/layouts/magnifierw.png"
theme.layout_max = "/home/rob/.config/awesome/themes/Material/layouts/maxw.png"
theme.layout_fullscreen = "/home/rob/.config/awesome/themes/Material/layouts/fullscreenw.png"
theme.layout_tilebottom = "/home/rob/.config/awesome/themes/Material/layouts/tilebottomw.png"
theme.layout_tileleft   = "/home/rob/.config/awesome/themes/Material/layouts/tileleftw.png"
theme.layout_tile = "/home/rob/.config/awesome/themes/Material/layouts/tilew.png"
theme.layout_tiletop = "/home/rob/.config/awesome/themes/Material/layouts/tiletopw.png"
theme.layout_spiral  = "/home/rob/.config/awesome/themes/Material/layouts/spiralw.png"
theme.layout_dwindle = "/home/rob/.config/awesome/themes/Material/layouts/dwindlew.png"
theme.layout_cornernw = "/home/rob/.config/awesome/themes/Material/layouts/cornernww.png"
theme.layout_cornerne = "/home/rob/.config/awesome/themes/Material/layouts/cornernew.png"
theme.layout_cornersw = "/home/rob/.config/awesome/themes/Material/layouts/cornersww.png"
theme.layout_cornerse = "/home/rob/.config/awesome/themes/Material/layouts/cornersew.png"

theme.awesome_icon = "/usr/share/awesome/icons/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80