-- Theme done mainly by barwinco, modified to fit my needs and taste,
-- with appreciation for all his hard work. - Rob

                -- [    Pro Gotham theme for Awesome 3.5.5    ] --
                -- [              author: barwinco            ] --
                -- [      http://github.com/barwinco/pro      ] --

-- patch for taglist: https://github.com/awesomeWM/awesome/pull/39

theme            = {}
theme.icons      = os.getenv("HOME") .. "/.config/awesome/themes/pro-gotham/icons/"
theme.wallpaper  = os.getenv("HOME") .. "/Dropbox/Photos/Backgrounds/NightSea.jpg"
theme.panel      = "png:" .. theme.icons .. "/panel/panel.png"
theme.font       = "Hack 9"

theme.fg_normal  = "#74AEAB"
theme.fg_focus   = "#DBDBDB"
theme.fg_urgent  = "#84BFBC"

theme.bg_normal  = "#383C4A"
theme.bg_focus   = "#2F343F"
theme.bg_urgent  = "#3F3F3F"
theme.bg_systray = "#0D1217"

theme.clockgf    = "#99D1CE"

-- | Borders | --

theme.border_width  = 1
theme.border_normal = "#383C4A"
theme.border_focus  = "#2F343F"
theme.border_marked = "#000000"

-- | Menu | --

theme.menu_height = 16
theme.menu_width  = 160

-- | Titlebar Buttons | --

theme.titlebar_close_button_normal = theme.icons .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = theme.icons .. "/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = theme.icons .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = theme.icons .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = theme.icons .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = theme.icons .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = theme.icons .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = theme.icons .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = theme.icons .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = theme.icons .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = theme.icons .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = theme.icons .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = theme.icons .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = theme.icons .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = theme.icons .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.icons .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = theme.icons .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = theme.icons .. "/titlebar/maximized_focus_active.png"

-- | Layout | --

theme.layout_floating             = theme.icons .. "/panel/layouts/floating.png"
theme.layout_uselesstile          = theme.icons .. "/panel/layouts/tile.png"
theme.layout_uselesstilebottom    = theme.icons .. "/panel/layouts/tilebottom.png"
theme.layout_uselesstileleft      = theme.icons .. "/panel/layouts/tileleft.png"
theme.layout_uselesstiletop       = theme.icons .. "/panel/layouts/tiletop.png"
-- theme.layout_fairv                = theme.icons .. "/panel/layouts/fairv.png"
-- theme.layout_fairh                = theme.icons .. "panel/layouts/fairh.png"

-- | Taglist | --

theme.taglist_bg_empty    = "png:" .. theme.icons .. "/panel/taglist/empty.png"
theme.taglist_bg_occupied = "png:" .. theme.icons .. "/panel/taglist/occupied.png"
theme.taglist_bg_urgent   = "png:" .. theme.icons .. "/panel/taglist/urgent.png"
theme.taglist_bg_focus    = "png:" .. theme.icons .. "/panel/taglist/focus.png"
theme.taglist_font        = "Terminus 11"

-- | Tasklist | --

theme.tasklist_font                 = "Hack 9"
theme.tasklist_disable_icon         = true
theme.tasklist_bg_normal            = "png:" .. theme.icons .. "panel/tasklist/normal.png"
theme.tasklist_bg_focus             = "png:" .. theme.icons .. "panel/tasklist/focus.png"
theme.tasklist_bg_urgent            = "png:" .. theme.icons .. "panel/tasklist/urgent.png"
theme.tasklist_fg_focus             = "#DBDBDB"
theme.tasklist_fg_urgent            = "#AA281F"
theme.tasklist_fg_normal            = "#74AEAB"
theme.tasklist_floating             = ""
theme.tasklist_sticky               = ""
theme.tasklist_ontop                = ""
theme.tasklist_maximized_horizontal = ""
theme.tasklist_maximized_vertical   = ""

-- | Widget | --

theme.widget_display   = theme.icons .. "/panel/widgets/display/widget_display.png"
theme.widget_display_r = theme.icons .. "/panel/widgets/display/widget_display_r.png"
theme.widget_display_c = theme.icons .. "/panel/widgets/display/widget_display_c.png"
theme.widget_display_l = theme.icons .. "/panel/widgets/display/widget_display_l.png"

-- | MPD | --

theme.mpd_prev  = theme.icons .. "/panel/widgets/mpd/mpd_prev.png"
theme.mpd_nex   = theme.icons .. "/panel/widgets/mpd/mpd_next.png"
theme.mpd_stop  = theme.icons .. "/panel/widgets/mpd/mpd_stop.png"
theme.mpd_pause = theme.icons .. "/panel/widgets/mpd/mpd_pause.png"
theme.mpd_play  = theme.icons .. "/panel/widgets/mpd/mpd_play.png"
theme.mpd_sepr  = theme.icons .. "/panel/widgets/mpd/mpd_sepr.png"
theme.mpd_sepl  = theme.icons .. "/panel/widgets/mpd/mpd_sepl.png"

-- | Separators | --

theme.spr    = theme.icons .. "/panel/separators/spr.png"
theme.sprtr  = theme.icons .. "/panel/separators/sprtr.png"
theme.spr4px = theme.icons .. "/panel/separators/spr4px.png"
theme.spr5px = theme.icons .. "/panel/separators/spr5px.png"

-- | Clock / Calendar | --

theme.widget_clock = theme.icons .. "/panel/widgets/widget_clock.png"
theme.widget_cal   = theme.icons .. "/panel/widgets/widget_cal.png"

-- | CPU / TMP | --

theme.widget_cpu    = theme.icons .. "/panel/widgets/widget_cpu.png"
-- theme.widget_tmp = theme.icons .. "/panel/widgets/widget_tmp.png"

-- | MEM | --

theme.widget_mem = theme.icons .. "/panel/widgets/widget_mem.png"

-- | FS | --

theme.widget_fs     = theme.icons .. "/panel/widgets/widget_fs.png"
theme.widget_fs_hdd = theme.icons .. "/panel/widgets/widget_fs_hdd.png"

-- | Mail | --

theme.widget_mail = theme.icons .. "/panel/widgets/widget_mail.png"

-- | NET | --

theme.widget_netdl = theme.icons .. "/panel/widgets/widget_netdl.png"
theme.widget_netul = theme.icons .. "/panel/widgets/widget_netul.png"

-- | Misc | --

theme.menu_submenu_icon = theme.icons .. "submenu.png"

return theme

