-- Copied from ProMaster theme and modified for my needs
-- Most of the credit has to go to 'barwinco' for all his
-- work. http://github.com/barwinco/pro

local gears      = require("gears")
local awful      = require("awful")
awful.rules      = require("awful.rules")
                   require("awful.autofocus")
local wibox      = require("wibox")
local beautiful  = require("beautiful")
local vicious    = require("vicious")
local naughty    = require("naughty")
local lain       = require("lain")
local menubar    = require("menubar")
local menugen    = require("menugen")

-- | Theme | --

local theme = "pro-gotham"

beautiful.init(os.getenv("HOME") .. "/.config/awesome/themes/" .. theme .. "/theme.lua")
beautiful.useless_gap_width = 10

-- | Error handling | --

if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end

-- Disable cursor animation:

local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
    oldspawn(s, false)
end

local home   = os.getenv("HOME")
local exec   = function (s) oldspawn(s, false) end
local shexec = awful.util.spawn_with_shell

modkey        = "Mod4"
terminal      = "termite"
browser       = "vivaldi-stable"
filemanager   = "termite -e vifm"
configuration = 'termite -e "vim -O $HOME/.config/awesome/rc.lua $HOME/.config/awesome/themes/' ..theme.. '/theme.lua"'
app_menu = menugen.build_menu()

-- | Table of layouts I use| --

local layouts =
{
    awful.layout.suit.floating,
    lain.layout.uselesstile.right,
    lain.layout.uselesstile.bottom,
    lain.layout.uselesstile.left,
    lain.layout.uselesstile.top
}

-- | Wallpaper | --

if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s)
    end
end

-- | Tags | --

tags = {
    names = { "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  " },
    layouts = {layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1],
    layouts[1], layouts[1] } }

for s = 1, screen.count() do
    tags[s] = awful.tag(tags.names, s, tags.layouts)
end

-- | Menu | --

menu_main = {
  { "poweroff",  "sudo poweroff"     },
  { "restart",   awesome.restart     },
  { "reboot",    "sudo reboot"       },
  { "quit",      awesome.quit        }}

mainmenu = awful.menu({ items = {
  { " awesome",       menu_main   },
  { " file manager",  filemanager },
  { " user terminal", terminal    },
  { " applications", app_menu  }}})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
menubar.menu_gen.all_menu_dirs = { "/usr/share/applications/", "/usr/local/share/applications" }

-- | Markup | --

markup = lain.util.markup

space3 = markup.font("Terminus 3", " ")
space2 = markup.font("Terminus 2", " ")
vspace1 = '<span font="Terminus 3"> </span>'
vspace2 = '<span font="Terminus 3"> </span>'
clockgf = beautiful.clockgf

-- | Widgets | --

spr = wibox.widget.imagebox()
spr:set_image(beautiful.spr)
spr4px = wibox.widget.imagebox()
spr4px:set_image(beautiful.spr4px)
spr5px = wibox.widget.imagebox()
spr5px:set_image(beautiful.spr5px)

widget_display = wibox.widget.imagebox()
widget_display:set_image(beautiful.widget_display)
widget_display_r = wibox.widget.imagebox()
widget_display_r:set_image(beautiful.widget_display_r)
widget_display_l = wibox.widget.imagebox()
widget_display_l:set_image(beautiful.widget_display_l)
widget_display_c = wibox.widget.imagebox()
widget_display_c:set_image(beautiful.widget_display_c)

cpu_widget = lain.widgets.cpu({
    settings = function()
        widget:set_markup(space3 .. cpu_now.usage .. "%" .. markup.font("Hack 8", " "))
    end
})

widget_cpu = wibox.widget.imagebox()
widget_cpu:set_image(beautiful.widget_cpu)
cpuwidget = wibox.widget.background()
cpuwidget:set_widget(cpu_widget)
cpuwidget:set_bgimage(beautiful.widget_display)

-- | MEM | --

mem_widget = lain.widgets.mem({
    settings = function()
        widget:set_markup(space3 .. mem_now.used .. "MB" .. markup.font("Hack 8", " "))
    end
})

widget_mem = wibox.widget.imagebox()
widget_mem:set_image(beautiful.widget_mem)
memwidget = wibox.widget.background()
memwidget:set_widget(mem_widget)
memwidget:set_bgimage(beautiful.widget_display)

-- | FS | --

fs_widget = wibox.widget.textbox()
vicious.register(fs_widget, vicious.widgets.fs, vspace1 .. "${/ avail_gb}GB" .. vspace1, 2)

widget_fs = wibox.widget.imagebox()
widget_fs:set_image(beautiful.widget_fs_hdd)
fswidget = wibox.widget.background()
fswidget:set_widget(fs_widget)
fswidget:set_bgimage(beautiful.widget_display)

-- | NET | --

net_widgetdl = wibox.widget.textbox()
net_widgetul = lain.widgets.net({
    iface = "eno1",
    settings = function()
        widget:set_markup(markup.font("Hack 4", "  ") .. net_now.sent)
        net_widgetdl:set_markup(markup.font("Hack 4", " ") .. net_now.received .. markup.font("Hack 4", " "))
    end
})

widget_netdl = wibox.widget.imagebox()
widget_netdl:set_image(beautiful.widget_netdl)
netwidgetdl = wibox.widget.background()
netwidgetdl:set_widget(net_widgetdl)
netwidgetdl:set_bgimage(beautiful.widget_display)

widget_netul = wibox.widget.imagebox()
widget_netul:set_image(beautiful.widget_netul)
netwidgetul = wibox.widget.background()
netwidgetul:set_widget(net_widgetul)
netwidgetul:set_bgimage(beautiful.widget_display)

-- | Clock / Calendar | --

mytextclock    = awful.widget.textclock(markup(clockgf, space3 .. "%I:%M" .. markup.font("Hack 8", " ")))
-- mytextcalendar = awful.widget.textclock(markup(clockgf, space3 .. "%a %d %b"))
lain.widgets.calendar:attach(mytextclock, { font = "Hack", font_size = 9 })

widget_clock = wibox.widget.imagebox()
widget_clock:set_image(beautiful.widget_clock)

clockwidget = wibox.widget.background()
clockwidget:set_widget(mytextclock)
clockwidget:set_bgimage(beautiful.widget_display)

-- | Taglist | --

mytaglist         = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag)
                    )

-- | Tasklist | --

mytasklist         = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

-- | PANEL | --

mywibox           = {}
mypromptbox       = {}
mylayoutbox       = {}

for s = 1, screen.count() do

    mypromptbox[s] = awful.widget.prompt()

    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end)))

    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    mywibox[s] = awful.wibox({ position = "top", screen = s, height = "22" })

    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(spr5px)
    left_layout:add(mytaglist[s])
    left_layout:add(spr5px)

    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then
        right_layout:add(spr)
        right_layout:add(spr5px)
        right_layout:add(mypromptbox[s])
        right_layout:add(spr5px)
    end

    right_layout:add(spr)
    right_layout:add(widget_cpu)
    right_layout:add(widget_display_l)
    right_layout:add(cpuwidget)
    right_layout:add(widget_display_r)
    right_layout:add(spr5px)
    right_layout:add(spr)
    right_layout:add(widget_mem)
    right_layout:add(widget_display_l)
    right_layout:add(memwidget)
    right_layout:add(widget_display_r)
    right_layout:add(spr5px)
    right_layout:add(spr)
    right_layout:add(widget_fs)
    right_layout:add(widget_display_l)
    right_layout:add(fswidget)
    right_layout:add(widget_display_r)
    right_layout:add(spr5px)
    right_layout:add(spr)
    right_layout:add(widget_netdl)
    right_layout:add(widget_display_l)
    right_layout:add(netwidgetdl)
    right_layout:add(widget_display_c)
    right_layout:add(netwidgetul)
    right_layout:add(widget_display_r)
    right_layout:add(widget_netul)
    right_layout:add(spr)
    right_layout:add(widget_clock)
    right_layout:add(widget_display_l)
    right_layout:add(clockwidget)
    right_layout:add(widget_display_r)
    right_layout:add(spr5px)
    right_layout:add(wibox.widget.systray())
    right_layout:add(spr5px)
    right_layout:add(spr)
    right_layout:add(mylayoutbox[s])

    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_bg(beautiful.panel)

    mywibox[s]:set_widget(layout)
end

-- | Mouse bindings | --

root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mainmenu:toggle() end)
))

-- | Key bindings | --


globalkeys = awful.util.table.join(

    awful.key({ modkey,           }, "w",      function () mainmenu:show() end),
    awful.key({ modkey            }, "r",      function () mypromptbox[mouse.screen]:run() end),
    awful.key({ modkey,           }, "Right",
        function ()
            awful.client.focus.bydirection("right")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "Left",
        function ()
            awful.client.focus.bydirection("left")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "Up",
        function ()
            awful.client.focus.bydirection("up")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "Down",
        function ()
            awful.client.focus.bydirection("down")
            if client.focus then client.focus:raise() end
        end),

-- Window Swapping
    awful.key({ modkey, "Mod1"   }, "Right", function (c) awful.client.swap.bydirection("right") end),
    awful.key({ modkey, "Mod1"   }, "Left", function (c) awful.client.swap.bydirection("left") end),
    awful.key({ modkey, "Mod1"   }, "Up", function (c) awful.client.swap.bydirection("up") end),
    awful.key({ modkey, "Mod1"   }, "Down", function (c) awful.client.swap.bydirection("down") end),

-- Resize windows
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),

-- Increase/decrease master windows and columns
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),

-- Standard keys for Awesome
    awful.key({ modkey, "Control" }, "r",      awesome.restart),
    awful.key({ modkey, "Shift"   }, "q",      awesome.quit),
    awful.key({ modkey,           }, "Return", function () exec(terminal) end),
    awful.key({ modkey, "Control" }, "Return", function () exec(rootterm) end),
    awful.key({ modkey,           }, "p",      function () menubar.show() end),
    awful.key({ modkey,           }, "space",  function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space",  function () awful.layout.inc(layouts, -1) end),

    -- Volume control
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn("pactl set-sink-volume 1 +5%") end),
    awful.key({ }, "XF86AudioLowerVolume", function () awful.util.spawn("pactl set-sink-volume 1 -5%") end)
)

local wa = screen[mouse.screen].workarea
ww = wa.width
wh = wa.height
ph = 22 -- (panel height)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle),
    awful.key({ modkey            }, "Next",     function () awful.client.moveresize( 20,  20, -40, -40) end),
    awful.key({ modkey            }, "Prior",    function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ modkey, "Shift"   }, "Down",     function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Up",       function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Left",     function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ modkey, "Shift"   }, "Right",    function () awful.client.moveresize( 20,   0,   0,   0) end),
    awful.key({ modkey, "Control" }, "KP_Left",  function (c) c:geometry( { width = ww / 2, height = wh, x = 0, y = ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Right", function (c) c:geometry( { width = ww / 2, height = wh, x = ww / 2, y = ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Up",    function (c) c:geometry( { width = ww, height = wh / 2, x = 0, y = ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Down",  function (c) c:geometry( { width = ww, height = wh / 2, x = 0, y = wh / 2 + ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Prior", function (c) c:geometry( { width = ww / 2, height = wh / 2, x = ww / 2, y = ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Next",  function (c) c:geometry( { width = ww / 2, height = wh / 2, x = ww / 2, y = wh / 2 + ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Home",  function (c) c:geometry( { width = ww / 2, height = wh / 2, x = 0, y = ph } ) end),
    awful.key({ modkey, "Control" }, "KP_End",   function (c) c:geometry( { width = ww / 2, height = wh / 2, x = 0, y = wh / 2 + ph } ) end),
    awful.key({ modkey, "Control" }, "KP_Begin", function (c) c:geometry( { width = ww, height = wh, x = 0, y = ph } ) end),
    awful.key({ modkey,           }, "f",        function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey,           }, "c",        function (c) c:kill() end),
    awful.key({ modkey,           }, "n",
        function (c)
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

awful.menu.menu_keys = {
    up    = { "k", "Up" },
    down  = { "j", "Down" },
    exec  = { "l", "Return", "Space" },
    enter = { "l", "Right" },
    back  = { "h", "Left" },
    close = { "q", "Escape" }
}

root.keys(globalkeys)

-- | Rules | --

awful.rules.rules = {
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     -- size_hints_honor = false,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "tracker-needle" },
      properties = { floating = true } },
    { rule = { class = "vlc" },
      properties = { floating = true } },
    { rule = { class = "gcolor2" },
      properties = { floating = true } },
    { rule = { class = "xmag" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    { rule = { class = "mpv" },
      properties = { floating = true } },
}

-- | Signals | --

client.connect_signal("manage", function (c, startup)
    c.size_hints_honor=false
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
    awful.client.setslave(c)
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        -- right_layout:add(awful.titlebar.widget.stickybutton(c))
        -- right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- | run_once | --

function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

-- | Autostart | --

run_once("gnome-keyring-daemon -s")
run_once("/usr/lib64/polkit-gnome/polkit-gnome-authentication-agent-1")
run_once("compton")
run_once("parcellite")
run_once("xclip")
run_once("recollindex -m -n -w 30")
run_once("dropbox start -i")