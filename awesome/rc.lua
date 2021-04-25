--[[ Configuration for Awesome 4.3 modified by folraed
     Uses some custom icons, but otherwise sticks to
     using available and stock Awesome libraries and
     extensions. Initially created for Awesome 4.2 ]]

-- Get some default Awesome libraries
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local lain  = require("lain")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Setting a sane icon default for naughty.notify
naughty.config.defaults['icon_size'] = 32

-- {{{ Error handling

-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end

-- }}}

-- {{{ Set some Awesome defaults

-- Get the theme
local theme_path = string.format("%s/.config/awesome/themes/%s/theme.lua", os.getenv("HOME"), "Onedark")
beautiful.init(theme_path)


-- Default terminal and editor
terminal = "kitty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey
modkey = "Mod4"
altkey = "Mod1"

-- Table of layouts to cover with awful.layout.inc, order matters
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.floating,
}
-- }}}

-- {{{ Helper functions

local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end

-- }}}

-- {{{ Menu

-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })


-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- }}}

-- {{{ Wibar

-- Create a few widgets
markup  = lain.util.markup

spacer = wibox.widget.textbox(" · ")

mytextclock = wibox.widget.textclock(" %a %b %e %l:%M %P ")
local month_calendar = awful.widget.calendar_popup.month()
month_calendar:attach( mytextclock, "tr" )
local cputemp = lain.widget.temp({
    tempfile = "/sys/devices/virtual/thermal/thermal_zone2/temp",
    settings = function()
      widget:set_markup(markup("#98c379", " " .. coretemp_now .. "°C "))
    end
})
local mycpu = lain.widget.cpu({
    settings = function()
        widget:set_markup(markup("#e5c07b", " " .. cpu_now[1].usage .. "% " .. cpu_now[2].usage ..
                                 "% " .. cpu_now[3].usage .. "% " .. cpu_now[4].usage .. "% "))
    end
})
local mymem = lain.widget.mem({
    settings = function()
        widget:set_markup(markup("#61afef", " " .. mem_now.used .. " Mb " .. mem_now.perc .. "% "))
    end
})
local myvolume = lain.widget.pulse({
    settings = function()
        widget:set_markup(markup("#c678dd", " " .. volume_now.left .. "% "))
    end
})

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))


local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)

    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    layouts = awful.layout.layouts
    tags = {
        settings = {
            {names = {"WEB", "VID", "DIR", "TXT", "RSS"},
             layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
         },
            {names = {"GEN", "MAP", "SYS", "DOC", "MSC"},
             layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
     }}}
     tags[s] = awful.tag(tags.settings[s.index].names, s, tags.settings[s.index].layout)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
   s.mytasklist = awful.widget.tasklist {
       screen   = s,
       filter   = awful.widget.tasklist.filter.currenttags,
       buttons  = tasklist_buttons,
       style    = {
           shape_border_width = 1,
           shape_border_color = '#282C34',
           shape  = gears.shape.rounded_rect,
       },
       layout   = {
           spacing = 20,
           spacing_widget = {
               {
                   forced_width = 5,
                   shape        = gears.shape.circle,
                   widget       = wibox.widget.separator
               },
               valign = 'center',
               halign = 'center',
               widget = wibox.container.place,
           },
           layout  = wibox.layout.flex.horizontal
       },
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mylayoutbox,
            spacer,
            s.mytaglist,
            spacer,
            s.mypromptbox,
        },
        s.mytasklist, -- Puts blank space in middle when nothing open
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            spacer,
            mycpu,
            spacer,
            mymem,
            spacer,
            cputemp,
            spacer,
            myvolume,
            spacer,
            mytextclock,
            spacer,
            wibox.widget.systray()
        },
    }
end)

-- }}}

-- {{{ Mouse bindings

root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
    -- awful.button({ }, 4, awful.tag.viewnext),
    -- awful.button({ }, 5, awful.tag.viewprev)
))

-- }}}

-- {{{ Key bindings

globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              { description="show help", group="awesome" }),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              { description = "go back", group = "tag" }),

    awful.key({ modkey,           }, "Left",
        function ()
            awful.client.focus.bydirection("left")
        end,
        { description = "focus window on left", group = "client" }
    ),
    awful.key({ modkey,           }, "Up",
        function ()
            awful.client.focus.bydirection("up")
        end,
        { description = "focus window above", group = "client" }
    ),
    awful.key({ modkey,           }, "Down",
        function ()
            awful.client.focus.bydirection("down")
        end,
        { description = "focus window below", group = "client" }
    ),
    awful.key({ modkey,           }, "Right",
        function ()
            awful.client.focus.bydirection("right")
        end,
        { description = "focus window on right", group = "client" }
    ),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "Left", function () awful.client.swap.bydirection("left")    end,
              { description = "swap with left window", group = "client" }),
    awful.key({ modkey, "Shift"   }, "Up", function () awful.client.swap.bydirection("up")    end,
              { description = "swap with above window", group = "client" }),
    awful.key({ modkey, "Shift"   }, "Down", function () awful.client.swap.bydirection("down")    end,
              { description = "swap with below window", group = "client" }),
    awful.key({ modkey, "Shift"   }, "Right", function () awful.client.swap.bydirection("right")    end,
              { description = "swap with right window", group = "client" }),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              { description = "focus the next screen", group = "screen" }),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              { description = "focus the previous screen", group = "screen" }),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              { description = "jump to urgent client", group = "client" }),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        { description = "go back", group = "client" }),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              { description = "open a terminal", group = "launcher" }),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              { description = "reload awesome", group = "awesome" }),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              { description = "quit awesome", group = "awesome" }),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.01)          end,
              { description = "increase master width factor", group = "layout" }),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.01)          end,
              { description = "decrease master width factor", group = "layout" }),
    awful.key({ modkey,           }, "j",     function () awful.client.incwfact( 0.01)        end,
              { description = "increase tiled window height", group = "layout" }),
    awful.key({ modkey,           }, "k",     function () awful.client.incwfact(-0.01)        end,
              { description = "decrease tiled window height", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              { description = "increase the number of master clients", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              { description = "decrease the number of master clients", group = "layout" }),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              { description = "increase the number of columns", group = "layout" }),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              { description = "decrease the number of columns", group = "layout" }),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              { description = "select next", group = "layout" }),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              { description = "select previous", group = "layout" }),

    -- Volume control
    awful.key({ }, "XF86AudioRaiseVolume", function () awful.spawn("pactl set-sink-volume 1 +3%") end),
    awful.key({ }, "XF86AudioLowerVolume", function () awful.spawn("pactl set-sink-volume 1 -3%") end),
    awful.key({ }, "XF86AudioMute", function () awful.spawn("pactl set-sink-mute 1 toggle") end),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              { description = "restore minimized", group = "client" }),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              { description = "run prompt", group = "launcher" }),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              { description = "lua execute prompt", group = "awesome" }),
    -- Menubar
    awful.key({ modkey }, "d", function() menubar.show() end,
              { description = "show the menubar", group = "launcher" }),

    -- Rofi
    awful.key({ modkey }, "p", function () awful.spawn('rofi -show drun -sidebar-mode -font "Hack 10" -width "20" -opacity "90" -terminal "kitty"') end,
              { description = "Rofi launcher", group = "launcher" }),

    awful.key({ modkey }, "w", function () awful.spawn('rofi -show window -font "Hack 10" -width "20" -opacity "90"') end,
              { description = "Rofi show windows", group = "launcher" }),

    -- Flameshot
    awful.key({ modkey }, "c", function () awful.spawn('flameshot gui') end,
        { description = "Run Flameshot", group = "launcher" })
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        { description = "toggle fullscreen", group = "client" }),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
        { description = "close", group = "client" }),
    awful.key({ modkey }, "Next",  function (c) c:relative_move( 10,  10, -10, -10) end,
        { description = "decrease float size", group = "client" }),
    awful.key({ modkey }, "Prior", function (c) c:relative_move(-10, -10,  10,  10) end,
        { description = "increase float size", group = "client" }),
    awful.key({ modkey, "Control" }, "Down",  function (c) c:relative_move(  0,  10,   0,   0) end,
        { description = "move float down", group = "client" }),
    awful.key({ modkey, "Control" }, "Up",    function (c) c:relative_move(  0, -10,   0,   0) end,
        { description = "move float up", group = "client" }),
    awful.key({ modkey, "Control" }, "Left",  function (c) c:relative_move(-10,   0,   0,   0) end,
        { description = "move float left", group = "client" }),
    awful.key({ modkey, "Control" }, "Right", function (c) c:relative_move( 10,   0,   0,   0) end,
        { description = "move float right", group = "client" }),
    awful.key({ modkey, altkey }, "j", function (c) c:relative_move( 0,   0,   0,   5) end,
        { description = "Increase widow height", group = "client" }),
    awful.key({ modkey, altkey }, "k", function (c) c:relative_move( 0,   0,   0,   -3) end,
        { description = "Decrease widow height", group = "client" }),
    awful.key({ modkey, altkey }, "h", function (c) c:relative_move( 0,   0,   5,   0) end,
        { description = "Increase widow width", group = "client" }),
    awful.key({ modkey, altkey }, "l", function (c) c:relative_move( 0,   0,   5,   0) end,
        { description = "Decrease widow width", group = "client" }),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
        { description = "toggle floating", group = "client" }),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
        { description = "move to master", group = "client" }),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
        { description = "move to screen", group = "client" }),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
        { description = "toggle keep on top", group = "client" }),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        { description = "minimize", group = "client" }),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        { description = "maximize", group = "client" })
)

-- Bind all key numbers to tags
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  { description = "view tag #"..i, group = "tag" }),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  { description = "toggle tag #" .. i, group = "tag" }),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  { description = "move focused client to tag #"..i, group = "tag" }),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  { description = "toggle focused client on tag #" .. i, group = "tag" })
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)

-- }}}

-- {{{ Rules

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     size_hints_honor = false,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        class = {
          "Sxiv",
          "mpv",
          "Zathura",
          "vlc",
          "okular" },

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },
}

-- }}}

-- {{{ Signals

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", { raise = true })
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", { raise = true })
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.minimizebutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            -- awful.titlebar.widget.stickybutton   (c),
            -- awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- }}}

--- run_once
function run_once(cmd)
  findme = cmd
  firstspace = cmd:find(" ")
  if firstspace then
     findme = cmd:sub(0, firstspace-1)
  end
  awful.spawn.with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

-- Autostart
run_once("setxkbmap -option caps:escape")
run_once("gnome-keyring-daemon --start --components=secrets")
run_once("/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1")
run_once("start-pulseaudio-x11")
run_once("ssh-agent")
run_once("picom")
run_once("clipit")
run_once("xclip")
run_once("recollindex -m")
