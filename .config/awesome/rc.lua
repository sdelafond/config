-- builtin
require("awful")
require("awful.autofocus")
require("awful.rules")
require("beautiful") -- theme
require("naughty") -- notifications
require("vicious") -- widgets

function my_debug(msg)
  if settings.debug_on then
    io.stderr:write(msg .. "\n")
    io.stderr:flush()
  end
end

---- Beautiful theme
beautiful.init(os.getenv("HOME") .. "/.config/awesome/seb.theme")
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")

---- Settings, in their own namespace
settings = {}

-- misc
settings.debug_on = true
settings.focus_dialogs = true
settings.master_width_factor = 0.5
settings.master_columns = 1
settings.master_windows = 1
settings.size_hints_honor = false
settings.no_overlap = true
settings.no_offscreen = true
settings.new_become_master = true

-- Keys
settings.keys = { none = {}, super = {"Mod4"}, alt = {"Mod1"},
		  shift = {"Shift"}, control = {"Control"} }
settings.keys.super_alt = {settings.keys.super[1], settings.keys.alt[1] }
settings.keys.super_alt_shift = {settings.keys.super[1], settings.keys.alt[1], settings.keys.shift[1] }
settings.keys.super_shift = {settings.keys.super[1], settings.keys.shift[1] }
settings.keys.super_control = {settings.keys.super[1], settings.keys.control[1] }
settings.keys.super_alt_control = {settings.keys.super[1], settings.keys.alt[1], settings.keys.control[1] }

-- Applications
settings.applications = { ["terminal"]        = 'xterm-screen',
			  ["lock_screen"]     = 'xscreensaver-command -lock',
			  ["screen_off"]      = 'sh -c "sleep 1 ; xset dpms force off"',
			  ["selection"]       = os.getenv("HOME") .. '/bin/selection.rb',
			  ["keyboard_layout"] = os.getenv("HOME") .. '/bin/keyboard-layout.sh',
			  ["volume"]          = os.getenv("HOME") .. '/bin/volume.sh',
                          ["killgkrellm"]     = 'pkill gkrellm',
                          ["mpc_pause"]       = 'mpc toggle' }

-- do that right away
awful.util.spawn(settings.applications.keyboard_layout)

-- Layouts
settings.layouts = { awful.layout.suit.tile,
		     awful.layout.suit.fair,
		     awful.layout.suit.tile.left,
		     awful.layout.suit.tile.bottom,
		     awful.layout.suit.tile.top,
		     awful.layout.suit.fair.horizontal,
		     awful.layout.suit.max,
		     awful.layout.suit.max.fullscreen,
		     awful.layout.suit.magnifier,
		     awful.layout.suit.floating }

-- Tags
settings.tags_defs = { { shortcut = "1", layout = awful.layout.suit.tile },
		       {  shortcut = "2", layout = awful.layout.suit.tile }, 
		       {  shortcut = "3", layout = awful.layout.suit.tile, name = "UT Dev" },
		       {  shortcut = "4", layout = awful.layout.suit.tile, name = "Home" },
		       {  shortcut = "5", layout = awful.layout.suit.max, name = "UT Desktop" },
		       {  shortcut = "6", layout = awful.layout.suit.tile, name = "Misc" }, -- nmaster = 2
		       {  shortcut = "7", layout = awful.layout.suit.tile, name = "Mappy" },
		       {  shortcut = "8", layout = awful.layout.suit.tile, name = "Text" },
		       {  shortcut = "9", layout = awful.layout.suit.tile, name = "VMs", mwfact = 0.2 },
		       {  shortcut = "F1", layout = awful.layout.suit.floating, name = "Pics/Video" }, -- mwfact = 0.2 },
		       {  shortcut = "F2", layout = awful.layout.suit.max, name = "Sound" },
		       {  shortcut = "F3", layout = awful.layout.suit.max, name = "Media" },
		       {  shortcut = "F4", layout = awful.layout.suit.floating, name = "Comm" },
		       {  shortcut = "F5", layout = awful.layout.suit.tile, name = "Gimp", mwfact = 0.2 },
		       {  shortcut = "F6", layout = awful.layout.suit.tile, name = "P2P" }, }

settings.tags_names = {}
for i, tag_def in ipairs(settings.tags_defs) do
  tag_shortcut = tag_def.shortcut
  if tag_def.name then
    tag_name = string.format("%s(%s)", tag_shortcut, tag_def.name)
  else
    tag_name = tag_shortcut
  end
  table.insert(settings.tags_names, tag_name)
end

settings.tags = {}
settings.last_selected_client_per_tag = {}
for s = 1, screen.count() do
  -- Each screen has its own tag table.
  settings.tags[s] = awful.tag(settings.tags_names, s)  
  settings.last_selected_client_per_tag[s] = {}
  for k, my_tag in ipairs(settings.tags[s]) do
    tag_def = settings.tags_defs[k]
    tag_shortcut = tag_def.shortcut
    if tag_def.name then
      tag_name = string.format("%s(%s)", tag_shortcut, tag_def.name)
    else
      tag_name = tag_shortcut
    end
    layout = tag_def.layout or settings.layouts[1] -- if no layout, 1st defined
    mwfact = tag_def.mwfact or settings.master_width_factor -- if no mvfact, use defaut
    nmaster = tag_def.nmaster or settings.master_windows -- if no nmaster, use defaut
    ncol = tag_def.ncol or settings.master_columns -- if no ncol, use defaut
    awful.layout.set(layout, my_tag)
    awful.tag.setmwfact(mwfact, my_tag)
    awful.tag.setnmaster(nmaster, my_tag)
    awful.tag.setncol(ncol, my_tag)
    settings.last_selected_client_per_tag[s][k] = nil
  end
  settings.tags[s][1].selected = true
end

function getTagByShortcut(shortcut)
  for k, my_tag_def in ipairs(settings.tags_defs) do
    if my_tag_def.shortcut == shortcut then
      return settings.tags[mouse.screen][k]
    end
  end

  for k, my_tag in ipairs(settings.tags[mouse.screen]) do
    if my_tag.shortcut == shortcut then
      return my_tag
    end
  end
  return nil    
end

-- Applications rules
awful.rules.rules = {
    -- All clients will match this rule.
--     { rule = { },
--       properties = { border_width = beautiful.border_width,
--                      border_color = beautiful.border_normal,
--                      focus = true,
--                      keys = clientkeys,
--                      buttons = clientbuttons } },
  { rule = { class = "gkrellm", name = "conf" }, properties = { floating = true } },
  { rule = { class = "nvidia" }, properties = { floating = true } },
  { rule = { class = "pinentry" }, properties = { floating = true } },
  { rule = { class = "frame", name = "preferences" }, properties = { floating = true } },

  -- default for terminals
  { rule = { class = "URxvt" }, properties = { tag = getTagByShortcut("6") } },

  -- local terminal
  { rule = { class = "URxvt", name = "hippie" }, properties = { tag = getTagByShortcut("1") } },
  { rule = { class = "URxvt", name = "hp" }, properties = { tag = getTagByShortcut("1") } },
  { rule = { class = "URxvt", name = "centurion" }, properties = { tag = getTagByShortcut("1") } },
  { rule = { class = "URxvt", name = "home" }, properties = { tag = getTagByShortcut("1") } },
  { rule = { class = "URxvt", name = "seb-debian" }, properties = { tag = getTagByShortcut("1") } },

  -- web
  { rule = { class = "Iceweasel" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Firefox" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Opera" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Konqueror" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Chromium" }, properties = { tag = getTagByShortcut("2") } },

  -- workstation at UT
  { rule = { class = "URxvt", name = "host52.untangle.com" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { class = "URxvt", name = "sid" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { class = "URxvt", name = "lemmiwinks" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { name = "VNC:" }, properties = { tag = getTagByShortcut("5") } },

  -- UT dev
  { rule = { class = "URxvt", name = "marvin" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { class = "URxvt", name = "untangle" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { class = "URxvt", name = "lemmiwinks" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { class = "URxvt", name = "xen" }, properties = { tag = getTagByShortcut("3") } },

  -- M
  { rule = { class = "URxvt", name = "10.0.1.180" }, properties = { tag = getTagByShortcut("7") } },
  { rule = { class = "URxvt", name = "cergy" }, properties = { tag = getTagByShortcut("7") } },

  -- Home
  { rule = { class = "URxvt", name = "weshyo" }, properties = { tag = getTagByShortcut("4") }, 
    callback = { function(c) c:swap(awful.client.getmaster()) end } },
  { rule = { class = "URxvt", name = "frisco" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "URxvt", name = "california" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "URxvt", name = "puff" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "URxvt", name = "beastie" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "URxvt", name = "t400" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "URxvt", name = "hippie" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "Vncviewer", name = "hp" }, properties = { tag = getTagByShortcut("F3") } },
  { rule = { class = "Vncviewer", name = "centurion" }, properties = { tag = getTagByShortcut("F3") } },
  { rule = { class = "Vncviewer", name = "hippie" }, properties = { tag = getTagByShortcut("4") } },

  -- Debian
  { rule = { class = "URxvt", name = "lenny" }, properties = { tag = getTagByShortcut("7") } },
  { rule = { class = "URxvt", name = "ud-bo-frontal" }, properties = { tag = getTagByShortcut("7") } },

  -- Text
  { rule = { class = "Evince" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { class = "Epdfview" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { name = "office" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { class = "OpenOffice.org" }, properties = { tag = getTagByShortcut("8") } },

  -- Graphic & Video
  { rule = { name = "qiv" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "feh" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "Geeqie" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "Geeqie", name = "Move - Geeqie" }, properties = { tag = getTagByShortcut("F1"), floating = true, focus = true } },
  { rule = { class = "Vlc" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true, floating = true } },
  { rule = { class = "Guvcview" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "guvcview" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "Xitk" }, properties = { tag = getTagByShortcut("F1"), switchtotag = true } },
  { rule = { class = "MPlayer" }, properties = { tag = getTagByShortcut("F1"), floating = true, switchtotag = true } },

  -- Sound
  { rule = { class = "esperanza" }, properties = { tag = getTagByShortcut("F2") } },
  { rule = { class = "Amarok" }, properties = { tag = getTagByShortcut("F2") } },
  { rule = { class = "banshee-1" }, properties = { tag = getTagByShortcut("F2") } },
  { rule = { class = "Rhythmbox" }, properties = { tag = getTagByShortcut("F2") } },
  { rule = { class = "Exaile" }, properties = { tag = getTagByShortcut("F2") } },
  { rule = { class = "Gmpc" }, properties = { tag = getTagByShortcut("F2") } },

  -- Communication
  { rule = { class = "Skype" }, properties = { tag = getTagByShortcut("F4"), floating = true } },
  { rule = { class = "Skype", name = "Call with untanglecavepc" }, 
    callback = function(c) 
                 c.sticky = true
                 -- c.ontop = true
                 c.above = true
               end },
  { rule = { class = "Ekiga" }, properties = { tag = getTagByShortcut("F4"), floating = true } },
  { rule = { class = "Kphone" }, properties = { tag = getTagByShortcut("F4"), floating = true } },
  { rule = { class = "Pidgin" }, properties = { tag = getTagByShortcut("F4"), floating = true } },

  -- Gimp
  { rule = { class = "Gimp" }, properties = { tag = getTagByShortcut("F5") }, 
    callback = function(c) awful.client.setslave(c) end },
  { rule = { class = "Gimp", name = "Toolbox" },
    callback = function(c) c:swap(awful.client.getmaster()) end },
  { rule = { class = "Dia" }, properties = { tag = getTagByShortcut("F5") },
    callback = function(c) awful.client.setslave(c) end },
  { rule = { class = "Dia", name = "Dia v" },
    callback = function(c) c:swap(awful.client.getmaster()) end },

  -- VMs
  { rule = { class = "VirtualBox" }, properties = { tag = getTagByShortcut("9") }, 
    callback = function(c) awful.client.setslave(c) end },
--   { rule = { class = "VirtualBox", name = "Sun VirtualBox" },
--     callback = function(c) c:swap(awful.client.getmaster()) end },

  -- Earth & P2P
  { rule = { class = "earth" }, properties = { tag = getTagByShortcut("F6") } },
  { rule = { class = "mule" }, properties = { tag = getTagByShortcut("F6") } },
}

---- Keybindings
---- attributes are in the order they'll be declared below
---- *_digits are special, they get expanded later on
settings.bindings = { ["command"] = {},
		      ["client"]  = {},
		      ["global"]  = {},
		      ["prompt"]  = {},
		      ["mouse"]   = {},
		      ["client_digits"]  = {},
		      ["root_digits"]  = {} }


settings.bindings.client = {
  [{settings.keys.super, "0"}] = function(c) c.sticky = not c.sticky ; c.above = not c.above end,
  [{settings.keys.super, "f"}] = function(c) my_debug("toggling float for " .. c.name) ; awful.client.floating.toggle(c) ; end,
 [{settings.keys.super, "k"}] = function(c) c:kill() end,
 [{settings.keys.super, "m"}] = function(c)
				 c.maximized_horizontal = not c.maximized_horizontal
				 c.maximized_vertical = not c.maximized_vertical 
			       end,
 [{settings.keys.super, "z"}] = awful.client.movetoscreen,
 [{settings.keys.super, ";"}] = function(c) c:swap(awful.client.getmaster()) end,
}

settings.bindings.command = {
  [{settings.keys.none, "XF86AudioRaiseVolume"}] = settings.applications.volume .. " up",
  [{settings.keys.none, "XF86AudioLowerVolume"}] = settings.applications.volume .. " down",
  [{settings.keys.none, "XF86AudioMute"}] = settings.applications.volume .. " mute",
  [{settings.keys.super, "s"}] = settings.applications.terminal,
  [{settings.keys.super, "Space"}] = settings.applications.mpc_pause,
  [{settings.keys.super, "l"}] = settings.applications.lock_screen,
  [{settings.keys.super, "o"}] = settings.applications.screen_off,
  [{settings.keys.none, "F1"}] = settings.applications.selection,
  [{settings.keys.super, "F1"}] = settings.applications.selection .. " gs",
  [{settings.keys.super_shift, "F1"}] = settings.applications.selection .. " gi",
  [{settings.keys.super_control, "F1"}] = settings.applications.selection .. " gm",
  [{settings.keys.none, "F2"}] = settings.applications.keyboard_layout
}

function switch_screen(b)
  awful.util.spawn(settings.applications.killgkrellm)
  for i,c in pairs(client.get()) do
    if not c.class == 'Gkrellm' then
      if b then
        awful.client.movetoscreen(c, mouse.screen)
      else
        awful.client.movetoscreen(c)	 
      end
--    rules.apply(c)
    end
  end
  if not b then 
    awful.screen.focus_relative(1)
  end
  awesome.restart()
  for s = 1, screen.count() do
    make_wibox(s, mywibox, mytaglist, promptbox, mylayoutbox, mybatwidget, datewidget, mytasklist, mouse.screen)
  end
end

function focus_next_client(j)
  i = 0
  while true do
    i = i + 1
    awful.client.focus.byidx(j)
    if client.focus and not client.focus.skip_taskbar then
      client.focus:raise()
      break
    end
    if i > 10 then
      break
    end
  end
end

settings.bindings.global = {
  [{settings.keys.super_alt, "r"}] = awesome.restart,

  [{settings.keys.super, "p"}] = function() focus_next_client(-1) end,
  [{settings.keys.super, "n"}] = function() focus_next_client(1) end,

  [{settings.keys.super, "Tab"}] = function() awful.client.focus.history.previous() ; if client.focus then client.focus:raise() end end,
  [{settings.keys.super, "u"}] = awful.client.urgent.jumpto,
  
  [{settings.keys.super, "Left"}] = awful.tag.viewprev,
  [{settings.keys.super, "Right"}] = awful.tag.viewnext,
  
--   [{settings.keys.super, "f"}] = function() awful.screen.focus_relative(1) end,
--   [{settings.keys.super, "b"}] = function() awful.screen.focus_relative(-1) end,

  [{settings.keys.super_alt, "z"}] = function() switch_screen(false) end,
  [{settings.keys.super_alt_shift, "z"}] = function() switch_screen(true) end,
  
  [{settings.keys.super_alt, "j"}] = function() awful.tag.incmwfact(-0.05) end,
  [{settings.keys.super_alt, "k"}] = function() awful.tag.incmwfact(0.05) end,
  
  [{settings.keys.super_alt, "i"}] = function() awful.tag.incncol(1) end,
  [{settings.keys.super_alt, "o"}] = function() awful.tag.incncol(-1) end,
  
  [{settings.keys.super_alt, "h"}] = function() awful.tag.incnmaster(1) end,
  [{settings.keys.super_alt, "l"}] = function() awful.tag.incnmaster(-1) end,
  
  [{settings.keys.super_alt, "n"}] = function() awful.layout.inc(settings.layouts, 1) end,
  [{settings.keys.super_alt, "p"}] = function() awful.layout.inc(settings.layouts, -1) end,

  [{settings.keys.super_alt, "f"}] = function() awful.layout.set(awful.layout.suit.floating) end,
  [{settings.keys.super_alt, "m"}] = function() awful.layout.set(awful.layout.suit.max) end,
  [{settings.keys.super_alt, "t"}] = function() awful.layout.set(awful.layout.suit.tile.left) end,

  [{settings.keys.control, "0"}] = function()
                                     awful.tag.viewmore(settings.tags[mouse.screen])
                                   end
}

settings.bindings.prompt = {
  [{settings.keys.super_control, "r"}] = function() mypromptbox[mouse.screen]:run() end,

  [{settings.keys.super_control, "l"}] = function()
					  awful.prompt.run({prompt = "Run Lua code: "},
							   mypromptbox[mouse.screen].widget,
							   awful.util.eval, nil,
							   awful.util.getdir("cache") .. "/history_eval")
					end,

  [{settings.keys.super_control, "i"}] = function()
					  local s = mouse.screen
					  if mypromptbox[s].text then
					    mypromptbox[s].text = nil
					  else
					    mypromptbox[s].text = nil
					    if client.focus.class then
					      mypromptbox[s].text = "Class: " .. client.focus.class .. " "
					    end
					    if client.focus.name then
					      mypromptbox[s].text = mypromptbox[s].text .. "Name: ".. client.focus.name .. " "
					    end
					    if client.focus.role then
					      mypromptbox[s].text = mypromptbox[s].text .. "Role: ".. client.focus.role
					    end
					  end
					end
}

settings.bindings.root_mouse = {
  [{settings.keys.none, 2}] = settings.applications.volume .. " mute",
  [{settings.keys.none, 4}] = settings.applications.volume .. " up",
  [{settings.keys.none, 5}] = settings.applications.volume .. " down"
}

settings.bindings.mouse = {
  [{settings.keys.super, 1}] = awful.mouse.client.move,
  [{settings.keys.super, 2}] = function(c) c:kill() end,
  [{settings.keys.super, 3}] = awful.mouse.client.resize
}

settings.bindings.client_digits = {
  -- Mod+Shift+##: Move window to tag
  [settings.keys.super_shift] = function(my_tag)
				 return function(c)
					  awful.client.movetotag(my_tag, c)
					end
			       end,

  -- Mod+Control+##: Toggle window on tag
  [settings.keys.super_control] = function(my_tag)
				   return function(c)
					    awful.client.toggletag(my_tag, c)
					  end
				 end
}

settings.bindings.root_digits = {
  [settings.keys.control] = function(my_tag)
			     return function()
				      awful.tag.viewonly(my_tag)
				    end
			   end,

  [settings.keys.super_alt_control] = function(my_tag)
				       return function()
						my_tag.selected = not my_tag.selected
					      end
				     end
}

-- Create a wibox for each screen and add it
mywibox = {}
mytextbox = {}
mylayoutbox = {}
mybatwidget = {}
mycpuwidget = {}

promptbox = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })

mylayoutbox.buttons = awful.util.table.join(awful.button(settings.keys.none, 1, function() awful.layout.inc(settings.layouts, 1) end),
			   awful.button(settings.keys.none, 3, function() awful.layout.inc(settings.layouts, -1) end),
			   awful.button(settings.keys.none, 4, function() awful.layout.inc(settings.layouts, 1) end),
			   awful.button(settings.keys.none, 5, function() awful.layout.inc(settings.layouts, -1) end))
mytaglist = {}
mytaglist.buttons = awful.util.table.join(awful.button(settings.keys.none, 1, awful.tag.viewonly),
					  awful.button(settings.keys.super, 1, awful.client.movetotag),
					  awful.button(settings.keys.none, 3, function(tag) tag.selected = not tag.selected end),
					  awful.button(settings.keys.super, 3, awful.client.toggletag),
					  awful.button(settings.keys.none, 4, awful.tag.viewnext),
					  awful.button(settings.keys.none, 5, awful.tag.viewprev))
mytasklist = {}
mytasklist.buttons = awful.util.table.join(awful.button(settings.keys.none, 1, function(c) client.focus = c; c:raise() end),
					   awful.button(settings.keys.none, 3, function() awful.menu.clients({ width=250 }) end),
					   awful.button(settings.keys.none, 4, function() awful.client.focus.byidx(1) ; client.focus:raise() end),
					   awful.button(settings.keys.none, 5, function() awful.client.focus.byidx(-1) ; client.focus:raise() end))

datewidget = widget({ type = 'textbox' })
--datewidget:set_background_color('#0000AA')
--datewidget:set_border_color('#000000')
--datewidget:set_color('#AECF96')
vicious.register(datewidget, vicious.widgets.date, '<span weight="bold" color="white">%b %d, %R </span>')

function make_wibox(s, mywibox, mytaglist, promptbox, mylayoutbox, mybatwidget, datewidget, mytasklist, systray_screen)
  wibox = awful.wibox({ position = "top", screen = s, fg = beautiful.fg_normal, bg = beautiful.bg_normal })
  wibox.widgets = { { datewidget,
                      mytaglist[s],
                      promptbox,
                      mylayoutbox[s],
                      layout = awful.widget.layout.horizontal.leftright
                    },
                    s == systray_screen and widget({ type = "systray" }) or nil,
                    mybatwidget[s],
                    --                         mycpuwidget[s],
                    mytasklist[s],
                    layout = awful.widget.layout.horizontal.rightleft
                  }
  mywibox[s] = wibox
end

for s = 1, screen.count() do
  mylayoutbox[s] = awful.widget.layoutbox(s)
  mylayoutbox[s]:buttons(mylayoutbox.buttons)

  mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.noempty, mytaglist.buttons)

  mytasklist[s] = awful.widget.tasklist(function(c)
					  return awful.widget.tasklist.label.currenttags(c, s)
					end, mytasklist.buttons)

  mybatwidget[s] = awful.widget.progressbar()
  mybatwidget[s]:set_width(18)
  --   mybatwidget[s]:set_height(10)
  mybatwidget[s]:set_vertical(true)
  mybatwidget[s]:set_background_color('#111111')
  --  mybatwidget[s]:set_border_color('#000000')
  mybatwidget[s]:set_color('#AECF96')
  mybatwidget[s]:set_gradient_colors({ '#FF5656', '#AECF96', '#88A175' })
  vicious.register(mybatwidget[s], vicious.widgets.bat, '$2', 61, 'BAT0')

--   mycpuwidget[s] = awful.widget.graph()
--   mycpuwidget[s]:set_width(50)
--   mycpuwidget[s]:set_background_color('#494B4F')
--   mycpuwidget[s]:set_color('#FF5656')
--   mycpuwidget[s]:set_gradient_colors({ '#FF5656', '#88A175', '#AECF96' })
--   vicious.register(mycpuwidget, vicious.widgets.cpu, '$1', 3)

  -- the wibox itself
  make_wibox(s, mywibox, mytaglist, promptbox, mylayoutbox, mybatwidget, datewidget, mytasklist, 1)
end

-------------------------------------------------------
-- You shouldn't have to edit the code after this, 
-- it takes care of applying the settings above.
-------------------------------------------------------

---- Create bindings
--- This reads the binding tables and turns them into actual keybindings

globalbuttons = {}
globalkeys = {}
clientkeys = {}
clientbuttons = {}

for binding, command in pairs(settings.bindings.root_mouse) do
  globalbuttons = awful.util.table.join(globalbuttons, awful.button(binding[1], binding[2],
								    function() 
                                                                      my_debug(command)
                                                                      awful.util.spawn(command)
                                                                    end))
end
root.buttons(globalbuttons)

for binding, f in pairs(settings.bindings.client) do
  clientkeys = awful.util.table.join(clientkeys, awful.key(binding[1], binding[2], f))
end

for keys, f in pairs(settings.bindings.mouse) do
  clientbuttons = awful.util.table.join(clientbuttons, awful.button(keys[1], keys[2], f))
end

for binding, f in pairs(awful.util.table.join(settings.bindings.global, settings.bindings.prompt)) do
  globalkeys = awful.util.table.join(globalkeys, awful.key(binding[1], binding[2], f))
end

-- Keyboard digit bindings
for i, my_tag_def in pairs(settings.tags_defs) do
  shortcut = my_tag_def.shortcut
  for mod, f in pairs(settings.bindings.root_digits) do
    globalkeys = awful.util.table.join(globalkeys, awful.key(mod, shortcut, f(getTagByShortcut(shortcut))))
  end
  for mod, f in pairs(settings.bindings.client_digits) do
    clientkeys = awful.util.table.join(clientkeys, awful.key(mod, shortcut, f(getTagByShortcut(shortcut))))
  end
end

-- Command bindings
for keys, command in pairs(settings.bindings.command) do
  globalkeys = awful.util.table.join(globalkeys, awful.key(keys[1], keys[2],
			       function() my_debug(command) awful.util.spawn(command) end))
end

--- Set keys
root.keys(globalkeys)

---- Hooks

-- Focus/unfocus
client.add_signal("focus", function(c)
			     if not awful.client.ismarked(c) then
			       c.border_color = beautiful.border_focus
			     end
			   end)
client.add_signal("unfocus", function(c)
			       c.border_color = beautiful.border_normal
			     end)
-- Manage hook
function manage_client(c)
  my_debug(string.format("Managing: name = '%s', class = '%s' instance = '%s'", c.name or "", c.class or "", c.instance or ""))

  -- Add bindings
  c:buttons(clientbuttons)
  c:keys(clientkeys)

  -- sloppy focus
  c:add_signal("mouse::enter", function(c)
				 if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
				 and awful.client.focus.filter(c) then
				 client.focus = c
			       end
			     end)

  -- focus dialogs right away 
  if settings.focus_dialogs and c.type and c.type == "dialog" then
    client.focus = c
  end

  if c.transient_for then
    awful.placement.centered(c, c.transient_for)
  end


  -- Prevent new windows from becoming master
  if not settings.new_become_master then awful.client.setslave(c) end

  -- Create border
  c.border_width = beautiful.border_width
  c.border_color = beautiful.border_normal

  c.size_hints_honor = settings.size_hints_honor

  awful.client.movetoscreen(c, mouse.screen)
end
client.add_signal("manage", manage_client)
client.add_signal("new", 
                  function(c)
                    c:add_signal("property::urgent",
                                 function()
                                   if client.focus == c and c.urgent then
                                     c.urgent = false
                                 end
                               end)
                  end)
