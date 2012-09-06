---- builtin
require("awful")
require("awful.autofocus")
require("awful.rules")
require("beautiful") -- theme
require("debian.menu")
require("naughty") -- notifications
require("vicious") -- widgets

local capi = {
  client = client,
  mouse = mouse,
  screen = screen,
}

---- Beautiful theme
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

---- homegrown modules & functions
require("lib/client-seb")

-- debugging to ~/.xsession-errors
function my_debug(msg)
  if settings.debug_on then
    io.stderr:write(msg .. "\n")
    io.stderr:flush()
  end
end

-- Simple function to load additional LUA files from rc/.
function loadrc(name, mod)
   local success
   local result

   -- Which file? In rc/ or in lib/?
  local path = awful.util.getdir("config") .. "/" ..
  (mod and "lib" or "rc") ..
      "/" .. name .. ".lua"

   -- If the module is already loaded, don't load it again
   if mod and package.loaded[mod] then return package.loaded[mod] end

   -- Execute the RC/module file
   success, result = pcall(function() return dofile(path) end)
   if not success then
      naughty.notify({ title = "Error while loading an RC file",
                              text = "When loading `" .. name ..
                           "`, got the following error:\n" .. result,
                              preset = naughty.config.presets.critical
                     })
      return print("E: error loading RC file '" .. name .. "': " .. result)
    end

   -- Is it a module?
   if mod then
     return package.loaded[mod]
   end

   return result
end

-- Focus a relative screen (similar to `awful.screen.focus_relative`)
last_coords_per_screen = {}
local function screen_focus(i)
  local s = awful.util.cycle(screen.count(), mouse.screen + i)
  local c = awful.client.focus.history.get(s, 0)
  local coords = mouse.coords()
  last_coords_per_screen[mouse.screen] = coords
  my_debug(string.format("Coords: %s , %s", coords.x, coords.y))
  mouse.screen = s
  if last_coords_per_screen[s] then
    mouse.coords(last_coords_per_screen[s])
  end
  if c then client.focus = c end
end
awful.screen.focus_relative = screen_focus

---- env
env = {}
env.host = os.getenv("HOST_SHORT")

---- Settings, in their own namespace
settings = {}

-- Misc
settings.debug_on = true
settings.focus_dialogs = true
settings.master_width_factor = 0.5
settings.master_columns = 1
settings.master_windows = 1
settings.size_hints_honor = false
settings.no_overlap = true
settings.no_offscreen = true
settings.new_become_master = true

lastTag = "1"
currentTag = "1"

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
                          ["terminal_without_screen"] = 'xterm',
			  ["lock_screen"]     = 'xscreensaver-command -lock',
			  ["screen_off"]      = 'sh -c "sleep 1 ; xset dpms force off"',
			  ["selection"]       = os.getenv("HOME") .. '/bin/browser-maybe-selection.rb',
			  ["keyboard_layout"] = os.getenv("HOME") .. '/bin/keyboard-layout.sh',
			  ["volume"]          = os.getenv("HOME") .. '/bin/volume.sh',
                          ["killgkrellm"]     = 'pkill gkrellm',
                          ["mpc_pause"]       = 'mpc toggle' }

-- do that right away
awful.util.spawn(settings.applications.keyboard_layout)

-- Layouts
if env.host == "hp" then
  settings.default_layout = awful.layout.suit.max
  settings.ut_layout = awful.layout.suit.max
  settings.hp_tag = "1"
  settings.centurion_tag = "4"
  settings.nic = "wlan0"
  settings.nic_unit = "b"
  settings.nic_autoscale = true
elseif env.host == "centurion" then
  settings.default_layout = awful.layout.suit.tile
  settings.ut_layout = awful.layout.suit.max
  settings.hp_tag = "4"
  settings.centurion_tag = "1"
  settings.nic = "eth3"
  settings.nic_unit = "mb"
  settings.nic_autoscale = false
elseif env.host == "seb-debian" then
  settings.default_layout = awful.layout.suit.tile
  settings.ut_layout = awful.layout.suit.tile
  settings.hp_tag = "4"
  settings.centurion_tag = "4"
  settings.nic = "eth0"
  settings.nic_unit = "mb"
  settings.nic_autoscale = false
else
  settings.default_layout = awful.layout.suit.tile
  settings.ut_layout = awful.layout.suit.max
  settings.hp_tag = "4"
  settings.centurion_tag = "4"
  settings.nic = "eth0"
  settings.nic_unit = "mb"
  settings.nic_autoscale = true
end
settings.layouts = { settings.default_layout,
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
settings.tags_defs = { { shortcut = "1", layout = settings.default_layout },
		       {  shortcut = "2", layout = settings.default_layout }, 
		       {  shortcut = "3", layout = settings.default_layout, name = "UT Dev" },
		       {  shortcut = "4", layout = settings.default_layout, name = "Home" },
		       {  shortcut = "5", layout = settings.ut_layout, name = "UT" },
		       {  shortcut = "6", layout = settings.default_layout, name = "Misc" }, -- nmaster = 2
		       {  shortcut = "7", layout = settings.default_layout, name = "Mappy" },
		       {  shortcut = "8", layout = settings.default_layout, name = "Txt" },
		       {  shortcut = "9", layout = settings.default_layout, name = "VMs", mwfact = 0.2 },
		       {  shortcut = "F1", layout = awful.layout.suit.floating, name = "Media" }, -- mwfact = 0.2 },
		       {  shortcut = "F2", layout = awful.layout.suit.max, name = "Snd" },
		       {  shortcut = "F3", layout = awful.layout.suit.max, name = "VNC" },
		       {  shortcut = "F4", layout = awful.layout.suit.floating, name = "Comm" },
		       {  shortcut = "F5", layout = settings.default_layout, name = "Gimp", mwfact = 0.2 },
		       {  shortcut = "F6", layout = settings.default_layout, name = "P2P" }, }

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
--settings.last_selected_client_per_tag = {}
for s = 1, screen.count() do
  -- Each screen has its own tag table.
  settings.tags[s] = awful.tag(settings.tags_names, s)  
--  settings.last_selected_client_per_tag[s] = {}
  for k, my_tag in ipairs(settings.tags[s]) do
    tag_def = settings.tags_defs[k]
    layout = tag_def.layout or settings.layouts[1] -- if no layout, 1st defined
    mwfact = tag_def.mwfact or settings.master_width_factor -- if no mvfact, use defaut
    nmaster = tag_def.nmaster or settings.master_windows -- if no nmaster, use defaut
    ncol = tag_def.ncol or settings.master_columns -- if no ncol, use defaut
    awful.layout.set(layout, my_tag)
    awful.tag.setmwfact(mwfact, my_tag)
    awful.tag.setnmaster(nmaster, my_tag)
    awful.tag.setncol(ncol, my_tag)
--    settings.last_selected_client_per_tag[s][k] = nil
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
  { rule = { class = "XTerm" }, properties = { tag = getTagByShortcut("6") } },

  -- local terminal
  { rule = { name = "urxvt" }, properties = { tag = getTagByShortcut("1") } },
  { rule = { name = "hp" }, properties = { tag = getTagByShortcut(settings.hp_tag) } },
  { rule = { name = "centurion" }, properties = { tag = getTagByShortcut(settings.centurion_tag) } },
  { rule = { name = "home" }, properties = { tag = getTagByShortcut(settings.centurion_tag) } },
  { rule = { name = "seb-debian" }, properties = { tag = getTagByShortcut("1") } },

  -- web
  { rule = { class = "Iceweasel" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Firefox" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Opera" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Konqueror" }, properties = { tag = getTagByShortcut("2") } },
  { rule = { class = "Chromium" }, properties = { tag = getTagByShortcut("2") } },

  -- workstation at UT
  { rule = { name = "host52.untangle.com" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { name = "host51.untangle.com" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { name = "sid" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { name = "lemmiwinks" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { name = "VNC:" }, properties = { tag = getTagByShortcut("5") } },

  -- UT dev
  { rule = { name = "marvin" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { name = "untangle" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { name = "lemmiwinks" }, properties = { tag = getTagByShortcut("3") } },
  { rule = { name = "xen" }, properties = { tag = getTagByShortcut("3") } },

  -- M
  { rule = { name = "10.0.1.180" }, properties = { tag = getTagByShortcut("7") } },
  { rule = { name = "cergy" }, properties = { tag = getTagByShortcut("7") } },
  { rule = { name = "ud-bo-frontal" }, properties = { tag = getTagByShortcut("7") } },
  { rule = { class = "URxvt", name = "th2.prod" }, properties = { tag = getTagByShortcut("6") } },
  { rule = { class = "URxvt", name = "pp" }, properties = { tag = getTagByShortcut("5") } },
  { rule = { class = "URxvt", name = "demo" }, properties = { tag = getTagByShortcut("3") } },

  -- Home
  { rule = { name = "weshyo" }, properties = { tag = getTagByShortcut("4") } }, 
  { rule = { name = "proliant" }, properties = { tag = getTagByShortcut("4") }, 
    callback = { function(c) c:swap(awful.client.getmaster()) end } },
  { rule = { name = "frisco" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { name = "california" }, properties = { tag = getTagByShortcut(settings.centurion_tag) } },
  { rule = { name = "puff" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { name = "beastie" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { name = "t400" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { name = "hippie" }, properties = { tag = getTagByShortcut("4") } },
  { rule = { class = "Vncviewer", name = "hp" }, properties = { tag = getTagByShortcut("F3") } },
  { rule = { class = "Vncviewer", name = "centurion" }, properties = { tag = getTagByShortcut("F3") } },
  { rule = { class = "Vncviewer", name = "hippie" }, properties = { tag = getTagByShortcut("F3") } },

  -- Debian
  { rule = { name = "lenny" }, properties = { tag = getTagByShortcut("7") } },

  -- Text
  { rule = { class = "Evince" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { class = "Epdfview" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { name = "office" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { name = "OpenOffice.org" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { class = "OpenOffice.org" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { name = "LibreOffice" }, properties = { tag = getTagByShortcut("8") } },
  { rule = { class = "LibreOffice" }, properties = { tag = getTagByShortcut("8") } },

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
  { rule = { class = "rdesktop" }, properties = { tag = getTagByShortcut("9") } },
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
  [{settings.keys.none, "F1"}] = settings.applications.selection .. " default",
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
    make_wibox(s, mywibox, mytaglist, promptbox, mylayoutbox, 
               mybatwidget, mynetwidget, mycpuwidget, mycpuwidget2,
               mymemwidget, datewidget, mytasklist, mouse.screen)
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
  [{settings.keys.super, "a"}] = function() awful.tag.viewonly(lastTag) end,
  [{settings.keys.super, "j"}] = function() awful.tag.viewonly(lastTag) end,
  [{settings.keys.super, "a"}] = function() awful.tag.viewonly(lastTag) end,
  
  [{settings.keys.super, "Left"}] = awful.tag.viewprev,
  [{settings.keys.super, "Right"}] = awful.tag.viewnext,
  
  [{settings.keys.super, "f"}] = function() awful.screen.focus_relative(1) end,
  [{settings.keys.super, "b"}] = function() awful.screen.focus_relative(-1) end,

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

  [{settings.keys.super_control, "s"}] = function()
					  awful.prompt.run({prompt = "Run Lua code: "},
							   mypromptbox[mouse.screen].widget,
							   awful.util.eval, nil,
							   awful.util.getdir("cache") .. "/history_eval")
					end,

  [{settings.keys.super_control, "l"}] = function()
					  awful.prompt.run({prompt = "Select window by name: "},
							   mypromptbox[mouse.screen].widget,
							   function(name)
                                                             for k, c in pairs(client.get()) do
--                                                               if string.find(string.lower(c.name), string.lower(name), 1, true) then
                                                               if string.match(string.lower(c.name), string.lower(name)) then
                                                                 my_debug(c.name)
                                                                 awful.tag.viewonly(c:tags()[1])
                                                                 c:swap(awful.client.getmaster())
                                                                 c:raise()
                                                                 client.focus = c
                                                                 break
                                                               end
                                                             end
                                                           end,
                                                           nil,
							   awful.util.getdir("cache") .. "/windowbyname_eval")
					end,

  [{settings.keys.super_control, "i"}] = function()
                                           local txt = ""
                                           if client.focus.class then
                                             txt = "Class: " .. client.focus.class .. " "
                                           end
                                           if client.focus.name then
                                             txt = txt .. "Name: ".. client.focus.name .. " "
                                           end
                                           if client.focus.role then
                                             txt = txt .. "Role: ".. client.focus.role
                                           end
                                           awful.prompt.run({prompt = txt},
                                                            mypromptbox[mouse.screen].widget,
                                                            function() end)
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

mypromptbox = {}

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

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                  } })
mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon), menu = mymainmenu })

mybatwidget = awful.widget.progressbar()
mybatwidget:set_width(18)
--   mybatwidget[s]:set_height(10)
mybatwidget:set_vertical(true)
mybatwidget:set_background_color('#111111')
--  mybatwidget[s]:set_border_color('#000000')
mybatwidget:set_color('#AECF96')
mybatwidget:set_gradient_colors({ '#FF5656', '#AECF96', '#88A175' })
vicious.register(mybatwidget, vicious.widgets.bat, '$2', 61, 'BAT0')

mycpuwidget = awful.widget.graph()
mycpuwidget:set_width(50)
mycpuwidget:set_background_color("#111111")
mycpuwidget:set_color("#FF5656")
mycpuwidget:set_gradient_angle(0)
mycpuwidget:set_gradient_colors({ "#AA0000", "#AA00AA", "#0000AA" })
vicious.register(mycpuwidget, vicious.widgets.cpu, "$1", 1.5)

mynetwidget = awful.widget.graph()
mynetwidget:set_width(50)
mynetwidget:set_background_color("#111111")
mynetwidget:set_color("#FF5656")
mynetwidget:set_gradient_angle(0)
mynetwidget:set_gradient_colors({ "#00AA00", "#00AA66", "#00AAAA" })
mynetwidget:set_scale(settings.nic_autoscale)
vicious.register(mynetwidget, vicious.widgets.net, "${" .. settings.nic .. " down_" .. settings.nic_unit .. "}", 1)

mymemwidget = awful.widget.graph()
mymemwidget:set_width(50)
mymemwidget:set_background_color("#111111")
mymemwidget:set_color("#FF5656")
mycpuwidget:set_gradient_angle(0)
mymemwidget:set_gradient_colors({ "#FFD700", "#ADFF2F", "#00AA00" })
-- vicious.register(mymemwidget, vicious.widgets.mem, "$1", 1.5)

mycpuwidget2 = widget({ type = "textbox" })
-- mycpuwidget2:set_background_color("#111111")
-- mycpuwidget2:set_color("#FF5656")
vicious.register(mycpuwidget2, vicious.widgets.cpuinf,
                 function (widget, args)
                   return string.format("%.1f+%.1fGHz", args["{cpu0 ghz}"], args["{cpu1 ghz}"])
                 end)

function make_wibox(s, mywibox, mytaglist, mypromptbox, mylayoutbox, 
                    mybatwidget, mynetwidget, mycpuwidget, mycpuwidget2, 
                    mymemwidget, datewidget, mytasklist, systray_screen)
  wibox = awful.wibox({ position = "top", screen = s, fg = beautiful.fg_normal, bg = beautiful.bg_normal })
  wibox.widgets = { { mylauncher,
                      datewidget,
                      mytaglist[s],
                      mypromptbox[s],
                      mylayoutbox[s],
                      mybatwidget,
                      mynetwidget,
                      mycpuwidget2,
                      mycpuwidget,
--                      mymemwidget,
                      layout = awful.widget.layout.horizontal.leftright
                    },
                    s == systray_screen and widget({ type = "systray" }) or nil,
                    mytasklist[s],
                    layout = awful.widget.layout.horizontal.rightleft
                  }
  mywibox[s] = wibox
end

for s = 1, screen.count() do
  mylayoutbox[s] = awful.widget.layoutbox(s)
  mylayoutbox[s]:buttons(mylayoutbox.buttons)

  mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })

  mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.noempty, mytaglist.buttons)

  mytasklist[s] = awful.widget.tasklist(function(c)
					  return awful.widget.tasklist.label.currenttags(c, s)
					end, mytasklist.buttons)

  -- the wibox itself
  make_wibox(s, mywibox, mytaglist, mypromptbox, mylayoutbox, mybatwidget,
             mynetwidget, mycpuwidget, mycpuwidget2,
             mymemwidget, datewidget, mytasklist, 1)
end

---- modules
loadrc("quake")
loadrc("xrandr")

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
--  if not settings.new_become_master and not string.match(c.name, "QuakeConsoleNeedsUniqueName") then
  if not settings.new_become_master then
    awful.client.setslave(c)
  end

  -- Create border
  c.border_width = beautiful.border_width
  c.border_color = beautiful.border_normal

  c.size_hints_honor = settings.size_hints_honor

  awful.client.movetoscreen(c, mouse.screen)
end
client.add_signal("manage", manage_client)


client.add_signal("new", 
                  function(c) -- don't mark focused client as "urgent"
                    c:add_signal("property::urgent",
                                 function()
                                   if client.focus == c and c.urgent then
                                     c.urgent = false
                                 end
                               end)
                  end)

awful.tag.attached_add_signal(nil, "property::selected",
                              function(t)
--                                my_debug(string.format("Tag: '%s'", t.name))
                                if currentTag.name ~= t.name then
                                  lastTag = currentTag
                                  currentTag = t
                                end
                              end
                            )
