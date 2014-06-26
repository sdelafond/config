local awful = require("awful")

-- debugging to ~/.xsession-errors
function my_debug(msg)
  if settings.debug_on then
    io.stderr:write(msg .. "\n")
    io.stderr:flush()
  end
end

-- simple function to load additional LUA files from rc/.
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

-- focus a relative screen (similar to `awful.screen.focus_relative`)
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
awful.screen.focus_relative = screen_focus -- monkey-patching

-- movetoscreen that doesn't reset mouse coordinates
local function movetoscreen(c, s)
    local sel = c or client.focus
    if sel then
        local sc = screen.count()
        if not s then
            s = sel.screen + 1
        end
        if s > sc then s = 1 elseif s < 1 then s = sc end
        sel.screen = s
--        capi.mouse.coords(capi.screen[s].geometry)
    end
end
awful.client.movetoscreen = movetoscreen -- monkey-patching

-- avoid automatic focus of sticky clients
-- http://article.gmane.org/gmane.comp.window-managers.awesome/5264/
local function check_focus(obj)
  if not client.focus or not client.focus:isvisible() then
    local idx = 0
    local c = aclient.focus.history.get(obj.screen, idx)
    local fc = c
    while c.sticky do
      idx = idx+1
      c = aclient.focus.history.get(obj.screen, idx)
    end
    if c then
      client.focus = c
    elseif fc then
      client.focus = fc
    end
  end
end
awful.autofocus.check_focus = check_focus -- monkey-patching
