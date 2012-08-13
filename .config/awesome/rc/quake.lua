local quake = loadrc("quake", "vbe/quake")

local quakeconsole = {}
for s = 1, screen.count() do
   quakeconsole[s] = quake({ terminal = settings.applications.terminal_without_screen,
			     height = 0.3,
			     screen = s })
end

settings.bindings.global = awful.util.table.join(
  settings.bindings.global,
  { [ {settings.keys.super, "`"} ] = function () quakeconsole[mouse.screen]:toggle() end }
)
