local capi = {
  client = client,
  screen = screen
}

local awful = require("awful")

function movetoscreen(c, s)
    local sel = c or capi.client.focus
    if sel then
        local sc = capi.screen.count()
        if not s then
            s = sel.screen + 1
        end
        if s > sc then s = 1 elseif s < 1 then s = sc end
        sel.screen = s
--        capi.mouse.coords(capi.screen[s].geometry)
    end
end

awful.client.movetoscreen = movetoscreen
