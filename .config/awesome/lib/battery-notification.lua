function batteryInfo(adapter)
  local path = "/sys/class/power_supply/"..adapter.."/energy_now"
  local fcur = io.open(path)
  if fcur == nil then -- no warning if there is no battery present
    return nil
  end

  -- no warning if the battery is charging
  local fstatus = "/sys/class/power_supply/"..adapter.."/status"
  if io.open(fstatus):read() == "Charging" then
    return nil
  end

  local fcap = io.open("/sys/class/power_supply/"..adapter.."/energy_full_design")
  local cur = fcur:read()
  local cap = fcap:read()
  local battery = math.floor(cur * 100 / cap)

  if tonumber(battery) < 5 then
    naughty.notify({ title = "Battery Warning",
                     text       = "Battery low: "..battery.."%",
                     timeout    = 10,
                     preset = naughty.config.presets.critical
                   })
  end
end

battery_timer = timer({timeout = 60})
battery_timer:add_signal("timeout", function()
                                      batteryInfo("BAT0")
                                    end)
battery_timer:start()
