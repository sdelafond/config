---- Beautiful theme settings
theme = {}
theme.font = "DejaVu Sans 8"
theme.taglist_font = "DejaVu Sans 8"

-- theme.bg_normal = #22222222
theme.bg_normal = "#222222"
theme.fg_normal = "#999999"
theme.taglist_bg_focus = "#888888"
theme.bg_focus = "#285577"
theme.fg_focus = "#ffffff"
theme.bg_sbfocus = "#11335565"
theme.bg_urgent = "#EEEE00"
theme.fg_urgent = "#000000"
theme.border_width = "1"
theme.border_normal = "#333333"
theme.border_focus = "#4C7899"
theme.border_marked = "#91231c"
theme.wallpaper_cmd = { "awsetbg -f /home/seb/.config/awesome/background.png" }

theme.layout_fairh = "/usr/share/awesome/themes/default/layouts/fairhw.png"
theme.layout_fairv = "/usr/share/awesome/themes/default/layouts/fairvw.png"
theme.layout_floating  = "/usr/share/awesome/themes/default/layouts/floatingw.png"
theme.layout_magnifier = "/usr/share/awesome/themes/default/layouts/magnifierw.png"
theme.layout_max = "/usr/share/awesome/themes/default/layouts/maxw.png"
theme.layout_fullscreen = "/usr/share/awesome/themes/default/layouts/fullscreenw.png"
theme.layout_tilebottom = "/usr/share/awesome/themes/default/layouts/tilebottomw.png"
theme.layout_tileleft   = "/usr/share/awesome/themes/default/layouts/tileleftw.png"
theme.layout_tile = "/usr/share/awesome/themes/default/layouts/tilew.png"
theme.layout_tiletop = "/usr/share/awesome/themes/default/layouts/tiletopw.png"
theme.layout_spiral  = "/usr/share/awesome/themes/default/layouts/spiralw.png"
theme.layout_dwindle = "/usr/share/awesome/themes/default/layouts/dwindlew.png"

theme.awesome_icon           = "/usr/share/awesome/themes/sky/awesome-icon.png"
theme.tasklist_floating_icon = "/usr/share/awesome/themes/sky/layouts/floating.png"

theme.menu_submenu_icon     = "/usr/share/awesome/themes/default/submenu.png"
-- theme.taglist_squares_sel   = "/usr/share/awesome/themes/default/taglist/squarefw.png"
-- theme.taglist_squares_unsel = "/usr/share/awesome/themes/default/taglist/squarew.png"

-- Naughty
naughty.config.presets.normal.bg = theme.bg_widget
for _,preset in pairs({"normal", "low", "critical"}) do
  naughty.config.presets[preset].font = "DejaVu Sans 8"
end

return theme