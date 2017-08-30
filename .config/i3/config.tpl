# This file has been auto-generated by i3-config-wizard(1).
# It will not be overwritten, so edit it as you like.
#
# Should you change your keyboard layout some time, delete
# this file and re-run i3-config-wizard(1).
#

# i3 config file (v4)
#
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4

# Font for window titles. Will also be used by the bar unless a different font
# is used in the bar {} block below.
#font pango:monospace 8

# This font is widely installed, provides lots of unicode glyphs, right-to-left
# text rendering and scalability on retina/hidpi displays (thanks to pango).
font pango:DejaVu Sans Mono Bold 10
#FontAwesome Bold 10

# Before i3 v4.8, we used to recommend this one as the default:
# font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# The font above is very space-efficient, that is, it looks good, sharp and
# clear in small sizes. However, its unicode glyph coverage is limited, the old
# X core fonts rendering does not support right-to-left and this being a bitmap
# font, it doesn’t scale on retina/hidpi displays.

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# start a terminal
bindsym $mod+t exec urxvt

# kill focused window
bindsym $mod+space kill

# start dmenu (a program launcher)
bindsym $mod+d exec dmenu_run
# There also is the (new) i3-dmenu-desktop which only displays applications
# shipping a .desktop file. It is a wrapper around dmenu, so you need that
# installed.
# bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

# change focus
bindsym $mod+p focus left
bindsym $mod+n focus right
bindsym $mod+Shift+p focus up
bindsym $mod+Shift+n focus down

# # alternatively, you can use the cursor keys:
# bindsym $mod+Left focus left
# bindsym $mod+Down focus down
# bindsym $mod+Up focus up
# bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+j move left
bindsym $mod+Shift+k move down
bindsym $mod+Shift+l move up
bindsym $mod+Shift+semicolon move right

# # alternatively, you can use the cursor keys:
# bindsym $mod+Shift+Left move left
# bindsym $mod+Shift+Down move down
# bindsym $mod+Shift+Up move up
# bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+h split h

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+m fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+f floating toggle

# change focus between tiling / floating windows
#bindsym $mod+space focus mode_toggle

# focus the parent container
bindsym $mod+j focus parent

# focus the child container
bindsym $mod+k focus child

# switch to workspace
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
#bindsym $mod+0 workspace 10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+F1 move container to workspace F1
bindsym $mod+Shift+F2 move container to workspace F2
bindsym $mod+Shift+F3 move container to workspace F3
bindsym $mod+Shift+F4 move container to workspace F4
bindsym $mod+Shift+F5 move container to workspace F5
bindsym $mod+Shift+F6 move container to workspace F6
bindsym $mod+Shift+F7 move container to workspace F7
bindsym $mod+Shift+F8 move container to workspace F8
bindsym $mod+Shift+F9 move container to workspace F9

# reload the configuration file
bindsym $mod+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart
bindsym $mod+Shift+w exec /home/seb/bin/i3-write.sh
# exit i3 (logs you out of your X session)
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym j resize shrink width 10 px or 10 ppt
        bindsym k resize grow height 10 px or 10 ppt
        bindsym l resize shrink height 10 px or 10 ppt
        bindsym semicolon resize grow width 10 px or 10 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"

# Start i3bar to display a workspace bar (plus the system information i3status
# finds out, if available)
bar {
#  status_command i3status
  status_command py3status -c ~/.config/i3status/config
  position top
  tray_output primary
  colors {
    urgent_workspace #2f343a #EEEE00 #222222
    focused_workspace #2f343a #336d99 #222222
    focused_background #003300
  }
}

# Added by Seb

## Variables
set $app.terminal xsc
set $app.terminal_without_screen xterm
set $app.screen_lock bash -c "xset dpms force off && xscreensaver-command -lock"
set $app.screen_off bash -c "sleep 1 && xset dpms force off"
set $app.selection ~/bin/browser-maybe-selection.rb
set $app.keyboard_layout ~/bin/keyboard-layout.sh
set $app.volume ~/bin/pulse-volume.sh
set $app.backlig xbacklight
set $app.backlight ~/bin/xbacklight.sh
# screens will be overridden on a per-host basis
set $screen1 I3_SCREEN_1
set $screen2 I3_SCREEN_2
set $screen3 I3_SCREEN_3

## Colors
client.focused_inactive #333333 #285577 #ffffff #484e50 #5f676a
client.focused #4c7899 #397aac #ffffff #2e9ef4  #285577

## Settings
focus_on_window_activation none

## Keyboard bindings

# urgent WS
bindsym $mod+u [urgent=latest] focus

# sound
bindsym XF86AudioRaiseVolume exec $app.volume up
bindsym XF86AudioLowerVolume exec $app.volume down
bindsym XF86AudioMute exec $app.volume mute

# brightness
bindsym XF86MonBrightnessUp exec $app.backlight -inc
bindsym XF86MonBrightnessDown exec $app.backlight -dec

# screen lock/off
bindsym $mod+l exec $app.screen_lock
bindsym $mod+o exec $app.screen_off
bindsym $mod+Escape exec $app.screen_off

# selection -> web
bindsym $mod+F1 exec $app.selection default
bindsym $mod+F2 exec $app.selection default chromium

# keyboard layout
bindsym $mod+F12 exec $app.keyboard_layout

# workspaces
workspace 1 output $screen1
workspace 2 output $screen1
workspace 3 output $screen1
workspace 6 output $screen1

workspace F1 output $screen2
workspace F2 output $screen2
workspace F3 output $screen2
workspace F6 output $screen2
workspace F7 output $screen2

workspace 4 output $screen3
workspace 9 output $screen3
workspace F5 output $screen3
workspace F8 output $screen3

bindsym Ctrl+1 workspace 1
bindsym Ctrl+2 workspace 2
bindsym Ctrl+3 workspace 3
bindsym Ctrl+4 workspace 4
bindsym Ctrl+5 workspace 5
bindsym Ctrl+6 workspace 6
bindsym Ctrl+7 workspace 7
bindsym Ctrl+8 workspace 8
bindsym Ctrl+9 workspace 9
bindsym Ctrl+F1 workspace F1
bindsym Ctrl+F2 workspace F2
bindsym Ctrl+F3 workspace F3
bindsym Ctrl+F4 workspace F4
bindsym Ctrl+F5 workspace F5
bindsym Ctrl+F6 workspace F6
bindsym Ctrl+F7 workspace F7
bindsym Ctrl+F8 workspace F8
bindsym Ctrl+F9 workspace F9
bindsym Ctrl+F10 workspace F10
bindsym Ctrl+F11 workspace F11
bindsym Ctrl+F12 workspace F12
bindsym $mod+Ctrl+1 workspace F1
bindsym $mod+Ctrl+2 workspace F2
bindsym $mod+Ctrl+3 workspace F3
bindsym $mod+Ctrl+4 workspace F4
bindsym $mod+Ctrl+5 workspace F5
bindsym $mod+Ctrl+6 workspace F6
bindsym $mod+Ctrl+7 workspace F7
bindsym $mod+Ctrl+8 workspace F8
bindsym $mod+Ctrl+9 workspace F9
bindsym $mod+Ctrl+0 workspace F10
# bindsym $mod+Ctrl+11 workspace F11
# bindsym $mod+Ctrl+12 workspace F12

bindsym $mod+Ctrl+h workspace F1; workspace 4; workspace 1
bindsym $mod+Ctrl+d workspace F6; workspace 6
bindsym $mod+Ctrl+u workspace F3; workspace 3
bindsym $mod+Ctrl+m workspace F7; workspace 7

# last workspace
bindsym $mod+a workspace back_and_forth

## Mouse bindings
# FIXME
bindsym --whole-window $mod+button3 exec $app.volume mute
bindsym --whole-window $mod+button4 exec $app.volume up
bindsym --whole-window $mod+button5 exec $app.volume down

## Clients assignments

# terminals
assign [class="^URxvt$" title="mine.nu$"] 4
assign [class="^URxvt$" title="hetz"] 4

assign [class="^URxvt$" title="centurion$"] 4
assign [class="^URxvt$" title="lanner$"] 2
assign [class="^URxvt$" title="omnia"] 2

assign [class="^URxvt$" title="bpi"] 5
assign [class="^URxvt$" title="beastie$"] 5

assign [class="^URxvt$" title="untangle"] 3
assign [class="^URxvt$" title="ngfw"] 3

assign [class="^URxvt$" title="debian.org"] 6

assign [class="^URxvt$" title="recette-"] 3
assign [class="^URxvt$" title="qt-"] 3
assign [class="^URxvt$" title="prod-"] 5
assign [class="^URxvt$" title="th2.prod"] 2
assign [class="^URxvt$" title="py.priv"] 2
assign [class="^URxvt$" title="infra-"] 7
assign [class="^URxvt$" title="deploy-"] 7
assign [class="^URxvt$" title="seb-debian$"] 7

assign [class="^URxvt$" title="x230$"] 1
assign [class="^URxvt$" title="x1$"] 1
assign [class="^URxvt$" title="home$"] 1

assign [class="^URxvt$" ] 6

# office
assign [class="(?i)office" ] 8

# browsers
assign [class="(?i)firefox$" title="Default"] F1
assign [class="(?i)firefox$" title="Untangle"] F3
assign [class="(?i)firefox$" title="Debian"] F6
assign [class="(?i)irefox$" title="app"] F7
assign [class="(?i)irefox$"] F1
assign [class="(?i)chromium$"] F2

# VNC & co
assign [class="(?i)rdp"] F5
assign [class="(?i)vnc"] F5
assign [class="(?i)rdesktop"] F5

# VM
assign [class="(?i)qemu"] F9
assign [class="(?i)kvm"] F9
assign [class="(?i)virtualbox"] F9
assign [class="(?i)steam"] F9

# vids, pics, etc
assign [class="(?i)vlc"] F8
#assign [title="(?i)^qiv"] F8
for_window [title="(?i)^qiv"] floating enable, move window to workspace F8
#assign [class="(?i)feh"] F8
for_window [class="(?i)feh"] floating enable, move window to workspace F8
assign [class="(?i)geeqie"] F8
assign [class="(?i)mplayer"] F8
assign [class="(?i)gimp"] F8
assign [class="(?i)dia"] F8
assign [class="(?i)darktable"] F9

# sound
assign [class="(?i)gmpc"] 9
for_window [class="(?i)spotify"] move window to workspace 9
