# SETTINGS & INSTALLATION
# [minbif]
# /admin typing_notice true
# /set im_send_typing = ON
#   -> send typing messages to buddies
# /set im_typing_allwin = OFF
#   -> show typing notifications in all windows
# /statusbar window add typing_notice
#
# [bitlbee]
# /set im_send_typing = ON
#   -> send typing messages to buddies
# /set im_typing_allwin = OFF
#   -> show typing notifications in all windows
# /statusbar window add typing_notice
#
# Changelog:
#
# 2009-12-20 (version 2.1.1)
# * reformat installation instructions.
# * tiny refactoring.
#
# 2009-08-25 (version 2.1)
# * externalize some utilities in im_utils.pm
#
# 2009-08-17 (version 2.0)
# * Work with minbif too
#
# 2006-11-02 (version 1.6.1)
# * Sending typing works again.
#
# 2006-10-27 (version 1.6)
# * 'channel sync' re-implemented.
# * bitlbee_send_typing was a string setting, It's a boolean now, like it should.
#
# 2006-10-24 (version 1.5)
#
# * Sending notices to online users only.
# * Using the new get_channel function;
#
# 2005-12-15 (version 1.42):
# * Fixed small bug with typing notices disappearing under certain circumstances
#   in channels
# * Fixed bug that caused outgoing notifications not to work
# * root cares not about our typing status.
#
# 2005-12-04 (version 1.41):
# * Implemented stale states in statusbar (shows "(stale)" for OSCAR connections)
# * Introduced bitlbee_typing_allwin (default OFF). Set this to ON to make
#   typing notifications visible in all windows.
#
# 2005-12-03 (version 1.4):
# * Major code cleanups and rewrites for bitlbee 1.0 with the updated typing
#   scheme. TYPING 0, TYPING 1, and TYPING 2 are now supported from the server.
# * Stale states (where user has typed in text but has stopped typing) are now
#   recognized.
# * Bug where user thinks you are still typing if you close the window after
#   typing something and then erasing it quickly.. fixed.
# * If a user signs off while they are still typing, the notification is removed
# This update by Matt "f0rked" Sparks
#
# 2005-08-26:
# Some fixes for AIM, Thanks to Dracula.
#
# 2005-08-16:
# AIM supported, for sending notices, using CTCP TYPING 0. (Use the AIM patch from Hanji http://get.bitlbee.org/patches/)
#
# 2004-10-31:
# Sends typing notice to the bitlbee server when typing a message in irssi. bitlbee > 0.92
#
# 2004-06-11:
# shows [typing: ] in &bitlbee with multiple users.

use strict;
use Irssi::TextUI;
require "im_utils.pm";

use vars qw($VERSION %IRSSI);

$VERSION = '2.1';
%IRSSI = (
    authors     => 'Tijmen "timing" Ruizendaal, Matt "f0rked" Sparks, Sébastien Delafond',
    contact     => 'tijmen.ruizendaal@gmail.com, root@f0rked.com, seb@debian.org',
    name        => 'im_typing_notice',
    description => '1. Adds an item to the status bar wich shows [typing] when someone is typing a message on the supported IM networks
                    2. Sending typing notices to the supported IM networks (the other way around)',
    license     => 'GPLv2',
    url         => 'http://symlink.me/repositories/show/minbif/scripts/irssi',
    changed     => '2009-08-25',
);

my $KEEP_TYPING_TIMEOUT = 1;
my $STOP_TYPING_TIMEOUT = 7; # How often to check if we are typing, or on msn,
                             # how long to keep the typing notice up, or check
                             # if the other user is still typing...

my %timer_tag;

my %typing;
my %tag;
my $line;
my %out_typing;
my $lastkey;
my $keylog_active = 1;
my $command_char = Irssi::settings_get_str('cmdchars');
my $to_char = Irssi::settings_get_str("completion_char");

my @control_channels;

&set_local_channels();

sub set_local_channels {
  @control_channels = @{&ImUtils::get_channels()};
}

sub event_ctcp_msg {
    my ($server, $msg, $from, $address) = @_;
#     print("CTCP: $server, $msg, $from, $address");
#     print "CTCP: $msg $from $address";
    return unless ImUtils::is_server_im($server, \@control_channels);
    if (my($type) = $msg =~ "TYPING ([0-9])") {
        Irssi::signal_stop();
        if ($type == 0) {
            unset_typing($from);
        }
        elsif ($type == 1) {
            $typing{$from}=1;
            if ($address !~ /\@(login\.oscar\.aol\.com|YAHOO|login\.icq\.com)/i # bitlbee
		&& $address !~ /:(aim|yahoo|icq)\d+$/i) { # minbif; won't work if the user renamed their accounts
                Irssi::timeout_remove($tag{$from});
                $tag{$from}=Irssi::timeout_add_once($STOP_TYPING_TIMEOUT*1000,"unset_typing",$from);
            }
            redraw($from);
        }
        elsif ($type == 2) {
            stale_typing($from);
        }
    }
}

sub unset_typing {
    my($from,$no_redraw)=@_;
    delete $typing{$from} if $typing{$from};
    Irssi::timeout_remove($tag{$from});
    redraw($from) if !$no_redraw;
}

sub stale_typing {
    my($from)=@_;
    $typing{$from}=2;
    redraw($from);
}

sub redraw {
    my($from)=@_;
    my $window = Irssi::active_win();
    my $channel = $window->get_active_name();
    if ($from eq $channel
        || $channel =~ /&chat_0/
        || Irssi::settings_get_bool("im_typing_allwin")) {
      Irssi::statusbar_items_redraw('typing_notice');
    }
}

sub event_msg {
    my ($server,$data,$from,$address,$target) = @_;
    return unless ImUtils::is_server_im($server, \@control_channels);
    my $channel=Irssi::active_win()->get_active_name();
    unset_typing $from, "no redraw";
    unset_typing $channel;
}

sub event_quit {
    my($server,$nick,$address,$reason)=@_;
    return unless ImUtils::is_server_im($server, \@control_channels);
    unset_typing $nick;
}

sub typing_notice {
    my ($item, $get_size_only) = @_;
    my $window = Irssi::active_win();
    my $channel = $window->get_active_name();

    if (exists($typing{$channel})) {
        $item->default_handler($get_size_only, "{sb typing}", 0, 1);
    }
    else {
        $item->default_handler($get_size_only, "", 0, 1);
        Irssi::timeout_remove($tag{$channel});
    }
    if (ImUtils::is_nick_im($channel, \@control_channels) || $channel =~ /&chat_0/
        || Irssi::settings_get_bool("im_typing_allwin")) {
        foreach my $key (keys(%typing)) {
            $line .= " ".$key;
        }
        if ($line ne "") {
            $item->default_handler($get_size_only, "{sb typing:$line}", 0, 1);
            $line = "";
        }
    }
}

sub empty {
    my $from = shift;
    delete($typing{$from});
    Irssi::statusbar_items_redraw('typing_notice');
}

sub window_change {
    Irssi::statusbar_items_redraw('typing_notice');
    my $win = Irssi::active_win() ? Irssi::active_win()->{active} : undef;
    if (ref $win && ImUtils::is_server_im($win->{server}, \@control_channels)) {
        if (!$keylog_active) {
            $keylog_active = 1;
            Irssi::signal_add_last('gui key pressed', 'key_pressed');
            #print "Keylog started";
        }
    }
    else {
        if ($keylog_active) {
            $keylog_active = 0;
            Irssi::signal_remove('gui key pressed', 'key_pressed');
            #print "Keylog stopped";
        }
    }
}

sub key_pressed {
    return unless Irssi::settings_get_bool("im_send_typing");
    my $key = shift;
    if ($key != 9 && $key != 10 && $lastkey != 27 && $key != 27
       && $lastkey != 91 && $key != 126 && $key != 127)
    {
        my $server = Irssi::active_server();
        my $window = Irssi::active_win();
        my $nick = $window->get_active_name();
        if (ImUtils::is_server_im($server, \@control_channels) &&
            $nick ne "(status)" &&
            $nick ne "root")
        {
            if (ImUtils::is_nick_im($nick, \@control_channels)) {
                my $input = Irssi::parse_special("\$L");
                my ($first_word) = split(/ /,$input);
                if ($input !~ /^$command_char.*/ && $first_word =~ s/$to_char$//){
                    send_typing($first_word);
                }
            }
            else {
                my $input = Irssi::parse_special("\$L");
                if ($input !~ /^$command_char.*/ && length($input) > 0){
                    send_typing($nick);
                }
            }
        }
    }
    $lastkey = $key;
}

sub out_empty {
    my ($a) = @_;
    my($nick,$tag)=@{$a};
    delete($out_typing{$nick});
    #print $winnum."|".$nick;
    if (my $server=Irssi::server_find_tag($tag)) {
        $server->command("^CTCP $nick TYPING 0");
    }
}

sub send_typing {
    my $nick = shift;

    if (!exists($out_typing{$nick}) || time - $out_typing{$nick} > $KEEP_TYPING_TIMEOUT) {
      #print "send_typing ". $nick;

	return unless grep { $_->{nick} eq $nick } @{&ImUtils::get_all_im_nicks(\@control_channels)};

	#print "Send typing";
        my $server = Irssi::active_server();
        $server->command("^CTCP $nick TYPING 1");

        $out_typing{$nick} = time;

        ### Reset 'stop-typing' timer
        if ($timer_tag{$nick}) {
            Irssi::timeout_remove($timer_tag{$nick});
            delete($timer_tag{$nick});
        }
        $timer_tag{$nick} = Irssi::timeout_add_once($STOP_TYPING_TIMEOUT*1000, 'out_empty', ["$nick", $server->{tag}]);
    }
}

Irssi::settings_add_bool("im","im_send_typing",1);
Irssi::settings_add_bool("im","im_typing_allwin",1);

Irssi::signal_add("ctcp msg", "event_ctcp_msg");
Irssi::signal_add("message private", "event_msg");
Irssi::signal_add("message public", "event_msg");
Irssi::signal_add("message quit", "event_quit");
Irssi::signal_add_last('window changed', 'window_change');
Irssi::signal_add_last('gui key pressed', 'key_pressed');

Irssi::statusbar_item_register('typing_notice', undef, 'typing_notice');
Irssi::statusbars_recreate_items();

Irssi::signal_add_last('channel sync', "set_local_channels");
