#!/usr/bin/perl

# Based on Minbif GPlus Renamer by Justin Wheeler & Matias Russitto
# Sébastien Delafond <seb@debian.org>,  Wed, 28 Oct 2015 08:01:29 +0100

use strict;

use Irssi;
use Irssi::Irc;

use vars qw( $VERSION %IRSSI );

our $VERSION = '0.01';
our %IRSSI = (
    authors => do { use utf8; 'Sébastien Delafond' },
    contact => 'seb@debian.org',
    name    => 'hipchat-minbif-rename',
    description => 'Rename XMPP HipChat contacts in minbif to their full names',
    license => 'GPL',
);

my $minbif_channel  = '&minbif';
my $gplus_server = qr/chat\.hipchat\.com/i;
my $unnamed_pattern = qr/^_\d+_\d+/;

my %minbif_servers;
my %changed_nicks;
my %attempted_to_rename;

sub message_join {
    my ($server, $channel, $nick, $address) = @_;

    return if $channel ne $minbif_channel
           || $address !~ $gplus_server
           || $nick    !~ $unnamed_pattern;

    $minbif_servers{ $server->{ tag } } = 1;
    $attempted_to_rename{ $nick }       = q{};

    $server->redirect_event(
        'whois', 1, ":$nick", -1, undef,
        {
#            'event 311' => 'redir whois_data',
            'event 311' => 'redir ignore_it',
            'event 312' => 'redir ignore_it',
            'event 319' => 'redir ignore_it',
            'event 320' => 'redir extended_data',
        },
    );

    $server->send_raw("WHOIS $nick $nick");

    return;
}

sub whois_data {
    my ($server, $data) = @_;

    my ($me, $nick, $user, $host, $star, $real_name) = split /\s+/, $data, 6;

    return if !exists $minbif_servers{ $server->{ tag } }
           || $nick !~ $unnamed_pattern
           || !exists $attempted_to_rename{ $nick };

    if (my ($actual_name) = $real_name =~ m{:(.+)(?=\s+\[)}) {
        if ($actual_name !~ $unnamed_pattern && $actual_name ne q{}) {
           _change_nickname( $server, $nick, $actual_name );
       }
    }

    Irssi::signal_stop();

    return;
}

sub _change_nickname {
    my ($server, $old_nick, $new_nick) = @_;

    $new_nick = _clean_nick( $new_nick );

    foreach my $changed_nick ( keys %changed_nicks ) {
        delete $changed_nicks{ $changed_nick }
            if (time - $changed_nicks{ $changed_nick }) > 10;

        delete $changed_nicks{ $changed_nick }
               if $attempted_to_rename{ $changed_nick } ne $new_nick;
    }

    return if exists $changed_nicks{ $old_nick };

    $changed_nicks{ $old_nick } = time;

    $attempted_to_rename{ $old_nick } = $new_nick;

    Irssi::print("Renaming $old_nick to $new_nick");

    $server->command("quote SVSNICK $old_nick $new_nick");

    return;
}

sub nick_used {
    my ($server, $data) = @_;

    return if !exists $minbif_servers{ $server->{ tag } };

    my ($nick, $new_nick, $used_message) = split /\s+/, $data, 3;

    my ( $old_nick ) = grep { $attempted_to_rename{ $_ } eq $new_nick }
                            keys %attempted_to_rename;

    return if !$old_nick;

    if ($new_nick eq substr( "${old_nick}_HipChat", 0, 29 )) {
        Irssi::print(
            qq{I tried renaming $old_nick to $new_nick with and without a }
          .  q{HipChat suffix, but was unsuccessful.  You'll need to rename }
          .  q{this user manually.}
        );

        return;
    }

    Irssi::print("$new_nick appears to be used -- trying ${new_nick}_HipChat");

    _change_nickname( $server, $old_nick, "${new_nick}_HipChat" );

    Irssi::signal_stop();

    return;
}

sub extended_data {
    my ($server, $data) = @_;

    return if !exists $minbif_servers{ $server->{ tag } };

    my ($me, $nick, $rest) = split /\s+/, $data, 3;

    return if $nick !~ $unnamed_pattern;

    if ($rest =~ m/:Full\s+Name:\s+([\w\s]+)/m) {
        return if !$1;
	my $fullname = $1;
	$fullname =~ s/[\s_]//g;

        _change_nickname( $server, $nick, "U_$fullname" );
    }

    return;
}

sub _clean_nick {
    my ($name) = @_;

    $name =~ s/[^\w\d_-]+/_/g;
    $name =~ s/_{2,}/_/g;

    return $name;
}

sub nick_change {
    my ($server, $new_nick, $old_nick, $host) = @_;

    return if !exists $minbif_servers{ $server->{ tag } };

    delete $attempted_to_rename{ $old_nick };

    return;
}

sub ignore_it {
    my ($server, $info) = @_;

    return if !exists $minbif_servers{ $server->{ tag } };

    my ($me, $them, $everything_else) = split /\s+/, $info, 3;

    # If we just WHOIS'd them, but haven't tried renaming yet.
    if (exists $attempted_to_rename{ $them }) {
        Irssi::signal_stop();
    }

    return;
}

Irssi::signal_add_first('message join'  => 'message_join' );
Irssi::signal_add_first('message nick'  => 'nick_change'  );
Irssi::signal_add_first('whois_data'    => 'whois_data'   );
Irssi::signal_add_first('extended_data' => 'extended_data');
Irssi::signal_add_first('event 433'     => 'nick_used'    );
Irssi::signal_add_first('event 311'     => 'ignore_it'    );
Irssi::signal_add_first('event 312'     => 'ignore_it'    );
Irssi::signal_add_first('event 319'     => 'ignore_it'    );
Irssi::signal_add_first('event 320'     => 'extended_data');
