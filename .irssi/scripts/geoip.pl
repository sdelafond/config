use strict;
use Irssi;
use Socket;

use vars qw($VERSION %IRSSI);
$VERSION = "0.0.3";
%IRSSI = (
    authors         => "Toni Viemerö",
    contact         => "toni.viemero\@iki.fi",
    name            => "geoip",
    description     => "Print the users country name in /WHOIS replies",
    license         => "Public Domain",
    changed         => "Wed Nov 19 14:34:12 EET 2003"
    );

sub event_whois {
  my ($server, $data, $nick, $host) = @_;
  my ($me, $nick, $user, $host) = split(" ", $data);
  $server->printformat($nick, MSGLEVEL_CRAP, 'whois_geoip', $host, &geoip($host));
}

sub cmd_geoip {
  Irssi::print(&geoip(shift));
}

sub geoip {
  my $geoip_bin = Irssi::settings_get_str("geoip_bin");
  my $host = lc shift;
  if ($host eq "") {
    return "USAGE: /GEOIP <host/ip>";
  }
  my $info = `$geoip_bin $host`;
  chomp($info);
  return $info;
}

Irssi::theme_register([
    'whois_geoip' => '{whois geoip %|$1}'
                      ]);

Irssi::command_bind('geoip', \&cmd_geoip);
Irssi::signal_add_last('event 311', 'event_whois');
Irssi::settings_add_str("misc", "geoip_bin", "~/bin/ip2city.py");
