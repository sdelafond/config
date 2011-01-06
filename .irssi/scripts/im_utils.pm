package ImUtils;

use strict;

sub is_server_im {
  my ($server, $control_channels) = @_;

  my @control_channels = @{$control_channels};
  my $server_tag = $server->{tag};

  return (grep {@{$_}[1] eq $server_tag} @control_channels);
}

sub is_channel_im {
  my ($channel, $control_channels) = @_;

  my @control_channels = @{$control_channels};

  return (grep {@{$_}[0] eq $channel} @control_channels);
}

sub is_im {
  my ($server, $channel, $control_channels) = @_;

  return (is_server_im($server, $control_channels) && is_channel_im($channel, $control_channels));
}

sub is_nick_im {
  my ($nick, $control_channels) = @_;
  return grep {$_ eq $nick} &get_all_im_nicks($control_channels);
}

sub get_channels {
  my @control_channels = ();

  foreach my $channel(&Irssi::channels()) {
    my $name = $channel->{name};
    my $topic = $channel->{topic};
    my $type = $channel->{server}->{tag};
#    print("looking at: $name, $type");
    if (($name =~ m/^&/) && (($topic eq "Welcome to the control channel. Type \x02help\x02 for help information.") || ($type eq "minbif"))) {
#      print("found $name, $type");
      push(@control_channels, [$name, $type]);
    }
  }

  return \@control_channels;
}

sub get_all_im_nicks {
  my ($control_channels) = @_;

  my @control_channels = @{$control_channels};

  my @nicks = ();
  foreach my $list (@control_channels) {
    my ($channel, $server_tag) = @{$list};
    push(@nicks, Irssi::server_find_tag($server_tag)->channel_find($channel)->nicks());
  }

  return \@nicks;
}

1;
