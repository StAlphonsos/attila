## identities_config.pl - declare identites for reply, be

$IDENTITIES{'StA'} = 'attila <attila@stalphonsos.com>';
$IDENTITIES{'ClueFactory'} = 'Sean Levy <snl@cluefactory.com>';
$IDENTITIES{'Gmail'} = 'Sean Levy <cluefactory@gmail.com>';
$IDENTITIES{'kWantera'} = 'Sean Levy <slevy@kwantera.com>';

$ID_SMTP{' default'} = '!msmtp -t';
$ID_SMTP{'StA'} = '!msmtp -t -a stalphonsos';
$ID_SMTP{'ClueFactory'} = '!msmtp -t -a cluefactory';
$ID_SMTP{'Gmail'} = '!msmtp -t -a gmail';
$ID_SMTP{'kWantera'} = '!msmtp -t -a kWantera';

1;
__END__

# Local variables:
# mode: perl
# indent-tabs-mode: nil
# tab-width: 4
# perl-indent-level: 4
# End:
