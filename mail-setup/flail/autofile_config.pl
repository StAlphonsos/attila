## autofile_config.pl - config for autofiling
##

use vars qw($AUTO_FILE);

sub kwt { return "slevy@kwantera.com/@_"; }
sub sta { return "attila@stalphonsos.com/@_"; }
sub clf { return "snl@cluefactory.com/@_"; }
sub gml { return "cluefactory@gmail.com/@_"; }

$AUTO_FILE = {
    'Subject' => {
        'jobs' => 'jobs\.perl\.org',
        kwt('Alerter') => '\[KWT\]',
        kwt('Nagios') => '\[WATCHER\]',
        'offline' => '\[offline\]',
    },
    'To,Cc' => {
        'flail' => '\[flail\]',
        'claka' => '\[(claka|crackalaka)\]',
    },
    'From,Sender' => {
        'guru' => '.*@guru.com',
        'odesk' => '.*@odesk.com',
        'inphonex' => '.*\.inphonex.com',
        'domains' => '@256domains\.com|pdqregistrar\.com',
        'slashdot' => 'slashdot@(|.+)\.slashdot\.org',
        'pair' => '.*@pair\.com',
        'voicemail' => 'Voicemail System.*donotreply@InPhonex.com',
        'mailnull-notifications' => 'no_reply@mailnull\.com',
        'itzamna-logs' => '(root|MAILER-DAEMON)@(itzamna\.|)cluefactory\.com',
        'bcc-snl' => 'snl@(.*\.|)(cluefactory|bitsend|hardbits)',
        'bcc-attila' => 'attila@(.*\.|)(stalphonsos|cluefactory)',
        'bcc-kwantera' => 'slevy@kwantera\.com',
        'freebsd' => '@freebsd\.org',
        'freelancer' => '@freelancer\.com',
        'george' => 'george@ceetonetechnology\.com',
        'nycbug' => '@nycbug\.org',
        'remailer' => 'penstat@gmail\.com',
        'sandra' => 'sandyleguizamon@yahoo\.com\.mx'
    },
#    ':Content' => {
#        POSIX::strftime("htmlspam_%G%m%d%H",localtime) =>
#            '(?is)<(|/)(html|body|font|div|span|p|form|input|script)(|.*?)>',
#    },
};


1;
__END__

# Local variables:
# mode: perl
# indent-tabs-mode: nil
# tab-width: 4
# perl-indent-level: 4
# End:
