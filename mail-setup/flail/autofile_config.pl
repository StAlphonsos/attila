## autofile_config.pl - config for autofiling
##

use vars qw($AUTO_FILE);

sub kwt { return "slevy\@kwantera.com/@_"; }
sub sta { return "attila\@stalphonsos.com/@_"; }
sub clf { return "snl\@cluefactory.com/@_"; }
sub gml { return "cluefactory\@gmail.com/@_"; }

$AUTO_FILE = {
    'Subject' => {
        'jobs' => 'jobs\.perl\.org',
        kwt('Alerter') => '\[KWT\]',
        kwt('Nagios') => '\[WATCHER\]',
        'offline' => '\[offline\]',
        'blog' => '\[(haqistan|thisland)\]',
    },
    'To,Cc' => {
        'flail' => '\[flail\]',
        'claka' => '\[(claka|crackalaka)\]',
        'osdhud' => '\[osdhud\]',
        'blog' => '@haqistan\.net',
        'cmu' => 'pdl-alumni@ece\.cmu\.edu',
        'freebsd' => '@freebsd\.org',
        'openbsd' => '@(|.*\.)(openbsd|mtier)\.org',
        'torproject' => '@(|.*\.)(torproject\.org)',
        'github' => '@(|.*\.)github\.(com|net|org|io)',
        'nycbug' => '@(|.*\.)nycbug\.org',
    },
    'From,Sender' => {
        'guru' => '.*@guru.com',
        'odesk' => '.*@odesk.com',
        'inphonex' => '.*\.inphonex.com',
        'domains' => '@256domains\.com|pdqregistrar\.com|gray\.enom@256',
        'slashdot' => 'slashdot@(|.+)\.slashdot\.org',
        'pair' => '.*@pair\.com',
        'voicemail' => 'Voicemail System.*donotreply@InPhonex.com',
        'mailnull' => '@mailnull\.com',
        'itzamna-logs' => '(root|MAILER-DAEMON)@(itzamna\.|)cluefactory\.com',
        'bcc-snl' => 'snl@(.*\.|)(cluefactory|bitsend|hardbits)',
        'bcc-attila' => 'attila@(.*\.|)(stalphonsos|cluefactory)',
        'bcc-kwantera' => 'slevy@kwantera\.com',
        'freebsd' => '@freebsd\.org',
        'openbsd' => '@(|.*\.)(openbsd|mtier)\.org',
        'torproject' => '@(|.*\.)(torproject\.org)',
        'github' => '@(|.*\.)github\.(com|net|org|io)',
        'freelancer' => '@(|.*\.)freelancer\.com',
        'george' => 'george@ceetonetechnology\.com',
        'nycbug' => '@(|.*\.)nycbug\.org',
        'remailer' => 'penstat@gmail\.com',
        'sandra' => 'sandyleguizamon@yahoo\.com\.mx',
        'netcraft' => 'announce@lists\.netcraft\.com',
        'paypal' => 'paypal@e\.paypal\.com|service@paypal\.com',
        'usbank' => 'alerts@cs\.usbank-email\.com|@(|.*\.)usbank\.com|usbankbillpay@customercenter.net',
        'yes' => '@yucatanyes\.com',
        'shelters' => '@animalsasia\.org',
        'blog' => 'wordpress@thisland\.haqistan\.net',
        'shopping' => '@(|.*\.)vermontcountrystore\.com|@(|.*\.)veganessentials|@(|.*\.)alliedelectronics\.com',
        'couchsurfing' => '@(|.*\.)couchsurfing\.',
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
