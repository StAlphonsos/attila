#!/bin/sh
##
offlineimap
flail -1 'incoming; fspam; sync; autofile; sync; ssz'
spamfish `latest_spam_folder`
#mu index
