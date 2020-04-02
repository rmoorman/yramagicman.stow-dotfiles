# Empty the Trash on all mounted volumes and the main HDD
# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"
alias purge="sudo purge"
alias toimg="hdiutil convert -format UDRW -o "
alias musync="rsync -rav ~/Music/iTunes/iTunes\ Media/Music jonathan@10.0.1.8:/home/jonathan/Music"
alias fastflix="sudo /sbin/ipfw add 2000 deny tcp from 173.194.55.0/24 to me; sudo /sbin/ipfw add 2001 deny tcp from 206.111.0.0/16 to me"
alias dnd="launchctl unload -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null"
alias udnd="launchctl load -w /System/Library/LaunchAgents/com.apple.notificationcenterui.plist 2> /dev/null"
alias poweroff="emptytrash; sudo shutdown -h now"
alias cmdtocb='history | tail -n 1 | awk '"'"'{for(i=2;i<NF;i++)printf "%s",$i OFS; if (NF) printf "%s",$NF; printf ORS}'"'"' | pbcopy'
alias ql="qlmanage -p"
alias lsblk='diskutil list'
alias eject="diskutil eject"
alias rmspot="sudo rm -rfv ~/Library/Caches/com.spotify.client/Storage/"
alias blsrm="find ~/Library/ -type f -name '*.localstorage*' -ls -delete"
alias cwd="pwd | c"
alias csleep="sudo shutdown -s now"
# Get OS X Software Updates, and update installed Ruby gems, Homebrew, npm,
# and their installed packages
alias update='gp; npm install -g npm@latest;  npm update -g; \
gem update; brew update; brew upgrade --all; brew linkapps; git checkout master;  bell'
alias stfu="osascript -e 'set volume output muted true'"
alias shutup="osascript -e 'set volume output muted true'"
alias louder="osascript -e 'set volume 7'"
alias headphones="osascript -e 'set volume 2'"
alias bell="tput bel"
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl"
# show and hide things
# Show/hide hidden files in Finder
alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"
# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
 # Merge PDF files
# Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'
# Disable Spotlight
alias spotoff="sudo mdutil -a -i off"
# Enable Spotlight
alias spoton="sudo mdutil -a -i on"
# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
alias plistbuddy="/usr/libexec/PlistBuddy"
# Clean up LaunchServices to remove duplicates in the “Open With” menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
# OS X has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"
# OS X has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"
# Trim new lines and copy to clipboard
alias c="tr -d '\n' | pbcopy"
alias iftop="iftop -i en1"
alias eiftop="iftop -i en0"
# View HTTP traffic
alias sniff="sudo tcpdump -s 0 -A -i en1 port 80"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\""
alias localip="ipconfig getifaddr en1"
alias ips="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"

_myos="$(uname)"
if [[ $_myos == Darwin ]]; then
# Add note to Notes.app (OS X 10.8)
# Usage: `note 'foo'` or `echo 'foo' | note`
function note() {
    local text
    if [ -t 0 ]; then # argument
    text="$1"
    else # pipe
    text=$(cat)
    fi
    body=$(echo "$text" | sed -E 's|$|<br>|g')
    osascript >/dev/null <<EOF
tell application "Notes"
    tell account "iCloud"
    tell folder "Notes"
    make new note with properties {name:"$text", body:"$body"}
    end tell
    end tell
end tell
EOF
}
# Add reminder to Reminders.app (OS X 10.8)
# Usage: `remind 'foo'` or `echo 'foo' | remind`
function remind() {
    local text
    if [ -t 0 ]; then
    text="$1" # argument
    else
    text=$(cat) # pipe
    fi
    osascript >/dev/null <<EOF
tell application "Reminders"
    tell the default list
    make new reminder with properties {name:"$text"}
    end tell
end tell
EOF
}
# Manually remove a downloaded app or file from the quarantine
function unquarantine() {
    for attribute in com.apple.metadata:kMDItemDownloadedDate com.apple.metadata:kMDItemWhereFroms com.apple.quarantine; do
    xattr -r -d "$attribute" "$@"
    done
}
function shop(){
    find ./ -name $1 -exec open -a /Applications/Adobe\ Photoshop\ CS6/Adobe\ Photoshop\ CS6.app {} \;
}
function tw(){
    open $@ -a TextWrangler.app
}
fi

if [[ "$(pgrep docker | wc -l)" -eq 1 ]]; then
    open -a Docker.app
fi
[[ "$(which nvm > /dev/null)" ]] || nvm_load
