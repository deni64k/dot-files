export PLACEMENT=home MACHINE=laptop

sh ${HOME}/.fehbg
setxkbmap -layout "us,ru" -option "grp:toggle"
trayer --edge top --align right --widthtype request --height 16 --transparent true --alpha 255 &

exec xmonad
