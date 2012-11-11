# the following variable defines the session which is started if the user
# doesn't explicitely select a session
DEFAULT_SESSION=i3

xrdb -merge "${HOME}/.Xresources"
xmodmap ~/.xmodmap
xsetroot -cursor_name left_ptr &  # sets the cursor icon
sh ~/scripts/monitors.sh

case $1 in
xmonad)
    trayer --edge top --align right --SetDockType true --SetPartialStrut true \
         --expand true --width 2 --transparent true --tint 0x000000 --height 12 &
    nm-applet --sm-disable &

    exec xmonad
    ;;
i3|gnome-session|awesome)
	exec $1
	;;
*)
	exec $DEFAULT_SESSION
	;;
esac
