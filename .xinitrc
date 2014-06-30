# set cursor
xsetroot -cursor_name left_ptr

# re-bind Caps_Lock
/usr/bin/xmodmap -e "remove control = Control_L"
/usr/bin/xmodmap -e "remove mod4 = Super_L"
/usr/bin/xmodmap -e "keysym Control_L = Super_L"
/usr/bin/xmodmap -e "keysym Super_L = Control_L"
/usr/bin/xmodmap -e "add control = Control_L"
/usr/bin/xmodmap -e "add mod4 = Super_L"
/usr/bin/xmodmap -e "clear Lock"
/usr/bin/xmodmap -e "keycode 66 = Super_R"

# load ~/.Xresources
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Keyboard repeat rate
xset r rate 200 60

echo "fuck off" >> ~/coolcat.txt

# Start Xmonad
exec xmonad
