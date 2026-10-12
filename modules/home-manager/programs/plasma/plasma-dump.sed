# rc2nix copies KConfig text into Nix strings without decoding it.
# Nix has no \x escape, so "\x3d" would become "x3d".

# plasma-manager wrote these keys with an escaped [$e] marker. They are
# copies of the real [$e] key, so drop them.
/\\x5b\$e\\x5d" = /d

# plasma-manager escapes = [ ] as \x3d \x5b \x5d. Decode them.
s/\\x3d/=/g
s/\\x5b/[/g
s/\\x5d/]/g

# A [$e] marker means KDE expands variables like $HOME in the value.
# plasma-manager sets this marker with shellExpand.
s/^( *.*)\[\$e\]" = (.*);$/\1" = { value = \2; shellExpand = true; };/
