#!/usr/bin/awk -f
function char_to_int(a,   i) {
    # Check upper case, lower case, then all characters
    for (i = 65; i < 91; ++i) {
        if (sprintf("%c", i) == a) { return i }
    }
    for (i = 97; i < 123; ++i) {
        if (sprintf("%c", i) == a) { return i }
    }
    for (i = 0; i < 128; ++i) {
        if (sprintf("%c", i) == a) { return i }
    }
    return 0
}
{
    len = length($0)
    for (i = 1; i <= len; ++i) {
        print char_to_int(substr($0, i, 1))
    }
}
