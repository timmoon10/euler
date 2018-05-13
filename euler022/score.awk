#!/usr/bin/awk -f
function char_score(a,   i) {
    offset = 64 # ASCII code for 'A' is 65
    for (i = 1; i <= 26; ++i) {
        if (sprintf("%c", offset + i) == a) { return i }
    }
    return 0
}
BEGIN { total_score = 0 }
{
    len = length($0)
    score = 0
    for (i = 1; i <= len; ++i) {
        score += char_score(substr($0, i, 1))
    }
    total_score += NR * score
}
END {
    print total_score
}
