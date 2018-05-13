#!/usr/bin/awk -f
BEGIN { i = 0 }
{
    ++i
    for (j = 1; j <= i; ++j) { vals[i "," j] = $j }
}
END {
    for (--i; i > 0; --i) {
        for (j = 1; j <= i; ++j) {
            left = vals[(i+1) "," j]
            right = vals[(i+1) "," (j+1)]
            vals[i "," j] += (left > right) ? left : right
        }
    }
    print vals["1,1"]
}
