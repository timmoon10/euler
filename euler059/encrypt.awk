#!/usr/bin/awk -f
# Apply XOR encryption/decryption
# Usage: cipher.awk key message
# The key and message should be plaintext files where each line is a
# decimal number (e.g. ASCII codes).
function xor(x, y) {
    # Need to define xor since mawk doesn't implement it.
    result = 0
    for(unit = 1; x >= unit || y >= unit; unit *= 2) {
        x_unit = int(x / unit) % 2
        y_unit = int(y / unit) % 2
        result += unit * (x_unit - y_unit) ^ 2 
    }
    return result
}
BEGIN {
    key_length = 0
    pos = 0
}
{
    for (i = 1; i <= NF; ++i) {
        if (FNR == NR) { key[key_length++] = $i }
        else { print xor($i, key[pos++ % key_length]) }
    }
}
