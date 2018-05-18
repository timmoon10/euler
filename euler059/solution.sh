#!/bin/sh
# Key: god

# Create temporary files
key=$(mktemp)
cipher=$(mktemp)
message=$(mktemp)

# Attempt to decrypt cipher
sed 's/,/\n/g' p059_cipher.txt > ${cipher}
echo $1 | ./char_to_int.awk > ${key}
./encrypt.awk ${key} ${cipher} > ${message}
cat ${message} | ./int_to_char.awk
cat ${message} | awk 'BEGIN { sum = 0 } { sum += $1 } END { print sum }'

# Clean up temporary files
rm ${key} ${cipher} ${message}
