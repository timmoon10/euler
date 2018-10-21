#!/usr/bin/python3

# Initial triangle, pentagon, and hexagon numbers
tri_ind = 286
pent_ind = 166
hex_ind = 144

# Find next number that is triangle, pentagonal, and hexagonal
tri_val = tri_ind * (tri_ind + 1) // 2
pent_val = pent_ind * (3*pent_ind - 1) // 2
hex_val = hex_ind * (2*hex_ind - 1)
while tri_val != pent_val or tri_val != hex_val:
    tri_val += tri_ind + 1
    tri_ind += 1
    while pent_val < tri_val:
        pent_val += 3*pent_ind + 1
        pent_ind += 1
    while hex_val < tri_val:
        hex_val += 4*hex_ind + 1
        hex_ind += 1
        
# Print result
print("T(%d) = P(%d) = H(%d) = %d"
      % (tri_ind, pent_ind, hex_ind, hex_val))
