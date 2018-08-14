#!/usr/bin/python

def num_triangles(p):
    """Compute the number of integer right triangles with perimeter p.

    Given integer p > 0, we seek positive integers a <= b <= c such
    that a^2 + b^2 = c^2 and a + b + c = p. Churning with algebra
    obtains b = (p^2 - 2*p*a) / (2*(p-a)).
    """
    num = 0
    for a in range(1, p):
        numer = p*p - 2*p*a
        denom = 2*(p-a)
        b = numer // denom
        if a > b or b > (p - a - b):
            break
        elif b * denom == numer:
            num += 1
    return num

if __name__ == "__main__":
    best = (12, 1)
    for p in range(best[0]+1, 1001):
        num = num_triangles(p)
        if num > best[1]:
            best = (p, num)
    print(best)
