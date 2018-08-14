#!/usr/bin/python

class TailDigits:
    """The last few digits of a long non-negative integer."""
    
    # "static const" parameters
    length = 10
    base = 10

    # List of digits
    digits = []

    def __init__(self, n = 0):
        if n < 0: raise Exception("expected a non-negative number")
        self.digits = [0] * self.length
        if n > 0:
            for i in range(self.length):
                self.digits[i] = n % self.base
                n = n // self.base
                if n <= 0: break
        
    def __add__(self, other):
        result = TailDigits()
        digit = 0
        for i in range(result.length):
            digit += self.digits[i] + other.digits[i]
            result.digits[i] = digit % result.base
            digit = digit // result.base
        return result

    def __mul__(self, other):
        result = TailDigits()
        digit = 0
        for i in range(result.length):
            for j in range(i+1):
                digit += self.digits[j] * other.digits[i-j]
            result.digits[i] = digit % result.base
            digit = digit // result.base
        return result

    def __str__(self):
        result = str()
        for d in reversed(self.digits):
            result += str(d)
        return result

    def __pow__(self, n):
        if n <= 0: return TailDigits(1)
        result = self
        i = 1
        while 2*i < n:
            result *= result
            i *= 2
        if i == n: return result
        else:      return result * (self ** (n - i))
    
if __name__ == "__main__":
    result = TailDigits()
    for i in range(1, 1000+1): result += TailDigits(i) ** i
    print(result)
