#!/usr/bin/python

## @package p004
#  Script for Project Euler, Problem 4
#
#  Finds the largest palindrome number that is the product of two
#  3-digit numbers

from __future__ import print_function, division
import sys, math

## Calculate prime numbers
#  Uses the sieve of Eratosthanes to generate prime numbers
#
#  @param n Minimum number of primes to calculate
#  @param pList Initial list of primes
#
#  @return p A list of prime numbers
def getPrimes(n, 
              p = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53]):

    # Calculate primes with sieve of Eratosthenes
    while len(p) < int(n):

        # Sieve parameters
        sieveSize = p[-1]       # Number of entries in sieve
        sieveStart = p[-1] + 1  # First entry in sieve
        isSearching = True      # True if sieve has not found any
                                # primes
        
        # Apply sieve until a prime number is found
        while isSearching:
            
            # Initialize sieve
            sieve = [True] * sieveSize

            # Remove composite numbers in sieve
            for pcurr in p:
                ii = int(math.ceil(sieveStart/pcurr))
                while ii*pcurr < sieveStart + sieveSize:
                    sieve[ii*pcurr-sieveStart] = False
                    ii += 1

            # Check sieve for primes
            for ii in range(sieveSize):
                if sieve[ii]:
                    p.append(sieveStart+ii)
                    isSearching = False

            # Shift sieve position if no prime number has been found
            if isSearching:
                sieveStart += sieveSize
        
    # Return primes
    return p

## Calculate prime factors
#  Performs trial division with prime numbers until all prime factors
#  are found
#
#  @param num Number to factor
#  @param p List of prime numbers
#
#  @return [factors,p] factors is a list of prime factors and p is a
#  list of prime numbers
def getFactors(num, 
               p = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53]):
    
    # Initialization
    factors = []      # List of prime factors
    pn = 0            # Index of current prime number
    num = int(num)    # Check that num is an integer

    # Perform trial division until all prime factors are found
    while num > 1:

        # If current prime is not a factor
        if num % p[pn]:

            # Move to next prime
            pn += 1

            # Calculate more primes if list runs out
            if pn >= len(p):
                p = getPrimes(2*len(p),p)

        # If current prime is a factor
        else:

            # Record factor
            factors.append(p[pn])
            
            # Factor out the prime factor
            num //= p[pn]

    # Return factors and list of prime numbers
    return [factors,p]


# Executable script
if __name__ == "__main__":

    # Initialization
    p = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53]  # List of
                                                       # primes

    # Check all 6-digit numbers
    for num in range(999999,100000,-1):

        # Skip numbers that are not palindromes
        if num//100000 != num%10 or (num//10000)%10 != (num//10)%10 or (num//1000)%10 != (num//100)%10:
            continue

        # Get prime factorization
        [factors,p] = getFactors(num,p)

        # Skip if greatest prime factor is over 999
        if factors[-1] > 999:
            continue

        # Check all permutations of prime factors
        for ii in range(1,int(2**(len(factors)-1))):

            # Construct factors
            factor1 = 1
            factor2 = 1
            for jj in range(len(factors)):
                if ii & 1<<jj:
                    factor1 *= factors[jj]
                else:
                    factor2 *= factors[jj]

            # Output solution if factors are both 3-digit numbers
            if 100<=factor1 and factor1<1000 and 100<=factor2 and factor2<1000:
                print("%d = %d * %d" % (num, factor1, factor2))
                sys.exit(0)

    # Error if no solution has been found
    print("Error: failed to find solution")
