#!/usr/bin/python

## @package p003
#  Script for Project Euler, Problem 3
#
#  Finds the prime factors of 600851475143

from __future__ import print_function, division
import math

## Calculate prime numbers
#  Uses the sieve of Eratosthanes to generate prime numbers
#
#  @param n Minimum number of primes to calculate
#  @param pList Initial list of primes
#
#  @return p A list of prime numbers
def getPrimes(n, 
              p = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53]):

    # Initialization
    n = int(n)

    # Calculate primes with sieve of Eratosthenes
    while len(p) < n:

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
    factors = []     # List of prime factors
    pn = 0           # Index of current prime number
    numCurr = num    # Current number being factored

    # Perform trial division until all prime factors are found
    while numCurr > 1:

        # If current prime is not a factor
        if numCurr % p[pn]:

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
            numCurr /= p[pn]

    # Return factors and list of prime numbers
    return [factors,p]


# Executable script
if __name__ == "__main__":

    # Parameters
    ## Number to prime factorize
    num = 600851475143

    # Factor number
    [factors,_] = getFactors(num)

    print("Prime factors of %d = %s" % (num,factors))
