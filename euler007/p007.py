#!/usr/bin/python

## @package p007
#  Script for Project Euler, Problem 7
#
#  Finds the 10001th prime number

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

# Executable script
if __name__ == "__main__":

    ## Prime number to find
    n = 10001
    
    ## List of prime numbers
    p = getPrimes(n)

    # Output answer
    print("%dth prime = %d" % (n,p[n-1]))
