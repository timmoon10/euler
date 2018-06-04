#!/usr/bin/python

## @package p002
#  Script for Project Euler, Problem 2
#
#  Calculates the sum of all even Fibonacci numbers less than four
#  million

from __future__ import print_function, division

# Executable script
if __name__ == "__main__":
    
    # Parameters
    ## Maximum Fibonacci number
    nmax = 4000000
    ## Maximum number of iterations
    maxiter = nmax

    # Initialize variables
    ## Current Fibonacci number (even)
    ncurrent = 2
    ## Previous Fibonacci number (odd)
    nprev = 1
    ## Fibonacci number two before current (number before nprev) (odd)
    nprev2 = 1
    ## Sum of even Fibonacci numbers
    nsum = 2

    # Iterate Fibonacci algorithm
    for ii in range(int(maxiter)):
        
        # Calculate Fibonacci numbers
        nprev2   = nprev    + ncurrent
        nprev    = ncurrent + nprev2
        ncurrent = nprev2   + nprev

        if ncurrent > nmax:
            # Stop if Fibonacci number has reached maximum
            break
        else:
            # Add value of current Fibonacci number (even)
            nsum += ncurrent

    # Output result
    print("Sum of Fibonacci numbers below %g = %d" % (nmax, nsum))
