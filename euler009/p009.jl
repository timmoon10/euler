# Script for Project Euler, Problem 9
#
# Finds a Pythagorean triple {a,b,c} such that a<b<c, a^2+b^2=c^2 and
# a+b+c=1000. The product a*b*c is evaluated.

# Check all combinations of a, b, and c
for a = 1:1000
    for b = 1:a-1
    	c = 1000 - a - b;

	# Output result if a, b, and c form a Pythagorean triple
    	if a*a+b*b == c*c
	   println("a = ", a);
	   println("b = ", b);
	   println("c = ", c);
	   println("a*b*c = ", a*b*c);
	   return;
	end # if

    end # for
end # for
