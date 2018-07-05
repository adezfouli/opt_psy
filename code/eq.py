from numba import double
from sympy import *

# defining symbols
k, H, b, T, a, l, q, v1, v2, A, A1, A2 = symbols('k, H, b, T, a, l, q, v1, v2, A, A1, A2')

# for deriving equations in the case of independent cost
L = -v1 * (a*k *(v1 * k + k * l * v2) + (k * b)) + -v2 * (a *k * l * (k * v1 + l *k * v2) + k *l * b) + v1 * A1 + v2*A2
print simplify(solve([diff(L, v1) - diff(L, v2)], v1)[v1].subs(A1, A2))

# defining the cost function
Kv = (a * k ** 2 * v1 + b * k)

L = -v1 * Kv + v1 * A
opt_v = solve(diff(L, v1).subs(A, l*(H)), v1)[0]
print 'Optimal outcome rate (single dimension and constant reward field) :', opt_v

# defining the new reward (Lagrangian)
L = -v1 * Kv + v1 * A
opt_v = solve(diff(L, v1).subs(A, l*(H - l*T*v1)), v1)[0]
print 'Optimal outcome rate (single dimension) :', opt_v

# the ratio requirement that causes highest outcome rate
print 'max ratio (k): ', solve(diff(k * opt_v.subs(a, 0), k), k)

# one outcome effect of reward
print 'max reward magnitude:', solve(diff(opt_v.subs(a, 0), l), l)


# two outcomes independent cost
L = -v1 * (a * k** 2 * v1 + b) + - v2 * (a * k** 2 * v2 + b)  + v1 * A + l * v2 * A
print 'two outcomes different l', simplify(solve([(diff(L.subs(b, 0), v1)).subs(A, H - v1 * T - l * v2 * T),
                      diff(L.subs(b, 0), v2).subs(A, H - v1 * T - l * v2 * T)], (v1, v2)))


# two outcomes independent different ration requirements
L = -v1 * (a * k** 2 * l ** 2 * v1 + b *k * l) + - v2 * (a * k** 2 * v2 + b * k)  + v1 * A + v2 * A
print 'two outcomes different ratio-requirement: ', simplify(solve([(diff(L.subs(b, 0), v1)).subs(A, H - v1 * T - v2 * T),
                      diff(L.subs(b, 0), v2).subs(A, H - v1 * T - v2 * T)], (v1, v2)))




############### these are not included in the paper ##############################

# one outcome inproportional reward
k, H, b, T, a, l, q, v1, v2, A = symbols('k, H, b, T, a, l, q, v1, v2, A')
Kv = (a * k ** 2 * v1 + b * k)
L = -v1 * Kv  + v1 * A
opt_v = solve(diff(L, v1).subs(A, l* (H - T*v1)), v1)[0].subs(b, 0)
print 'opitmal v (inprop):', opt_v


# comparison with two outcome situation
m = 2 * a * k ** 2
T_c = m * atan(1/l) / (l-1)
opt_v_2 = H * (l - 1) / (T * l - T_c)

print "Tc", double(T_c.subs([(H, 1), (T, T_c * 2), (a, 1), (k, 3), (l,1.2)]))
print "inprop one outcome", double(opt_v.subs([(H, 1), (T, T_c * 3), (a, .1), (k, 3), (l,5)]))
print "inprop two outcomes", double(opt_v_2.subs([(H, 1), (T, T_c * 3), (a, .1), (k, 3), (l,5)]))


# two outcomes independent motivational drive
L = -v1 * (a * k** 2 * v1 + b) + - v2 * (a * k** 2 * v2 + b)  + v1 * A1 + l * v2 * A2
print 'two outcomes different l and motivation', simplify(solve([(diff(L.subs(b, 0), v1)).subs(A1, H - v1 * T),
                      diff(L.subs(b, 0), v2).subs(A2, H - l * v2 * T)], (v1, v2)))
