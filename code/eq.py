from numba import double
from sympy import *

# defining symbols
k, H, b, T, a, l, q, v1, v2, A, A1, A2 = symbols('k, H, b, T, a, l, q, v1, v2, A, A1, A2')

# defining the cost function
Kv = (a * k ** 2 * v1 + b * k)

# defining the new reward (Lagrangian)
L = -v1 * Kv + v1 * A
opt_v = solve(diff(L, v1).subs(A, l*(H - l*T*v1)), v1)[0]
print 'Optimal outcome rate (single dimension - Equation D.13) :', opt_v


L = -v1 * Kv + v1 * A
opt_v = solve(diff(L, v1).subs(A, l*(H)), v1)[0]
print 'Optimal outcome rate (single dimension and constant reward field - Equation D.15) :', opt_v


# two outcomes independent different ration requirements
L = -v1 * (a * k** 2 * l ** 2 * v1 + b *k * l) + - v2 * (a * k** 2 * v2 + b * k)  + v1 * A + v2 * A
print 'two outcomes different ratio-requirement - Equation F.6: ', \
    simplify(solve([(diff(L.subs(b, 0), v1)).subs(A, H - v1 * T - v2 * T),
                      diff(L.subs(b, 0), v2).subs(A, H - v1 * T - v2 * T)], (v1, v2)))


# for deriving equations in the case of independent cost
L = -v1 * (a*k *(v1 * k + k * l * v2) + (k * b)) + -v2 * (a *k * l * (k * v1 + l *k * v2) + k *l * b) + v1 * A1 + v2*A2
print 'two outcomes dependent case - Equation G.6: ', \
    simplify(solve([diff(L, v1) - diff(L, v2)], v1)[v1].subs(A1, A2))
