clear
% two actions rotational
w0 = sym('w0','real');
w1 = sym('w1','real');
w2 = sym('w2','real');
w3 = sym('w3','real');
alpha = sym('alpha','real');
T = sym('T','real');
l = sym('l','real');
H = sym('H','real');
t = sym('t','real');
x0 = sym('x0','real');
y0 = sym('y0','real');
v0 = sym('v0','real');
m = sym('m','real');
m2 = sym('m2','real');
w = sym('w','real');
%defining fields

D3 = H - w1 - w2;
A = [D3, l*D3, 0];
phi = 0;
B = curl(A, [w1, w2, w3]);
E = diff(A, w0) + gradient(phi, [w1, w2, w3])' ;
w = B(3);
w = (l -1) / m;

x = [x0 + v0 / w * sin(w * t + alpha) , y0 + v0/w * cos(w * t + alpha), 0];
V = diff(x, t);

%calculating the trajectory in the outcome space
aPath = subs(A, [w1 , w2], [x(1), x(2)]);

% enforcing the transversality condition, and that the starting point is zero
sol = solve((m * subs(V, t, T) ) == subs(aPath, t, T), subs(x, t, 0) == [0,0,0], [x0, y0, alpha, v0]);
% use sol.alpha, sol.v0, sol.x0, sol.y0 for drawing the path


% finding the duration for this the path is only an arc ( v1 >0 and v2 > 0) for Equation H.12 in the paper
solve(sol.y0(1) ==0, T)
% the second solution (which is admissible) is equivalent to T_c = m * atan(1/l) / (l - 1) as in H.12 in the paper

%%% for calculating the speed in the first part of the path (when it has two segments)

%speed at the begining of the circular segment, which is equal to H_c * (l - 1) / m
simplify(subs(sol.v0, T, m * atan(1/l) / (l - 1)))

% the motivational drive, H_c, in the above equation at the begining of the second segment of the path
% is: (H - a1 * T), where a1 is the constant speed in the first segment of
% the path. Enforcing that the speed at the beginning of the circular path should
% be equal to the speed at the end of constant path, we have:

a1 = sym('a1','real');
subs(solve((H - a1 * T) * (l - 1) / m == a1, a1), T - m * atan(1/l) / (l - 1))
