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

%-cross(diff(x, t), B) == diff(diff(x,t),t) eular-lagrange conditions

V = diff(x, t);

%calculating the path
aPath = subs(A, [w1 , w2], [x(1), x(2)]);

% transversality condition, and that the starting point is zero
sol = solve((m * subs(V, t, T) ) == subs(aPath, t, T), subs(x, t, 0) == [0,0,0], [x0, y0, alpha, v0]);

% finding the duration for this the path is only an arc ( v1 >0 and v2 > 0)
solve(sol.y0(1) ==0, T)
% the second solution (which is admissible) is equivalent to m * atan(1/l) / (l - 1);

gSub = subs(x, [alpha, v0, x0, y0], [sol.alpha(1), sol.v0(1), sol.x0(1), sol.y0(1)]);

%%% for calculating the speed in the first part of the path (when it has two segments)
%speed at the begining of the circular segment
simplify(subs(sol.v0, T, m * atan(1/l) / (l - 1)))

% the motivational drive at the begining of the second segment of the path
% is: (H - a1 * T), where a1 is the constant speed in the first segment of
% the path
a1 = sym('a1','real');
subs(solve((H - a1 * T) * (l - 1) / m == a1, a1), T - m * atan(1/l) / (l - 1))

%%%%%%%%%%%%%%% not part of the paper %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%l = 1.1;
%m = 1;
a1 = sym('a1','real');
v = diff(gSub, t);
aa = subs(A, [w1 , w2], [gSub(1), gSub(2)]);
S = v(1) * aa(1) + v(2) * aa(2) - 1/2 * m * v(1) ^ 2 - 1/2 * m * v(2) ^ 2;
S = subs(S, T, m * atan(1/l) / (l - 1));
S = int(S, t);
assume(l > 1);
arcS = simplify(subs(S, t, m * atan(1/l) / (l - 1)) - subs(S, t, 0));
% arcS = H^2 * (-(l - (l^2 + 1)^(1/2))/(l^2 + 1)^(1/2));
totalValue = - T*((m*a1^2)/2 - H*l*a1) - (T^2*a1^2*l)/2 + subs(arcS, H, H - a1 * T);
assume(l > 1)
assume(H > 0)
assume(m > 0)
optV = simplify(solve(diff(totalValue, a1) == 0, a1))
double(subs(optV, [m, H, l, T], [1.111, 2, 1.21, 6]))
double(subs(H * (l -1) / (m + T * (l-1)), [m, H, l, T], [1.111, 2, 1.21, 6]))
%%%%%

simH = 2;
simL = 1.11;
simT = 2;
simM = 0.0001;
simT =  simM * atan(1/simL) / (simL - 1) ;
simT1 =  -(simM * ( atan(simL - (simL^2 + 1)^(1/2))))/(simL/2 - 1/2);

double(subs(sol.y0(1), [T, H, l, m], [simT, simH, simL, simM]))

g = subs(gSub, [T, H, l, m], [simT, simH, simL, simM]);
ezplot(g(1) ,g(2), [0, simT])
hold on
