%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% Fused scalar expression windows (SCALAR_EXPR) must keep exact interpreter
% semantics, including the fallback when values are not real double scalars.
%=============================================================================
% basic fused arithmetic on doubles
s = 0;
for k = 1:100
  s = s + k * 2 - 1;
end
asserts.isequal(s, 10000);
%=============================================================================
% division and unary minus
v = 1;
for k = 1:10
  v = -v / 2 + 1;
end
asserts.isapprox(v, 0.666992187500000, 1e-12);
%=============================================================================
% fallback: variable switches to a matrix mid-loop, semantics must follow
a = 0;
for k = 1:10
  if k == 5
    a = [1 2; 3 4];
  end
  a = a + k * 1.5 - 0.5;
end
asserts.isequal(class(a), 'double');
asserts.isequal(size(a), [2 2]);
asserts.isequal(a, [65.5 66.5; 67.5 68.5]);
%=============================================================================
% fallback: complex values
c = 1i;
for k = 1:5
  c = c + k * 2.0 - 1.0;
end
asserts.isequal(c, 25 + 1i);
%=============================================================================
% fallback: integer types keep integer arithmetic (saturation semantics)
n = int8(100);
n = n + n * 2 - 1;
asserts.isequal(class(n), 'int8');
asserts.isequal(n, int8(126));
%=============================================================================
% fallback: single precision preserved
f = single(0.5);
for k = 1:4
  f = f + f * 2.0 - 0.25;
end
asserts.isequal(class(f), 'single');
asserts.isapprox(double(f), 30.5, 1e-6);
%=============================================================================
% NaN / Inf propagation identical to plain evaluation
x = 0;
x = x + 1 / 0 - 2;
asserts.isequal(x, Inf);
y = 0;
y = y + 0 / 0 + 1;
asserts.istrue(isnan(y));
%=============================================================================
% mixed nested expressions with parentheses
p = 2;
q = 3;
r = 0;
for k = 1:50
  r = r + (p * k - q) / 2 + (k - 1) * 0.25;
end
asserts.isapprox(r, 1506.25, 1e-9);
%=============================================================================
% loop variable participates and stays fresh each iteration
t = 0;
for k = 1:1000
  t = t + k * 1.0 - 0.5;
end
asserts.isequal(t, 500000);
%=============================================================================
% profiler on: fused fast path disabled, results identical
profile('clear');
profile('on');
sp = 0;
for k = 1:100
  sp = sp + k * 2 - 1;
end
profile('off');
profile('clear');
asserts.isequal(sp, 10000);
%=============================================================================
% expression result feeding further generic code
w = 0;
for k = 1:20
  w = w + k * 3 - 2;
  if w > 100
    w = w - 100;
  end
end
asserts.isequal(w, 90);
%=============================================================================
% fused comparison driving a while header
k = 0;
n = 0;
while k < 1000
  k = k + 1;
  n = n + 2;
end
asserts.isequal(k, 1000);
asserts.isequal(n, 2000);
%=============================================================================
% every comparison operator, as an if condition over an expression
counts = zeros(1, 6);
for k = 1:20
  if k * 2 < 10
    counts(1) = counts(1) + 1;
  end
  if k * 2 <= 10
    counts(2) = counts(2) + 1;
  end
  if k * 2 > 30
    counts(3) = counts(3) + 1;
  end
  if k * 2 >= 30
    counts(4) = counts(4) + 1;
  end
  if k * 2 == 10
    counts(5) = counts(5) + 1;
  end
  if k * 2 ~= 10
    counts(6) = counts(6) + 1;
  end
end
asserts.isequal(counts, [4 5 5 6 1 19]);
%=============================================================================
% comparison fallback: NaN compares false for every operator except ~=
bad = 0 / 0;
asserts.isfalse(bad < 1);
asserts.isfalse(bad <= 1);
asserts.isfalse(bad > 1);
asserts.isfalse(bad >= 1);
asserts.isfalse(bad == 1);
asserts.istrue(bad ~= 1);
nanTaken = 0;
while bad < 1
  nanTaken = nanTaken + 1;
  break;
end
asserts.isequal(nanTaken, 0);
%=============================================================================
% comparison fallback: non-double-scalar operands keep generic semantics
m = [1 2; 3 4];
if m * 2 > 100
  hit = 1;
else
  hit = 0;
end
asserts.isequal(hit, 0);
if m * 2 < 100
  allSmall = 1;
else
  allSmall = 0;
end
asserts.isequal(allSmall, 1);
%=============================================================================
% comparison on integer and single operands
i8 = int8(20);
asserts.istrue(i8 * 2 > 30);
sg = single(1.5);
asserts.istrue(sg * 2 >= 3);
%=============================================================================
% short-circuit operands: comparisons feeding && and ||
andHits = 0;
orHits = 0;
for k = 1:10
  if k > 3 && k < 7
    andHits = andHits + 1;
  end
  if k < 2 || k > 9
    orHits = orHits + 1;
  end
end
asserts.isequal(andHits, 3);
asserts.isequal(orHits, 2);
%=============================================================================
% Ctrl-C responsiveness relies on the loop backedge poll surviving fusion:
% a fused while condition must still terminate normally.
guard = 0;
while guard < 50000
  guard = guard + 1;
end
asserts.isequal(guard, 50000);
%=============================================================================
% profiler on: fused comparison path disabled, results identical
profile('clear');
profile('on');
pk = 0;
while pk < 100
  pk = pk + 1;
end
profile('off');
profile('clear');
asserts.isequal(pk, 100);
%=============================================================================
% indexed reads inside a fused window
xr = (1:1000) * 0.5;
acc = 0;
for k = 1:1000
  acc = acc + xr(k) * 2 - 1;
end
asserts.isequal(acc, 499500);
%=============================================================================
% indexed read fallback: non-double array keeps its own arithmetic
xi = int8([10 20 30]);
si = int8(0);
for k = 1:3
  si = si + xi(k) * 2;
end
asserts.isequal(class(si), 'int8');
asserts.isequal(si, int8(120));
%=============================================================================
% indexed read fallback: complex array
xc = [1 + 1i, 2 + 2i];
sc = 0;
for k = 1:2
  sc = sc + xc(k) * 1;
end
asserts.isequal(sc, 3 + 3i);
%=============================================================================
% indexed read fallback: the array becomes a matrix mid-loop
xm = [1 2 3];
sm = 0;
for k = 1:3
  if k == 2
    xm = [10 20 30; 40 50 60];
  end
  sm = sm + xm(k) * 1;
end
asserts.isequal(sm, 61);
%=============================================================================
% indexed read of a two-dimensional array through a linear index
x2 = [1 2; 3 4];
s2 = 0;
for k = 1:4
  s2 = s2 + x2(k) * 1;
end
asserts.isequal(s2, 10);
%=============================================================================
% indexed read reflects writes made in the same loop
xw = zeros(1, 5);
sw = 0;
for k = 1:5
  xw(k) = k * 2;
  sw = sw + xw(k) * 1;
end
asserts.isequal(sw, 30);
%=============================================================================
% profiler on: fused indexed read disabled, results identical
profile('clear');
profile('on');
xp = [1 2 3];
sp2 = 0;
for k = 1:3
  sp2 = sp2 + xp(k) * 2;
end
profile('off');
profile('clear');
asserts.isequal(sp2, 12);
%=============================================================================
% indexed reads with constant offsets inside a fused window
xo = (1:8) * 0.5;
so = 0;
for k = 2:8
  so = so + xo(k - 1) * 2 - 1;
end
asserts.isequal(so, 21);
%=============================================================================
% fused indexed expression result feeding an indexed assignment
xs = 1:8;
ys = zeros(1, 8);
for k = 2:8
  ys(k) = xs(k - 1) + 0.5 * (xs(k) - xs(k - 1));
end
asserts.isequal(ys, [0 1.5 2.5 3.5 4.5 5.5 6.5 7.5]);
%=============================================================================
% fused indexed assignment updates the value used by later indexed reads
zf = zeros(1, 10);
for k = 2:10
  zf(k) = zf(k - 1) + k * 0.25 - 0.5;
end
asserts.isequal(zf, [0 0 0.25 0.75 1.5 2.5 3.75 5.25 7 9]);
%=============================================================================
% indexed assignment fallback: non-double target preserves its type semantics
zi = int8(zeros(1, 4));
for k = 1:4
  zi(k) = k * 40 - 10;
end
asserts.isequal(class(zi), 'int8');
asserts.isequal(zi, int8([30 70 110 127]));
%=============================================================================
% indexed assignment fallback: complex target remains complex
zc = complex(zeros(1, 3));
for k = 1:3
  zc(k) = k * 2 + 1i;
end
asserts.isequal(zc, [2 + 1i 4 + 1i 6 + 1i]);
%=============================================================================
% indexed reads with a local offset inside a fused indexed assignment
delay = 3;
xe = zeros(1, 10);
xe(1:3) = [1 2 3];
for k = delay + 1:10
  xe(k) = xe(k) + 0.5 * xe(k - delay);
end
asserts.isequal(xe, [1 2 3 0.5 1 1.5 0.25 0.5 0.75 0.125]);
%=============================================================================
% 2-D shifted local slices used by graphics fluid/Jacobi style workloads
q = zeros(6, 6);
rhs = reshape(1:16, 4, 4);
I = 2:5;
J = 2:5;
for iteration = 1:3
  q(I, J) = (q(I - 1, J) + q(I + 1, J) + q(I, J - 1) + q(I, J + 1)) / 4 + rhs;
end
expectedQ = zeros(6, 6);
expectedQ(2:5, 2:5) = [ ...
  4.375 12.6875 20.9375 22.875; ...
  7.25 17.875 28.375 30; ...
  9.3125 20.5 31 32.0625; ...
  9 18.375 26.625 27.5];
asserts.isapprox(q, expectedQ, 1e-12);
%=============================================================================
% vector linear read/write fast paths preserve shape and repeated-index order
linearA = reshape(1:12, 3, 4);
linearIdx = [3 1; 6 12];
linearRead = linearA(linearIdx);
asserts.isequal(size(linearRead), size(linearIdx));
asserts.isequal(linearRead, [3 1; 6 12]);
linearA([1 3 1]) = [10 20 30];
asserts.isequal(linearA(1), 30);
asserts.isequal(linearA(3), 20);
%=============================================================================
% logical-mask linear read/write fast paths preserve orientation and assignment
% cardinality.
maskA = 1:5;
mask = [true false true false true];
maskRead = maskA(mask);
asserts.isequal(maskRead, [1 3 5]);
maskA(mask) = [10 20 30];
asserts.isequal(maskA, [10 2 20 4 30]);
maskColumnSource = (1:5)';
maskColumn = mask';
maskColumnRead = maskColumnSource(maskColumn);
asserts.isequal(size(maskColumnRead), [3 1]);
asserts.isequal(maskColumnRead, [1; 3; 5]);
maskA([false false false false false]) = 99;
asserts.isequal(maskA, [10 2 20 4 30]);
%=============================================================================
