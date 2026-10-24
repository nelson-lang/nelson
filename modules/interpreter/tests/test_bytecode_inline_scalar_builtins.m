%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--REUSE PROCESS-->
%=============================================================================
x = 0.25;
R = abs(-x) + sin(x) + cos(x) + tan(x) + exp(x) + log(x) + log2(x) + log10(x) + gamma(x) + atan(x) + floor(x) + ceil(x) + fix(-1.9) + round(2.5) + sqrt(9);
REF = abs(-0.25) + sin(0.25) + cos(0.25) + tan(0.25) + exp(0.25) + log(0.25) + log2(0.25) + log10(0.25) + gamma(0.25) + atan(0.25) + floor(0.25) + ceil(0.25) + fix(-1.9) + round(2.5) + sqrt(9);
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
asserts.isequal(abs(-3), 3);
asserts.isequal(fix(-1.9), -1);
asserts.isequal(round(2.5), 3);
asserts.isequal(sqrt(9), 3);
asserts.isequal(log2(8), 3);
asserts.isequal(log10(1000), 3);
asserts.isapprox(tan(0.25), sin(0.25) / cos(0.25), 1e-12);
%=============================================================================
asserts.isequal(gamma(172), Inf);
asserts.isequal(gamma(-2), Inf);
%=============================================================================
R = log(-1);
asserts.isfalse(isreal(R));
asserts.isapprox(abs(R), pi, 1e-12);
R = sqrt(-1);
asserts.isfalse(isreal(R));
asserts.isapprox(abs(R), 1, 1e-12);
R = log2(-8);
asserts.isfalse(isreal(R));
asserts.isapprox(abs(R), abs(log2(-8)), 1e-12);
R = log10(-10);
asserts.isfalse(isreal(R));
asserts.isapprox(abs(R), abs(log10(-10)), 1e-12);
%=============================================================================
asserts.isequal(min(7, 3), 3);
asserts.isequal(max(7, 3), 7);
asserts.isequal(min(NaN, 3), 3);
asserts.isequal(max(NaN, 3), 3);
asserts.isequal(min(NaN, NaN), NaN);
asserts.isequal(max(NaN, NaN), NaN);
asserts.isequal(1 / min(-0, 0), -Inf);
asserts.isequal(1 / min(0, -0), -Inf);
asserts.isequal(1 / max(-0, 0), Inf);
asserts.isequal(1 / max(0, -0), Inf);
%=============================================================================
asserts.isequal(mod(160001, 3), 2);
asserts.isequal(mod(160001, 997), 481);
asserts.isequal(mod(5.5, 2), 1.5);
asserts.isequal(mod(-6, 4), 2);
asserts.isequal(mod(6, 0), 6);
asserts.isequal(mod(0.94, 0.01), 0);
asserts.isequal(rem(7, 3), 1);
asserts.isequal(rem(-7, 3), -1);
asserts.isequal(rem(7, 0), NaN);
asserts.isapprox(atan2(1, -1), 3 * pi / 4, 1e-12);
%=============================================================================
hits = 0;
for p = 2:10
  if mod(120, p) == 0
    hits = hits + 1;
  end
end
asserts.isequal(hits, 7);
misses = 0;
for p = 2:10
  if mod(121, p) ~= 0
    misses = misses + 1;
  end
end
asserts.isequal(misses, 9);
%=============================================================================
eval('sin = [10 20 30]; asserts.isequal(sin(2), 20); clear sin');
eval('floor = [7 8 9]; asserts.isequal(floor(3), 9); clear floor');
eval('sqrt = [9 16 25]; asserts.isequal(sqrt(2), 16); clear sqrt');
eval('mod = [4 5 6]; asserts.isequal(mod(2), 5); clear mod');
eval('rem = [3 6 9]; asserts.isequal(rem(3), 9); clear rem');
eval('atan2 = [2 4 6]; asserts.isequal(atan2(2), 4); clear atan2');
%=============================================================================
asserts.isequal(mod(15, 4), 3);
eval('mod = [11 22 33]; asserts.isequal(mod(2), 22); clear mod');
asserts.isequal(mod(15, 4), 3);
asserts.isequal(rem(15, 4), 3);
eval('rem = [11 22 33]; asserts.isequal(rem(2), 22); clear rem');
asserts.isequal(rem(15, 4), 3);
%=============================================================================
eval('mod = [8 9 10];');
asserts.isequal(mod(2), 9);
eval('clear mod');
asserts.isequal(mod(15, 4), 3);
%=============================================================================
n = 160001;
p = 2;
hits = 0;
while p <= 64
  if mod(n, p) == 0
    hits = hits + 1;
  end
  p = p + 1;
end
asserts.isequal(hits, 0);
asserts.isequal(p, 65);
%=============================================================================
n = 25;
p = 2;
misses = 0;
while p <= 5
  if mod(n, p) ~= 0
    misses = misses + 1;
  end
  p = p + 1;
end
asserts.isequal(misses, 3);
%=============================================================================
n = 24;
p = 2;
hits = 0;
while p <= 8
  if rem(n, p) == 0
    hits = hits + 1;
  end
  p = p + 1;
end
asserts.isequal(hits, 5);
%=============================================================================
mod = zeros(4, 4);
n = 2;
p = 3;
hits = 0;
if mod(n, p) == 0
  hits = hits + 1;
end
asserts.isequal(hits, 1);
clear mod;
%=============================================================================
