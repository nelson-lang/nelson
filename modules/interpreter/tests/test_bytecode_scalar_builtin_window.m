%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% Scalar math builtins called inside fused typed windows must give the result
% of the function call: same values, fallback for complex results and other
% classes, and a variable of the same name still takes precedence.
%=============================================================================
x = linspace(-3, 3, 13);
u = unary_all(x);
assert_isequal(u(1, :), abs(x));
assert_isequal(u(3, :), ceil(x));
assert_isequal(u(6, :), fix(x));
assert_isequal(u(7, :), floor(x));
assert_isequal(u(12, :), round(x));
p = abs(x) + 0.25;
tol = 1e-12;
assert_isapprox(u(2, :), atan(x), tol);
assert_isapprox(u(4, :), cos(x), tol);
assert_isapprox(u(5, :), exp(x), tol);
assert_isapprox(u(8, :), gamma(p), tol);
assert_isapprox(u(9, :), log(p), tol);
assert_isapprox(u(10, :), log2(p), tol);
assert_isapprox(u(11, :), log10(p), tol);
assert_isapprox(u(13, :), sin(x), tol);
assert_isapprox(u(14, :), sqrt(p), tol);
assert_isapprox(u(15, :), tan(x), tol);
y = linspace(2, -2, 13);
b = binary_all(x, y);
assert_isapprox(b(1, :), atan2(x, y), tol);
assert_isequal(b(2, :), max(x, y));
assert_isequal(b(3, :), min(x, y));
assert_isapprox(b(4, :), mod(x, 1.5), tol);
assert_isapprox(b(5, :), rem(x, 1.5), tol);
%=============================================================================
% NaN and infinite inputs
b = binary_all([NaN 1], [2 NaN]);
assert_isequal(b(2, :), max([NaN 1], [2 NaN]));
assert_isequal(b(3, :), min([NaN 1], [2 NaN]));
u = unary_all([Inf -Inf NaN]);
assert_isequal(u(7, :), floor([Inf -Inf NaN]));
%=============================================================================
% a complex result leaves the typed window
assert_isequal(sum_sqrt(-2:2), sum(sqrt(-2:2)));
assert_isequal(sum_log(-1:1), sum(log(-1:1)));
%=============================================================================
% other classes take the function call
assert_isequal(min_running(int8(1:5)), int8(-5));
assert_isequal(class(min_running(int8(1:5))), 'int8');
assert_isequal(class(abs_single(single(-2))), 'single');
%=============================================================================
% builtins in conditions and indexed stores
assert_isequal(count_far(10), 5);
assert_isequal(fill_abs(4), [1 2 3 4]);
%=============================================================================
% a variable of the same name created while the loop runs is indexed
assert_isequal(shadow_after_eval(), [1 2 30 40]);
assert_isequal(shadow_sqrt_after_eval(), [1 sqrt(2) sqrt(3) 400 500 600]);
% a global declared while the loop runs is indexed too
define_global_abs();
assert_isequal(shadow_global_abs(), [1 20 30]);
clear global abs
abs_values = 0;
for k = -2:2
  abs_values = abs_values + abs(k);
end
assert_isequal(abs_values, 6);
%=============================================================================
function r = unary_all(x)
  r = zeros(15, numel(x));
  for k = 1:numel(x)
    t = x(k);
    u = abs(t) + 0.25;
    r(1, k) = abs(t);
    r(2, k) = atan(t);
    r(3, k) = ceil(t);
    r(4, k) = cos(t);
    r(5, k) = exp(t);
    r(6, k) = fix(t);
    r(7, k) = floor(t);
    r(8, k) = gamma(u);
    r(9, k) = log(u);
    r(10, k) = log2(u);
    r(11, k) = log10(u);
    r(12, k) = round(t);
    r(13, k) = sin(t);
    r(14, k) = sqrt(u);
    r(15, k) = tan(t);
  end
end
%=============================================================================
function r = binary_all(x, y)
  r = zeros(5, numel(x));
  for k = 1:numel(x)
    a = x(k);
    c = y(k);
    r(1, k) = atan2(a, c);
    r(2, k) = max(a, c);
    r(3, k) = min(a, c);
    r(4, k) = mod(a, 1.5);
    r(5, k) = rem(a, 1.5);
  end
end
%=============================================================================
function s = sum_sqrt(v)
  s = 0;
  for k = 1:numel(v)
    s = s + sqrt(v(k));
  end
end
%=============================================================================
function s = sum_log(v)
  s = 0;
  for k = 1:numel(v)
    s = s + log(v(k));
  end
end
%=============================================================================
function n = min_running(v)
  n = v(1) - v(1);
  for k = 1:numel(v)
    n = min(n, -v(k));
  end
end
%=============================================================================
function r = abs_single(v)
  r = abs(v);
end
%=============================================================================
function c = count_far(n)
  c = 0;
  for k = 1:n
    if abs(k - 5) > 2
      c = c + 1;
    end
  end
end
%=============================================================================
function r = fill_abs(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = abs(-k);
  end
end
%=============================================================================
function r = shadow_after_eval()
  r = zeros(1, 4);
  for k = 1:4
    if k == 3
      eval('abs = [10 20 30 40];'); %#ok<NLS0022,NLS0034>
    end
    r(k) = abs(k);
  end
end
%=============================================================================
function r = shadow_sqrt_after_eval()
  r = zeros(1, 6);
  for k = 1:6
    if k == 3
      assignin('caller', 'unused_by_this_test', 1); %#ok<NLS0022>
    end
    if k == 4
      eval('sqrt = [100 200 300 400 500 600];'); %#ok<NLS0022,NLS0034>
    end
    r(k) = sqrt(k);
  end
end
%=============================================================================
function define_global_abs()
  global abs %#ok<NLS0034>
  abs = [10 20 30];
end
%=============================================================================
function r = shadow_global_abs()
  r = zeros(1, 3);
  for k = 1:3
    if k == 2
      eval('global abs'); %#ok<NLS0022>
    end
    r(k) = abs(k);
  end
end
%=============================================================================
