%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Scalar fast paths of power, logical operators, colon ranges and && / ||
% branch conditions must give the generic operator result, class and error.
%=============================================================================
% power: bit for bit the element-wise power with a scalar base
bases = [0, -0, 0.5, 1, 2, 3.7, 1e3, Inf, NaN];
exponents = [0, 1, 2, 3, -1, -2, 0.5, 2.5, 64, 65, 1e3, Inf, -Inf, NaN];
[fused, direct] = power_table(bases, exponents);
for i = 1:numel(bases)
  for j = 1:numel(exponents)
    expected = typecast(bases(i) .^ [exponents(j), exponents(j)], 'uint64');
    assert_isequal(typecast(fused(i, j), 'uint64'), expected(1));
    assert_isequal(typecast(direct(i, j), 'uint64'), expected(1));
  end
end
assert_isequal(sum_squares(4), 30);
assert_isequal(1 / (-0) ^ 3, -Inf);
%=============================================================================
% power: a negative base or another class keeps the generic result
assert_isequal(negative_powers(), [4, -8, 0.25]);
r = negative_power_loop(-8, 1 / 3);
assert_isfalse(isreal(r));
assert_isapprox(r, (-8) ^ (1 / 3), 1e-14);
assert_isequal(class(single(2) ^ 2), 'single');
assert_isequal(class(int8(3) ^ 2), 'int8');
assert_isequal(int8(3) ^ 2, int8(9));
%=============================================================================
% logical operators on logical scalars
assert_isequal(not_loop(5), true);
assert_isequal(class(not_loop(4)), 'logical');
assert_isequal(and_or_loop(10), [7, 10]);
t = true;
f = false;
assert_isequal(t & 1, true);
assert_isequal(0 | f, false);
assert_isequal(class(t & 2), 'logical');
assert_isequal(~t, false);
assert_istrue(issparse(sparse(true) & true));
assert_istrue(issparse(~sparse(true)));
% a NaN operand cannot be converted to logical, in a loop as in a statement
msg = 'Conversion to logical with NaN is not possible.';
assert_checkerror('r = NaN & true;', msg);
assert_checkerror('r = false | NaN;', msg);
assert_checkerror('r = ~NaN;', msg);
assert_isequal(nan_in_loop_message(), msg);
%=============================================================================
% && and || as branch conditions
assert_isequal(branch_truth_table(), [0 0 0 1; 0 1 1 1; 0 0 0 1; 0 0 0 1; 0 1 1 1]);
assert_isequal(while_and(10), 4);
assert_isequal(while_or(10), 10);
assert_isequal(short_circuit_skips(), [1 1]);
assert_checkerror('if 1 && [1 2], end', ...
  'Operand to && operator must be convertible to logical scalar values.');
assert_checkerror('if [1 2] && 1, end', ...
  'Operand to && operator must be convertible to logical scalar values.');
assert_checkerror('if 0 || [1 2], end', ...
  'Operand to || operator must be convertible to logical scalar values.');
assert_checkerror('while 1 && [1 2], end', ...
  'Operand to && operator must be convertible to logical scalar values.');
assert_isequal(and_condition_message(1, [1 2]), ...
  'Operand to && operator must be convertible to logical scalar values.');
assert_isequal(or_condition_message(0, [1 2]), ...
  'Operand to || operator must be convertible to logical scalar values.');
%=============================================================================
% colon ranges of two double scalars
assert_isequal(colon_loop(3), [1 2 3]);
assert_isequal(size(colon_pair(5, 1)), [1 0]);
assert_isequal(colon_pair(0.5, 3), [0.5 1.5 2.5]);
assert_isequal(colon_pair(-2, 1), [-2 -1 0 1]);
assert_isequal(class(colon_pair(1, 3)), 'double');
assert_isequal(class(int8(1):int8(3)), 'int8');
assert_isequal(size(colon_pair(NaN, 3)), [1 1]);
%=============================================================================
% scalar builtins with one and two inputs at separate call sites
assert_isequal(round_both(2.567), [3, 2.57]);
assert_isequal(max_both([1 9 4]), [9, 5]);
%=============================================================================
% builtins taking the evaluator still compute their result and see their caller
assert_isequal(neutral_builtins(), {zeros(1, 2), ones(2, 'int8'), 7, true, int16(3), ...
  [1 2 3 4], [3 2 1], 5, [1 1], 6});
assert_isequal(evalin_after_neutral(), 7);
%=============================================================================
% anonymous functions called repeatedly keep their results
assert_isequal(anonymous_repeated(50), 50 * 51 / 2 + 50 * 2);
%=============================================================================
% a builtin input class overload found on the path is dispatched
d = fullfile(tempdir(), ['nelson_ovl_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@double'));
fid = fopen(fullfile(d, '@double', 'ndims.m'), 'wt');
fprintf(fid, 'function r = ndims(x)\n  r = 42;\nend\n');
fclose(fid);
assert_isequal(ndims_loop(3), [2 2 2]);
addpath(d);
assert_isequal(ndims_loop(3), [42 42 42]);
rmpath(d);
clear functions
assert_isequal(ndims_loop(3), [2 2 2]);
rmdir(d, 's');
%=============================================================================
function [fused, direct] = power_table(bases, exponents)
  fused = zeros(numel(bases), numel(exponents));
  direct = fused;
  for i = 1:numel(bases)
    for j = 1:numel(exponents)
      a = bases(i);
      b = exponents(j);
      fused(i, j) = a ^ b;
      direct(i, j) = a .^ b;
    end
  end
end
%=============================================================================
function s = sum_squares(n)
  s = 0;
  for k = 1:n
    s = s + k ^ 2;
  end
end
%=============================================================================
function r = negative_powers()
  r = zeros(1, 3);
  b = [2, 3, -2];
  for k = 1:3
    r(k) = (-2) ^ b(k);
  end
end
%=============================================================================
function r = negative_power_loop(a, b)
  r = 0;
  for k = 1:2
    r = a ^ b;
  end
end
%=============================================================================
function x = not_loop(n)
  x = false;
  for k = 1:n
    x = ~x;
  end
end
%=============================================================================
function r = and_or_loop(n)
  a = 0;
  o = 0;
  for k = 1:n
    if (k > 3) & (k < n + 1)
      a = a + 1;
    end
    if (k > 3) | (k < 4)
      o = o + 1;
    end
  end
  r = [a, o];
end
%=============================================================================
function r = branch_truth_table()
  r = zeros(5, 4);
  values = [0 0; 0 1; 1 0; 1 1];
  for k = 1:4
    r(:, k) = [branch_plain(values(k, 1), values(k, 2)); branch_nested(values(k, 1), values(k, 2))];
  end
end
%=============================================================================
function r = branch_plain(a, b)
  r = zeros(3, 1);
  if a && b
    r(1) = 1;
  end
  if a || b
    r(2) = 1;
  end
  if ~(a && b)
    r(3) = 0;
  elseif a && b
    r(3) = 1;
  end
end
%=============================================================================
function r = branch_nested(a, b)
  r = zeros(2, 1);
  if (a && b) || (a && b)
    r(1) = 1;
  end
  if (a || b) && (b || a)
    r(2) = 1;
  end
end
%=============================================================================
function n = while_and(limit)
  n = 0;
  k = 0;
  while k < limit && n < 4
    k = k + 1;
    n = n + 1;
  end
end
%=============================================================================
function n = while_or(limit)
  n = 0;
  while n < 2 || n < limit
    n = n + 1;
  end
end
%=============================================================================
function r = short_circuit_skips()
  r = [0 0];
  if false && error('not evaluated')
    r(1) = 2;
  else
    r(1) = 1;
  end
  if true || error('not evaluated')
    r(2) = 1;
  end
end
%=============================================================================
function r = and_condition_message(a, b)
  r = '';
  try
    if a && b
      r = 'in';
    end
  catch e
    r = e.message;
  end
end
%=============================================================================
function r = or_condition_message(a, b)
  r = '';
  try
    if a || b
      r = 'in';
    end
  catch e
    r = e.message;
  end
end
%=============================================================================
function r = nan_in_loop_message()
  r = '';
  values = [1 0 NaN];
  try
    for k = 1:3
      if values(k) >= 0 || values(k)
        r = 'in';
      end
    end
  catch e
    r = e.message;
  end
end
%=============================================================================
function r = colon_loop(n)
  r = [];
  for k = 1:2
    r = 1:n;
  end
end
%=============================================================================
function r = colon_pair(a, b)
  r = a:b;
end
%=============================================================================
function r = round_both(x)
  r = zeros(1, 2);
  for k = 1:2
    r(1) = round(x);
    r(2) = round(x, 2);
  end
end
%=============================================================================
function r = max_both(v)
  r = zeros(1, 2);
  for k = 1:2
    r(1) = max(v);
    r(2) = max(k, 5);
  end
end
%=============================================================================
function r = neutral_builtins()
  a = 3;
  b = 4;
  t0 = tic();
  r = {zeros(1, 2), ones(2, 'int8'), max(a, 7), isequal(a, 3), cast(a, 'int16'), ...
    cat(2, [1 2], [a b]), fliplr([1 2 3]), hypot(a, b), diff([1 2 3]), gcd(12, 18)};
  assert_istrue(toc(t0) >= 0);
end
%=============================================================================
function r = evalin_after_neutral()
  a = 1; %#ok<NLS0004>
  z = zeros(1, 2); %#ok<NLS0004>
  a = 7; %#ok<NLS0004>
  r = peek_caller('a');
end
%=============================================================================
function v = peek_caller(name)
  v = evalin('caller', name); %#ok<NLS0022>
end
%=============================================================================
function s = anonymous_repeated(n)
  offset = 2;
  f = @(x) x + offset;
  g = @(x) eval('x');
  s = 0;
  for k = 1:n
    s = s + f(k) + g(0);
  end
end
%=============================================================================
function r = ndims_loop(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = ndims(5);
  end
end
%=============================================================================
