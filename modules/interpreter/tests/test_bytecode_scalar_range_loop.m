%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter'), '/tests']);
[a, b, c, k] = scalar_range_loop_values(1, 1, 5, 0, 1);
asserts.isequal([a, b, c, k], [5, 8, 8, 5]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 50, 0, 1);
asserts.isequal([a, b, c, k], [12586269025, 20365011074, 20365011074, 50]);
[a, b, c, k] = scalar_range_loop_values(5, -2, 1, 0, 1);
asserts.isequal([a, b, c, k], [2, 3, 3, 1]);
[a, b, c, k] = scalar_range_loop_values(0.5, 0.25, 1, 0, 1);
asserts.isequal([a, b, c, k], [2, 3, 3, 1]);
[a, b, c, k] = scalar_range_loop_values(2, 1, 1, 0, 1);
asserts.isequal([a, b, c, k], [0, 1, 0, 42]);
[a, b, c, k] = scalar_range_loop_values(1, 0, 5, 0, 1);
asserts.isequal([a, b, c, k], [0, 1, 0, 42]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 3, single(0), single(1));
asserts.isequal(class(a), 'single');
asserts.isequal([a, b, c], single([2, 3, 3]));
[a, b, c, k] = scalar_range_loop_values(1, 1, 35, single(0), single(1));
asserts.isequal([a, b, c], single([9227465, 14930352, 14930352]));
[a, b, c, k] = scalar_range_loop_values(1, 1, 3, false, true);
asserts.isequal([a, b, c, k], [2, 3, 3, 3]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 35, false, true);
asserts.isequal([a, b, c, k], [9227465, 14930352, 14930352, 35]);
A = [1, 2; 3, 4];
B = [5, 6; 7, 8];
[a, b, c, k] = scalar_range_loop_values(1, 1, 3, A, B);
asserts.isequal(a, A + 2 * B);
asserts.isequal(b, 2 * A + 3 * B);
asserts.isequal(c, b);
[a, b, c, k] = scalar_range_loop_values(1, 1, 35, A, B);
asserts.isequal(a, 5702887 * A + 9227465 * B);
asserts.isequal(b, 9227465 * A + 14930352 * B);
asserts.isequal(c, b);
[a, b, c, k] = scalar_range_loop_values(1, 1, 3, 1i, 1);
asserts.isequal([a, b, c], [2 + 1i, 3 + 2i, 3 + 2i]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 35, 1i, 1);
asserts.isequal([a, b], [9227465 + 5702887i, 14930352 + 9227465i]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 35, sparse(0), sparse(1));
asserts.istrue(issparse(a) && issparse(b) && issparse(c));
asserts.isequal([full(a), full(b), full(c)], [9227465, 14930352, 14930352]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 2000, 0, 1);
asserts.isequal([a, b, c, k], [Inf, Inf, Inf, 2000]);
[a, b, c, k] = scalar_range_loop_values(1, 1, 3, NaN, 1);
asserts.istrue(all(isnan([a, b, c])));
% The induction value and overwritten temporaries remain visible after a script loop.
a = 0;
b = 1;
for k = 1:5
  c = a + b;
  a = b;
  b = c;
end
asserts.isequal([a, b, c, k], [5, 8, 8, 5]);
profile('clear');
profile('on');
[a, b, c, k] = scalar_range_loop_values(1, 1, 5, 0, 1);
profile('off');
profile('clear');
asserts.isequal([a, b, c, k], [5, 8, 8, 5]);
r = 0;
for index = 1:100
  r = r + index;
end
asserts.isequal(r, 5050);
r = 0;
for index = 100:-1:1
  r = r + index;
end
asserts.isequal([r, index], [5050, 1]);
r = 0;
for index = 0.5:0.25:9
  r = r + index;
end
asserts.isequal([r, index], [166.25, 9]);
count = 0;
for index = 1:1000000
  count = count + 1;
end
asserts.isequal([count, index], [1000000, 1000000]);
count = 0;
for limit = [1000, 2, 35]
  for index = 1:limit
    count = count + 1;
  end
end
asserts.isequal([count, index], [1037, 35]);
% Evaluate the same arithmetic through a while loop as an independent path.
x = 1;
y = 2;
index = 0;
while index < 10
  t = (x + y) * 2;
  y = t / 4;
  x = -y / 2 + 0.5;
  index = index + 1;
end
expected = [x, y, t];
x = 1;
y = 2;
for index = 1:10
  t = (x + y) * 2;
  y = t / 4;
  x = -y / 2 + 0.5;
end
asserts.isequal([x, y, t], expected);
x = -0.;
for index = 1:35
  z = -x;
  x = -z;
end
asserts.isequal(1 ./ [x, z], [-Inf, Inf]);
total = 0;
for index = 1:3
  total = total + index;
  index = 99;
end
asserts.isequal([total, index], [6, 99]);
total = 0;
for outer = 1:3
  for inner = 1:5
    total = total + inner;
  end
end
asserts.isequal([total, outer, inner], [45, 3, 5]);
last = 100;
total = 1;
for index = 1:last
  last = 1;
  total = total + 1;
end
asserts.isequal([total, index, last], [101, 100, 1]);
% A branch and a type change force ordinary execution.
a = 0;
for index = 1:5
  if index == 3
    a = [1, 2];
  end
  a = a + index;
end
asserts.isequal(a, [13, 14]);
% Failed loads retain assignments performed before the error.
total = 0;
failed = false;
try
  for index = 1:5
    total = total + 1;
    result = undefined_scalar_range_operand + total;
  end
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal([total, index], [1, 1]);
if ismodule('debugger')
  previousBreakpoints = dbstatus();
  dbstop in bytecode_local_scalar_plus at 13
  asserts.isfalse(isempty(dbstatus()));
  [a, b, c, k] = scalar_range_loop_values(1, 1, 5, 0, 1);
  dbclear all
  if ~isempty(previousBreakpoints)
    dbstop(previousBreakpoints);
  end
  asserts.isequal([a, b, c, k], [5, 8, 8, 5]);
end
rmpath([modulepath('interpreter'), '/tests']);
