%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A + B, A - B, A .* B and A * B on real dense doubles in a loop give the
% operator results, its errors, and still reach a double overload on the path.
%=============================================================================
A = magic(3);
B = A' / 7;
x = [1; -2; 0.5];
operands = {{A, B}, {A, 2}, {3, B}, {x, x'}, {[1 2 3], [4; 5]}, {zeros(0, 3), zeros(0, 3)}, ...
  {A, 1:3}, {rand(5, 4), rand(5, 4)}, {1:100, (1:100) / 3}};
for k = 1:numel(operands)
  a = operands{k}{1};
  b = operands{k}{2};
  r = loop_ops(a, b);
  assert_isequal(r{1}, feval('plus', a, b));
  assert_isequal(r{2}, feval('minus', a, b));
  assert_isequal(r{3}, feval('times', a, b));
end
assert_isequal(loop_mtimes(A, x), feval('mtimes', A, x));
assert_isequal(loop_mtimes(x', A), feval('mtimes', x', A));
assert_isequal(loop_mtimes(A, B), feval('mtimes', A, B));
%=============================================================================
% other classes keep their own result
assert_isequal(class(loop_plus(int8([1 2]), int8([3 4]))), 'int8');
assert_isequal(class(loop_plus(single([1 2]), [3 4])), 'single');
assert_istrue(issparse(loop_plus(sparse([1 0]), sparse([0 1]))));
assert_isequal(loop_plus([1 + 2i, 3], [1 2]), [2 + 2i, 5]);
assert_isequal(loop_plus(true(1, 2), [1 2]), [2 3]);
%=============================================================================
% errors are the operator ones
assert_checkerror('loop_plus([1 2 3], [1 2])', operator_error('plus', [1 2 3], [1 2]));
assert_checkerror('loop_mtimes([1 2 3], [1 2])', operator_error('mtimes', [1 2 3], [1 2]));
%=============================================================================
% a double overload found on the path is called
d = fullfile(tempdir(), ['nelson_plus_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@double'));
fid = fopen(fullfile(d, '@double', 'plus.m'), 'wt');
fprintf(fid, 'function r = plus(a, b)\n  r = 42;\nend\n');
fclose(fid);
assert_isequal(loop_plus([1 2], [3 4]), [4 6]);
addpath(d);
assert_isequal(loop_plus([1 2], [3 4]), 42);
rmpath(d);
assert_isequal(loop_plus([1 2], [3 4]), [4 6]);
rmdir(d, 's');
%=============================================================================
function r = loop_ops(a, b)
  r = cell(1, 3);
  for k = 1:2
    r = {a + b, a - b, a .* b};
  end
end
%=============================================================================
function r = loop_mtimes(a, b)
  r = [];
  for k = 1:2
    r = a * b;
  end
end
%=============================================================================
function r = loop_plus(a, b)
  r = [];
  for k = 1:2
    r = a + b;
  end
end
%=============================================================================
function msg = operator_error(name, a, b)
  msg = '';
  try
    feval(name, a, b);
  catch e
    msg = e.message;
  end
end
%=============================================================================
