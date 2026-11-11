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
% y = a * b, a + b, a - b and a .* b in a loop, y already holding a double
% array of the result shape, give the bits a new result has, and leave the
% values shared with y or overloads untouched.
%=============================================================================
shapes = {[3 3 1], [4 5 3], [2 2 2], [30 30 30], [50 50 50], [1 3 4]};
for s = 1:numel(shapes)
  m = shapes{s}(1); k = shapes{s}(2); n = shapes{s}(3);
  A = reshape(sin((1:m * k) * 1.37) * 3.1, m, k);
  B = reshape(cos((1:k * n) * 0.91) / 7.3, k, n);
  assert_isequal(num2hex(loop_mtimes(A, B)), num2hex(A * B));
end
%=============================================================================
M = reshape(sin(1:12), 3, 4);
N = reshape(cos(1:12), 3, 4);
r = loop_elementwise(M, N);
assert_isequal(num2hex(r{1}), num2hex(M + N));
assert_isequal(num2hex(r{2}), num2hex(M - N));
assert_isequal(num2hex(r{3}), num2hex(M .* N));
assert_isequal(num2hex(r{4}), num2hex(M + 2.5));
assert_isequal(num2hex(r{5}), num2hex(0.5 - N));
assert_isequal(num2hex(r{6}), num2hex(3 .* N));
%=============================================================================
% the result takes its own shape and class
assert_isequal(scalar_into_vector(), 5);
assert_isequal(shape_change(), [14; 32; 50]);
assert_isequal(class(class_change()), 'int8');
assert_isequal(complex_result(), [1 + 1i, 2 + 1i]);
%=============================================================================
% a copy of y keeps its values
[y, z] = shared_target();
assert_isequal(y, [11 12 13]);
assert_isequal(z, [1 2 3]);
%=============================================================================
% a double overload found on the path is called
d = fullfile(tempdir(), ['nelson_inplace_', num2str(round(rand() * 1e9))]);
mkdir(fullfile(d, '@double'));
fid = fopen(fullfile(d, '@double', 'plus.m'), 'wt');
fprintf(fid, 'function r = plus(a, b)\n  r = 42;\nend\n');
fclose(fid);
addpath(d);
r = loop_elementwise(M, N);
rmpath(d);
clear functions
rmdir(d, 's');
assert_isequal(r{1}, 42);
r = loop_elementwise(M, N);
assert_isequal(num2hex(r{1}), num2hex(M + N));
assert_isequal(num2hex(r{2}), num2hex(M - N));
%=============================================================================
function y = loop_mtimes(A, B)
  y = zeros(size(A, 1), size(B, 2));
  for k = 1:3
    y = A * B;
  end
end
%=============================================================================
function r = loop_elementwise(M, N)
  p = zeros(size(M)); q = p; t = p; u = p; v = p; w = p;
  for k = 1:3
    p = M + N;
    q = M - N;
    t = M .* N;
    u = M + 2.5;
    v = 0.5 - N;
    w = 3 .* N;
  end
  r = {p, q, t, u, v, w};
end
%=============================================================================
function y = scalar_into_vector()
  y = [1 2];
  for k = 1:2
    y = 2 + 3;
  end
end
%=============================================================================
function y = shape_change()
  A = [1 2 3; 4 5 6; 7 8 9];
  x = [1; 2; 3];
  y = zeros(2, 2);
  for k = 1:2
    y = A * x;
  end
end
%=============================================================================
function y = class_change()
  y = [1 2 3];
  for k = 1:2
    y = int8([1 2 3]) + int8([4 5 6]);
  end
end
%=============================================================================
function y = complex_result()
  y = [0 0];
  for k = 1:2
    y = [1 2] + 1i;
  end
end
%=============================================================================
function [y, z] = shared_target()
  y = [1 2 3];
  z = y;
  for k = 1:2
    y = z + 10;
  end
end
%=============================================================================
