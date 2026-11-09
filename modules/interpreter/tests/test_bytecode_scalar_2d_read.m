%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% x = m(i, j) on a real double matrix with two scalar subscripts takes a direct
% path in the generic parens read; it must match the generic result and leave
% every other case (growth, classes, N-D, non-integral index) to that path.
%=============================================================================
m = magic(6);
for i = 1:6
  for j = 1:6
    assert_isequal(m(i, j), m((j - 1) * 6 + i));
  end
end
%=============================================================================
% computed subscripts, including end
i = 2;
j = 3;
assert_isequal(m(i + 1, j * 2), m(3, 6));
assert_isequal(m(end, j), m(6, 3));
assert_isequal(m(i, end), m(2, 6));
assert_isequal(m(end, end), m(36));
%=============================================================================
% the value is a fresh scalar: writing it does not touch the matrix
m0 = magic(6);
x = m(2, 2);
x = x + 1;
assert_isequal(m(2, 2), m0(2, 2));
%=============================================================================
% errors stay on the generic path with the base untouched
failed = false;
try
  x = m(7, 1);
catch
  failed = true;
end
assert_istrue(failed);
failed = false;
try
  x = m(1, 7);
catch
  failed = true;
end
assert_istrue(failed);
failed = false;
try
  x = m(1.5, 1);
catch
  failed = true;
end
assert_istrue(failed);
failed = false;
try
  x = m(0, 1);
catch
  failed = true;
end
assert_istrue(failed);
assert_isequal(m, magic(6));
%=============================================================================
% other classes and shapes use the generic result
m3 = magic(3);
z = complex(m3, ones(3));
assert_isequal(z(2, 3), complex(m3(2, 3), 1));
s = single(m3);
assert_isequal(class(s(2, 3)), 'single');
assert_isequal(s(2, 3), single(m3(2, 3)));
b = m3 > 4;
assert_isequal(b(1, 1), true);
assert_isequal(b(1, 2), false);
assert_isequal(class(b(2, 2)), 'logical');
n = reshape(1:24, 2, 3, 4);
assert_isequal(n(2, 5), 10);
assert_isequal(n(1, 12), 23);
c = {1, 'a'; 2, 'b'};
assert_isequal(c(2, 2), {'b'});
%=============================================================================
% loops in a function hit the same path
m5 = magic(5);
assert_isequal(sum_2d_in_function(m5), sum(m5(:)));
%=============================================================================
function s = sum_2d_in_function(m)
  s = 0;
  for i = 1:size(m, 1)
    for j = 1:size(m, 2)
      s = s + m(i, j);
    end
  end
end
%=============================================================================
