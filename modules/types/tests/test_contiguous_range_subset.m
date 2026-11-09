%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% v(a:b), v(a:end), m(i, :) and m(:, j) give the elements, shape and class of
% a general index, and the errors of a general index.
%=============================================================================
v = 1:10;
assert_isequal(v(3:5), [3 4 5]);
assert_isequal(v(4:end), [4 5 6 7 8 9 10]);
assert_isequal(v(10:end), 10);
assert_isequal(size(v(5:4)), [1 0]);
assert_isequal(v((3:5)'), [3 4 5]);
c = (1:5)';
assert_isequal(c(2:4), [2; 3; 4]);
m = magic(4);
assert_isequal(size(m(2:3)), [1 2]);
assert_isequal(m((2:3)'), [5; 9]);
assert_isequal(m(2:3, :), [5 11 10 8; 9 7 6 12]);
assert_isequal(m(:, 2), [2; 11; 7; 14]);
assert_isequal(m(3, :), [9 7 6 12]);
s.data = 1:6;
assert_isequal(s.data(2:4), [2 3 4]);
%=============================================================================
text = 'hello world';
assert_isequal(text(7:end), 'world');
L = logical([1 0 1 1]);
assert_isequal(L(2:3), [false true]);
i8 = int8(1:5);
assert_isequal(i8(2:3), int8([2 3]));
sv = single(1:4);
assert_isequal(sv(2:3), single([2 3]));
%=============================================================================
r = loop_reads(v, m, 3, 6);
assert_isequal(r{1}, [3 4 5 6]);
assert_isequal(r{2}, [6 7 8 9 10]);
assert_isequal(r{3}, [1 2 3]);
assert_isequal(r{4}, [3 4 5 6]);
assert_isequal(r{5}, [9 7 6 12]);
assert_isequal(r{6}, [3; 10; 6; 15]);
assert_isequal(r{7}, [4 5 6 7]);
%=============================================================================
% the result is a copy
w = v(2:4);
w(1) = 100;
assert_isequal(v(2), 2);
%=============================================================================
% out of range or non-integer bounds take the general path and its errors
assert_isequal(range_error(v, 0, 2), index_error(v, [0 1 2]));
assert_isequal(range_error(v, 9, 11), index_error(v, [9 10 11]));
assert_isequal(range_error(m, 1, 17), index_error(m, 1:17));
%=============================================================================
function r = loop_reads(v, m, a, b)
  r = {};
  idx = a:b;
  for k = 1:2
    r = {v(a:b), v(b:end), v(1:3), v(idx), m(a, :), m(:, a), v(a + 1:b + 1)};
  end
end
%=============================================================================
function msg = range_error(v, a, b)
  msg = '';
  for k = 1:2
    try
      w = v(a:b); %#ok<NLS0004>
    catch e
      msg = e.message;
    end
  end
end
%=============================================================================
function msg = index_error(v, idx)
  msg = '';
  try
    w = v(idx); %#ok<NLS0004>
  catch e
    msg = e.message;
  end
end
%=============================================================================
