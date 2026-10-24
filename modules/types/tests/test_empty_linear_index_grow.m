%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an empty array grown through a linear index becomes a row, except a 0x1
% column (and an N-D empty whose only non-singleton dimension is zero)
%=============================================================================
a = zeros(0, 1);
a(3) = 1;
assert_isequal(a, [0; 0; 1]);
%=============================================================================
% a single element is a scalar: the next growth is a row
a = zeros(0, 1);
a(end + 1) = 1;
assert_isequal(size(a), [1 1]);
a(end + 1) = 2;
assert_isequal(a, [1 2]);
%=============================================================================
a = zeros(0, 1);
a(2:3) = [7 8];
assert_isequal(a, [0; 7; 8]);
%=============================================================================
a = zeros(0, 1);
a(2) = 1;
for k = 3:40
  a(end + 1) = k;
end
assert_isequal(size(a), [40 1]);
assert_isequal(a(3:end), (3:40)');
%=============================================================================
s = strings(0, 1);
s(3) = "x";
assert_isequal(size(s), [3 1]);
assert_isequal(ismissing(s), [true; true; false]);
assert_isequal(s(3), "x");
%=============================================================================
l = false(0, 1);
l(3) = true;
assert_isequal(l, [false; false; true]);
%=============================================================================
i8 = int8(zeros(0, 1));
i8(2) = 5;
assert_isequal(i8, int8([0; 5]));
%=============================================================================
z = complex(zeros(0, 1));
z(2) = 1i;
assert_isequal(z, [0; 1i]);
%=============================================================================
c = char(zeros(0, 1));
c(3) = 'x';
assert_isequal(size(c), [3 1]);
assert_isequal(c(3), 'x');
%=============================================================================
c = cell(0, 1);
c{3} = 1;
assert_isequal(size(c), [3 1]);
c = cell(0, 1);
c(3) = {1};
assert_isequal(size(c), [3 1]);
%=============================================================================
st = repmat(struct('a', 1), 0, 1);
st(2) = struct('a', 4);
assert_isequal(size(st), [2 1]);
%=============================================================================
% the other 2-D empties grow as a row
a = zeros(0, 0);
a(3) = 1;
assert_isequal(size(a), [1 3]);
a = zeros(1, 0);
a(3) = 1;
assert_isequal(size(a), [1 3]);
a = zeros(0, 3);
a(3) = 1;
assert_isequal(size(a), [1 3]);
a = zeros(3, 0);
a(3) = 1;
assert_isequal(size(a), [1 3]);
s = strings(3, 0);
s(3) = "x";
assert_isequal(size(s), [1 3]);
%=============================================================================
% N-D empties
a = zeros(1, 1, 0);
a(3) = 1;
assert_isequal(size(a), [1 1 3]);
a = zeros(1, 1, 0);
a(2) = 4;
assert_isequal(a(:), [0; 4]);
a = zeros(0, 1, 1);
a(2) = 4;
assert_isequal(size(a), [2 1]);
%=============================================================================
msg = _('Attempt to grow array along ambiguous dimension.');
a = zeros(0, 0, 2);
assert_checkerror('a(3) = 1;', msg);
a = zeros(2, 0, 3);
assert_checkerror('a(3) = 1;', msg);
a = zeros(1, 0, 2);
assert_checkerror('a(3) = 1;', msg);
a = zeros(0, 1, 0);
assert_checkerror('a(3) = 1;', msg);
s = strings(0, 0, 2);
assert_checkerror('s(3) = "x";', msg);
%=============================================================================
