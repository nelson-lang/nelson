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
% Direct element writes x(k) = v bypass the generic subset assignment; every
% case the fast path must refuse has to keep the expected behavior.
%=============================================================================
% in-place write, in bounds
x = [1 2 3];
x(2) = 99;
asserts.isequal(x, [1 99 3]);
%=============================================================================
% copy on write: an alias taken before the write keeps the old value
x = [1 2 3];
b = x;
x(2) = 99;
asserts.isequal(b, [1 2 3]);
asserts.isequal(x, [1 99 3]);
%=============================================================================
% write in a loop, the classic preallocated accumulation
n = 100;
v = zeros(1, n);
for k = 1:n
  v(k) = k * 0.5;
end
asserts.isequal(v(1), 0.5);
asserts.isequal(v(n), 50);
asserts.isequal(sum(v), 2525);
%=============================================================================
% growth past the end still grows and zero-fills
g = [1 2];
g(5) = 7;
asserts.isequal(g, [1 2 0 0 7]);
asserts.isequal(class(g), 'double');
%=============================================================================
% complex value turns the array complex
c = [1 2 3];
c(2) = 4 + 5i;
asserts.isequal(c, [1, 4 + 5i, 3]);
asserts.isequal(class(c), 'double');
%=============================================================================
% real value into a complex array keeps the other elements complex
d = [1 + 1i, 2 + 2i];
d(1) = 7;
asserts.isequal(d, [7, 2 + 2i]);
%=============================================================================
% N-D array through a linear index keeps its shape
nd = zeros(2, 2, 2);
nd(7) = 5;
asserts.isequal(size(nd), [2 2 2]);
asserts.isequal(nd(7), 5);
%=============================================================================
% logical index selects elements, it is not an ordinal
l = [1 2 3];
l(logical([0 1 0])) = 8;
asserts.isequal(l, [1 8 3]);
%=============================================================================
% char and integer values are converted element-wise, values must match
% (the resulting array class is a separate known gap, see the class-retention
% report: reference keeps the left-hand class, Nelson adopts the value class)
ch = [1 2 3];
ch(2) = 'a';
asserts.isequal(double(ch), [1 97 3]);
i8 = [1 2 3];
i8(2) = int8(-5);
asserts.isequal(double(i8), [1 -5 3]);
%=============================================================================
% non-double arrays keep their own assignment semantics
sg = single([1 2 3]);
sg(2) = 2.5;
asserts.isequal(class(sg), 'single');
asserts.isequal(double(sg), [1 2.5 3]);
ints = int8([1 2 3]);
ints(2) = 300;
asserts.isequal(class(ints), 'int8');
asserts.isequal(double(ints(2)), 127);
%=============================================================================
% sparse arrays stay sparse
sp = sparse([1 2 3]);
sp(2) = 9;
asserts.istrue(issparse(sp));
asserts.isequal(full(sp), [1 9 3]);
%=============================================================================
% invalid ordinals are rejected, not silently written
z = [1 2 3];
asserts.checkerror('z(0) = 1;', _('Zero or negative index encountered.'));
asserts.checkerror('z(-1) = 1;', _('index must either be real positive integers or logicals.'));
asserts.checkerror('z(1.5) = 1;', _('index must either be real positive integers or logicals.'));
asserts.checkerror('z(NaN) = 1;', _('index must either be real positive integers or logicals.'));
asserts.isequal(z, [1 2 3]);
%=============================================================================
% two-dimensional element write, same guarantees
m = zeros(2, 2);
mb = m;
m(1, 2) = 4;
asserts.isequal(m, [0 4; 0 0]);
asserts.isequal(mb, zeros(2, 2));
m(3, 3) = 1;
asserts.isequal(size(m), [3 3]);
%=============================================================================
% profiler on: fast path disabled, results identical
profile('clear');
profile('on');
pv = zeros(1, 10);
for k = 1:10
  pv(k) = k;
end
profile('off');
profile('clear');
asserts.isequal(pv, 1:10);
%=============================================================================
