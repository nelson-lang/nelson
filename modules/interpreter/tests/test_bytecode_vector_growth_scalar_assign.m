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
% In-place growth of a real double vector through a scalar linear index (fast
% path) must match the generic result: shape rules, zero fill, copy-on-write
% and the fallbacks that stay on the generic path.
%=============================================================================
% growth from [] builds a row
v = [];
for k = 1:40
  v(k) = k * 2; %#ok<NLS0039>
end
assert_isequal(v, 2 * (1:40));
%=============================================================================
% a 0x1 empty grown one element at a time becomes a scalar, then a row
c = zeros(0, 1);
for k = 1:40
  c(k) = k; %#ok<NLS0039>
end
assert_isequal(c, 1:40);
c = zeros(0, 1);
c(3) = 1;
assert_isequal(c, [0; 0; 1]);
%=============================================================================
% a scalar grows as a row, vectors keep their orientation, gaps are zero filled
s = 5;
s(2) = 6;
assert_isequal(s, [5 6]);
c = [1; 2];
c(4) = 9;
assert_isequal(c, [1; 2; 0; 9]);
r = [1 2];
r(5) = 7;
assert_isequal(r, [1 2 0 0 7]);
a = zeros(1, 1, 0);
a(3) = 1;
assert_isequal(size(a), [1 1 3]);
%=============================================================================
% the value may be the base itself
a = 3;
a(2) = a;
assert_isequal(a, [3 3]);
%=============================================================================
% a co-owner of the data must not see the growth
a = [1 2 3];
b = a;
a(4) = 4;
assert_isequal(b, [1 2 3]);
assert_isequal(a, [1 2 3 4]);
%=============================================================================
% an undefined variable is created from empty
clear u
u(3) = 1;
assert_isequal(u, [0 0 1]);
%=============================================================================
% constant index and end + 1 (fused opcodes)
v = [];
v(3) = 7;
assert_isequal(v, [0 0 7]);
v(end + 1) = 8;
assert_isequal(v, [0 0 7 8]);
%=============================================================================
% a matrix base keeps the generic error
m = zeros(2, 2);
assert_checkerror('m(5) = 1;', _('Attempt to grow array along ambiguous dimension.'));
assert_isequal(m, zeros(2, 2));
%=============================================================================
% generic path fallbacks: class promotion and conversion
z = [1 2];
z(4) = 1i;
assert_isequal(z, [1 2 0 1i]);
w = [1 2];
w(3) = single(3);
assert_isequal(class(w), 'double');
assert_isequal(w, [1 2 3]);
i8 = int8([1 2]);
i8(4) = 3;
assert_isequal(i8, int8([1 2 0 3]));
%=============================================================================
% an invalid index still errors and leaves the base untouched
x = [1 2];
failed = false;
try
  x(2.5) = 1;
catch
  failed = true;
end
assert_istrue(failed);
assert_isequal(x, [1 2]);
failed = false;
try
  x(0) = 1;
catch
  failed = true;
end
assert_istrue(failed);
assert_isequal(x, [1 2]);
%=============================================================================
% a long append loop stays exact
v = [];
for k = 1:20000
  v(k) = k; %#ok<NLS0039>
end
assert_isequal(size(v), [1 20000]);
assert_isequal(v, 1:20000);
%=============================================================================
% end + 1 and computed indices run in the typed window too
v = [];
for k = 1:40
  v(end + 1) = k * 2; %#ok<NLS0039>
end
assert_isequal(v, 2 * (1:40));
c = [1; 2];
for k = 3:40
  c(end + 1) = k; %#ok<NLS0039>
end
assert_isequal(c, (1:40)');
w = zeros(1, 5);
for k = 1:4
  w(k + 1) = k;
end
assert_isequal(w, [0 1 2 3 4]);
w(end) = 9;
assert_isequal(w, [0 1 2 3 9]);
w(1 + end) = 10;
assert_isequal(w, [0 1 2 3 9 10]);
clear u
u(end + 1) = 5;
assert_isequal(u, 5);
% a computed index that is not an ordinal errors and leaves the base untouched
x = [1 2];
k = 3;
failed = false;
try
  x(k / 2) = 1;
catch
  failed = true;
end
assert_istrue(failed);
assert_isequal(x, [1 2]);
%=============================================================================
% cell growth through a scalar brace index uses the same amortized path
c = {};
for k = 1:40
  c{k} = k * 2; %#ok<NLS0039>
end
assert_isequal(c, num2cell(2 * (1:40)));
c = {};
for k = 1:40
  c{end + 1} = k; %#ok<NLS0039>
end
assert_isequal(c, num2cell(1:40));
c = {1; 2};
for k = 3:10
  c{k} = k; %#ok<NLS0039>
end
assert_isequal(c, num2cell((1:10)'));
c = cell(0, 1);
c{3} = 'x';
assert_isequal(size(c), [3 1]);
assert_isequal(c{1}, []);
assert_isequal(c{3}, 'x');
c = {};
c{5} = 1;
assert_isequal(size(c), [1 5]);
assert_isequal(c{2}, []);
assert_isequal(c{5}, 1);
% a co-owner must not see the growth
c = {1, 2};
d = c;
c{3} = 3;
assert_isequal(d, {1, 2});
assert_isequal(c, {1, 2, 3});
% a matrix cell and a non-cell base stay on the generic path
m = cell(2, 2);
failed = false;
try
  m{5} = 1;
catch
  failed = true;
end
assert_istrue(failed);
assert_isequal(m, cell(2, 2));
e = [];
e{2} = 'a';
assert_isequal(e, {[], 'a'});
%=============================================================================
% function locals use the same path
assert_isequal(grow_in_function(50), (1:50) * 3);
assert_isequal(grow_column_in_function(50), (1:50)');
%=============================================================================
% fused scalar windows: the value is a plain local, a constant or a negation
assert_isequal(store_local_in_function(30), 1:30);
assert_isequal(store_constant_in_function(30), 7 * ones(1, 30));
assert_isequal(store_negated_in_function(30), -(1:30));
% a non-double value leaves the typed window and promotes the base
assert_isequal(store_int8_in_function(4), int8([0 0 0 5]));
% a non-scalar value leaves the typed window too
assert_isequal(store_vector_in_function(), [1 2 3 2 3]);
%=============================================================================
function r = grow_in_function(n)
  r = [];
  for k = 1:n
    r(k) = 3 * k; %#ok<NLS0039>
  end
end
%=============================================================================
function r = store_local_in_function(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = k;
  end
end
%=============================================================================
function r = store_constant_in_function(n)
  r = [];
  for k = 1:n
    r(k) = 7; %#ok<NLS0039>
  end
end
%=============================================================================
function r = store_negated_in_function(n)
  r = zeros(1, n);
  for k = 1:n
    r(k) = -k;
  end
end
%=============================================================================
function r = store_int8_in_function(n)
  r = [];
  x = int8(5);
  r(n) = x;
end
%=============================================================================
function r = store_vector_in_function()
  r = [1 2 3];
  k = 4;
  w = [2 3];
  r(k:k + 1) = w;
end
%=============================================================================
function r = grow_column_in_function(n)
  r = [1; 2];
  for k = 3:n
    r(end + 1) = k; %#ok<NLS0039>
  end
end
%=============================================================================
