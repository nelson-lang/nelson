%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Direct and fused indexing paths must give the generic result: two-subscript
% reads and writes through local indices, `end` in any subscript position,
% cell appends, struct array element fields and c(k) = {x}.
%=============================================================================
% two-subscript reads through local indices, plain and fused
m = magic(5);
assert_isequal(read_all_2d(m), m);
assert_isequal(sum_2d(m), sum(m(:)));
assert_isequal(weighted_sum_2d(m), sum(sum(m .* ((1:5)' + (1:5)))));
% N-D base: the second subscript spans the trailing extents
n = reshape(1:24, 2, 3, 4);
assert_isequal(read_pair(n, 2, 2), 4);
assert_isequal(read_pair(n, 1, 12), 23);
% other classes take the generic path with the same result
assert_isequal(read_pair(int8(m), 2, 3), int8(m(2, 3)));
assert_isequal(read_pair(single(m), 4, 1), single(m(4, 1)));
assert_isequal(read_pair(m > 10, 1, 2), m(1, 2) > 10);
assert_isequal(read_pair({1, 'a'; 2, 'b'}, 2, 2), {'b'});
assert_isequal(read_pair(complex(m, 1), 3, 3), complex(m(3, 3), 1));
% non scalar subscripts and errors
assert_isequal(read_pair(m, 1:2, 3), m(1:2, 3));
assert_isequal(read_pair(m, 2, [1 5]), m(2, [1 5]));
failed = false;
try
  read_pair(m, 6, 1);
catch
  failed = true;
end
assert_istrue(failed);
failed = false;
try
  read_pair(m, 1.5, 1);
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
% two-subscript writes, computed subscripts and end in any position
assert_isequal(fill_2d(4, 3), (1:4)' * 10 + (1:3));
w = zeros(3, 4);
w = write_end_row(w, 7);
assert_isequal(w(3, :), 7 * ones(1, 4));
assert_isequal(w(1:2, :), zeros(2, 4));
w = write_end_col(zeros(3, 4), 5);
assert_isequal(w(:, 4), 5 * ones(3, 1));
w = write_end_end(zeros(3, 4));
assert_isequal(w(3, 4), 1);
assert_isequal(nnz(w), 1);
% a 2-D write on an N-D base folds the last subscript
t = zeros(2, 3, 4);
t = write_end_col(t, 9);
assert_isequal(t(:, 12), [9; 9]);
assert_isequal(nnz(t), 2);
% out of range writes grow through the generic path
g = grow_2d(zeros(2, 2));
assert_isequal(size(g), [3 4]);
assert_isequal(g(3, 4), 34);
% class promotion and non double values leave the fused store
p = promote_2d(zeros(2, 2));
assert_isequal(p, [0 0; 0 1i]);
q = int8(zeros(2));
q = write_value_2d(q, 2, 2, 5);
assert_isequal(q, int8([0 0; 0 5]));
%=============================================================================
% `end` at the head of a fused index computation: cell and array appends
assert_isequal(append_strings(3), {'x1', 'x2', 'x3'});
s = append_structs(3);
assert_isequal(size(s), [1 3]);
assert_isequal([s.id], [1 2 3]);
v = append_mixed();
assert_isequal(v, {1, 'two', [3 3]});
r = reads_with_end([4 5 6]);
assert_isequal(r, [6 5 6 4]);
%=============================================================================
% struct array element fields read in place
sv = struct('a', {10, 20, 30}, 'b', {'x', 'y', 'z'});
assert_isequal(read_fields(sv, 'a'), [10 20 30]);
assert_isequal(read_b(sv, 2), 'y');
one = struct('a', 5);
assert_isequal(read_fields(one, 'a'), 5);
nested = struct('inner', {struct('v', 1), struct('v', 2)});
assert_isequal(read_nested(nested, 2), 2);
failed = false;
try
  read_missing(sv, 1);
catch
  failed = true;
end
assert_istrue(failed);
failed = false;
try
  read_b(sv, 4);
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
% struct arrays grown one field assignment at a time
g = grow_struct(5);
assert_isequal(size(g), [1 5]);
assert_isequal([g.a], 1:5);
assert_isequal({g.b}, {[], [], [], [], []});
g = struct('a', {1; 2});
g(4).a = 4;
assert_isequal(size(g), [4 1]);
assert_isequal(g(3).a, []);
h = g;
g(5).a = 5;
assert_isequal(numel(h), 4);
assert_isequal(numel(g), 5);
g2 = repmat(struct('a', 0), 2, 2);
failed = false;
try
  g2(5).a = 1;
catch
  failed = true;
end
assert_istrue(failed);
g3 = struct('a', 1);
g3(3).newfield = 'n';
assert_isequal(fieldnames(g3), {'a'; 'newfield'});
assert_isequal(g3(3).newfield, 'n');
assert_isequal(g3(1).newfield, []);
%=============================================================================
% c(k) = {x} on a cell
c = paren_cell_append(4);
assert_isequal(c, {1, 2, 3, 4});
c = {1, 2};
c(4) = {'d'};
assert_isequal(c, {1, 2, [], 'd'});
d = c;
c(1) = {9};
assert_isequal(d{1}, 1);
m2 = cell(2, 2);
failed = false;
try
  m2(5) = {1};
catch
  failed = true;
end
assert_istrue(failed);
e = [];
e(2) = {'a'};
assert_isequal(e, {[], 'a'});
nest = {1};
nest(2) = {{2}};
assert_isequal(nest{2}, {2});
%=============================================================================
% struct array element fields through a computed index
assert_isequal(read_field_offset(sv, 1), 20);
assert_isequal(read_field_offset(sv, 2), 30);
idx = [3 1];
assert_isequal(sv(idx(1)).a, 30);
assert_isequal(sv(idx(2)).b, 'x');
failed = false;
try
  read_field_offset(sv, 3);
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
% c{expr} = v on a cell, including growth, and other bases on the generic path
c = brace_computed(4);
assert_isequal(c, {2, 4, 6, 8});
c = {};
k = 1;
c{k + 2} = 'z';
assert_isequal(c, {[], [], 'z'});
sa = string({'a', 'b'});
k = 1;
sa{k + 1} = 'c';
assert_isequal(sa, string({'a', 'c'}));
%=============================================================================
% small concatenations of double scalars and single operand brackets
k = 3;
assert_isequal([k, k], [3 3]);
assert_isequal([k; 2 * k], [3; 6]);
assert_isequal([k, 2 * k, -1], [3 6 -1]);
assert_isequal([k, int8(2)], int8([3 2]));
assert_isequal(class([k, single(2)]), 'single');
assert_isequal([k, 1i], [3 1i]);
assert_isequal([k, true], [3 1]);
assert_isequal(['a', 'b'], 'ab');
assert_isequal([k, []], 3);
assert_isequal(size([k; []]), [1 1]);
x1 = 'text';
assert_isequal([x1], 'text');
c1 = {1, 2};
assert_isequal([c1], {1, 2});
st1 = struct('f', 1);
assert_isequal([st1], st1);
assert_isequal([c1{:}], [1 2]);
assert_isequal(build_rows(4), [1 2; 2 4; 3 6; 4 8]);
% x = [a, b] into a same shape double vector is written in place
assert_isequal(last_pair(5), [5 10]);
assert_isequal(last_column(5), [5; -5]);
r1 = [1 2];
r2 = r1;
r1 = [7, 8];
assert_isequal(r2, [1 2]);
assert_isequal(r1, [7 8]);
r3 = [1 2 3];
k = 4;
r3 = [k, k];
assert_isequal(r3, [4 4]);
r4 = int8([1 2]);
r4 = [k, k];
assert_isequal(class(r4), 'double');
r5 = [1 2];
r5 = [r5(2), r5(1)];
assert_isequal(r5, [2 1]);
r6 = [1; 2];
r6 = [k, k];
assert_isequal(size(r6), [1 2]);
%=============================================================================
% indexed reads in three-instruction fused windows
v = 1:10;
assert_isequal(sum_indexed(v), 55);
assert_isequal(sum_2d(magic(4)), sum(sum(magic(4))));
assert_isequal(sum_indexed(int8(1:5)), int8(15));
%=============================================================================
% reads with and without end around nested indexing and calls
x = [1 2 3 4];
y = [10 20 30];
assert_isequal(x(end), 4);
assert_isequal(m(end, 1), m(5, 1));
assert_isequal(x(y(end) / 10), 3);
assert_isequal(x(round(end / 2)), 2);
assert_isequal(m(2, numel(y)), m(2, 3));
%=============================================================================
function r = read_all_2d(m)
  r = zeros(size(m));
  for i = 1:size(m, 1)
    for j = 1:size(m, 2)
      r(i, j) = m(i, j);
    end
  end
end
%=============================================================================
function s = sum_2d(m)
  s = 0;
  for i = 1:size(m, 1)
    for j = 1:size(m, 2)
      s = s + m(i, j);
    end
  end
end
%=============================================================================
function s = weighted_sum_2d(m)
  s = 0;
  for i = 1:size(m, 1)
    for j = 1:size(m, 2)
      s = s + m(i, j) * (i + j);
    end
  end
end
%=============================================================================
function r = read_pair(m, i, j)
  r = m(i, j);
end
%=============================================================================
function m = fill_2d(rows, cols)
  m = zeros(rows, cols);
  for i = 1:rows
    for j = 1:cols
      m(i, j) = i * 10 + j;
    end
  end
end
%=============================================================================
function w = write_end_row(w, value)
  for j = 1:size(w, 2)
    w(end, j) = value;
  end
end
%=============================================================================
function w = write_end_col(w, value)
  for i = 1:size(w, 1)
    w(i, end) = value;
  end
end
%=============================================================================
function w = write_end_end(w)
  w(end, end) = 1;
end
%=============================================================================
function g = grow_2d(g)
  i = 3;
  j = 4;
  g(i, j) = i * 10 + j;
end
%=============================================================================
function p = promote_2d(p)
  i = 2;
  j = 2;
  p(i, j) = 1i;
end
%=============================================================================
function q = write_value_2d(q, i, j, v)
  q(i, j) = v;
end
%=============================================================================
function c = append_strings(n)
  c = {};
  for k = 1:n
    c{end + 1} = sprintf('x%d', k); %#ok<NLS0039>
  end
end
%=============================================================================
function s = append_structs(n)
  s = struct('id', {});
  for k = 1:n
    s(end + 1).id = k; %#ok<NLS0039>
  end
end
%=============================================================================
function v = append_mixed()
  v = {};
  v{end + 1} = 1;
  v{end + 1} = 'two';
  v{end + 1} = [3 3];
end
%=============================================================================
function r = reads_with_end(x)
  r = [x(end), x(end - 1), x(end), x(1)];
end
%=============================================================================
function r = read_fields(sv, name)
  r = zeros(1, numel(sv));
  for k = 1:numel(sv)
    if strcmp(name, 'a')
      r(k) = sv(k).a;
    end
  end
end
%=============================================================================
function r = read_b(sv, k)
  r = sv(k).b;
end
%=============================================================================
function r = read_nested(nested, k)
  r = nested(k).inner.v;
end
%=============================================================================
function r = read_missing(sv, k)
  r = sv(k).missing;
end
%=============================================================================
function c = paren_cell_append(n)
  c = {};
  for k = 1:n
    c(k) = {k}; %#ok<NLS0039>
  end
end
%=============================================================================
function r = read_field_offset(sv, k)
  r = sv(k + 1).a;
end
%=============================================================================
function s = grow_struct(n)
  s = struct('a', {}, 'b', {});
  for k = 1:n
    s(k).a = k; %#ok<NLS0039>
  end
end
%=============================================================================
function c = brace_computed(n)
  c = {};
  for k = 1:n
    c{2 * k - k} = 2 * k; %#ok<NLS0039>
  end
end
%=============================================================================
function m = build_rows(n)
  m = zeros(n, 2);
  for k = 1:n
    m(k, :) = [k, 2 * k];
  end
end
%=============================================================================
function s = sum_indexed(v)
  s = 0;
  if isinteger(v)
    s = cast(s, class(v));
  end
  for k = 1:numel(v)
    s = s + v(k);
  end
end
%=============================================================================
function p = last_pair(n)
  p = [0 0];
  for k = 1:n
    p = [k, 2 * k];
  end
end
%=============================================================================
function c = last_column(n)
  c = [0; 0];
  for k = 1:n
    c = [k; -k];
  end
end
%=============================================================================
