%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
subs = [1; 3; 1; 2; 3; 3];
v = [1; 2; 3; 4; 5; 6];
%=============================================================================
% default sum: single stays single, every other class sums in double
R = accumarray(subs, single(v));
assert_isequal(class(R), 'single');
assert_isequal(R, single([4; 4; 13]));
R = accumarray(subs, single(v), [5 1]);
assert_isequal(R, single([4; 4; 13; 0; 0]));
R = accumarray(subs, single(v), [5 1], [], single(2.5));
assert_isequal(R, single([4; 4; 13; 2.5; 2.5]));
R = accumarray(subs, single(2));
assert_isequal(R, single([4; 2; 6]));
R = accumarray([1; 1; 1], single([1e8; 1; 1]));
assert_isequal(R, single(1e8));
R = accumarray([1; 1], single([3e38; 3e38]));
assert_isequal(R, single(Inf));
%=============================================================================
int_classes = {'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', 'uint64'};
for k = 1:numel(int_classes)
  c = int_classes{k};
  vv = cast(v, c);
  R = accumarray(subs, vv);
  assert_isequal(R, [4; 4; 13]);
  R = accumarray(subs, vv, [5 1], [], 7);
  assert_isequal(R, [4; 4; 13; 7; 7]);
  R = accumarray(subs, cast(2, c));
  assert_isequal(R, [4; 2; 6]);
  R = accumarray(subs, vv, [], @max);
  assert_isequal(R, cast([3; 4; 6], c));
  R = accumarray(subs, vv, [], @min);
  assert_isequal(R, cast([1; 4; 2], c));
  R = accumarray(subs, vv, [5 1], @max);
  assert_isequal(R, cast([3; 4; 6; 0; 0], c));
  R = accumarray(subs, vv, [5 1], @max, cast(9, c));
  assert_isequal(R, cast([3; 4; 6; 9; 9], c));
  R = accumarray([1 1 1; 2 1 2; 1 1 1; 1 2 2], vv(1:4), [], @max);
  assert_isequal(R, cast(cat(3, [3 0; 0 0], [0 4; 2 0]), c));
  R = accumarray(subs, vv, [], @(x) {x});
  assert_isequal(R, {cast([1; 3], c); cast(4, c); cast([2; 5; 6], c)});
  R = accumarray(cast(subs, c), v);
  assert_isequal(R, [4; 4; 13]);
end
% integer sums do not saturate
assert_isequal(accumarray([1; 1; 1], int8([100; 100; -100])), 100);
assert_isequal(accumarray([1; 1], uint8([200; 100])), 300);
assert_isequal(accumarray([1; 3], int8([1; 2]), [], [], NaN), [1; NaN; 2]);
%=============================================================================
% logical and char values
R = accumarray(subs, logical(mod(v, 2)));
assert_isequal(R, [2; 0; 1]);
R = accumarray(subs, logical(mod(v, 2)), [], @max);
assert_isequal(R, [true; false; true]);
R = accumarray(subs, logical(mod(v, 2)), [], @(x) {x});
assert_isequal(R, {[true; true]; false; [false; true; false]});
R = accumarray(subs, char(v + 64));
assert_isequal(R, [132; 68; 205]);
R = accumarray(subs, char(v + 64), [], @(x) x(end));
assert_isequal(R, ['C'; 'D'; 'F']);
% named @max and @min keep char, an anonymous wrapper gives the class max returns
R = accumarray([1; 3; 1; 2], ('ACBD')', [], @max);
assert_isequal(R, ['B'; 'D'; 'C']);
R = accumarray([1; 3; 1; 2], ('ACBD')', [], @min);
assert_isequal(R, ['A'; 'D'; 'C']);
R = accumarray([1; 2], 'AB', [], @max);
assert_isequal(R, ['A'; 'B']);
R = accumarray([1; 3], 'AB', [], @max, ' ');
assert_isequal(R, ['A'; ' '; 'B']);
assert_checkerror('accumarray([1; 3], ''AB'', [], @max, 0)', ...
_('The fill value must have the same class as the accumulated result.'));
R = accumarray([1; 3], 'AB', [], @(x) x(1), ' ');
assert_isequal(R, ['A'; ' '; 'B']);
R = accumarray([1; 2], [65; 66], [], @(x) char(x));
assert_isequal(R, ['A'; 'B']);
%=============================================================================
% single values through functions
R = accumarray(subs, single(v), [], @max);
assert_isequal(R, single([3; 4; 6]));
R = accumarray(subs, single(v), [], @prod);
assert_isequal(R, single([3; 4; 60]));
R = accumarray(subs, single(v), [], @mean);
assert_isequal(class(R), 'single');
assert_isapprox(double(R), [2; 4; 13 / 3], 1e-6);
R = accumarray(subs, single(v), [5 1], @max, single(-1));
assert_isequal(R, single([3; 4; 6; -1; -1]));
R = accumarray(subs, single(v), [], @(x) {x});
assert_isequal(R, {single([1; 3]); single(4); single([2; 5; 6])});
%=============================================================================
% the output class follows the function result
R = accumarray(subs, v, [], @(x) int8(sum(x)));
assert_isequal(R, int8([4; 4; 13]));
R = accumarray(subs, v, [5 1], @(x) int8(sum(x)));
assert_isequal(R, int8([4; 4; 13; 0; 0]));
R = accumarray(subs, int16(v), [], @(x) single(sum(x)));
assert_isequal(R, single([4; 4; 13]));
R = accumarray(subs, v, [], @(x) numel(x) > 1);
assert_isequal(R, [true; false; true]);
R = accumarray([1; 2], 3, [], @(x) int8(x(1)));
assert_isequal(R, int8([3; 3]));
R = accumarray([1; 1; 2], uint16(7), [3 1], @max);
assert_isequal(R, uint16([7; 7; 0]));
%=============================================================================
% complex values
z = complex(v, flipud(v));
R = accumarray(subs, z);
assert_isequal(R, [4 + 10i; 4 + 3i; 13 + 8i]);
R = accumarray(subs, z, [5 1], [], 7);
assert_isequal(R, [4 + 10i; 4 + 3i; 13 + 8i; 7; 7]);
R = accumarray(subs, single(z));
assert_isequal(R, single([4 + 10i; 4 + 3i; 13 + 8i]));
R = accumarray(subs, single(z), [], @max);
assert_isequal(R, single([1 + 6i; 4 + 3i; 6 + 1i]));
R = accumarray(subs, z, [], @prod);
assert_isequal(R, [-21 + 22i; 4 + 3i; -29 + 174i]);
R = accumarray(subs, 2 + 3i);
assert_isequal(R, [4 + 6i; 2 + 3i; 6 + 9i]);
R = accumarray(subs, v, [5 1], [], 1i);
assert_isequal(R, [4; 4; 13; 1i; 1i]);
R = accumarray([1; 3], single([1; 2]), [], [], single(1i));
assert_isequal(R, single([1; 1i; 2]));
R = accumarray([1; 1], [1 + 1i; 1 - 1i]);
assert_istrue(isreal(R));
assert_isequal(R, 2);
R = accumarray(subs, z, [], @(x) real(sum(x)));
assert_istrue(isreal(R));
assert_isequal(R, [4; 4; 13]);
R = accumarray([1; 2], [1; 2], [], @(x) x(1) * (1 + (x(1) == 2) * 1i));
assert_isequal(R, [1; 2 + 2i]);
%=============================================================================
% sparse output keeps double or single
R = accumarray(subs, single(v), [3 1], [], 0, true);
assert_istrue(issparse(R));
assert_isequal(class(R), 'single');
assert_isequal(full(R), single([4; 4; 13]));
R = accumarray([1; 3], single([1; 2]), [3 1], @max, 0, true);
assert_istrue(issparse(R));
assert_isequal(full(R), single([1; 0; 2]));
R = accumarray([1 1; 2 3; 1 1], [1; 2; 3], [2 3], [], 0, true);
assert_isequal(full(R), [4 0 0; 0 0 2]);
R = accumarray([1; 2], [1; 2], [], [], single(0), true);
assert_istrue(issparse(R));
R = accumarray([1; 2], [1; 2], [], [], 0, int8(1));
assert_istrue(issparse(R));
%=============================================================================
% fill values and empty fills
assert_isequal(accumarray(subs, v, [5 1], [], 2.5), [4; 4; 13; 2.5; 2.5]);
assert_isequal(accumarray(subs, int8(v), [5 1], [], 2.5), [4; 4; 13; 2.5; 2.5]);
assert_isequal(accumarray(subs, single(v), [5 1], [], []), single([4; 4; 13; 0; 0]));
assert_isequal(accumarray(subs, single(v), [5 1], [], {}), single([4; 4; 13; 0; 0]));
assert_isequal(accumarray([1; 3], int8([100; 100]), [], @max, []), int8([100; 0; 100]));
R = accumarray([1; 3], [1; 2], [], @(x) {x}, []);
assert_isequal(R, {1; []; 2});
R = accumarray([1; 3], int8([1; 2]), [], @(x) {x}, {int8(5)});
assert_isequal(R, {int8(1); int8(5); int8(2)});
%=============================================================================
% empty subscripts
assert_isequal(accumarray(zeros(0, 1), single(zeros(0, 1))), single(zeros(0, 1)));
assert_isequal(accumarray(zeros(0, 1), single(zeros(0, 1)), [3 1]), single([0; 0; 0]));
assert_isequal(accumarray(zeros(0, 1), int8(zeros(0, 1)), [3 1]), [0; 0; 0]);
assert_isequal(accumarray(zeros(0, 1), single(zeros(0, 1)), [3 1], @max), [0; 0; 0]);
assert_isequal(accumarray(zeros(0, 1), single(zeros(0, 1)), [3 1], @max, 4), [4; 4; 4]);
assert_isequal(accumarray(zeros(0, 2), single(zeros(0, 1))), single(zeros(0, 0)));
assert_isequal(accumarray(zeros(0, 2), zeros(0, 1), [2 2]), zeros(2, 2));
assert_isequal(accumarray(zeros(0, 1), 1), zeros(0, 1));
%=============================================================================
% subscript layouts
assert_isequal(accumarray([1 2 1], 5), [0 5]);
assert_isequal(accumarray([1; 2; 1], [5 6 7]), [12; 6]);
assert_isequal(accumarray({[1; 2; 1]}, [5; 6; 7]), [12; 6]);
assert_isequal(accumarray([1 1 1; 2 1 1], [1; 2]), [1; 2]);
assert_isequal(accumarray([1 1 1; 2 1 1], [1; 2], [2 1 1]), [1; 2]);
assert_isequal(accumarray(single([1; 2; 1]), [1; 2; 3]), [4; 2]);
%=============================================================================
% errors
msg_fill = _('The fill value must have the same class as the accumulated result.');
assert_checkerror('accumarray(subs, single(v), [5 1], [], 7)', msg_fill);
assert_checkerror('accumarray(subs, v, [5 1], [], single(2.5))', msg_fill);
assert_checkerror('accumarray(subs, v, [5 1], [], int8(9))', msg_fill);
assert_checkerror('accumarray(subs, v, [5 1], [], true)', msg_fill);
assert_checkerror('accumarray(subs, int8(v), [5 1], @max, -1)', msg_fill);
assert_checkerror('accumarray(subs, v, [5 1], @(x) int8(sum(x)), 2.5)', msg_fill);
assert_checkerror('accumarray([1; 3], [1; 2], [], [], ''a'')', msg_fill);
assert_checkerror('accumarray([1; 3], [1; 2], [], [], {3})', msg_fill);
assert_checkerror('accumarray([1; 3], [1; 2], [], @(x) {x}, 0)', msg_fill);
assert_checkerror('accumarray([1; 3], single([1; 2]), [], [], 1i)', msg_fill);
msg_same = _('The accumulation function must return values of a single class.');
assert_checkerror('accumarray([1; 2], [1; 2], [], @(x) local_mixed(x))', msg_same);
assert_checkerror('accumarray([1; 2], [65; 66], [], @(x) struct(''a'', x))', ...
_('The accumulation function must return a numeric, logical, char or cell value.'));
assert_checkerror('accumarray([1; 2], [65; 66], [], @(x) string(x))', ...
_('The accumulation function must return a numeric, logical, char or cell value.'));
msg_scalar = _('The accumulation function returned a non-scalar value.');
assert_checkerror('accumarray([1; 3], [1; 2], [], @(x) [x x])', msg_scalar);
assert_checkerror('accumarray([1; 3], [1; 2], [], @(x) [])', msg_scalar);
msg_sparse = _('Sparse output requires double or single values.');
assert_checkerror('accumarray(subs, int8(v), [3 1], [], 0, true)', msg_sparse);
assert_checkerror('accumarray(subs, logical(v), [3 1], @max, 0, true)', msg_sparse);
msg_sparse_res = _('Sparse output requires double or single results.');
assert_checkerror('accumarray([1; 2], [1; 2], [], @(x) int8(sum(x)), 0, true)', msg_sparse_res);
assert_checkerror('accumarray([1; 3], [1; 2], [3 1], @(x) {x}, [], true)', msg_sparse_res);
msg_sparse_fill = _('The fill value must be zero for sparse output.');
assert_checkerror('accumarray([1; 3], [1; 2], [3 1], [], 5, true)', msg_sparse_fill);
assert_checkerror('accumarray([1; 3], [1; 2], [3 1], [], false, true)', msg_sparse_fill);
assert_checkerror('accumarray([1 1 1], 1, [1 1 2], [], 0, true)', ...
_('Sparse output requires a two-dimensional result.'));
msg_flag = _('The sparse flag must be a logical scalar.');
assert_checkerror('accumarray([1; 2], [1; 2], [], [], 0, 2)', msg_flag);
assert_checkerror('accumarray([1; 2], [1; 2], [], [], 0, [1 1])', msg_flag);
assert_checkerror('accumarray([1; 2], [1; 2], [], [], 0, ''a'')', msg_flag);
msg_vals = _('The number of values must match the number of subscript rows.');
assert_checkerror('accumarray([1; 3], [1; 2; 3])', msg_vals);
assert_checkerror('accumarray([1; 2; 1; 2], single([1 2; 3 4]))', msg_vals);
assert_checkerror('accumarray([1 2 1], [5; 6; 7])', msg_vals);
assert_checkerror('accumarray([1; 2], [])', msg_vals);
assert_checkerror('accumarray(zeros(0, 1), zeros(0, 0))', msg_vals);
assert_checkerror('accumarray([1; 3], {1; 2})', _('Values must be numeric, logical or char.'));
msg_subs = _('Subscripts must be positive integers.');
assert_checkerror('accumarray([0; 1], [1; 2])', msg_subs);
assert_checkerror('accumarray([1.5; 1], [1; 2])', msg_subs);
assert_checkerror('accumarray([NaN; 1], [1; 2])', msg_subs);
assert_checkerror('accumarray([Inf; 1], [1; 2])', msg_subs);
msg_subs_type = _('Subscripts must be a real numeric matrix or a cell array of vectors.');
assert_checkerror('accumarray(true(2, 1), [1; 2])', msg_subs_type);
assert_checkerror('accumarray(''AB'', [1; 2])', msg_subs_type);
assert_checkerror('accumarray(sparse([1; 2]), [1; 2])', msg_subs_type);
assert_checkerror('accumarray([], [])', ...
_('Subscripts must be a non-empty column vector or a two-dimensional matrix.'));
assert_checkerror('accumarray([4; 1], [1; 2], [3 1])', _('Subscripts exceed the requested output size.'));
msg_size = _(['The size must be a vector of nonnegative integers with one entry per ', ...
'subscript column ([n 1] for column subscripts).']);
assert_checkerror('accumarray([1; 2], [1; 2], 3)', msg_size);
assert_checkerror('accumarray([1; 2], [1; 2], [1 5])', msg_size);
assert_checkerror('accumarray([1; 2], [1; 2], [5 2])', msg_size);
assert_checkerror('accumarray([1 1; 2 2], [1; 2], [3 3 3])', msg_size);
assert_checkerror('accumarray([1 1; 2 2], [1; 2], 2)', msg_size);
assert_checkerror('accumarray([1; 2], [1; 2], [2.5 1])', msg_size);
assert_checkerror('accumarray([1; 3], [1; 2], [], [], [1 2])', _('The fill value must be a full scalar.'));
assert_checkerror('accumarray([1; 3], [1; 2], [], [], sparse(1))', _('The fill value must be a full scalar.'));
%=============================================================================
function r = local_mixed(x)
  if x(1) == 1
    r = int8(x);
  else
    r = 2.5;
  end
end
%=============================================================================
