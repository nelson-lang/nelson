%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% size, numel and friends report the shape of the calendar duration array
a = calmonths([1; 2]);
assert_isequal(size(a), [2 1]);
assert_isequal(numel(a), 2);
assert_isequal(length(a), 2);
assert_isequal(ndims(a), 2);
assert_isequal(size(a, 1), 2);
[r, c] = size(a);
assert_isequal([r, c], [2 1]);
assert_isfalse(isscalar(a));
assert_istrue(iscolumn(a));
assert_isfalse(isrow(a));
assert_istrue(isvector(a));
assert_isfalse(isempty(a));
assert_istrue(isempty(caldays([])));
assert_istrue(isscalar(calyears(1)));
assert_isequal(calmonths(a(end)), 2);
%=============================================================================
% concatenation keeps one array
b = [calmonths(1); calmonths(2)];
assert_isequal(size(b), [2 1]);
assert_isequal(numel(b), 2);
b = [calmonths(1), caldays(2), calyears(1)];
assert_isequal(size(b), [1 3]);
%=============================================================================
% two-subscript indexing
m = calmonths([1 2; 3 4]);
assert_isequal(size(m), [2 2]);
assert_isequal(calmonths(m(2, :)), [3 4]);
%=============================================================================
% parenthesis assignment and deletion keep one calendarDuration array
a = calmonths([1; 2; 3]);
a(2) = calyears(1);
assert_isequal(class(a), 'calendarDuration');
assert_isequal(size(a), [3 1]);
assert_isequal(calmonths(a), [1; 12; 3]);
a(4) = caldays(5);
assert_isequal(size(a), [4 1]);
assert_isequal(split(a, 'days'), [0; 0; 0; 5]);
a(1) = [];
assert_isequal(size(a), [3 1]);
assert_isequal(calmonths(a), [12; 3; 0]);
a.Format = 'md';
assert_isequal(a.Format, 'md');
%=============================================================================
% transpose and reshape
assert_isequal(size(calmonths([1; 2])'), [1 2]);
assert_isequal(size(calmonths([1; 2]).'), [1 2]);
c = reshape(calmonths(1:6), 2, 3);
assert_isequal(size(c), [2 3]);
assert_isequal(calmonths(c), reshape(1:6, 2, 3));
%=============================================================================
