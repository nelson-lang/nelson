%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% NaN never matches another NaN in set operations
asserts.isequal(intersect([NaN 1], [NaN 1]), 1);
asserts.isequal(setdiff([NaN 1], [NaN 1]), NaN);
asserts.isequal(setxor(NaN, NaN), [NaN NaN]);
asserts.isequal(union([NaN 1], [NaN 2]), [1 2 NaN NaN]);
%=============================================================================
% exact equality: no truncation at 15 digits
asserts.istrue(isempty(intersect(1, 1 + eps)));
asserts.isequal(setdiff(1, 1 + eps), 1);
%=============================================================================
% -0 == 0
asserts.isequal(intersect(0, -0), 0);
asserts.istrue(isempty(setxor(0, -0)));
asserts.isequal(union([-0 1], 0), [0 1]);
%=============================================================================
% union indices: ia refers to A, ib to elements only found in B
[c, ia, ib] = union([2 1], [3 1]);
asserts.isequal(c, [1 2 3]);
asserts.isequal(ia, [2; 1]);
asserts.isequal(ib, 1);
%=============================================================================
[c, ia, ib] = intersect([5 7 1], [3 1 1]);
asserts.isequal(c, 1);
asserts.isequal(ia, 3);
asserts.isequal(ib, 2);
%=============================================================================
% setdiff orientation follows A
r = setdiff([1 2], []);
asserts.isequal(size(r), [1 2]);
asserts.isequal(r, [1 2]);
r = setdiff([1; 2], []);
asserts.isequal(size(r), [2 1]);
%=============================================================================
% duplicated values: first occurrence indices
[c, ia] = setdiff([3 3 1 2 2], [2 4 4]);
asserts.isequal(c, [1 3]);
asserts.isequal(ia, [3; 1]);
%=============================================================================
% stable option
asserts.isequal(setdiff([3 1 2 1], 2, 'stable'), [3 1]);
[c, ia, ib] = union([2 1], [3 1], 'stable');
asserts.isequal(c, [2 1 3]);
asserts.isequal(ia, [1; 2]);
asserts.isequal(ib, 1);
[c, ia, ib] = setxor([NaN 1], [NaN 2], 'stable');
asserts.isequal(c, [NaN 1 NaN 2]);
asserts.isequal(ia, [1; 2]);
asserts.isequal(ib, [1; 2]);
%=============================================================================
% cell arrays of char
[c, ia, ib] = intersect({'b', 'a', 'b', 'c'}, {'a', 'd', 'd'});
asserts.isequal(c, {'a'});
asserts.isequal(ia, 2);
asserts.isequal(ib, 1);
asserts.isequal(union({'b', 'a'}, {'c'}), {'a', 'b', 'c'});
%=============================================================================
% integer types
asserts.isequal(intersect(int8([3 1 2]), int8([2 5])), int8(2));
%=============================================================================
