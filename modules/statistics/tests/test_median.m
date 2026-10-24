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
%=============================================================================
asserts.isequal(nargin('median'), -4);
asserts.isequal(nargout('median'), 1);
%=============================================================================
asserts.isequal(median([3 1 2]), 2);
asserts.isequal(median([4 1 2 3]), 2.5);
%=============================================================================
M = [1 4 7; 2 5 8; 3 6 9];
asserts.isequal(median(M), [2 5 8]);
asserts.isequal(median(M, 2), [4; 5; 6]);
asserts.isequal(median(M, 'all'), 5);
%=============================================================================
asserts.isequal(median([1 NaN 3], 'omitnan'), 2);
asserts.istrue(isnan(median([1 NaN 3], 'includenan')));
%=============================================================================
asserts.isequal(median(uint8([1 5 9])), uint8(5));
asserts.isequal(median(uint8([1 5])), uint8(3));
%=============================================================================
asserts.isequal(median([1 NaN 3], 'omitnan'), 2);
%=============================================================================
% vector of dimensions (vecdim)
A = reshape(1:24, [2 3 4]);
assert_isequal(size(median(A, [1 2])), [1 1 4]);
assert_isequal(median(A, [1 2]), reshape([3.5 9.5 15.5 21.5], [1 1 4]));
assert_isequal(size(median(A, [1 3])), [1 3]);
assert_isequal(median(A, [1 3]), [10.5 12.5 14.5]);
assert_isequal(median(A, [2 3]), [12; 13]);
assert_isequal(median(A, [1 2 3]), 12.5);
assert_isequal(median(A, [1 2 3]), median(A, 'all'));
% a vector that lists a single effective dimension behaves like a scalar dim
assert_isequal(median(A, [2]), median(A, 2));
% omitnan combined with a vector of dimensions
C = cat(3, [1 NaN; 3 4], [5 6; NaN 8]);
assert_isequal(median(C, [1 2], 'omitnan'), reshape([3 6], [1 1 2]));
%=============================================================================
