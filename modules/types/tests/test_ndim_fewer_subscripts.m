%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Indexing an N-D array with fewer subscripts than dimensions folds the
% trailing dimensions into the last subscript (column-major rule).
%=============================================================================
A = reshape(1:24, [3 4 2]);
%=============================================================================
% Read: last subscript spans the product of the trailing dimensions.
assert_isequal(size(A(:, :)), [3 8]);
assert_isequal(A(:, :), [1 4 7 10 13 16 19 22; 2 5 8 11 14 17 20 23; 3 6 9 12 15 18 21 24]);
assert_isequal(size(A(2, :)), [1 8]);
assert_isequal(A(2, :), [2 5 8 11 14 17 20 23]);
assert_isequal(size(A(:, 5)), [3 1]);
assert_isequal(A(:, 5), [13; 14; 15]);
assert_isequal(A(3, 8), 24);
assert_isequal(A(2, 5), 14);
assert_isequal(A(1, 1), 1);
%=============================================================================
% Read on a larger 3-D array.
B = reshape(1:60, [3 4 5]);
assert_isequal(size(B(:, :)), [3 20]);
assert_isequal(B(3, 20), 60);
assert_isequal(B(1, 1), 1);
%=============================================================================
% Read on a 4-D array: full fold (L = 2) and partial fold (L = 3).
C = reshape(1:24, [2 3 2 2]);
assert_isequal(size(C(:, :)), [2 12]);
assert_isequal(C(2, 7), 14);
assert_isequal(size(C(:, :, :)), [2 3 4]);
assert_isequal(C(2, 3, 3), 18);
%=============================================================================
% Assignment through a full colon keeps the original shape.
A2 = reshape(1:24, [3 4 2]);
A2(:, :) = reshape(101:124, [3 8]);
assert_isequal(size(A2), [3 4 2]);
assert_isequal(A2(1, 1, 1), 101);
assert_isequal(A2(3, 4, 2), 124);
assert_isequal(A2(3, 8), 124);
%=============================================================================
% Assignment into one folded row.
A3 = reshape(1:24, [3 4 2]);
A3(2, :) = 200:207;
assert_isequal(size(A3), [3 4 2]);
assert_isequal(A3(2, :), 200:207);
assert_isequal(A3(2, 1, 1), 200);
assert_isequal(A3(2, 4, 2), 207);
%=============================================================================
% Assignment into one folded column.
A4 = reshape(1:24, [3 4 2]);
A4(:, 5) = [-1; -2; -3];
assert_isequal(size(A4), [3 4 2]);
assert_isequal(A4(:, :, 2), [-1 16 19 22; -2 17 20 23; -3 18 21 24]);
%=============================================================================
% Scalar assignment addresses the folded element.
A5 = reshape(1:24, [3 4 2]);
A5(3, 8) = 999;
assert_isequal(size(A5), [3 4 2]);
assert_isequal(A5(3, 4, 2), 999);
%=============================================================================
% Scalar broadcast into the whole folded region.
A6 = reshape(1:24, [3 4 2]);
A6(:, :) = 7;
assert_isequal(A6, reshape(repmat(7, 1, 24), [3 4 2]));
%=============================================================================
% A leading subscript may grow while the folded trailing extent stays fixed.
A7 = reshape(1:24, [3 4 2]);
A7(4, 8) = 5;
assert_isequal(size(A7), [4 4 2]);
assert_isequal(A7(4, 8), 5);
assert_isequal(A7(3, 8), 24);
assert_isequal(A7(4, 1), 0);
%=============================================================================
% Growing a leading subscript in a 4-D array.
C2 = reshape(1:24, [2 3 2 2]);
C2(3, 7) = 77;
assert_isequal(size(C2), [3 3 2 2]);
assert_isequal(C2(3, 7), 77);
%=============================================================================
% The folded (last) subscript cannot exceed the trailing product, read...
assert_checkerror('A = reshape(1:24, [3 4 2]); A(3,9)', _('Index exceeds array bounds.'));
% ... or in assignment.
cmd = 'A = reshape(1:24, [3 4 2]); A(3,9) = 5;';
assert_checkerror(cmd, _('Index exceeds array bounds.'));
%=============================================================================
% Cell content indexing (curly braces) folds the same way.
Cc = reshape(num2cell(1:24), [3 4 2]);
assert_isequal(size(Cc(:, :)), [3 8]);
assert_isequal(Cc{3, 8}, 24);
assert_isequal(Cc{2, 5}, 14);
assert_isequal([Cc{2, :}], [2 5 8 11 14 17 20 23]);
%=============================================================================
% Cell content assignment keeps the original shape.
Cc{3, 8} = 999;
assert_isequal(size(Cc), [3 4 2]);
assert_isequal(Cc{3, 4, 2}, 999);
%=============================================================================
% Cell content assignment may grow a leading subscript.
Dd = reshape(num2cell(1:24), [3 4 2]);
Dd{4, 8} = 77;
assert_isequal(size(Dd), [4 4 2]);
assert_isequal(Dd{4, 8}, 77);
assert_isequal(Dd{3, 8}, 24);
%=============================================================================
% Linear indexing (a single subscript) is unchanged.
A = reshape(1:24, [3 4 2]);
assert_isequal(A(5), 5);
assert_isequal(A(24), 24);
assert_isequal(A([1 5 24]), [1 5 24]);
assert_isequal(A(:), (1:24)');
%=============================================================================
% More subscripts than dimensions: trailing subscripts must be 1.
assert_isequal(A(2, 3, 2, 1), A(2, 3, 2));
assert_checkerror('A = reshape(1:24, [3 4 2]); A(2,3,2,2)', _('Index exceeds array bounds.'));
%=============================================================================
