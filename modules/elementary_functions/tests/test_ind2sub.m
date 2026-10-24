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
ind = [3 4 5 6];
sz = [3 3];
[row, col] = ind2sub(sz, ind);
row_REF = [3 1 2 3];
col_REF = [1 2 2 2];
asserts.isequal(row, row_REF);
asserts.isequal(col, col_REF);
%=============================================================================
ind = [3 4 5 6];
sz = [2 2 2];
[I1, I2, I3] = ind2sub(sz, ind);
I1_REF = [1 2 1 2];
I2_REF = [2 2 1 1];
I3_REF = [1 1 2 2];
asserts.isequal(I1, I1_REF);
asserts.isequal(I2, I2_REF);
asserts.isequal(I3, I3_REF);
%=============================================================================
A = rand(3, 4, 2);
[row, col, page] = ind2sub(size(A), 14);
row_REF = 2;
col_REF = 1;
page_REF = 2;
asserts.isequal(row, row_REF);
asserts.isequal(col, col_REF);
asserts.isequal(page, page_REF);
%=============================================================================
[rowIndex, columnIndex] = ind2sub([3 3], [4 5 6]);
asserts.isequal(rowIndex, [1 2 3]);
asserts.isequal(columnIndex, [2 2 2]);
%=============================================================================
