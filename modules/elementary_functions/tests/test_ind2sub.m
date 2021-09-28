%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
ind = [3 4 5 6];
sz = [3 3];
[row, col] = ind2sub(sz, ind);
row_REF = [3     1     2     3];
col_REF = [1     2     2     2];
assert_isequal(row, row_REF);
assert_isequal(col, col_REF);
%=============================================================================
ind = [3 4 5 6];
sz = [2 2 2];
[I1, I2, I3] = ind2sub(sz, ind);
I1_REF = [1     2     1     2];
I2_REF = [2     2     1     1];
I3_REF = [1     1     2     2];
assert_isequal(I1, I1_REF);
assert_isequal(I2, I2_REF);
assert_isequal(I3, I3_REF);
%=============================================================================
A = rand(3, 4, 2);
[row, col, page] = ind2sub(size(A), 14);
row_REF = 2;
col_REF = 1;
page_REF = 2;
assert_isequal(row, row_REF);
assert_isequal(col, col_REF);
assert_isequal(page, page_REF);
%=============================================================================
