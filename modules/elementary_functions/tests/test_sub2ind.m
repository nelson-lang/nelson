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
row = [1 2 3 1];
col = [2 2 2 3];
sz = [3 3];
ind = sub2ind(sz, row, col);
REF = [ 4     5     6     7];
assert_isequal(ind, REF);
%=============================================================================
I1 = [1 2 1 2];
I2 = [2 2 1 1];
I3 = [1 1 2 2];
sz = [2 2 2];
ind = sub2ind(sz, I1, I2, I3);
REF = [3     4     5     6];
assert_isequal(ind, REF);
%=============================================================================
A = rand(3, 4, 2);
R = sub2ind(size(A), 2, 1, 2);
REF = 14;
assert_isequal(R, REF);
%=============================================================================
