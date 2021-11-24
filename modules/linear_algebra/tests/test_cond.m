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
A = [7, 2, 3; 1, 3, 4; 6, 4, 5];
R = cond(A);
assert_isapprox(R, 51.0278, 1e-4);
%=============================================================================
A = [7, 2, 3; 1, 3, 4; 6, 4, 5];
p = 1;
R = cond(A, p);
assert_isapprox(R, 57.2727, 1e-4);
%=============================================================================
R = cond ([1, 2; 2, 1]);
assert_isapprox(R, 3, 1e-1);
%=============================================================================
R = cond([1, 2, 3; 4, 5, 6; 7, 8, 9]);
assert_istrue(R > 1e16);
%=============================================================================
