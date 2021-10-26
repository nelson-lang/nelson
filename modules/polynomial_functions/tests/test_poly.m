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
A = [1 8 -10; -4 2 4; -5 2 8];
e = eig(A);
R = poly(e);
REF = [1.0000  -11.0000    0.0000  -84.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1 2 3; 4 5 6; 7 8 0];
R = poly(A);
REF = [ 1.0000   -6.0000  -72.0000  -27.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1    2    3;  4    5    6; 7    8    0];
R = poly(A);
REF = [1  -6 -72 -27];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1    2    3];
R = poly(A);
REF = [ 1    -6    11    -6];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = 2;
R = poly(A);
REF = [1, -2];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = 0;
R = poly(A);
REF = [1, 0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [];
R = poly(A);
REF = 1;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1i, 2i];
R = poly(A);
REF = [1, -3i, -2];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
