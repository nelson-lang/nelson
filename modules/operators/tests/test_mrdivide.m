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
R = 2 / [1;2;3];
REF = [0                   0   0.666666666666667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1 1 3; 2 0 4; -1 6 -1];
B = [2 19 8];
R1 = B / A;
R2 = mrdivide(B, A);
assert_isequal(R1, R2);
REF = [ 1.0000    2.0000    3.0000];
assert_isapprox(R1, R2, 1e-4);
%=============================================================================
A = [1 0; 2 0; 1 0];
B = [1 2];
R = B / A;
RR = R*A-B;
REF = [ 0 -2];
assert_isapprox(RR, REF, 1e-4);
%=============================================================================
A = [1 0; 2 0; 1 0];
B = [1 2];
R = A / B;
REF = [0.2 ; 0.4; 0.2];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [3., -24., 30.];
B = [   9.   -36.    30.;
  -36.   192.  -180.;
   30.  -180.   180.];
R = A / B;
REF = [1 1 1];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = B / A;
REF = [   1.206060606060606;
-6.812121212121213;
 6.606060606060606];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [6 2; 4 1] / [4 7;8 5];
REF = [ -0.388888888888889   0.944444444444444;
-0.333333333333333   0.666666666666667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [3i 4i 6i+4] / [5 9i-4 8];
REF = [ 0.365591397849462 + 0.252688172043011i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
assert_checkerror('R = [1; 4] / [2 4];', _('Requested divide operation requires arguments to have correct dimensions.'));
%=============================================================================
assert_checkerror('R = 2 / [1 2 3];', _('Requested divide operation requires arguments to have correct dimensions.'))
%=============================================================================
