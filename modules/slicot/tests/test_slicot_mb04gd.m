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
% http://slicot.org/objects/software/shared/doc/MB04GD.html
% [A_OUT, JPVT_OUT, TAU, INFO] = slicot_mb04gd(A_IN, JPVT_IN)
assert_isequal(nargin('slicot_mb04gd'), 2);
assert_isequal(nargout('slicot_mb04gd'), 4);
%=============================================================================
M = 6;
N = 5;
A_IN = [1.    2.    6.    3.    5.;
-2.   -1.   -1.    0.   -2.;
5.    5.    1.    5.    1.;
-2.   -1.   -1.    0.   -2.;
4.    8.    4.   20.    4.;
-2.   -1.   -1.    0.   -2.];
JPVT_IN = zeros(1, M);
[A_OUT, JPVT_OUT, TAU, INFO] = slicot_mb04gd(A_IN, JPVT_IN);
%=============================================================================
A_OUT_REF = [0.0000     -1.0517     -1.8646     -1.9712      1.2374;
0.0000     -1.0517     -1.8646     -1.9712      1.2374;
0.1859     -1.0517     -1.8646     -1.9712      1.2374;
-0.6955     -0.4765      4.6768      0.0466     -7.4246;
0.0496      0.0992     -0.3823      6.7059     -5.4801;
0.1502      0.3004      0.1502      0.7511    -22.6274];
assert_isapprox(A_OUT, A_OUT_REF, 1e-4);
%=============================================================================
JPVT_OUT_REF = int32([2, 4, 6, 3, 1, 5]);
assert_isequal(JPVT_OUT, JPVT_OUT_REF);
%=============================================================================
TAU_REF = [0.0000      1.9332      1.1691      1.7265      1.1768];
assert_isapprox(TAU, TAU_REF, 1e-4);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
