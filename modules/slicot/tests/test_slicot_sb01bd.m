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
% http://slicot.org/objects/software/shared/doc/SB01BD.html
% [A_OUT, WR_OUT, WI_OUT, NFP, NAP, NUP, F, Z, IWARN, INFO] = slicot_sb01bd(DICO, ALPHA, A_IN, B_IN, WR_IN, WI_IN, TOL)
assert_isequal(nargin('slicot_sb01bd'), 7);
assert_isequal(nargout('slicot_sb01bd'), 10);
%=============================================================================
N = 4;
M = 2;
NP = 2;
ALPHA = -.4;
TOL = 1.E-8;
DICO = 'C';

A_IN = [  -6.8000   0.0000  -207.0000   0.0000;
1.0000   0.0000     0.0000   0.0000;
43.2000   0.0000     0.0000  -4.2000;
0.0000   0.0000     1.0000   0.0000];

B_IN = [   5.6400   0.0000;
0.0000   0.0000;
0.0000   1.1800;
0.0000   0.0000];

WR_IN = [-0.5000; -0.5000];
WI_IN = [ 0.1500; -0.1500];

[A_OUT, WR_OUT, WI_OUT, NFP, NAP, NUP, F, Z, IWARN, INFO] = slicot_sb01bd(DICO, ALPHA, A_IN, B_IN, WR_IN, WI_IN, TOL);
%=============================================================================
A_OUT_REF = [-3.3984    -43.1607    -21.2945    -11.5012;
207.0176     -3.3984    -18.0175    102.5410;
0.0000      0.0000     -0.5000     -0.2286;
0.0000      0.0000      0.0984     -0.5000];
assert_isapprox(A_OUT, A_OUT_REF, 1e-4);
%=============================================================================
WR_OUT_REF = [-0.5000;-0.5000];
assert_isapprox(WR_OUT, WR_OUT_REF, 1e-4);
%=============================================================================
WI_OUT_REF = [ 0.1500; -0.1500];
assert_isapprox(WI_OUT, WI_OUT_REF, 1e-4);
%=============================================================================
NFP_REF = int32(2);
assert_isequal(NFP, NFP_REF);
%=============================================================================
NAP_REF = int32(2);
assert_isequal(NAP, NAP_REF);
%=============================================================================
NUP_REF = int32(0);
assert_isequal(NUP, NUP_REF);
%=============================================================================
F_REF = [-0.0876     -4.2138      0.0837    -18.1412;
-0.0233     18.2483     -0.4259     -4.8120;
0.0000      0.0000      0.0000      0.0000;
0.0000      0.0000      0.0000      0.0000];
assert_isapprox(F, F_REF, 1e-4);
%=============================================================================
Z_REF = [      0.0208     -0.9998      0.0001      0.0048;
0.0231      0.0005     -0.9996      0.0138;
0.9995      0.0208      0.0231      0.0004;
-0.0009      0.0048      0.0137      0.9999];
assert_isapprox(Z, Z_REF, 1e-4);
%=============================================================================
IWARN_REF = int32(0);
assert_isequal(IWARN, IWARN_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
