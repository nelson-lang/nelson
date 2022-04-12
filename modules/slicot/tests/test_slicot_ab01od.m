%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/AB01OD.html
% [A_OUT, B_OUT, U_OUT, V, NCONT_OUT, INDCON_OUT, KSTAIR_OUT, INFO] = slicot_ab01od(STAGES, JOBU, JOBV, A_IN, B_IN, U_IN, NCONT_IN, INDCON_IN, KSTAIR_IN, TOL)
assert_isequal(nargin('slicot_ab01od'), 10);
assert_isequal(nargout('slicot_ab01od'), 8);
%=============================================================================
N = 5;
M = 2;
TOL = 0;
STAGES = 'F';
JOBU = 'N';
JOBV = 'N';
A_IN = [17.0   24.0    1.0    8.0   15.0;
23.0    5.0    7.0   14.0   16.0;
4.0    6.0   13.0   20.0   22.0;
10.0   12.0   19.0   21.0    3.0;
11.0   18.0   25.0    2.0    9.0];

% SLICOT 5.0 have an error in the example.
A_IN = A_IN.';

B_IN = [   -1.0   -4.0;
4.0    9.0;
-9.0  -16.0;
16.0   25.0;
-25.0  -36.0];

U_IN = zeros(N, N);
INDCON_IN = N;
NCONT_IN = 1;
KSTAIR_IN = zeros(1,N);
[A_OUT, B_OUT, U_OUT, V, NCONT_OUT, INDCON_OUT, KSTAIR_OUT, INFO] = slicot_ab01od(STAGES, JOBU, JOBV, A_IN, B_IN, U_IN, NCONT_IN, INDCON_IN, KSTAIR_IN, TOL);
%=============================================================================
A_OUT_REF = [12.8848      3.2345     11.8211      3.3758     -0.8982;
4.4741    -12.5544      5.3509      5.9403      1.4360;
14.4576      7.6855     23.1452     26.3872    -29.9557;
0.0000      1.4805     27.4668     22.6564     -0.0072;
0.0000      0.0000    -30.4822      0.6745     18.8680];
assert_isapprox(A_OUT, A_OUT_REF, 1e-4);
%=============================================================================
B_OUT_REF = [31.1199     47.6865;
3.2480      0.0000;
0.0000      0.0000;
0.0000      0.0000;
0.0000      0.0000];
assert_isapprox(B_OUT, B_OUT_REF, 1e-4);
%=============================================================================
U_OUT_REF = zeros(N, N);
assert_isequal(U_OUT, U_OUT_REF);
%=============================================================================
V_REF = [0     0; 0     0];
assert_isequal(V, V_REF);
%=============================================================================
NCONT_OUT_REF = int32(5);
assert_isequal(NCONT_OUT, NCONT_OUT_REF);
%=============================================================================
INDCON_OUT_REF = int32(3);
assert_isequal(INDCON_OUT, INDCON_OUT_REF);
%=============================================================================
KSTAIR_OUT_REF = int32([2 2 1 0 0]);
assert_isequal(KSTAIR_OUT, KSTAIR_OUT_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
