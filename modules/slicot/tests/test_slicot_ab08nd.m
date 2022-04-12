%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/AB08ND.html
% [NU, RANK, DINFZ, NKROR, NKROL, INFZ, KRONR, KRONL, AF, BF, INFO] = slicot_ab08nd(EQUIL, N, M, P, A, B, C, D, TOL)
assert_isequal(nargin('slicot_ab08nd'), 9);
assert_isequal(nargout('slicot_ab08nd'), 11);
%=============================================================================
N = 6;
M = 2;
P = 3;
TOL = 0.0;
EQUIL = 'N';
%=============================================================================
A  = [1.0   0.0   0.0   0.0   0.0   0.0;
0.0   1.0   0.0   0.0   0.0   0.0;
0.0   0.0   3.0   0.0   0.0   0.0;
0.0   0.0   0.0  -4.0   0.0   0.0;
0.0   0.0   0.0   0.0  -1.0   0.0;
0.0   0.0   0.0   0.0   0.0   3.0];
%=============================================================================
B = [0.0  -1.0;
-1.0   0.0;
1.0  -1.0;
0.0   0.0;
0.0   1.0;
-1.0  -1.0];
%=============================================================================
C = [1.0   0.0   0.0   1.0   0.0   0.0;
0.0   1.0   0.0   1.0   0.0   1.0;
0.0   0.0   1.0   0.0   0.0   1.0];
D = [0.0   0.0;
0.0   0.0;
0.0   0.0];
%=============================================================================
% Check the observability and compute the ordered set of the observability indices (call the routine with M = 0).
[NU, RANK, DINFZ, NKROR, NKROL, INFZ, KRONR, KRONL, AF, BF, INFO] = slicot_ab08nd(EQUIL, N, 0, P, A, B, C, D, TOL);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
AF_REF = zeros(6, 6); AF_REF(1, 1) = -1;
assert_isapprox(AF, AF_REF, 1e-4);
KRONL_REF = int32([1, 2, 2, 0, 0, 0, 0]);
assert_isequal(KRONL, KRONL_REF);
INFZ_REF = int32(zeros(1, N));
assert_isequal(INFZ, INFZ_REF);
NKROL_REF = int32(3);
assert_isequal(NKROL, NKROL_REF);
NKROR_REF = int32(0);
assert_isequal(NKROR, NKROR_REF);
DINFZ_REF = int32(0);
assert_isequal(DINFZ, DINFZ_REF);
RANK_REF = int32(0);
assert_isequal(RANK, RANK_REF);
NU_REF = int32(1);
assert_isequal(NU, NU_REF);
%=============================================================================
% Check the controllability and compute the ordered set of the controllability indices (call the routine with P = 0)
[NU, RANK, DINFZ, NKROR, NKROL, INFZ, KRONR, KRONL, AF, BF, INFO] = slicot_ab08nd(EQUIL, N, M, 0, A, B, C, D, TOL);
NU_REF = int32(1);
assert_isequal(NU, NU_REF);
RANK_REF = int32(0);
assert_isequal(RANK, RANK_REF);
DINFZ_REF = int32(0);
assert_isequal(DINFZ, DINFZ_REF);
NKROR_REF = int32(2);
assert_isequal(NKROR, NKROR_REF);
NKROL_REF = int32(0);
assert_isequal(NKROL, NKROL_REF);
INFZ_REF = int32(zeros(1, 6));
assert_isequal(INFZ, INFZ_REF);
KRONR_REF = int32([2, 3, 0, 0, 0, 0, 0]);
assert_isequal(KRONR, KRONR_REF);
KRONL_REF = int32(zeros(1, 7));
assert_isequal(KRONL, KRONL_REF);
AF_REF = [-4.0000     -0.0000      0.0000     -0.3405     -0.6667     -0.8333;
0.0000      0.8182     -0.0000      0.9705      0.0000     -1.1667;
-0.0000      0.0000      1.6667      0.0000      0.0000      0.0000;
0.0000     -1.0285     -0.0000      0.6818      0.6667     -0.8333;
0.0000      0.0000     -0.9428      0.0000      2.3333      0.0000;
0.0000      0.0000      0.0000      1.6583      0.0000      1.5000;
0.0000      0.0000      0.0000      0.0000      1.7321      0.0000;
0.0000      0.0000      0.0000      0.0000      0.0000      2.0000];
assert_isapprox(AF, AF_REF, 1e-4);
BF_REF = [1   -1    0    0    0   -1;
-1    0    0    0   -4    0;
1    1    0    3    0    0;
0   -1    1    0    0    0;
0    1    0    0    0    0;
-1    0    0    0    0    0;
-1    0    0    0    0    0;
0    0    0    0    0    3];
assert_isequal(BF, BF_REF);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
% Compute the structural invariants of the given system.
[NU, RANK, DINFZ, NKROR, NKROL, INFZ, KRONR, KRONL, AF, BF, INFO] = slicot_ab08nd(EQUIL, N, M, P, A, B, C, D, TOL);
NU_REF = int32(2);
assert_isequal(NU, NU_REF);
RANK_REF = int32(2);
assert_isequal(RANK, RANK_REF);
DINFZ_REF = int32(1);
assert_isequal(DINFZ, DINFZ_REF);
NKROR_REF = int32(0);
assert_isequal(NKROR, NKROR_REF);
NKROL_REF = int32(1);
assert_isequal(NKROL, NKROL_REF);
INFZ_REF = int32([2              0              0              0              0              0]);
assert_isequal(INFZ, INFZ_REF);
KRONR_REF = int32(zeros(1, 7));
assert_isequal(KRONR, KRONR_REF);
KRONL_REF = int32([2, 0, 0, 0, 0, 0, 0]);
assert_isequal(KRONL, KRONL_REF);
AF_REF = [1.7046      0.6822      1.7046      0.6822      0.0000      0.0000      0.0000      0.0000
0.6822     -0.6850      0.6822     -0.6850      0.0000      0.0000      0.0000      0.0000
-1.7321     -0.0000      0.1658      0.0440      0.0000      0.0000      0.0000      0.0000
0.0000     -2.0000      0.0687     -0.2590      0.0000      0.0000      0.0000      0.0000
0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000
0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000
0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000
0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000];
assert_isapprox(AF, AF_REF, 1e-4);
BF_REF = [0.9378      0.0000      0.0000      0.0000      0.0000     -1.5494      2.5750      0.6936     -0.7006
0.0190      0.0190      0.0000      0.0000      0.0000     -1.9365     -0.1633     -0.1633      1.6947
1.7321      0.8711      0.9378      0.0000      0.0000      0.0000     -1.0042     -0.2000     -1.0042
0.0000      0.0000      0.0190      0.0190      0.0000      0.0000      1.2649     -1.7889     -1.7889
0.0000      1.6330     -0.0000      0.8711     -0.1209      0.0000      0.0000      0.2582      0.0000
0.0000      0.0000      0.2418      0.0000     -0.4556     -0.3163      0.0000     -1.2910     -0.5774
0.0000      0.0000      0.0000      0.9113     -0.7083     -0.0999     -0.7908      0.0000     -0.5774
0.0000      0.0000      0.0000      0.0000     -0.6457      0.6319     -1.0771     -1.0467     -1.7321];
assert_isapprox(BF, BF_REF, 1e-4);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
