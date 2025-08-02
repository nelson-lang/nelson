%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
L = 9;
N = 9;
M = 3;
P = 3;
TOL= 1.e-7;
EQUIL='N';
A_IN = [1     0     0     0     0     0     0     0     0;
0     1     0     0     0     0     0     0     0;
0     0     1     0     0     0     0     0     0;
0     0     0     1     0     0     0     0     0;
0     0     0     0     1     0     0     0     0;
0     0     0     0     0     1     0     0     0;
0     0     0     0     0     0     1     0     0;
0     0     0     0     0     0     0     1     0;
0     0     0     0     0     0     0     0     1];

E_IN = [0     0     0     0     0     0     0     0     0;
1     0     0     0     0     0     0     0     0;
0     1     0     0     0     0     0     0     0;
0     0     0     0     0     0     0     0     0;
0     0     0     1     0     0     0     0     0;
0     0     0     0     1     0     0     0     0;
0     0     0     0     0     0     0     0     0;
0     0     0     0     0     0     1     0     0;
0     0     0     0     0     0     0     1     0];

B =[-1     0     0;
0     0     0;
0     0     0;
0    -1     0;
0     0     0;
0     0     0;
0     0    -1;
0     0     0;
0     0     0];

C = [ 0     1     1     0     3     4     0     0     2;
0     1     0     0     4     0     0     2     0;
0     0     1     0    -1     4     0    -2     2];

D = [ 1     2    -2;
0    -1    -2;
0     0     0];
%=============================================================================
% default call for the fortran routine
M = 3; P = 3;
[A_OUT, E_OUT, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, INFO] = slicot_ag08bd(EQUIL, M, P, A_IN, E_IN, B, C, D, TOL);
%=============================================================================
NFZ_REF = int32(1);
assert_isequal(NFZ, NFZ_REF);
%=============================================================================
NRANK_REF = int32(11);
assert_isequal(NRANK, NRANK_REF);
%=============================================================================
NIZ_REF = int32(2);
assert_isequal(NIZ, NIZ_REF);
%=============================================================================
DINFZ_REF = int32(2);
assert_isequal(DINFZ, DINFZ_REF);
%=============================================================================
NKROR_REF = int32(1);
assert_isequal(NKROR, NKROR_REF);
%=============================================================================
NINFE_REF = int32(5);
assert_isequal(NINFE, NINFE_REF);
%=============================================================================
NKROL_REF = int32(0);
assert_isequal(NKROL, NKROL);
%=============================================================================
INFZ_REF = int32([0, 1, 0, 0, 0, 0, 0, 0, 0, 0]);
assert_isequal(INFZ, INFZ_REF);
%=============================================================================
KRONR_REF = int32(zeros(1, 13));
KRONR_REF(1) = int32(2);
KRONR_REF(3) = int32(1);
assert_isequal(KRONR, KRONR_REF);
%=============================================================================
INFE_REF = int32([1, 1, 1, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0]);
assert_isequal(INFE, INFE_REF);
%=============================================================================
KRONL_REF = int32(zeros(1, 13));
KRONL_REF(1) = int32(1);
KRONL_REF(2) = int32(1);
assert_isequal(KRONL, KRONL_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
% Compute poles (we need tp call fortran routine with M = 0, P = 0)
M = 0; P = 0;
[A_OUT, E_OUT, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, INFO] = slicot_ag08bd(EQUIL, M, P, A_IN, E_IN, B, C, D, TOL);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
%  Check the observability and compute the ordered set of the observability indices (call the routine with M = 0).
M = 0; P = 3;
[A_OUT, E_OUT, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, INFO] = slicot_ag08bd(EQUIL, M, P, A_IN, E_IN, B, C, D, TOL);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
% Check the controllability and compute the ordered set of the controllability indices (call the routine with P = 0)
M = 3; P = 0;
[A_OUT, E_OUT, NFZ, NRANK, NIZ, DINFZ, NKROR, NINFE, NKROL, INFZ, KRONR, INFE, KRONL, INFO] = slicot_ag08bd(EQUIL, M, P, A_IN, E_IN, B, C, D, TOL);
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================

