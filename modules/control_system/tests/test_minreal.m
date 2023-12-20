%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sysIn = ss([1 0;0 -2], [-1;0], [2 1], 0, 3.2); 
sysOut = minreal(sysIn);
REF = ss(1, -1, 2, 0, 3.2);
assert_isequal(sysOut, REF);
%=============================================================================
A = [-2.0000   -2.4000; 0   -4.4000];
B = [0.6000;0.6000];
C = [4    -4];
D = 1;
[am, bm, cm, dm] = minreal(A, B, C, D);
assert_isequal(am, []);
assert_isequal(bm, ones(0, 1));
assert_isequal(cm, ones(1, 0));
assert_isequal(dm, 1);
%=============================================================================
sysIn = ss(A, B, C, D);
sysOut = minreal(sysIn);
REF = ss([], ones(0, 1), ones(0, 1), 1);
assert_isequal(sysOut, REF);
%=============================================================================
A = [ -1   0   0   0   0;   0  -1   1   0   0;   0   0  -2   0   0;   0   0   0  -1   1;   0   0   0   0  -3];
B = [1;   0;   1;   0;   1];
C = [1   0   0   0   0; 0   1   0   0   0; 0   0   0   1   0];
D = [0;  0;  0];
[am, bm, cm, dm] = minreal(A, B, C, D);

A_REF = [-2.2500   -0.5951   -0.0000;
   -0.4551   -1.7500   -1.1547;
   -0.2425   -0.5774   -2.0000];
B_REF = [-0.0000; -0.0000; -1.7321];
C_REF = [-0.0700    0.5000   -0.5774;
   -0.6301    0.5000         0;
    0.2100    0.5000         0];
D_REF = [0; 0; 0];

assert_isapprox(am, A_REF, 1e-4);
assert_isapprox(bm, B_REF, 1e-4);
assert_isapprox(cm, C_REF, 1e-4);
assert_isapprox(dm, D_REF, 1e-4);
%=============================================================================
sysIn = ss(A, B, C, D);
sysOut = minreal(sysIn);
[am, bm, cm, dm] = ssdata(sysOut);
assert_isapprox(am, A_REF, 1e-4);
assert_isapprox(bm, B_REF, 1e-4);
assert_isapprox(cm, C_REF, 1e-4);
assert_isapprox(dm, D_REF, 1e-4);
%=============================================================================
A = [-1.   0.   0.2113249   0.0002211;
 0.  -1.   0.7560439   0.3303271;
 0.   0.  -2.          0.;       
 0.   0.   0.         -2.];      
B = [0.6653811   0.8497452;
 0.6283918   0.685731 ;
 0.          0.;       
 0.          0.];       
C = [0.8782165   0.5608486   0.7263507   0.5442573;
 0.068374    0.6623569   0.1985144   0.2320748];
D = [0.   0. ;  0.   0.];
[am, bm, cm, dm] = minreal(A, B, C, D);

am_REF = [-1.0000    0.0000; 0.0000   -1.0000];
bm_REF = [0.0418   -0.0350;  0.9143    1.0914];
cm_REF = [-0.1482    1.0314; 0.4572    0.4841];
dm_REF = [0     0; 0     0];

assert_isapprox(am, am_REF, 1e-4);
assert_isapprox(abs(bm), abs(bm_REF), 1e-4);
assert_isapprox(abs(cm), abs(cm_REF), 1e-4);
assert_isapprox(dm, dm_REF, 1e-4);

sysIn = ss(A, B, C, D);
sysOut = minreal(sysIn);
[am, bm, cm, dm] = ssdata(sysOut);

assert_isapprox(am, am_REF, 1e-4);
assert_isapprox(abs(bm), abs(bm_REF), 1e-4);
assert_isapprox(abs(cm), abs(cm_REF), 1e-4);
assert_isapprox(dm, dm_REF, 1e-4);
%=============================================================================
