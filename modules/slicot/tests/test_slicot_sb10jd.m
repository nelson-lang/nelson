%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/SB01JD.html
% [A_OUT, B_OUT, C_OUT, D_OUT, E_OUT, NSYS, INFO] = slicot_sb10jd(A_IN, B_IN, C_IN, D_IN, E_IN)
assert_isequal(nargin('slicot_sb10jd'), 5);
assert_isequal(nargout('slicot_sb10jd'), 7);
%=============================================================================
A_IN = [2 -4; 4 2];
B_IN = [0 -1; 0 0.5];
C_IN = [0 -0.5; 0 -2];
D_IN = [0 0; 0 -1];
E_IN = [1 0; -3 0.5];
[A_OUT, B_OUT, C_OUT, D_OUT, E_OUT, NSYS, INFO] = slicot_sb10jd(A_IN, B_IN, C_IN, D_IN, E_IN);
%=============================================================================
A_OUT_REF = [-0.8391      5.0596;
-5.0596    -17.1609];
assert_isapprox(abs(A_OUT), abs(A_OUT_REF), 1e-4);
%=============================================================================
B_OUT_REF = [0.0000      0.4388;
0.0000     -2.0139];
assert_isapprox(abs(B_OUT), abs(B_OUT_REF), 1e-4);
%=============================================================================
C_OUT_REF = [-0.0416     -1.2504;
-0.1663     -5.0016];
assert_isapprox(abs(C_OUT), abs(C_OUT_REF), 1e-4);
%=============================================================================
D_OUT_REF = [0    0;
0   -1];
assert_isapprox(D_OUT, D_OUT_REF, 1e-4);
%=============================================================================
E_OUT_REF_1 = [-3.1623      0.4743;
-0.7208      0.1581];
E_OUT_REF_MKL_2019 = [     3.1977      0.0000;
-0.0000      0.1564];
if isapprox(E_OUT(1), E_OUT_REF_1(1), 1e-4)
  assert_isapprox(E_OUT, E_OUT_REF_1, 1e-4);
else
  assert_isapprox(E_OUT, E_OUT_REF_MKL_2019, 1e-4);
end
%=============================================================================
NSYS_REF = int32(2);
assert_isequal(NSYS, NSYS_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
