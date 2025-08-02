%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/MB03OD.html
% [A_OUT, JPVT_OUT, TAU, RANK, SVAL, INFO] = slicot_mb03od(JOBQR, A_IN, JPVT_IN, RCOND, SVLMAX)
assert_isequal(nargin('slicot_mb03od'), 5);
assert_isequal(nargout('slicot_mb03od'), 6);
%=============================================================================
M = 6;
N = 5;
JOBQR = 'Q';
RCOND = 5.D-16;
SVLMAX = 0.0;
JPVT_IN = zeros(1, N);
A_IN = [1.    2.    6.    3.    5.;
-2.   -1.   -1.    0.   -2.;
5.    5.    1.    5.    1.;
-2.   -1.   -1.    0.   -2.;
4.    8.    4.   20.    4.;
-2.   -1.   -1.    0.   -2.];

[A_OUT, JPVT_OUT, TAU, RANK, SVAL, INFO] = slicot_mb03od(JOBQR, A_IN, JPVT_IN, RCOND, SVLMAX);
%=============================================================================
A_OUT_REDUCED = A_OUT(1:4, 1:5);
A_OUT_REF = [-20.8327     -4.9442     -5.1842     -4.8002     -9.1683;
0.0000      5.6174      1.3118      5.2101      1.1875;
0.2098      0.1959     -5.0402     -1.2461     -3.1571;
0.0000      0.1511     -0.1597      1.5035     -0.7517];
assert_istrue(isapprox(A_OUT_REDUCED, A_OUT_REF, 1e-4));
%=============================================================================
JPVT_OUT_REF = int32([4, 3, 1, 5, 2]);
assert_isequal(JPVT_OUT, JPVT_OUT_REF);
%=============================================================================
TAU_REDUCED = TAU(1:4);
TAU_REF = [1.1440      1.1780      1.8633      1.4354];
assert_istrue( isapprox(TAU_REDUCED, TAU_REF, 1e-4));
%=============================================================================
RANK_REF = int32(4);
assert_isequal(RANK, RANK_REF);
%=============================================================================
SVAL_REF = [22.7257      1.4330      0.0000];
assert_isapprox(SVAL, SVAL_REF, 1e-4);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
