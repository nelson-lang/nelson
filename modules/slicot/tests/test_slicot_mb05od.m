%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% http://slicot.org/objects/software/shared/doc/MB05OD.html
% [A_OUT, MDIG, IDIG, IWARN, INFO] = slicot_mb05od(BALANC, NDIAG, DELTA, A_IN)
assert_isequal(nargin('slicot_mb05od'), 4);
assert_isequal(nargout('slicot_mb05od'), 5);
%=============================================================================
N = 3;
NDIAG = 9;
DELTA = 1.0;
BALANC = 'S';
A_IN = [2.0   1.0   1.0;
0.0   3.0   2.0;
1.0   0.0   4.0];
[A_OUT, MDIG, IDIG, IWARN, INFO] = slicot_mb05od(BALANC, NDIAG, DELTA, A_IN);
%=============================================================================
A_OUT_REF = [22.5984     17.2073     53.8144;
24.4047     27.6033     83.2241;
29.4097     12.2024     81.4177];
assert_isapprox(A_OUT, A_OUT_REF, 1e-4);
assert_isapprox(A_OUT, expm(A_IN), 1e-4);
%=============================================================================
MDIG_REF = int32(12);
assert_isequal(MDIG, MDIG_REF);
%=============================================================================
IDIG_REF = int32(15);
assert_isequal(IDIG, IDIG_REF);
%=============================================================================
IWARN_REF = int32(0);
assert_isequal(IWARN, IWARN_REF);
%=============================================================================
INFO_REF = int32(0);
assert_isequal(INFO, INFO_REF);
%=============================================================================
