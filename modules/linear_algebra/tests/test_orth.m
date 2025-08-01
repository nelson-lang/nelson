%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
M = [1 0 1; 0 1 0; 1 0 1];
R = orth(M);
REF_1 = [0.7071, 0;0, 1.0000;0.7071, 0];
REF_2 =  [-0.7071         0; -0    1.0000; -0.7071        -0];
assert_istrue(assert_isapprox(R, REF_1, 1e-4) || assert_isapprox(R, REF_2, 1e-4));
%=============================================================================
M = [1 0 1; 0 1 0; 1 0 1];
R = orth(M, 1);
REF_1 = [0.7071; 0; 0.7071];
REF_2 = [-0.7071; -0; -0.7071];
assert_istrue(assert_isapprox(R, REF_1, 1e-4) || assert_isapprox(R, REF_2, 1e-4));
%=============================================================================
