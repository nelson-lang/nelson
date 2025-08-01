%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [7, 2, 3; 1, 3, 4; 6, 4, 5];
R = cond(A);
assert_isapprox(R, 51.0278, 1e-4);
%=============================================================================
A = [7, 2, 3; 1, 3, 4; 6, 4, 5];
p = 1;
R = cond(A, p);
assert_isapprox(R, 57.2727, 1e-4);
%=============================================================================
R = cond ([1, 2; 2, 1]);
assert_isapprox(R, 3, 1e-1);
%=============================================================================
R = cond([1, 2, 3; 4, 5, 6; 7, 8, 9]);
assert_istrue(R > 1e16);
%=============================================================================
