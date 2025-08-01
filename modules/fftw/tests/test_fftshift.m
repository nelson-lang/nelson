%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
M = [10 20 30 40 50 60 70];
R = fftshift(M);
REF = [50     60     70     10     20     30     40];
assert_isequal(R, REF);
%=============================================================================
M = [ 0.,  1.,  2.; 3.,  4., -4.; -3., -2., -1.];
R = fftshift(M, 1);
REF = [ -3    -2    -1; 0     1     2; 3     4    -4];
assert_isequal(R, REF);
%=============================================================================
