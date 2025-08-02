%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nthroot(-27, 3), -3);
assert_isequal(nthroot(81, 4), 3);
assert_isequal(nthroot(Inf, 5), Inf);
assert_isequal(nthroot(10, 0), Inf);
assert_isequal(nthroot(-Inf, 5), -Inf);
assert_isequal(nthroot(-Inf, -5), 0);
assert_isequal(nthroot(1, [10 20]), [1 1]);
%%=============================================================================
N = [5 3 -1];
Y = nthroot(-8, N);
REF = [  -1.5157   -2.0000   -0.1250];
assert_isapprox(Y, REF, 1e-4);
%=============================================================================
X = [-2 -2 -2; 4 -3 -5];
N = [1 -1 3; 1/2 5 3];
Y = nthroot(X, N);
REF = [-2.0000   -0.5000   -1.2599; 16.0000   -1.2457   -1.7100];
assert_isapprox(Y, REF, 1e-4);
%=============================================================================
assert_checkerror('nthroot(10, i)', _('Both X and N are required to be real.'));
assert_checkerror('nthroot(1+3i, 2)', _('Both X and N are required to be real.'));
assert_checkerror('nthroot(-10, 2)', _('If X is negative, then N must be an odd integer.'));
%=============================================================================
