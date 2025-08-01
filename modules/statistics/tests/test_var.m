%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('var'), 3);
assert_isequal(nargout('var'), 1);
%=============================================================================
R = var([1,2,3], 0, 1);
REF = [NaN NaN NaN];
assert_isequal(R, REF);
%=============================================================================
A = [4 -7 3; 1 4 -2; 10 7 9];
R = var(A);
REF = [21.0000   54.3333   30.3333];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 1);
REF = [12.5000   24.5000   18.0000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [4 -2 1; 9 5 7];
R = var(A, 0, 2);
REF = [9; 4];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
assert_checkerror('var()', _('Wrong number of input arguments.'))
%=============================================================================
assert_isequal(var(36), 0);
%=============================================================================
