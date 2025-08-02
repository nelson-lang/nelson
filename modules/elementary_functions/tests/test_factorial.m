%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('factorial'), 1)
assert_isequal(nargout('factorial'), 1)
%=============================================================================
R = factorial(6);
REF = 720;
assert_isequal(R, REF);
%=============================================================================
R = factorial(single(6));
REF = single(720);
assert_isequal(R, REF);
%=============================================================================
R = factorial([4, 3; 2 1]);
REF = [24 6; 2 1];
assert_isequal(R, REF);
%=============================================================================
R = factorial(single([4, 3; 2 1]));
REF = single([24 6; 2 1]);
assert_isequal(R, REF);
%=============================================================================
R = factorial(171);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = factorial(single(35));
REF = single(Inf);
assert_isequal(R, REF);
%=============================================================================
R = factorial(0);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
R = factorial(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = factorial(-Inf);', _('#1 input argument: must be real positive integer values.'));
assert_checkerror('R = factorial(NaN);', _('#1 input argument: must be real positive integer values.'));
%=============================================================================
R = factorial(int8(255));
REF = int8(127);
assert_isequal(R, REF);
%=============================================================================
