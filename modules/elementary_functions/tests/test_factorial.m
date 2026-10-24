%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('factorial'), 1)
asserts.isequal(nargout('factorial'), 1)
%=============================================================================
R = factorial(6);
REF = 720;
asserts.isequal(R, REF);
%=============================================================================
R = factorial(single(6));
REF = single(720);
asserts.isequal(R, REF);
%=============================================================================
R = factorial([4, 3; 2 1]);
REF = [24 6; 2 1];
asserts.isequal(R, REF);
%=============================================================================
R = factorial(single([4, 3; 2 1]));
REF = single([24 6; 2 1]);
asserts.isequal(R, REF);
%=============================================================================
R = factorial(171);
REF = Inf;
asserts.isequal(R, REF);
%=============================================================================
R = factorial(single(35));
REF = single(Inf);
asserts.isequal(R, REF);
%=============================================================================
R = factorial(0);
REF = 1;
asserts.isequal(R, REF);
%=============================================================================
R = factorial(Inf);
REF = Inf;
asserts.isequal(R, REF);
%=============================================================================
asserts.checkerror('R = factorial(-Inf);', sprintf(_('#%d argument: must be real positive integer values.'), 1));
asserts.checkerror('R = factorial(NaN);', sprintf(_('#%d argument: must be real positive integer values.'), 1));
%=============================================================================
R = factorial(int8(255));
REF = int8(127);
asserts.isequal(R, REF);
%=============================================================================
