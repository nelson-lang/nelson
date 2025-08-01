%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('lt'), 2)
assert_isequal(nargout('lt'), 1)
%=============================================================================
% Nelson compares only real part for complex
A = 2 + i;
B = -3-2;
R = A < B;
assert_isfalse(R);
%=============================================================================
R = "Nel" < "Son";
assert_istrue(R);
%=============================================================================
R = 2 < 3;
assert_istrue(R);
%=============================================================================
R = 2 > 3;
assert_isfalse(R);
%=============================================================================
A = ["apple", "orange","tomato"];
B = 'orange';
R = (A <= B);
REF = logical([1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = [1 2; 3 4];
B = [1; 3];
R = A > B;
REF = logical([0, 1;
0, 1]);
assert_isequal(R, REF);
%=============================================================================
B = [1 2; 3 4];
A = [1; 3];
R = A > B;
REF = logical([0, 0;
0, 0]);
assert_isequal(R, REF);
%=============================================================================
