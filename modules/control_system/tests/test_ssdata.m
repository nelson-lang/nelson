%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ssdata'), -1);
asserts.isequal(nargout('ssdata'), -1);
%=============================================================================
A = [0 1; -2 -3];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
[a, b, c, d, Ts] = ssdata(sys);
asserts.isequal(a, A);
asserts.isequal(b, B);
asserts.isequal(c, C);
asserts.isequal(d, D);
asserts.isequal(Ts, 0);
%=============================================================================
asserts.checkerror('ssdata(42)', sprintf(_('%s expected.'), 'LTI model'));
%=============================================================================
