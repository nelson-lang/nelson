%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isstatic'), 1);
asserts.isequal(nargout('isstatic'), 1);
%=============================================================================
S = tf(3);
asserts.istrue(isstatic(S));
asserts.isequal(S.Ts, 0);
S.Ts = 0.5;
asserts.isfalse(isstatic(S));
%=============================================================================
S = tf(3, 2);
asserts.istrue(isstatic(S));
asserts.isequal(S.Ts, 0);
S.Numerator = [1 2];
asserts.istrue(isstatic(S));
S.Ts = -1;
asserts.isfalse(isstatic(S));
%=============================================================================
D = [2, 4; 3, 5];
sys1 = ss(D)
sys1.Ts = 0.2;
asserts.istrue(isstatic(sys1));
sys1.A = 1;
asserts.isfalse(isstatic(sys1));
%=============================================================================
J = [8 -3 -3; -3 8 -3; -3 -3 8];
F = 0.2 * eye(3);
A = -J \ F;
B = inv(J);
C = eye(3);
D = 0;
sys = ss(A, B, C, D);
asserts.isfalse(isstatic(sys));
%=============================================================================
