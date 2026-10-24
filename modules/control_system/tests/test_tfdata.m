%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('tfdata'), -1);
asserts.isequal(nargout('tfdata'), -1);
%=============================================================================
sys = tf([1 2], [1 3 2]);
[num, den, Ts] = tfdata(sys);
asserts.isequal(num, [0 1 2]);
asserts.isequal(den, [1 3 2]);
asserts.isequal(Ts, 0);
%=============================================================================
sys = ss([0 1; -2 -3], [0; 1], [1 0], 0);
[num, den] = tfdata(sys);
asserts.isapprox(num, [0 0 1], 1e-12);
asserts.isequal(den, [1 3 2]);
%=============================================================================
asserts.checkerror('tfdata(42)', sprintf(_('%s expected.'), 'LTI model'));
%=============================================================================
