%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('gampdf'), 3);
asserts.isequal(nargout('gampdf'), 1);
%=============================================================================
x = [0 0.5 1 2 5];
a = [0.5 1 2 3 5];
b = [1 2 2 4 1];
R = gampdf(x, a, b);
REF = [Inf 0.389400391535702 0.151632664928158 ...
       0.0189540831160198 0.175467369767851];
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:5), REF(2:5), 1e-12);
%=============================================================================
R = gampdf([0 1 2], [1 2 3]);
asserts.isapprox(R, [1 0.367879441171442 0.270670566473225], 1e-12);
%=============================================================================
R = gampdf(single(x), single(a), single(b));
asserts.isequal(class(R), 'single');
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:5), single(REF(2:5)), 1e-6);
%=============================================================================
R = gampdf([-1 Inf NaN 1 1], [2 2 2 -1 2], [1 1 1 1 NaN]);
asserts.isequal(R(1), 0);
asserts.isequal(R(2), 0);
asserts.istrue(all(isnan(R(3:5))));
%=============================================================================
R = gampdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('gampdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('gampdf({1}, 2, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('gampdf(1, {2}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('gampdf(1, 2, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isapprox(gampdf(2, 3, 2), exp(-1) / 4, 1e-12);
asserts.istrue(isnan(gampdf(1, -1, 2)));
%=============================================================================
