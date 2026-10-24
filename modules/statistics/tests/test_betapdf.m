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
asserts.isequal(nargin('betapdf'), 3);
asserts.isequal(nargout('betapdf'), 1);
%=============================================================================
x = [0 0.1 0.5 0.9 1];
a = [0.5 1 2 5 2];
b = [2 3 2 1 0.5];
R = betapdf(x, a, b);
REF = [Inf 2.43 1.5 3.2805 Inf];
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:4), REF(2:4), 1e-12);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = betapdf([0 1 0.5], [1 2 2], [3 1 2]);
asserts.isapprox(R, [3 2 1.5], 1e-12);
%=============================================================================
R = betapdf(single(x), single(a), single(b));
asserts.isequal(class(R), 'single');
asserts.istrue(isinf(R(1)) && R(1) > 0);
asserts.isapprox(R(2:4), single([2.43 1.5 3.2805]), 1e-6);
asserts.istrue(isinf(R(5)) && R(5) > 0);
%=============================================================================
R = betapdf([-1 2 NaN 0.5 0.5], [2 2 2 -1 2], [2 2 2 2 NaN]);
asserts.isequal(R(1), 0);
asserts.isequal(R(2), 0);
asserts.istrue(all(isnan(R(3:5))));
%=============================================================================
R = betapdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('betapdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('betapdf({1}, 2, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('betapdf(1, {2}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('betapdf(1, 2, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
asserts.isapprox(betapdf(0.5, 2, 2), 1.5, 1e-12);
asserts.istrue(isnan(betapdf(0.5, -1, 2)));
%=============================================================================
