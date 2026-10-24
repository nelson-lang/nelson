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
asserts.isequal(nargin('betastat'), 2);
asserts.isequal(nargout('betastat'), 2);
%=============================================================================
[m, v] = betastat([1 2], [3 4]);
asserts.isapprox(m, [0.25 0.3333333333333333], 1e-15);
asserts.isapprox(v, [0.0375 0.03174603174603174], 1e-15);
%=============================================================================
[m, v] = betastat(single([1 2]), single([3 4]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [0.25 0.3333333333333333], 1e-6);
asserts.isapprox(double(v), [0.0375 0.03174603174603174], 1e-6);
%=============================================================================
[m, v] = betastat([1 0 -1 NaN], [1 1 1 1]);
asserts.isapprox(m(1), 0.5, 1e-15);
asserts.isapprox(v(1), 0.08333333333333333, 1e-15);
asserts.istrue(all(isnan(m(2:4))));
asserts.istrue(all(isnan(v(2:4))));
%=============================================================================
asserts.checkerror('betastat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('betastat({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('betastat(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
[betaMean, betaVariance] = betastat(2, 2);
asserts.isapprox(betaMean, 0.5, 1e-12);
asserts.isapprox(betaVariance, 0.05, 1e-12);
%=============================================================================
