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
asserts.isequal(nargin('pcares'), 2);
asserts.isequal(nargout('pcares'), 2);
%=============================================================================
X = [1 2; 3 4; 5 8; 7 11];
[residuals, reconstructed] = pcares(X, 1);
asserts.isapprox(residuals, [-0.203204024134255 0.129760563114943; ...
  0.310268853749682 -0.198129251382277; -0.083466736673022 0.053299587929923; ...
  -0.023598092942406 0.015069100337410], 1e-12);
asserts.isapprox(reconstructed, [1.203204024134255 1.870239436885057; ...
  2.689731146250318 4.198129251382277; 5.083466736673022 7.946700412070077; ...
  7.023598092942406 10.984930899662590], 1e-12);
asserts.isapprox(X - reconstructed, residuals, 1e-12);
%=============================================================================
[residuals, reconstructed] = pcares(X, 0);
asserts.isequal(reconstructed, [4 6.25; 4 6.25; 4 6.25; 4 6.25]);
asserts.isequal(residuals, [-3 -4.25; -1 -2.25; 1 1.75; 3 4.75]);
%=============================================================================
[residuals, reconstructed] = pcares(X, 2);
asserts.istrue(max(abs(residuals(:))) < 1e-12);
asserts.isapprox(reconstructed, X, 1e-12);
%=============================================================================
[residuals, reconstructed] = pcares(single(X), 1);
asserts.isequal(class(residuals), 'single');
asserts.isequal(class(reconstructed), 'single');
asserts.isapprox(double(residuals), [-0.203204024134255 0.129760563114943; ...
  0.310268853749682 -0.198129251382277; -0.083466736673022 0.053299587929923; ...
  -0.023598092942406 0.015069100337410], 1e-6);
%=============================================================================
asserts.checkerror('pcares()', _('Not enough input arguments.'));
asserts.checkerror('pcares({1}, 1)', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('pcares([1 NaN], 1)', _('X must not contain NaN values.'));
asserts.checkerror('pcares([1 2], 3)', _('NumComponents must be an integer scalar between 0 and the number of variables.'));
%=============================================================================
