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
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('poissfit'), 2);
asserts.isequal(nargout('poissfit'), 2);
%=============================================================================
x = [0 1 2 3 5 8];
[LAMBDA, LAMBDACI] = poissfit(x);
asserts.isapprox(LAMBDA, 3.166666666666667, 1e-12);
asserts.isapprox(LAMBDACI, [1.906540194061123; 4.945142261931039], 1e-12);
%=============================================================================
[LAMBDA, LAMBDACI] = poissfit(x, 0.01);
asserts.isapprox(LAMBDA, 3.166666666666667, 1e-12);
asserts.isapprox(LAMBDACI, [1.607409296574247; 5.563830152734323], 1e-12);
%=============================================================================
xm = [0 2; 1 3; 2 4; 3 6; 5 8];
[LAMBDAM, LAMBDACIM] = poissfit(xm);
asserts.isequal(size(LAMBDAM), [1 2]);
asserts.isapprox(LAMBDAM, [2.2 4.6], 1e-12);
asserts.isapprox(LAMBDACIM, [1.098232073447368 2.916005407408936;
                            3.936407702660469 6.902258578966752], 1e-12);
%=============================================================================
[LAMBDA, LAMBDACI] = poissfit(single(x));
asserts.isequal(class(LAMBDA), 'double');
asserts.isequal(class(LAMBDACI), 'double');
asserts.isapprox(LAMBDA, 3.166666666666667, 1e-12);
%=============================================================================
[LAMBDA, LAMBDACI] = poissfit([1 NaN 3]);
asserts.istrue(isnan(LAMBDA));
asserts.istrue(all(isnan(LAMBDACI(:))));
[LAMBDA, LAMBDACI] = poissfit([0 0 0]);
asserts.isequal(LAMBDA, 0);
asserts.isequal(LAMBDACI(1), 0);
asserts.istrue(isfinite(LAMBDACI(2)) && LAMBDACI(2) > 0);
%=============================================================================
asserts.checkerror('poissfit()', _('Not enough input arguments.'));
asserts.checkerror('poissfit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('poissfit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('poissfit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('poissfit([-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('poissfit([1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('poissfit([1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('poissfit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('poissfit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('poissfit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================