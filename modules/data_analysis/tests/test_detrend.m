%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
% default removes the best-fit straight line
assert_isapprox(detrend([1 3 2 4 6]), [0 0.9 -1.2 -0.3 0.6], 1e-10);
%=============================================================================
% a row vector stays a row vector
assert_isequal(size(detrend([1 2 3])), [1 3]);
%=============================================================================
% a column vector stays a column vector
assert_isapprox(detrend([1; 3; 2; 4; 6]), [0; 0.9; -1.2; -0.3; 0.6], 1e-10);
%=============================================================================
% 'constant' and 0 remove the mean
assert_isapprox(detrend([1 3 2 4 6], 'constant'), [-2.2 -0.2 -1.2 0.8 2.8], 1e-10);
assert_isapprox(detrend([1 3 2 4 6], 0), [-2.2 -0.2 -1.2 0.8 2.8], 1e-10);
%=============================================================================
% 'linear' and 1 are the default
assert_isapprox(detrend([1 3 2 4 6], 'linear'), detrend([1 3 2 4 6]), 1e-12);
assert_isapprox(detrend([1 3 2 4 6], 1), detrend([1 3 2 4 6]), 1e-12);
%=============================================================================
% a perfect linear ramp detrends to zeros, column by column
R = detrend([1 2; 3 5; 5 8]);
assert_istrue(max(abs(R(:))) < 1e-10);
%=============================================================================
% a breakpoint gives a continuous piecewise-linear fit
R = detrend([1 2 3 10 11 12], 1, 3);
REF = [0.315789 -0.631579 -1.578947 2.526316 0.631579 -1.263158];
assert_isapprox(R, REF, 1e-5);
%=============================================================================
% single input keeps its class
R = detrend(single([1 2 4]));
assert_isequal(class(R), 'single');
assert_isapprox(double(R), [1 / 6 -1 / 3 1 / 6], 1e-5);
%=============================================================================
% a scalar detrends to zero
assert_isequal(detrend(5), 0);
%=============================================================================
% an unknown trend type errors
assert_checkerror('detrend([1 2 3], ''quadratic'')', _('Trend type must be ''constant'' or ''linear''.'));
%=============================================================================
