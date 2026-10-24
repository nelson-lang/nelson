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
asserts.isequal(nargin('tstat'), 1);
asserts.isequal(nargout('tstat'), 2);
%=============================================================================
[m, v] = tstat([0.5 1 1.5 3 Inf]);
asserts.istrue(all(isnan(m(1:2))));
asserts.isapprox(m(3:5), [0 0 0], 1e-15);
asserts.istrue(all(isnan(v(1:2))));
asserts.istrue(isinf(v(3)) && v(3) > 0);
asserts.isapprox(v(4), 3, 1e-15);
asserts.isapprox(v(5), 1, 1e-15);
%=============================================================================
[m, v] = tstat(single([3 4]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [0 0], 1e-7);
asserts.isapprox(double(v), [3 2], 1e-6);
%=============================================================================
[m, v] = tstat([0 -1 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
asserts.checkerror('tstat({1})', sprintf(_('%s must be real numeric.'), 'V'));
%=============================================================================
[tMean, tVariance] = tstat(5);
asserts.isequal(tMean, 0);
asserts.isapprox(tVariance, 5 / 3, 1e-12);
%=============================================================================
