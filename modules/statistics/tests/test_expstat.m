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
asserts.isequal(nargin('expstat'), 1);
asserts.isequal(nargout('expstat'), 2);
%=============================================================================
[m, v] = expstat(3);
asserts.isequal(m, 3);
asserts.isequal(v, 9);
%=============================================================================
[m, v] = expstat([1 2 3 Inf]);
asserts.isapprox(m(1:3), [1 2 3], 1e-15);
asserts.isapprox(v(1:3), [1 4 9], 1e-15);
asserts.istrue(isinf(m(4)) && m(4) > 0);
asserts.istrue(isinf(v(4)) && v(4) > 0);
%=============================================================================
[m, v] = expstat(single([1 2 3]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1 2 3], 1e-7);
asserts.isapprox(double(v), [1 4 9], 1e-6);
%=============================================================================
[m, v] = expstat([0 -1 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
[m, v] = expstat([]);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('expstat({1})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
[expMean, expVariance] = expstat(4);
asserts.isequal(expMean, 4);
asserts.isequal(expVariance, 16);
%=============================================================================
