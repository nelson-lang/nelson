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
asserts.isequal(nargin('chi2stat'), 1);
asserts.isequal(nargout('chi2stat'), 2);
%=============================================================================
[m, v] = chi2stat([1 2 3 Inf]);
asserts.isapprox(m(1:3), [1 2 3], 1e-15);
asserts.isapprox(v(1:3), [2 4 6], 1e-15);
asserts.istrue(isinf(m(4)) && isinf(v(4)));
%=============================================================================
[m, v] = chi2stat(single([1 2]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1 2], 1e-7);
asserts.isapprox(double(v), [2 4], 1e-6);
%=============================================================================
[m, v] = chi2stat([0 -1 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
[m, v] = chi2stat([]);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('chi2stat({1})', sprintf(_('%s must be real numeric.'), 'V'));
%=============================================================================
[chiMean, chiVariance] = chi2stat(4);
asserts.isequal(chiMean, 4);
asserts.isequal(chiVariance, 8);
%=============================================================================
