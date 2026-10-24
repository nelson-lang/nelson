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
asserts.isequal(nargin('poissstat'), 1);
asserts.isequal(nargout('poissstat'), 2);
%=============================================================================
[m, v] = poissstat([0 1 5 Inf]);
asserts.isapprox(m(1:3), [0 1 5], 1e-15);
asserts.isapprox(v(1:3), [0 1 5], 1e-15);
asserts.istrue(isinf(m(4)) && isinf(v(4)));
%=============================================================================
[m, v] = poissstat(single([1 5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [1 5], 1e-7);
asserts.isapprox(double(v), [1 5], 1e-7);
%=============================================================================
[m, v] = poissstat([-1 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
asserts.checkerror('poissstat({1})', _('LAMBDA must be a nonnegative numeric array.'));
%=============================================================================
[poissMean, poissVariance] = poissstat(3);
asserts.isequal(poissMean, 3);
asserts.isequal(poissVariance, 3);
%=============================================================================
