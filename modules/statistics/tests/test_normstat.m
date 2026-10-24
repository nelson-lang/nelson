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
asserts.isequal(nargin('normstat'), 2);
asserts.isequal(nargout('normstat'), 2);
%=============================================================================
[m, v] = normstat([0 1 2], [1 2 3]);
asserts.isapprox(m, [0 1 2], 1e-15);
asserts.isapprox(v, [1 4 9], 1e-15);
%=============================================================================
[m, v] = normstat(single([0 1]), single([2 3]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [0 1], 1e-7);
asserts.isapprox(double(v), [4 9], 1e-6);
%=============================================================================
[m, v] = normstat([1 NaN 1], [0 1 -1]);
asserts.isequal(m(1), 1);
asserts.isequal(v(1), 0);
asserts.istrue(all(isnan(m(2:3))));
asserts.istrue(all(isnan(v(2:3))));
%=============================================================================
[m, v] = normstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('normstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('normstat({1}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('normstat(1, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
[normalMean, normalVariance] = normstat(2, 3);
asserts.isequal(normalMean, 2);
asserts.isequal(normalVariance, 9);
%=============================================================================
