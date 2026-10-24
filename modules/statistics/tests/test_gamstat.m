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
asserts.isequal(nargin('gamstat'), 2);
asserts.isequal(nargout('gamstat'), 2);
%=============================================================================
[m, v] = gamstat([1 2 3], [4 5 6]);
asserts.isapprox(m, [4 10 18], 1e-15);
asserts.isapprox(v, [16 50 108], 1e-15);
%=============================================================================
[m, v] = gamstat(single([1 2]), single([4 5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [4 10], 1e-6);
asserts.isapprox(double(v), [16 50], 1e-5);
%=============================================================================
[m, v] = gamstat([1 0 -1 NaN], [1 1 1 1]);
asserts.isequal(m(1), 1);
asserts.isequal(v(1), 1);
asserts.istrue(all(isnan(m(2:4))));
asserts.istrue(all(isnan(v(2:4))));
%=============================================================================
[m, v] = gamstat([], []);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('gamstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('gamstat({1}, 1)', sprintf(_('%s must be real numeric.'), 'A'));
asserts.checkerror('gamstat(1, {1})', sprintf(_('%s must be real numeric.'), 'B'));
%=============================================================================
[gammaMean, gammaVariance] = gamstat(3, 2);
asserts.isequal(gammaMean, 6);
asserts.isequal(gammaVariance, 12);
%=============================================================================
