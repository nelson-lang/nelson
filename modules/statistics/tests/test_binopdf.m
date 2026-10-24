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
asserts.isequal(nargin('binopdf'), 3);
asserts.isequal(nargout('binopdf'), 1);
%=============================================================================
x = [0 1 2 5 10];
n = [10 10 10 10 10];
p = [0.2 0.2 0.5 0.5 0.8];
R = binopdf(x, n, p);
REF = [0.1073741824 0.268435456 0.0439453125 0.24609375 0.1073741824];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = binopdf([0 10 1.5 -1 11], 10, 0.2);
asserts.isapprox(R, [0.1073741824 0.0000001024 0 0 0], 1e-12);
%=============================================================================
R = binopdf([0 1 10], 10, [0 0 1]);
asserts.isequal(R, [1 0 1]);
%=============================================================================
R = binopdf(single(x), single(n), single(p));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single(REF), 1e-6);
%=============================================================================
R = binopdf([NaN 1 1 1], [10 -1 2.5 10], [0.5 0.5 0.5 NaN]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = binopdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('binopdf(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('binopdf({1}, 10, 0.5)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('binopdf(1, {10}, 0.5)', _('N must be a nonnegative integer numeric array.'));
asserts.checkerror('binopdf(1, 10, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
asserts.isapprox(binopdf(2, 4, 0.5), 0.375, 1e-12);
asserts.istrue(isnan(binopdf(1, 2, -0.1)));
asserts.checkerror('binopdf(1, 2, ''bad'')', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
