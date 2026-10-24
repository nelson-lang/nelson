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
asserts.isequal(nargin('binocdf'), 4);
asserts.isequal(nargout('binocdf'), 1);
%=============================================================================
x = [0 1 2 5 10];
n = [10 10 10 10 10];
p = [0.2 0.2 0.5 0.5 0.8];
R = binocdf(x, n, p);
REF = [0.1073741824 0.3758096384 0.0546875 0.623046875 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = binocdf(x, n, p, 'upper');
REF = [0.8926258176 0.6241903616 0.9453125 0.376953125 0];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = binocdf([1.5 -1 11], 10, 0.2);
asserts.isapprox(R, [0.3758096384 0 1], 1e-12);
%=============================================================================
R = binocdf([0 1 10], 10, [0 0 1]);
asserts.isequal(R, [1 1 1]);
%=============================================================================
R = binocdf(single(x), single(n), single(p));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([0.1073741824 0.3758096384 0.0546875 0.623046875 1]), 1e-6);
%=============================================================================
R = binocdf([NaN 1 1 1], [10 -1 2.5 10], [0.5 0.5 0.5 NaN]);
asserts.istrue(all(isnan(R)));
%=============================================================================
R = binocdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('binocdf(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('binocdf(1, 10, 0.5, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'binomial'));
asserts.checkerror('binocdf({1}, 10, 0.5)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('binocdf(1, {10}, 0.5)', _('N must be a nonnegative integer numeric array.'));
asserts.checkerror('binocdf(1, 10, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
asserts.isapprox(binocdf(2, 4, 0.5), 0.6875, 1e-12);
%=============================================================================
