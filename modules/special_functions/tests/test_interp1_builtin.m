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
% interp1 builtin: native methods and the calls delegated to the generic path
%=============================================================================
x = [0 1 2.5 3 4.5 6];
v = [1 -2 0.5 3 2 -1];
xq = [-0.5 0.75 2.5 3.8 6.5];
%=============================================================================
R = interp1(x, v, xq);
asserts.istrue(all(isnan(R([1 5]))));
asserts.isapprox(R(2:4), [-1.25 0.5 2.4666666666666668], 1e-12);
%=============================================================================
R = interp1(x, v, xq, 'nearest');
asserts.isequal(R(2:4), [-2 0.5 2]);
R = interp1(x, v, xq, 'previous');
asserts.isequal(R(2:4), [1 0.5 3]);
R = interp1(x, v, xq, 'next');
asserts.isequal(R(2:4), [-2 0.5 2]);
%=============================================================================
R = interp1(x, v, xq, 'pchip');
asserts.isapprox(R, [3.4749999999999996 -1.7593749999999999 0.5 2.6491851851851851 -2.3703703703703702], 1e-12);
%=============================================================================
R = interp1(x, v, xq, 'spline');
asserts.isapprox(R, [3.6162499999999986 -1.5586197916666662 0.5 3.7929471604938274 0.6163271604938263], 1e-12);
%=============================================================================
R = interp1(x, v, xq, 'makima');
asserts.isapprox(R, [2.8500000000000005 -1.7020833333333334 0.5 2.9244820031298904 -2.2949052338723703], 1e-12);
%=============================================================================
R = interp1(x, v, xq, 'linear', 'extrap');
asserts.isapprox(R, [2.5 -1.25 0.5 2.4666666666666668 -2], 1e-12);
%=============================================================================
R = interp1(x, v, xq, 'pchip', 0);
asserts.isapprox(R, [0 -1.7593749999999999 0.5 2.6491851851851851 0], 1e-12);
R = interp1(x, v, xq, 'makima', -9);
asserts.isapprox(R, [-9 -1.7020833333333334 0.5 2.9244820031298904 -9], 1e-12);
R = interp1(x, v, xq, 'spline', 7);
asserts.isapprox(R, [7 -1.5586197916666662 0.5 3.7929471604938274 7], 1e-12);
R = interp1(x, v, xq, 5);
asserts.isequal(R([1 5]), [5 5]);
%=============================================================================
R = interp1(0:5, v, [0.5 2.25 4.75], 'cubic');
asserts.isapprox(R, [-1.1875 1.20703125 -0.0625], 1e-12);
R = interp1(0:5, v, [-1 0.5 2.25 4.75], 'v5cubic', 3);
asserts.isapprox(R, [3 -1.1875 1.20703125 -0.0625], 1e-12);
%=============================================================================
% decreasing sample points and implicit sample points
R = interp1(fliplr(x), fliplr(v), xq);
asserts.isapprox(R(2:4), [-1.25 0.5 2.4666666666666668], 1e-12);
asserts.isapprox(interp1(v, [1.5 2.5 5.5]), [-0.5 -0.75 0.5], 1e-12);
asserts.isapprox(interp1(v, [1.5 2.5], "linear"), [-0.5 -0.75], 1e-12);
%=============================================================================
% output shape follows the query for a vector V
asserts.isequal(size(interp1(x, v, [1 2; 3 4])), [2 2]);
asserts.isequal(size(interp1(x, v', [1; 2; 3])), [3 1]);
asserts.isequal(size(interp1(x, v, zeros(1, 0))), [1 0]);
%=============================================================================
% matrix V: one column per data set
V = [v', 2 * v', -v'];
R = interp1(x, V, [0.75 2.5]);
asserts.isequal(size(R), [2 3]);
asserts.isapprox(R, [-1.25 -2.5 1.25; 0.5 1 -0.5], 1e-12);
R = interp1(x, V, [0.75 2.5; 3 4]);
asserts.isequal(size(R), [2 2 3]);
R = interp1(x, reshape(1:24, 6, 2, 2), 1.5);
asserts.isequal(size(R), [1 2 2]);
%=============================================================================
% classes
R = interp1(single(x), v, 0.5);
asserts.isequal(class(R), 'double');
R = interp1(x, single(v), 0.5);
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single(-0.5), 1e-6);
R = interp1(x, v + 1i * v, 0.5);
asserts.isfalse(isreal(R));
asserts.isapprox(R, -0.5 - 0.5i, 1e-12);
R = interp1(x, int16([10 20 30 40 50 60]), 0.5);
asserts.isequal(class(R), 'int16');
asserts.isequal(R, int16(15));
%=============================================================================
% pp form
pp = interp1(x, v, 'linear', 'pp');
asserts.isequal(pp.form, 'pp');
asserts.isequal(pp.breaks, x);
asserts.isapprox(ppval(pp, 0.75), -1.25, 1e-12);
%=============================================================================
% invalid calls
asserts.checkerror('interp1([0 1 1 2], 1:4, 0.5)', _('Sample points must be strictly monotonic.'), 'Nelson:interp1:samplePointsNotMonotonic');
asserts.checkerror('interp1(1:3, 1:4, 0.5)', _('X and V must be of the same length.'), 'Nelson:dimagree:xVLength');
asserts.checkerror('interp1(1:3, 1:3, 0.5, ''foo'')', _('Unknown interpolation method.'), 'Nelson:interp:unknownMethod');
asserts.checkerror('interp1(1:3, 1:3, 0.5 + 1i)', _('Sample points and query points must be real.'), 'Nelson:interp1:mustBeReal');
asserts.checkerror('interp1(1:3)', _('Not enough input arguments.'));
asserts.checkerror('[a, b] = interp1(1:3, 1:3, 2)', _('Too many output arguments.'));
%=============================================================================
