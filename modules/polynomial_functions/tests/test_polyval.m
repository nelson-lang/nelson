%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = [3 2 1];
x = [5 7 9];
R = polyval(p, x);
REF = [86 162 262];
asserts.isequal(R, REF);
%=============================================================================
asserts.isequal(polyval([1, 0, -1], 2), 3);
%=============================================================================
% polyval with centering/scaling argument mu.
x = (1:10)';
y = [1.1 3.9 9.2 15.8 25.1 35.9 49.2 63.8 81.1 99.9]';
[p, S, mu] = polyfit(x, y, 2);
xq = [2 5 8];
yq = polyval(p, xq, S, mu);
REFy = [4.02969696969698, 25.0042424242424, 63.9787878787879];
asserts.isapprox(yq, REFy, 1e-9);
%=============================================================================
% polyval with error estimate output delta (centered fit).
[yq2, delta] = polyval(p, xq, S, mu);
asserts.isapprox(yq2, REFy, 1e-9);
REFd = [0.197750964728112, 0.193487568924592, 0.190227326602768];
asserts.isapprox(delta, REFd, 1e-9);
asserts.isequal(size(delta), size(xq));
%=============================================================================
% delta is invariant to centering (fit without mu).
[p2, S2] = polyfit(x, y, 2);
[yq3, delta3] = polyval(p2, xq, S2);
asserts.isapprox(delta3, REFd, 1e-9);
%=============================================================================
% mu applies even when only y is requested.
asserts.isapprox(polyval(p, 5, [], mu), 25.0042424242424, 1e-9);
%=============================================================================
% delta requires S.
asserts.checkerror('[a, b] = polyval([1 2 3], 2, []);', _('S is required to compute error estimates.'));
asserts.checkerror('[a, b] = polyval([1 2 3], 2);', _('S is required to compute error estimates.'));
%=============================================================================
