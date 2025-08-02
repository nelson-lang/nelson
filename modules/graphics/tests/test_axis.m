%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
x = linspace(0,2*pi);
y = sin(x);
plot(x,y,'-o');
axis([0 2*pi -1.5 1.5]);
lim = axis();
REF = [0    6.2832   -1.5000    1.5000];
assert_isapprox(lim(1), REF(1), 1e-4);
assert_isapprox(lim(2), REF(2), 1e-4);
assert_isapprox(lim(3), REF(3), 1e-4);
assert_isapprox(lim(4), REF(4), 1e-4);
%=============================================================================
f = figure();
x = linspace(-10, 10, 200);
y = sin(4 * x) ./ exp(0.1 * x);
plot(x, y);
axis([-10 10 0 inf]);
lim = axis();
REF = [-10 10 0 inf];
assert_isapprox(lim(1), REF(1), 1e-4);
assert_isapprox(lim(2), REF(2), 1e-4);
assert_isapprox(lim(3), REF(3), 1e-4);
assert_istrue(isinf(lim(4)));
%=============================================================================
