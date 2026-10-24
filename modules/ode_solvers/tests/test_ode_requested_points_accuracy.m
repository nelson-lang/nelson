%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[t, y] = ode45(@(t, y) y, [0 0.25 0.5 1], 1);
asserts.isequal(size(t), [4 1]);
asserts.isequal(size(y), [4 1]);
asserts.istrue(max(abs(t - [0; 0.25; 0.5; 1])) < 1e-15);
asserts.istrue(max(abs(y - exp(t))) < 5e-4);
%=============================================================================
requested = linspace(0, 1, 101);
[tdense, ydense] = ode45(@(t, y)[y(2); -y(1)], requested, [0; 1]);
asserts.isequal(size(tdense), [101 1]);
asserts.isequal(size(ydense), [101 2]);
asserts.istrue(max(abs(tdense - requested(:))) < 1e-14);
asserts.istrue(max(abs(ydense(:, 1) - sin(tdense))) < 5e-4);
%=============================================================================
