%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('Events', @odeTestTwoEvents, 'MaxStep', 0.05);
[t, y, te, ye, ie] = ode45(@(t, y) 1, [0 1], 0, options);
asserts.isequal(length(te), 2);
asserts.istrue(abs(te(1) - 0.25) < 1e-10);
asserts.istrue(abs(te(2) - 0.5) < 1e-10);
asserts.istrue(abs(ye(1) - 0.25) < 1e-10);
asserts.istrue(abs(ye(2) - 0.5) < 1e-10);
asserts.isequal(ie(:), [1; 2]);
asserts.istrue(abs(t(length(t)) - 0.5) < 1e-10);

options = odeset('Events', @odeTestDirectionEvents, 'MaxStep', 0.05);
[td, yd, ted, yed, ied] = ode45(@(t, y) 1, [0 1], 0, options);
asserts.isequal(length(ted), 1);
asserts.isequal(ied(1), 2);
asserts.istrue(abs(ted(1) - 0.5) < 1e-10);
%=============================================================================
