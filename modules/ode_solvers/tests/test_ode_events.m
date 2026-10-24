%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(t, y) 1;
ev = @odeTestEventHalf;
options = odeset('Events', ev, 'InitialStep', 0.05, 'MaxStep', 0.05);
[t, y, te, ye, ie] = ode23(f, [0 1], 0, options);
asserts.istrue(abs(t(length(t)) - 0.5) < 1e-10);
asserts.istrue(abs(y(length(y)) - 0.5) < 1e-10);
asserts.istrue(abs(te(1) - 0.5) < 0.06);
asserts.istrue(abs(ye(1) - 0.5) < 0.06);
asserts.isequal(ie(1), 1);
%=============================================================================
options = odeset('Events', @odeTestQuadraticEvent, 'InitialStep', 0.2, 'MaxStep', 0.2, 'Refine', 1);
[t, y, te, ye, ie] = ode23(f, [0 1], 0, options);
asserts.istrue(abs(te(1) - 0.5) < 1e-6);
asserts.istrue(abs(ye(1) - 0.5) < 1e-6);
asserts.isequal(ie(1), 1);
%=============================================================================
% Event localization must follow the solution curve (Hermite), not the step
% chord: a falling ball hits the ground at exactly sqrt(2/g).
ballOptions = odeset('Events', @odeTestFallingBallEvent, 'RelTol', 1e-8, 'AbsTol', 1e-10);
ballSol = ode45(@(t, y)[y(2); -9.8], [0 3], [1; 0], ballOptions);
asserts.istrue(abs(ballSol.xe(1) - sqrt(2 / 9.8)) < 1e-9);
asserts.istrue(abs(ballSol.ye(1, 1)) < 1e-9);
%=============================================================================
badLengthOptions = odeset('Events', @odeTestBadEventLength);
asserts.checkerror('ode45(@(t, y) 1, [0 1], 0, badLengthOptions)', ...
  _('Event outputs must have the same number of elements.'));

badValueOptions = odeset('Events', @odeTestBadEventValue);
asserts.checkerror('ode45(@(t, y) 1, [0 1], 0, badValueOptions)', ...
  _('Event values must be finite numeric values.'));
%=============================================================================
