%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(t, y) - y;
sol = ode45(f, [0 0.5], 1, odeset('MaxStep', 0.05));
extended = odextend(sol, f, 1);
asserts.istrue(abs(extended.x(length(extended.x)) - 1) < 1e-12);
asserts.istrue(abs(extended.y(1, length(extended.x)) - exp(-1)) < 5e-3);
asserts.checkerror('odextend(extended, f, -1)', ...
  _('ODE solution cannot be extended in the requested direction.'));
%=============================================================================
