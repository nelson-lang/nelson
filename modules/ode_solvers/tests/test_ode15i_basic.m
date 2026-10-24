%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(t, y, yp) yp + y;
[t, y] = ode15i(f, [0 1], 1, -1, odeset('MaxStep', 0.05));
asserts.istrue(abs(t(length(t)) - 1) < 1e-12);
asserts.istrue(abs(y(size(y, 1), 1) - exp(-1)) < 1e-3);
%=============================================================================
residual = @(t, y, yp) yp + [-y(2); y(1)];
options = odeset('JPattern', eye(2), 'InitialStep', 0.02, 'MaxStep', 0.02, ...
  'RelTol', 1e-4, 'AbsTol', 1e-7);
[t, y] = ode15i(residual, [0 0.2], [1; 0], [0; -1], options);
asserts.istrue(abs(t(length(t)) - 0.2) < 1e-12);
asserts.istrue(norm(y(size(y, 1), :) - [cos(0.2), -sin(0.2)], inf) < 5e-4);
%=============================================================================
