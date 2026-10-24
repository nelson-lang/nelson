%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
residual = @(t, y, yp) yp + y;
[y0, yp0, resnrm] = decic(residual, 0, 1, 1, 0, 0);
asserts.isequal(y0, 1);
asserts.istrue(abs(yp0 + 1) < 1e-8);
asserts.istrue(resnrm < 1e-8);
[t, y] = ode15i(residual, [0 0.5], y0, yp0);
asserts.istrue(abs(y(length(y)) - exp(-0.5)) < 1e-3);
[tfull, yfull] = ode15i(residual, [0 1], y0, yp0);
asserts.istrue(abs(tfull(2) - 0.025) < 0.01);
asserts.istrue(abs(tfull(3) - 0.05) < 0.015);
asserts.istrue(abs(tfull(4) - 0.10) < 0.02);
asserts.istrue(abs(yfull(length(yfull)) - exp(-1)) < 1e-3);

residual2 = @(t, y, yp)[yp(1) + y(1); y(2) - 2];
[yc, ypc] = decic(residual2, 0, [1; 0], [1; 0], [0; 0], [0; 1]);
asserts.istrue(norm(residual2(0, yc, ypc), inf) < 1e-8);
asserts.isequal(yc(1), 1);
asserts.isequal(ypc(2), 0);

asserts.checkerror('decic(@(t,y,yp) yp + y, 0, 1, 1, 0, 1)', ...
  _('At least one initial component must be free.'));
asserts.checkerror('decic(1, 0, 1, 1, 0, 0)', sprintf(_('%s must be a function handle.'), 'Value'));
%=============================================================================
