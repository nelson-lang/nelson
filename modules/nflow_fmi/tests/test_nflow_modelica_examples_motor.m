%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~modelicaInfo().capable
  return
end
exdir = [modulepath('nflow_fmi'), '/examples/modelica'];
%=============================================================================
% DC motor: spins up to steady speed w_ss = V / (R b / Kt + Ke). Reference = ode45.
[t, w] = nflow_modelica_example_run([exdir, '/DCMotor.mo'], 'DCMotor', 5.0, 0.02, 'w');
fdc = @(tt, s)[(1.0 - 1.0 * s(1) - 0.1 * s(2)) / 0.5; (0.1 * s(1) - 0.1 * s(2)) / 0.01]; % [i';w']
[to, so] = ode45(fdc, [0 5], [0; 0]);
wss = 1.0 / (1.0 * 0.1 / 0.1 + 0.1);
assert_istrue(abs(w(end) - wss) < 1e-2);
assert_istrue(abs(w(end) - so(end, 2)) < 1e-2);
%=============================================================================
