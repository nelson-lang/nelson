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
% RLC: series circuit, damped oscillation of vC toward V = 1. Reference = ode45.
[t, vC] = nflow_modelica_example_run([exdir, '/RLC.mo'], 'RLC', 12.0, 0.02, 'vC');
frlc = @(tt, s)[(1.0 - 1.0 * s(1) - s(2)); s(1)]; % [iL'; vC'], R=L=C=V=1
[to, so] = ode45(frlc, [0 12], [0; 0]);
assert_istrue(abs(vC(end) - so(end, 2)) < 5e-3);
assert_istrue(abs(vC(end) - 1.0) < 1e-2); % settles to the source
assert_istrue(max(vC) > 1.05); % underdamped overshoot
%=============================================================================
