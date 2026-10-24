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
% Mass-spring-damper: decaying oscillation to rest. Reference = ode45.
[t, x] = nflow_modelica_example_run([exdir, '/MassSpringDamper.mo'], 'MassSpringDamper', 20.0, 0.02, 'x');
fmsd = @(tt, s)[s(2); (-1.0 * s(1) - 0.4 * s(2))]; % [x'; v'], m=k=1, c=0.4
[to, so] = ode45(fmsd, [0 20], [1; 0]);
assert_istrue(abs(x(1) - 1.0) < 1e-9);
assert_istrue(abs(x(end) - so(end, 1)) < 1e-2);
assert_istrue(abs(x(end)) < 5e-2); % came to rest
assert_istrue(min(x) < -0.1); % oscillated (undershoot)
%=============================================================================
