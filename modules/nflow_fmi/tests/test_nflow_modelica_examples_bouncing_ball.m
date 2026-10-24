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
% Bouncing ball (state events + reinit): lenient qualitative checks under a
% fixed-step Co-Simulation. It must not fall through the floor, must lose energy
% (bounce peaks below the drop height), and must bounce several times.
[t, h] = nflow_modelica_example_run([exdir, '/BouncingBall.mo'], 'BouncingBall', 3.0, 0.005, 'h');
assert_istrue(abs(h(1) - 1.0) < 1e-9);
assert_istrue(min(h) > -0.05); % stays essentially above the floor
assert_istrue(max(h(t > 0.5)) < 0.9); % energy lost after the first bounces
nbounce = sum(diff(sign(diff(h))) > 0); % local minima ~ impacts
assert_istrue(nbounce >= 3);
%=============================================================================
