%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% The "fmuMe" NFlow block embeds an FMI 3.0 Model Exchange FMU in a diagram.
% Unlike the Co-Simulation block, the FMU exposes only the model equations; its
% continuous states join the diagram's global state vector and are integrated by
% the shared NFlow solver (here fixed-step RK4, ode4). Event indicators feed the
% shared zero-crossing machinery, and located crossings trigger the FMU's
% discrete-state update (the EVENT phase).
%=============================================================================
% A missing FMU path must abort the run cleanly (error status, no crash).
d = struct();
d.name = 'FmuMeBlockTest';
d.sampleTime = 0.01;
d.duration = 1;
d.solver = 'ode4';
fmublock = struct('id', 'me1', 'type', 'fmuMe', 'inputs', 0, 'outputs', 1, ...
  'params', struct('path', 'D:/nflow_fmi/does_not_exist.fmu', ...
  'numContinuousStates', 2, 'numEventIndicators', 0));
scope = struct('id', 'sc1', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
d.blocks = {fmublock, scope};
d.connections = {struct('from', 'me1', 'fromIndex', 0, 'to', 'sc1', 'toIndex', 0)};
res = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(res.status, 'error');
%=============================================================================
% Van der Pol as a Model Exchange block: NFlow integrates its two states with
% ode4 and the output oscillates on the limit cycle.
fmu = [modulepath('nflow_fmi', 'root'), '/examples/VanDerPol.fmu'];
skip_testsuite(~isfile(fmu), ['VanDerPol.fmu not found: ', fmu]);
skip_testsuite(nflow_fmi_skip_no_toolchain(), ...
  'no C toolchain (compiler/cmake): bundled reference FMU cannot be built for this platform');
%=============================================================================
d.duration = 20;
d.blocks{1}.params.path = fmu;
res = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(res.status, 'done');
%=============================================================================
y = res.scopes(1).series;
if iscell(y)
  y = cell2mat(y);
end
y = y(:);
assert_istrue(numel(y) > 10);
assert_istrue(all(isfinite(y)));
% Van der Pol limit-cycle amplitude is about 2; the signal must swing and stay
% bounded (the shared solver integrates it, not the FMU).
assert_istrue((max(y) - min(y)) > 0.5);
assert_istrue(max(abs(y)) < 3);
%=============================================================================
% The block trajectory must match the standalone Model Exchange integration of
% the same FMU at the same fixed step (both are RK4).
r = fmiModelExchange(fmu, 20, 0.01);
col = find(strcmp(r.outputNames, 'x0'));
if ~isempty(col)
  n = min(numel(y), size(r.outputs, 1));
  assert_istrue(max(abs(y(1:n) - r.outputs(1:n, col))) < 1e-6);
end
%=============================================================================
% Bouncing ball as a Model Exchange block: the state event (floor contact) is
% located by the solver's zero-crossing detection and handled by the FMU's
% discrete update (velocity reflected). The height must stay non-negative and
% the ball must bounce several times.
bb = [modulepath('nflow_fmi', 'root'), '/examples/BouncingBall.fmu'];
if isfile(bb)
  bbBlk = struct('id', 'me1', 'type', 'fmuMe', 'inputs', 0, 'outputs', 2, ...
    'params', struct('path', 'BouncingBall.fmu', ...
    'numContinuousStates', 2, 'numEventIndicators', 1, 'outputWidths', [1, 1]));
  hScope = struct('id', 'sh', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
  vScope = struct('id', 'sv', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
  db = struct('name', 'BouncingBallMe', 'sampleTime', 0.005, 'duration', 3, ...
    'solver', 'ode4', ...
    'baseDir', [modulepath('nflow_fmi', 'root'), '/examples'], ...
    'blocks', {{bbBlk, hScope, vScope}}, ...
    'connections', {{...
      struct('from', 'me1', 'fromIndex', 0, 'to', 'sh', 'toIndex', 0), ...
      struct('from', 'me1', 'fromIndex', 1, 'to', 'sv', 'toIndex', 0)}});
  resb = jsondecode(__nflow_simulate__(jsonencode(db)));
  assert_isequal(resb.status, 'done');
  h = resb.scopes(1).series;
  if iscell(h)
    h = cell2mat(h);
  end
  h = h(:);
  v = resb.scopes(2).series;
  if iscell(v)
    v = cell2mat(v);
  end
  v = v(:);
  assert_istrue(all(isfinite(h)));
  % Height stays essentially non-negative (small tolerance for the bisection
  % localisation of the contact time).
  assert_istrue(min(h) > -0.05);
  % The ball must actually bounce: the velocity changes sign several times.
  vsign = sign(v(abs(v) > 1e-6));
  flips = sum(abs(diff(vsign)) > 0);
  assert_istrue(flips >= 3);
end
%=============================================================================
% Model Exchange block WITH input ports, end to end: the StateSpace reference
% FMU is a MIMO system (3 states, 3-wide input u, 3-wide output y). A Constant
% feeds its input; the whole thing must integrate through the shared solver
% without crashing and produce finite output (the block gathers its input ports
% and pushes them to the FMU before every derivative/output evaluation).
ss = [modulepath('nflow_fmi', 'root'), '/examples/StateSpace.fmu'];
if isfile(ss)
  d3 = struct('name', 'StateSpaceMe', 'sampleTime', 0.01, ...
    'duration', 1, 'solver', 'ode4', 'blocks', {{...
      struct('id', 'u1', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
        'params', struct('Value', 1)), ...
      struct('id', 'me1', 'type', 'fmuMe', 'inputs', 1, 'outputs', 1, ...
        'params', struct('path', ss, 'numContinuousStates', 3, 'numEventIndicators', 0)), ...
      struct('id', 'sc1', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct())}}, ...
    'connections', {{...
      struct('from', 'u1', 'fromIndex', 0, 'to', 'me1', 'toIndex', 0), ...
      struct('from', 'me1', 'fromIndex', 0, 'to', 'sc1', 'toIndex', 0)}});
  r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
  assert_isequal(r3.status, 'done');
  y3 = local_series(r3);
  assert_istrue(numel(y3) > 10);
  assert_istrue(all(isfinite(y3)));
end
%=============================================================================
function y = local_series(res)
  y = res.scopes(1).series;
  if iscell(y)
    y = cell2mat(y);
  end
  y = y(:);
end
%=============================================================================
