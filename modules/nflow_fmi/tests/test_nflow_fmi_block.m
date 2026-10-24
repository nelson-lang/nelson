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
% The "fmu" NFlow block embeds an FMI 3.0 Co-Simulation FMU in a diagram and
% is driven by the simulation engine (INIT loads/initializes, OUTPUT reads the
% FMU outputs, UPDATE advances one communication step).
%=============================================================================
% A missing FMU path must abort the run cleanly (error status, no crash).
d = struct();
d.name = 'FmuBlockTest';
d.sampleTime = 0.05;
d.duration = 20;
d.solver = 'discrete';
fmublock = struct('id', 'fmu1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, ...
  'params', struct('path', 'D:/nflow_fmi/does_not_exist.fmu'));
scope = struct('id', 'sc1', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
d.blocks = {fmublock, scope};
d.connections = {struct('from', 'fmu1', 'fromIndex', 0, 'to', 'sc1', 'toIndex', 0)};
res = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(res.status, 'error');
%=============================================================================
% Real FMU when available: the block reproduces the Van der Pol oscillation.
fmu = [modulepath('nflow_fmi', 'root'), '/examples/VanDerPol.fmu'];
skip_testsuite(~isfile(fmu), ['VanDerPol.fmu not found: ', fmu]);
skip_testsuite(nflow_fmi_skip_no_toolchain(), ...
  'no C toolchain (compiler/cmake): bundled reference FMU cannot be built for this platform');
%=============================================================================
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
% Van der Pol limit-cycle amplitude is about 2; the signal must swing.
assert_istrue((max(y) - min(y)) > 0.1);
assert_istrue(max(abs(y)) < 5);
%=============================================================================
% The block trajectory must match the direct Co-Simulation of the same FMU.
r = fmiCoSimulate(fmu, 20, 0.05);
col = find(strcmp(r.outputNames, 'x0'));
n = min(numel(y), size(r.outputs, 1));
assert_istrue(max(abs(y(1:n) - r.outputs(1:n, col))) < 1e-9);
%=============================================================================
% Tunable FMU parameter: overriding BouncingBall gravity (g) changes the
% trajectory. With weaker gravity the ball is higher at a fixed early time.
bb = [modulepath('nflow_fmi', 'root'), '/examples/BouncingBall.fmu'];
if isfile(bb)
  fmuBlk = struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 2, ...
    'params', struct('path', 'BouncingBall.fmu'));
  scp = struct('id', 's1', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
  base = struct('sampleTime', 0.02, 'duration', 0.4, 'solver', 'discrete', ...
    'baseDir', [modulepath('nflow_fmi', 'root'), '/examples'], ...
    'blocks', {{fmuBlk, scp}}, ...
    'connections', {{struct('from', 'f1', 'fromIndex', 0, 'to', 's1', 'toIndex', 0)}});
  hEnd = [0, 0];
  gvals = {[], -1.0};
  for kk = 1:2
    d2 = base;
    if ~isempty(gvals{kk})
      d2.blocks{1}.params.parameterValues = struct('g', gvals{kk});
    end
    res2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
    assert_isequal(res2.status, 'done');
    ys = res2.scopes(1).series;
    if iscell(ys)
      ys = cell2mat(ys);
    end
    hEnd(kk) = ys(end);
  end
  assert_istrue(hEnd(2) > hEnd(1));
end
%=============================================================================
