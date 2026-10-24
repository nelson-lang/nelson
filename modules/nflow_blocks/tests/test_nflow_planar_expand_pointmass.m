%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% Point-mass pendulum authored the palette way: a PlanarPointMass (no
% orientation, I irrelevant) held at distance L = 1 from the world origin by a
% PlanarDistance rod is a simple pendulum theta'' = -(g/L) sin(theta).
% Released horizontal, its COM path must match the ode45 pendulum.
odef2 = @(tt, z)[z(2); -(9.81 / 1) * sin(z(1))];
[tr2, zr2] = ode45(odef2, [0 2], [pi / 2; 0]);
d3 = struct();
d3.blocks = {...
  struct('id', 'wld', 'type', 'PlanarWorld', 'params', struct('gravity', 9.81)), ...
  struct('id', 'pm', 'type', 'PlanarPointMass', 'params', struct('m', 1, 'x0', 1, 'y0', 0)), ...
  struct('id', 'rod', 'type', 'PlanarDistance', 'params', struct('length', 1)), ...
  struct('id', 'sx', 'type', 'PlanarPositionSensor', 'params', struct('axis', 'x')), ...
  struct('id', 'sy', 'type', 'PlanarPositionSensor', 'params', struct('axis', 'y')), ...
  struct('id', 'tx', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'MX')), ...
  struct('id', 'ty', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'MY'))};
d3.physicalWires = {...
  struct('pins', {{'wld.frame', 'rod.a'}}), ...
  struct('pins', {{'rod.b', 'pm.com', 'sx.a', 'sy.a'}})};
d3.connections = {...
  struct('from', 'sx', 'to', 'tx', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sy', 'to', 'ty', 'fromIndex', 0, 'toIndex', 0)};
d3.solver = 'dae'; d3.runtime = 2.0; d3.sampleTime = 0.02;
out = sim(nflow_blocks_write_json(d3, 'test_planar_expand_pointmass'));
lg = out.logsout; nms = {lg.name};
MX = lg(find(strcmp(nms, 'MX'), 1)).values(:);
MY = lg(find(strcmp(nms, 'MY'), 1)).values(:);
t3 = linspace(0, 2, numel(MX))';
thr = interp1(tr2, zr2(:, 1), t3);
assert_istrue(max(abs(MX - sin(thr))) < 1e-2);
assert_istrue(max(abs(MY - (-cos(thr)))) < 1e-2);
%=============================================================================
