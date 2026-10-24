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
% Acausal palette N7: the planar frame-diagram expansion. A user authors a
% planar mechanism the palette way (PlanarWorld / PlanarBody with named frames /
% PlanarRevolute / sensors, wired by "physicalWires" over FRAME pins) and sim()
% lowers it, through NFlow.internal.planarExpand, into one native planarMechanicalIsland.
% These tests check that authored diagrams reproduce the directly-authored
% engine results (pendulum vs ode45; double pendulum energy conservation).
%=============================================================================
function p = pe_write(doc, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, jsonencode(doc)); fclose(fid);
end
%=============================================================================
% 1) Single pendulum, authored the palette way. PlanarBody's COM starts at
%    (-1, 0) with a body frame "pivot" at offset (1, 0) (the world origin); a
%    PlanarRevolute pins that frame to the PlanarWorld frame. Released
%    horizontal, phi(t) must match the independent Lagrangian pendulum ode45
%    (I + m L^2) phi'' = m g L cos(phi), the same reference as the direct-JSON
%    test, now reached through the frame-diagram expansion.
g = 9.81; m = 1; L = 1; I = 0.1;
coef = m * g * L / (I + m * L ^ 2);
odef = @(tt, z)[z(2); coef * cos(z(1))];
[tref, zref] = ode45(odef, [0 2], [0; 0]);

doc = struct();
doc.blocks = {...
  struct('id', 'wld', 'type', 'PlanarWorld', 'params', struct('gravity', 9.81)), ...
  struct('id', 'bod', 'type', 'PlanarBody', 'params', struct('m', 1, 'I', 0.1, ...
         'x0', -1, 'y0', 0, 'phi0', 0, 'frames', struct('pivot', [1, 0]))), ...
  struct('id', 'rev', 'type', 'PlanarRevolute', 'params', struct()), ...
  struct('id', 'sPh', 'type', 'PlanarPositionSensor', 'params', struct('axis', 'phi')), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
         'params', struct('VariableName', 'PH'))};
doc.physicalWires = {...
  struct('pins', {{'wld.frame', 'rev.a'}}), ...
  struct('pins', {{'rev.b', 'bod.pivot'}}), ...
  struct('pins', {{'bod.com', 'sPh.a'}})};
doc.connections = {struct('from', 'sPh', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
doc.solver = 'dae'; doc.runtime = 2.0; doc.sampleTime = 0.02;

out = sim(pe_write(doc, 'test_planar_expand_pend'));
lg = out.logsout; nms = {lg.name};
PH = lg(find(strcmp(nms, 'PH'), 1)).values;
t = linspace(0, 2, numel(PH))';
phiRef = interp1(tref, zref(:, 1), t);
assert_istrue(max(abs(PH(:) - phiRef)) < 5e-3);
%=============================================================================
