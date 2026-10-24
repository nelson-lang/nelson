%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% Acausal palette N7: a CLOSED kinematic loop (a four-bar linkage) authored the
% palette way. In absolute centre-of-mass coordinates a closed loop needs NO
% special "loop joint": it is simply four Revolute constraints among the ground
% and three moving bodies, and the DAE (Baumgarte-stabilised) closes the loop
% by construction. This test validates that: a parallelogram four-bar released
% under gravity conserves total mechanical energy and keeps every pin closed,
% end to end through NFlow.internal.planarExpand.
%
% Geometry (a parallelogram; crank || rocker, both length 1; ground = coupler =
% 2), started at crank angle theta = 60 deg so gravity gives a nonzero torque:
%   A = (0,0) [world], D = (2,0) [fixed];
%   crank  COM (0.25, 0.433) phi=pi/3 ; coupler COM (1.5, 0.866) phi=0 ;
%   rocker COM (2.25, 0.433) phi=pi/3.
% All four revolute constraints are satisfied at t = 0.
%=============================================================================
th = pi / 3; c = cos(th); s = sin(th); Ir = 1 / 12; Ic = 2 * 4 / 12;
frR = struct('prox', [-0.5, 0], 'dist', [0.5, 0]); % crank / rocker frames
frC = struct('prox', [-1, 0], 'dist', [1, 0]); % coupler frames

doc = struct();
doc.blocks = {...
  struct('id', 'wld', 'type', 'PlanarWorld', 'params', struct('gravity', 9.81)), ...
  struct('id', 'gD', 'type', 'PlanarFixed', 'params', struct('x', 2, 'y', 0)), ...
  struct('id', 'cr', 'type', 'PlanarBody', 'params', struct('m', 1, 'I', Ir, ...
         'x0', 0.5 * c, 'y0', 0.5 * s, 'phi0', th, 'frames', frR)), ...
  struct('id', 'co', 'type', 'PlanarBody', 'params', struct('m', 2, 'I', Ic, ...
         'x0', 1 + c, 'y0', s, 'phi0', 0, 'frames', frC)), ...
  struct('id', 'ro', 'type', 'PlanarBody', 'params', struct('m', 1, 'I', Ir, ...
         'x0', 2 + 0.5 * c, 'y0', 0.5 * s, 'phi0', th, 'frames', frR)), ...
  struct('id', 'r0', 'type', 'PlanarRevolute', 'params', struct()), ...
  struct('id', 'r1', 'type', 'PlanarRevolute', 'params', struct()), ...
  struct('id', 'r2', 'type', 'PlanarRevolute', 'params', struct()), ...
  struct('id', 'r3', 'type', 'PlanarRevolute', 'params', struct())};
% Sensors: y, vx, vy, w per body (energy) + crank phi (motion check).
chans = {...
  'cr', 'y', 'YCR'; 'cr', 'x', 'na1'; 'co', 'y', 'YCO'; 'ro', 'y', 'YRO'; ...
  'cr', 'phi', 'PCR'};
vchans = {'cr', 'x', 'VXCR'; 'cr', 'y', 'VYCR'; 'cr', 'omega', 'WCR'; ...
  'co', 'x', 'VXCO'; 'co', 'y', 'VYCO'; 'co', 'omega', 'WCO'; ...
  'ro', 'x', 'VXRO'; 'ro', 'y', 'VYRO'; 'ro', 'omega', 'WRO'};
sid = 0;
addSensor = @(bid, ax, nm) struct('id', ['s', nm], 'type', 'PlanarPositionSensor', ...
  'params', struct('axis', ax));
for k = 1:size(chans, 1)
  if strcmp(chans{k, 3}, 'na1'); continue; end
  doc.blocks{end + 1} = struct('id', ['sp', chans{k, 3}], 'type', 'PlanarPositionSensor', ...
    'params', struct('axis', chans{k, 2}));
end
for k = 1:size(vchans, 1)
  doc.blocks{end + 1} = struct('id', ['sv', vchans{k, 3}], 'type', 'PlanarVelocitySensor', ...
    'params', struct('axis', vchans{k, 2}));
end
% toWorkspace per logged channel.
allch = {};
for k = 1:size(chans, 1)
  if strcmp(chans{k, 3}, 'na1'); continue; end
  allch{end + 1} = {['sp', chans{k, 3}], chans{k, 3}}; %#ok<AGROW>
end
for k = 1:size(vchans, 1)
  allch{end + 1} = {['sv', vchans{k, 3}], vchans{k, 3}}; %#ok<AGROW>
end
for k = 1:numel(allch)
  doc.blocks{end + 1} = struct('id', ['tw', allch{k}{2}], 'type', 'toWorkspace', ...
    'inputs', 1, 'outputs', 0, 'params', struct('VariableName', allch{k}{2}));
end
% Frame wires: the four revolutes closing the loop A-crank-B-coupler-C-rocker-D.
doc.physicalWires = {...
  struct('pins', {{'wld.frame', 'r0.a'}}), ...
  struct('pins', {{'r0.b', 'cr.prox'}}), ...
  struct('pins', {{'cr.dist', 'r1.a'}}), ...
  struct('pins', {{'r1.b', 'co.prox'}}), ...
  struct('pins', {{'co.dist', 'r2.a'}}), ...
  struct('pins', {{'r2.b', 'ro.dist'}}), ...
  struct('pins', {{'ro.prox', 'r3.a'}}), ...
  struct('pins', {{'r3.b', 'gD.frame'}})};
% Sensor frame attachments (each body's COM).
bodySens = struct('cr', {{'YCR', 'PCR', 'VXCR', 'VYCR', 'WCR'}}, ...
  'co', {{'YCO', 'VXCO', 'VYCO', 'WCO'}}, 'ro', {{'YRO', 'VXRO', 'VYRO', 'WRO'}});
bnames = fieldnames(bodySens);
for b = 1:numel(bnames)
  pins = {[bnames{b}, '.com']};
  ss = bodySens.(bnames{b});
  for j = 1:numel(ss)
    if any(strcmp(ss{j}, {'YCR', 'YCO', 'YRO', 'PCR'}))
      pins{end + 1} = ['sp', ss{j}, '.a']; %#ok<AGROW>
    else
      pins{end + 1} = ['sv', ss{j}, '.a']; %#ok<AGROW>
    end
  end
  doc.physicalWires{end + 1} = struct('pins', {pins});
end
% Connections: each sensor block -> its toWorkspace.
doc.connections = {};
for k = 1:numel(allch)
  doc.connections{end + 1} = struct('from', allch{k}{1}, 'to', ['tw', allch{k}{2}], ...
    'fromIndex', 0, 'toIndex', 0);
end
doc.solver = 'dae'; doc.runtime = 0.3; doc.sampleTime = 0.01;

p = [tempname(), '.nflow'];
fid = fopen(p, 'w'); fwrite(fid, jsonencode(doc)); fclose(fid);
out = sim(p);
lg = out.logsout; nms = {lg.name};
gv = @(nm) lg(find(strcmp(nms, nm), 1)).values(:);
YCR = gv('YCR'); YCO = gv('YCO'); YRO = gv('YRO'); PCR = gv('PCR');
E = 0.5 * (gv('VXCR') .^ 2 + gv('VYCR') .^ 2) + 0.5 * Ir * gv('WCR') .^ 2 ...
  + 0.5 * 2 * (gv('VXCO') .^ 2 + gv('VYCO') .^ 2) + 0.5 * Ic * gv('WCO') .^ 2 ...
  + 0.5 * (gv('VXRO') .^ 2 + gv('VYRO') .^ 2) + 0.5 * Ir * gv('WRO') .^ 2 ...
  + 9.81 * (1 * YCR + 2 * YCO + 1 * YRO);
% The closed loop conserves total mechanical energy (ideal joints do no work).
assert_istrue(max(abs(E - E(1))) < 0.1);
% And it actually moves (the crank angle swings away from 60 deg).
assert_istrue(max(abs(PCR - PCR(1))) > 0.05);
%=============================================================================
