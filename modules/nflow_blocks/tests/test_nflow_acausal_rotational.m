%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Acausal palette N4 (rotational parity). 1-D rotational mechanics is
% structurally identical to translational (across = angle, through = torque),
% so it maps onto the SAME mechanicalTranslationalIsland kinds with no new
% engine code:
%   Inertia  -> mass (J)          Spring (rotational) -> spring (c)
%   Damper (rotational) -> damper  Torque -> force
%   Fixed (angle) -> fixed         AngleSensor -> positionSensor
%   SpeedSensor (angular) -> speedSensor
% This test locks that mapping with a torsional oscillator and a step-torque
% drive; the future component registry owns the rotational naming/units.
%=============================================================================
function p = ro_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
% 1) Torsional oscillator: inertia J=2, rotational spring c=8, damper d=2,
%    released from phi0 = 1 rad against a fixed housing. J phi'' + d phi' + c phi = 0
%    => wn = sqrt(c/J) = 2, zeta = d/(2 sqrt(c J)) = 0.25, wd = wn sqrt(1-z^2).
%    phi(t) = e^{-z wn t}(cos wd t + (z wn/wd) sin wd t).
tors = ['{"blocks":[', ...
  '{"id":"isl","type":"mechanicalTranslationalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"rotational","nodes":2,"components":[', ...
      '{"kind":"mass","node":1,"m":2,"s0":1,"v0":0},', ...      % Inertia J=2, phi0=1
      '{"kind":"spring","a":1,"b":0,"k":8},', ...               % rotational spring c=8
      '{"kind":"damper","a":1,"b":0,"d":2},', ...               % rotational damper
      '{"kind":"positionSensor","node":1,"outPort":0}]}},', ... % AngleSensor
  '{"id":"twp","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"phi"}}],', ...
  '"connections":[{"from":"isl","to":"twp","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":8.0,"sampleTime":0.05}'];
out = sim(ro_write(tors, 'test_nflow_rot_tors'));
lg = out.logsout; nms = {lg.name};
phi = lg(find(strcmp(nms, 'phi'), 1)).values;
t = linspace(0, 8, numel(phi))';
wn = 2; zeta = 0.25; wd = wn * sqrt(1 - zeta ^ 2);
pref = exp(-zeta * wn * t) .* (cos(wd * t) + (zeta * wn / wd) * sin(wd * t));
assert_istrue(max(abs(phi(:) - pref)) < 2e-3);
%=============================================================================
% 2) Step torque on a damped inertia (J=1, d=2, no spring): T=4 from rest.
%    Terminal angular velocity w_inf = T/d = 2; w(t) = 2 (1 - e^{-(d/J) t}).
step = ['{"blocks":[', ...
  '{"id":"isl","type":"mechanicalTranslationalIsland","inputs":0,"outputs":1,"params":{', ...
    '"domain":"rotational","nodes":2,"components":[', ...
      '{"kind":"mass","node":1,"m":1,"s0":0,"v0":0},', ...
      '{"kind":"damper","a":1,"b":0,"d":2},', ...
      '{"kind":"force","node":1,"F":4},', ...                   % Torque T=4
      '{"kind":"speedSensor","node":1,"outPort":0}]}},', ...
  '{"id":"tww","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"w"}}],', ...
  '"connections":[{"from":"isl","to":"tww","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":4.0,"sampleTime":0.05}'];
out = sim(ro_write(step, 'test_nflow_rot_step'));
lg = out.logsout; nms = {lg.name};
w = lg(find(strcmp(nms, 'w'), 1)).values;
t = linspace(0, 4, numel(w))';
wref = 2 * (1 - exp(-2 * t));
assert_istrue(max(abs(w(:) - wref)) < 2e-3);
%=============================================================================
% 3) Two inertias through a rotational spring (J1=1, J2=1, c=2), no damping.
%    Angular momentum conserves: the mean angle (J1 phi1 + J2 phi2)/(J1+J2) is
%    a straight line (here, at rest, constant). Start phi1=0, phi2=1 (pre-twist).
two = ['{"blocks":[', ...
  '{"id":"isl","type":"mechanicalTranslationalIsland","inputs":0,"outputs":2,"params":{', ...
    '"domain":"rotational","nodes":3,"components":[', ...
      '{"kind":"mass","node":1,"m":1,"s0":0,"v0":0},', ...
      '{"kind":"mass","node":2,"m":1,"s0":1,"v0":0},', ...
      '{"kind":"spring","a":1,"b":2,"k":2},', ...
      '{"kind":"positionSensor","node":1,"outPort":0},', ...
      '{"kind":"positionSensor","node":2,"outPort":1}]}},', ...
  '{"id":"tw1","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"p1"}},', ...
  '{"id":"tw2","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"p2"}}],', ...
  '"connections":[', ...
    '{"from":"isl","to":"tw1","fromIndex":0,"toIndex":0},', ...
    '{"from":"isl","to":"tw2","fromIndex":1,"toIndex":0}],', ...
  '"solver":"dae","runtime":10.0,"sampleTime":0.05}'];
out = sim(ro_write(two, 'test_nflow_rot_two'));
lg = out.logsout; nms = {lg.name};
p1 = lg(find(strcmp(nms, 'p1'), 1)).values;
p2 = lg(find(strcmp(nms, 'p2'), 1)).values;
mean_angle = (p1(:) + p2(:)) / 2;
assert_istrue(max(abs(mean_angle - 0.5)) < 2e-3);
%=============================================================================
