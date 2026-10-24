%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Acausal palette N3 (electromechanical): a DC motor as an electrical island
% coupled to a rotational-mechanical island through the causal signal graph.
% Because the armature current (inductor state) and the shaft speed (inertia
% state) are DIFFERENTIAL, the back-emf e = k*omega and the motor torque
% tau = k*i are functions of states, not of the coupling inputs, so there is NO
% algebraic loop; the existing two-island architecture models the coupling
% directly, no unified multi-domain block needed.
%
%   L di/dt = V - R i - k omega        (electrical loop)
%   J dw/dt = k i - d w                (mechanical shaft)
%
% Expected result: an ode45 integration of that 2-state system.
%=============================================================================
function p = dc_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
R = 1; L = 1; k = 1; J = 1; d = 1; V = 1;
odef = @(t, y)[(V - R * y(1) - k * y(2)) / L; % di/dt
                 (k * y(1) - d * y(2)) / J]; % dw/dt
[tref, yref] = ode45(odef, [0 6], [0; 0]);

% Electrical island: V(1-0) - ammeter(1-2) - R(2-3) - L(3-4) - backemf(4-0,signal).
% inputs = 1 (back-emf), outputs = 1 (loop current).
elec = ['{"id":"elec","type":"physicalIsland","inputs":1,"outputs":1,"params":{', ...
  '"domain":"electrical","nodes":5,"components":[', ...
    '{"kind":"vsource","a":1,"b":0,"V":1},', ...
    '{"kind":"currentSensor","a":1,"b":2,"outPort":0},', ...
    '{"kind":"resistor","a":2,"b":3,"R":1},', ...
    '{"kind":"inductor","a":3,"b":4,"L":1,"ic":0},', ...
    '{"kind":"vsource","a":4,"b":0,"port":0}]}}'];
% Mechanical (rotational) island: torque(node1,signal) - inertia(node1,J) -
% damper(1-0,d) - speedSensor(node1). inputs = 1 (torque), outputs = 1 (speed).
mech = ['{"id":"mech","type":"mechanicalTranslationalIsland","inputs":1,"outputs":1,"params":{', ...
  '"domain":"rotational","nodes":2,"components":[', ...
    '{"kind":"mass","node":1,"m":1,"s0":0,"v0":0},', ...
    '{"kind":"damper","a":1,"b":0,"d":1},', ...
    '{"kind":"force","node":1,"port":0},', ...
    '{"kind":"speedSensor","node":1,"outPort":0}]}}'];

model = ['{"blocks":[', elec, ',', mech, ',', ...
  '{"id":"gk1","type":"gain","inputs":1,"outputs":1,"params":{"Gain":', sprintf('%.15g', k), '}},', ...
  '{"id":"gk2","type":"gain","inputs":1,"outputs":1,"params":{"Gain":', sprintf('%.15g', k), '}},', ...
  '{"id":"twi","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"i"}},', ...
  '{"id":"tww","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"w"}}],', ...
  '"connections":[', ...
    '{"from":"elec","to":"gk1","fromIndex":0,"toIndex":0},', ...   % current -> k
    '{"from":"gk1","to":"mech","fromIndex":0,"toIndex":0},', ...   % k*i -> torque
    '{"from":"mech","to":"gk2","fromIndex":0,"toIndex":0},', ...   % speed -> k
    '{"from":"gk2","to":"elec","fromIndex":0,"toIndex":0},', ...   % k*w -> back-emf
    '{"from":"elec","to":"twi","fromIndex":0,"toIndex":0},', ...
    '{"from":"mech","to":"tww","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":6.0,"sampleTime":0.05}'];
out = sim(dc_write(model, 'test_nflow_dcmotor'));
lg = out.logsout; nms = {lg.name};
ii = lg(find(strcmp(nms, 'i'), 1)).values;
ww = lg(find(strcmp(nms, 'w'), 1)).values;
t = linspace(0, 6, numel(ii))';
iref = interp1(tref, yref(:, 1), t);
wref = interp1(tref, yref(:, 2), t);
assert_istrue(max(abs(ii(:) - iref)) < 5e-3);
assert_istrue(max(abs(ww(:) - wref)) < 5e-3);
% Steady state: i = w = 0.5.
assert_istrue(abs(ii(end) - 0.5) < 1e-2);
assert_istrue(abs(ww(end) - 0.5) < 1e-2);
%=============================================================================
