%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% MOSFETs NMOS / PMOS (Coselica NMOS / PMOS): square-law stateless three-terminal
% devices. Native physicalIsland kinds nmos / pmos (nonlinear, embedded-Newton).
% Pins D (drain), G (gate), S (source); the gate draws no current.
%   id = 0                                 vgs <= Vt   (cutoff)
%   id = Beta ((vgs-Vt) vds - vds^2/2)     vds < vgs-Vt (triode)
%   id = Beta (vgs-Vt)^2 / 2              otherwise    (saturation)
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Common-source NMOS switch ON: gate held above threshold pulls the drain below
% the 5 V rail as the transistor sinks current through the drain resistor.
base = ['{"version":"1.0.0","format":"nflow-2","name":"NMOS","blocks":[', ...
  '{"id":"VDD","type":"ConstantVoltage","params":{"V":5}},', ...
  '{"id":"VG","type":"ConstantVoltage","params":{"V":%s}},', ...
  '{"id":"RD","type":"Resistor","params":{"R":1000}},', ...
  '{"id":"Q","type":"NMOS","params":{"Beta":1e-3,"Vt":1}},', ...
  '{"id":"G","type":"Ground","params":{}},', ...
  '{"id":"VS","type":"VoltageSensor","params":{}},', ...
  '{"id":"sc","type":"scope","inputs":1,"outputs":0,"params":{}}],', ...
  '"connections":[{"from":"VS","to":"sc","fromIndex":0,"toIndex":0}],', ...
  '"physicalWires":[{"pins":["VDD.p","RD.p"]},', ...
  '{"pins":["RD.n","Q.D","VS.p"]},', ...
  '{"pins":["VG.p","Q.G"]},', ...
  '{"pins":["VDD.n","VG.n","Q.S","VS.n","G.p"]}],', ...
  '"solver":"dae","runtime":0.1,"sampleTime":0.02}'];
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(sprintf(base, '3'))));
assert_isequal(r.status, 'done');
vd_on = r.scopes(1).series(end);
assert_istrue(vd_on > 0 && vd_on < 4.5); % conducting: drain pulled down
%=============================================================================
% Gate below threshold: transistor in cutoff, drain sits at the full rail.
r = jsondecode(__nflow_simulate__(NFlow.internal.acausalExpand(sprintf(base, '0'))));
assert_isequal(r.status, 'done');
assert_istrue(r.scopes(1).series(end) > 4.9); % off: drain at the 5 V rail
%=============================================================================
