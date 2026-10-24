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
% Acausal palette N3 (smooth semiconductors). The physicalIsland block gains an
% exponential (Shockley) diode i = Is (exp(vd/Vt) - 1); IDAS's Newton handles
% the nonlinearity, so a half-wave rectifier + RC load simulates directly.
%=============================================================================
function p = di_write(json, stem)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, json); fclose(fid);
end
%=============================================================================
% Half-wave rectifier: sine source u(t) = 3 sin(2 pi t) -> diode -> RC load
% (R = 1000, C = 1e-3). Node cap voltage vc obeys
%   C vc' = Is (exp((u - vc)/Vt) - 1) - vc/R.
% Compare the physicalIsland simulation to an ode45 reference of that ODE.
Is = 1e-9; Vt = 0.025; R = 1000; C = 1e-3;
u = @(tt) 3 * sin(2 * pi * tt);
idiode = @(vd) Is * (exp(min(vd / Vt, 40)) - 1);
odef = @(tt, vc)(idiode(u(tt) - vc) - vc / R) / C;
[tref, vref] = ode45(odef, [0 2], 0);

rect = ['{"blocks":[', ...
  '{"id":"src","type":"sine","inputs":0,"outputs":1,"params":{"Amplitude":3,"Frequency":1,"Phase":0,"Bias":0}},', ...
  '{"id":"isl","type":"physicalIsland","inputs":1,"outputs":1,"params":{', ...
    '"domain":"electrical","nodes":3,"components":[', ...
      '{"kind":"vsource","a":1,"b":0,"port":0},', ...             % signal-driven source
      '{"kind":"diode","a":1,"b":2,"Is":1e-9,"Vt":0.025},', ...   % rectifying diode
      '{"kind":"resistor","a":2,"b":0,"R":1000},', ...            % load
      '{"kind":"capacitor","a":2,"b":0,"C":1e-3,"ic":0},', ...    % filter
      '{"kind":"voltageSensor","a":2,"b":0,"outPort":0}]}},', ...
  '{"id":"twvc","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"vc"}}],', ...
  '"connections":[', ...
    '{"from":"src","to":"isl","fromIndex":0,"toIndex":0},', ...
    '{"from":"isl","to":"twvc","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":2.0,"sampleTime":0.01}'];
out = sim(di_write(rect, 'test_nflow_diode_rect'));
lg = out.logsout; nms = {lg.name};
vc = lg(find(strcmp(nms, 'vc'), 1)).values;
t = linspace(0, 2, numel(vc))';
vcheck = interp1(tref, vref, t);
% The rectifier follows the peaks and holds between them; match the ode45 ref.
assert_istrue(max(abs(vc(:) - vcheck)) < 0.05);
% Physical sanity: output stays positive (one-way conduction) and below the peak.
assert_istrue(min(vc(:)) > -0.05);
assert_istrue(max(vc(:)) < 3.0);
%=============================================================================
