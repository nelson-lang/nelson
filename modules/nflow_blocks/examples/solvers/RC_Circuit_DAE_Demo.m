%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A charging RC circuit as a differential-algebraic (DAE) system -- the
% kind of acausal, physical model the constraint block + `dae` solver enable.
%
%   capacitor : C v_c' = i        (differential state v_c; here C = 1)
%   resistor + KVL : v_s = R i + v_c   ->   0 = v_s - R i - v_c   (algebraic i)
%
% with v_s = 1, R = 1, C = 1, v_c(0) = 0. The exact solution is the charging
% curve v_c(t) = 1 - e^{-t} and the current i(t) = e^{-t}. The current i is the
% algebraic state carried by the constraint block; the diagram feeds it the KVL
% residual g = v_s - R i - v_c and the DAE solver drives g -> 0.
%=============================================================================

model = ['{"blocks":[', ...
  '{"id":"vs","type":"constant","inputs":0,"outputs":1,"params":{"Value":1}},', ...
  '{"id":"vc","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":0}},', ...
  '{"id":"ci","type":"constraint","inputs":1,"outputs":1,"params":{"InitialCondition":1}},', ...
  '{"id":"gni","type":"gain","inputs":1,"outputs":1,"params":{"Gain":-1}},', ...
  '{"id":"gnvc","type":"gain","inputs":1,"outputs":1,"params":{"Gain":-1}},', ...
  '{"id":"sg","type":"sum","inputs":3,"outputs":1,"params":{}},', ...
  '{"id":"twvc","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"vc"}},', ...
  '{"id":"twi","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"i"}}],', ...
  '"connections":[', ...
  '{"from":"ci","to":"vc","fromIndex":0,"toIndex":0},', ...          % C v_c' = i
  '{"from":"ci","to":"gni","fromIndex":0,"toIndex":0},', ...         % -i
  '{"from":"vc","to":"gnvc","fromIndex":0,"toIndex":0},', ...        % -v_c
  '{"from":"vs","to":"sg","fromIndex":0,"toIndex":0},', ...          % v_s
  '{"from":"gni","to":"sg","fromIndex":0,"toIndex":1},', ...
  '{"from":"gnvc","to":"sg","fromIndex":0,"toIndex":2},', ...        % g = v_s - i - v_c
  '{"from":"sg","to":"ci","fromIndex":0,"toIndex":0},', ...
  '{"from":"vc","to":"twvc","fromIndex":0,"toIndex":0},', ...
  '{"from":"ci","to":"twi","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":1.0,"sampleTime":0.1}'];

modelFile = [tempdir(), '/RC_Circuit_DAE_Demo.nflow'];
fid = fopen(modelFile, 'w'); fwrite(fid, model); fclose(fid);

out = sim(modelFile);
names = {out.logsout.name};
vc = out.logsout(find(strcmp(names, 'vc'), 1)).values;
i = out.logsout(find(strcmp(names, 'i'), 1)).values;
fprintf('RC circuit DAE: v_c(1) = %.5f (1 - e^{-1} = %.5f), i(1) = %.5f (e^{-1} = %.5f)\n', ...
  vc(end), 1 - exp(-1), i(end), exp(-1));
%=============================================================================
