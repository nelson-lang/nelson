%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Differential-algebraic (DAE) demo: a differential state and an
% algebraic constraint solved together by the IDAS backend.
%
%   x' = -x,  x(0) = 1        (a differential state -> x(t) = e^{-t})
%   0  = z - 3x               (a constraint block -> the algebraic state z = 3x)
%
% The `constraint` block owns the algebraic state z (its output); the diagram
% feeds it the residual g = z - 3x, and the solver drives g -> 0 so z tracks 3x.
% This only runs under solver = "dae" (set below); an ODE solver or code
% generation rejects the constraint block.
%=============================================================================

model = ['{"blocks":[', ...
  '{"id":"ig","type":"integrator","inputs":1,"outputs":1,"params":{"InitialCondition":1}},', ...
  '{"id":"gm1","type":"gain","inputs":1,"outputs":1,"params":{"Gain":-1}},', ...   % x' = -x
  '{"id":"gm3","type":"gain","inputs":1,"outputs":1,"params":{"Gain":-3}},', ...   % -3x
  '{"id":"cz","type":"constraint","inputs":1,"outputs":1,"params":{"InitialCondition":3}},', ...
  '{"id":"sg","type":"sum","inputs":2,"outputs":1,"params":{}},', ...              % g = z + (-3x)
  '{"id":"twx","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"x"}},', ...
  '{"id":"twz","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"z"}}],', ...
  '"connections":[', ...
  '{"from":"ig","to":"gm1","fromIndex":0,"toIndex":0},', ...
  '{"from":"gm1","to":"ig","fromIndex":0,"toIndex":0},', ...
  '{"from":"ig","to":"gm3","fromIndex":0,"toIndex":0},', ...
  '{"from":"cz","to":"sg","fromIndex":0,"toIndex":0},', ...
  '{"from":"gm3","to":"sg","fromIndex":0,"toIndex":1},', ...
  '{"from":"sg","to":"cz","fromIndex":0,"toIndex":0},', ...
  '{"from":"ig","to":"twx","fromIndex":0,"toIndex":0},', ...
  '{"from":"cz","to":"twz","fromIndex":0,"toIndex":0}],', ...
  '"solver":"dae","runtime":1.0,"sampleTime":0.1}'];

modelFile = [tempdir(), '/Constraint_DAE_Demo.nflow'];
fid = fopen(modelFile, 'w'); fwrite(fid, model); fclose(fid);

out = sim(modelFile);
names = {out.logsout.name};
x = out.logsout(find(strcmp(names, 'x'), 1)).values;
z = out.logsout(find(strcmp(names, 'z'), 1)).values;
fprintf('DAE demo: x(1) = %.6f (e^{-1} = %.6f), z(1) = %.6f (3*x = %.6f)\n', ...
  x(end), exp(-1), z(end), 3 * x(end));
%=============================================================================
