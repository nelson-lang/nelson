%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Regression: a diagram holding a subsystem, a continuous state and a
% zero-crossing surface took the whole process down under a continuous solver.
% The scan looking for a triggered subsystem's control surface walked into a
% subsystem's inner blocks but kept reading the OUTER state vector, so an inner
% block's id resolved back to the subsystem being scanned and the scan recursed
% until the stack ran out. That is the most ordinary diagram there is - a step
% (which declares the surface) driving a subsystem, feeding an integrator - so
% it is pinned here on every native and variable-step backend.
%=============================================================================
inner = struct();
inner.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'x', 60, 'y', 60, 'rotation', 0, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'g', 'type', 'gain', 'x', 220, 'y', 60, 'rotation', 0, ...
    'params', struct('Gain', 3)), ...
  struct('id', 'out1', 'type', 'labelSink', 'x', 380, 'y', 60, 'rotation', 0, ...
    'params', struct('label', 'out1'))};
inner.connections = {...
  struct('from', 'in1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
inner.externalInputs = {struct('id', 'in1', 'name', 'in1')};
inner.externalOutputs = {struct('id', 'out1', 'name', 'out1')};

doc = struct();
doc.version = '1.0.0';
doc.format = 'nflow-2';
doc.name = 'subsystem_solver_zc';
doc.runtime = 5;
doc.sampleTime = 0.01;
doc.blocks = {...
  struct('id', 'u', 'type', 'step', 'x', 40, 'y', 160, 'rotation', 0, ...
    'params', struct('Time', 0.5)), ...
  struct('id', 'sub', 'type', 'subsystem', 'x', 220, 'y', 160, 'rotation', 0, ...
    'params', struct('name', 'Inner', 'subsystem', inner)), ...
  struct('id', 'i', 'type', 'integrator', 'x', 400, 'y', 160, 'rotation', 0, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'x', 580, 'y', 160, 'rotation', 0, ...
    'params', struct('VariableName', 'y_sub_zc', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
doc.connections = {...
  struct('from', 'u', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};

model = [tempname(), '.nflow'];
fid = fopen(model, 'wt');
fwrite(fid, jsonencode(doc));
fclose(fid);

% 3 * integral of a unit step from t = 0.5 over 5 s = 3 * 4.5.
solvers = {'ode1', 'ode4', 'ode45', 'variableStiff'};
for k = 1:numel(solvers)
  out = sim(model, 'Solver', solvers{k});
  asserts.istrue(isfield(out, 'y_sub_zc'));
  y = out.y_sub_zc;
  asserts.isapprox(y(end), 13.5, 1e-3, 1e-2);
end

% The default fixed-step path (no solver) keeps working too.
outFixed = sim(model);
asserts.isapprox(outFixed.y_sub_zc(end), 13.5, 1e-3, 1e-2);
rmfile(model);
%=============================================================================
