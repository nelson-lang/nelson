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
% <--ENGLISH IMPOSED-->
%=============================================================================
% An exported FMU's doStep must honor the communication step: a step larger than
% the model's internal rate is covered by several internal updates (inputs held
% constant = zero-order hold), not a single one. Regression: export an
% integrator with internal dt = 0.01, drive it with u = 1 at a communication
% step of 0.05 (5x the model rate); the state must still integrate to ~= t.
% Without sub-stepping, doStep would advance only one internal dt per call and
% the output would be ~5x too small.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'u', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('GotoTag', 'u', 'isExternalPort', true)), ...
  struct('id', 'i', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'u', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01; % model internal step
d.duration = 1.0;
d.variables = struct();
f = [work, filesep, 'Int.nflow'];
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
fmu = NFlow.exportfmu(f, [work, filesep, 'out']);
names = unzip(fmu, [work, filesep, 'chk']);
hasBin = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBin, 'no C toolchain: FMU exported source-only');
%=============================================================================
% Drive with u = 1 at a communication step of 0.05 (5 internal sub-steps each).
sim = struct();
sim.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'F', 'type', 'fmu', 'inputs', 1, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'ysub', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
sim.connections = {...
  struct('from', 'c', 'to', 'F', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'F', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
sim.sampleTime = 0.05; % communication step > model step
sim.duration = 1.0;
sim.solver = 'discrete';
sim.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(sim)));
assert_isequal(r.status, 'done');
y = evalin('base', 'ysub');
y = y(:);
% Sub-stepping covers the whole time: y(end) ~= 1 (integral of 1 over [0,1]).
% One-step-per-doStep would give ~0.2 (5x too small).
assert_istrue(y(end) > 0.9);
assert_istrue(y(end) < 1.05);
evalin('base', 'clear(''ysub'')');
%=============================================================================
