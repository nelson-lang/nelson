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
% FMU export must drive stateful blocks from external inputs. Regression for a
% codegen bug where an integrator's default saturation limits ("-inf"/"inf")
% were resolved to a +/-inf number and then clamped to 0, generating
% fmin(fmax(state + u*dt, 0), 0) -- which pins the state to 0, so the exported
% FMU ignored its input and always output zero. A pure input -> integrator ->
% output model, driven with u = 1, must integrate to ~= t.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% Model: external input u -> integrator (unbounded) -> external output y.
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
d.sampleTime = 0.01;
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
% Drive the exported FMU with u = 1 through an fmu block; y = integral(1) ~= t.
sim = struct();
sim.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'F', 'type', 'fmu', 'inputs', 1, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'yint', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
sim.connections = {...
  struct('from', 'c', 'to', 'F', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'F', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
sim.sampleTime = 0.01;
sim.duration = 1.0;
sim.solver = 'discrete';
sim.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(sim)));
assert_isequal(r.status, 'done');
y = evalin('base', 'yint');
y = y(:);
% The state must actually integrate the input (not be pinned to 0).
assert_istrue(y(end) > 0.9); % integral of 1 over [0, 1] ~= 0.99 (discrete)
assert_istrue(y(end) < 1.05);
evalin('base', 'clear(''yint'')');
%=============================================================================
