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
% FMU export round-trip: export a model as an FMI 3.0 Co-Simulation FMU,
% then reimport it through nflow's own FMU block and co-simulate it. The
% exported FMU is only loadable when a C toolchain built its platform binary, so
% the test skips otherwise. y = 2 * 5 = 10 at every step.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');

% Build and export: constant(5) -> gain(2) -> external output y.
d = struct();
d.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', 2)), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'k', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.5; d.variables = struct();

modelFile = [tempname(), '.nflow'];
fid = fopen(modelFile, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
fmu = NFlow.exportfmu(modelFile, tempdir());

% Only a binary FMU is loadable; skip if the toolchain produced a source-only one.
co = tempname();
if isdir(co)
  rmdir(co, 's');
end
names = unzip(fmu, co);
hasBinary = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBinary, 'no C toolchain: FMU exported source-only, cannot load');

% Reimport: an nflow diagram with a FMU block loading the exported FMU.
d2 = struct();
d2.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_y', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 0.5; d2.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r.status, 'done');
y = evalin('base', 'rt_y'); y = y(:);
assert_istrue(numel(y) >= 1);
assert_istrue(max(abs(y - 10.0)) < 1e-9);
evalin('base', 'clear(''rt_y'')');
%=============================================================================
