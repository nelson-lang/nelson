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
% A fromWorkspace block referencing a base-workspace variable by name (no
% embedded times/values) is resolved (baked into times/values) by the simulator
% before it runs. Code generation must do the SAME -- both nflow_codegenerate and
% NFlow.exportfmu now call resolveWorkspaceInputs -- otherwise the generated code
% would emit a silent 0.0 while the simulation reads the live variable. The
% compiled FMU must reproduce the direct simulation exactly.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
tt = (0:0.25:1)';
assignin('base', 'nflow_fw_simin', [tt, [10; 20; 30; 40; 50]]);
fwParams = struct('VariableName', 'nflow_fw_simin', 'SampleTime', '0', ...
  'Interpolate', 'on', 'OutputAfterFinalValue', 'Holding final value');
%=============================================================================
% Direct simulation (reads the live variable, baked by resolveWorkspaceInputs).
dsim = struct();
dsim.blocks = {...
  struct('id', 'fw', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, 'params', fwParams), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'fwv_ys', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
dsim.connections = {struct('from', 'fw', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dsim.sampleTime = 0.25; dsim.duration = 1.0; dsim.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(dsim)));
assert_isequal(r.status, 'done');
ys = evalin('base', 'fwv_ys'); ys = ys(:)';
evalin('base', 'clear(''fwv_ys'')');
assert_isequal(ys(1:5), [10, 20, 30, 40, 50]); % the baked ramp
%=============================================================================
% Export to an FMU (the generator bakes the same variable) and co-simulate.
dexp = struct();
dexp.blocks = {...
  struct('id', 'fw', 'type', 'fromWorkspace', 'inputs', 0, 'outputs', 1, 'params', fwParams), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dexp.connections = {struct('from', 'fw', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
dexp.sampleTime = 0.25; dexp.duration = 1.0; dexp.variables = struct();
modelFile = [tempname(), '.nflow'];
fid = fopen(modelFile, 'wt'); fwrite(fid, jsonencode(dexp)); fclose(fid);
fmu = NFlow.exportfmu(modelFile, tempdir());

co = tempname();
if isdir(co)
  rmdir(co, 's');
end
names = unzip(fmu, co);
hasBinary = any(cellfun(@(s) endsWith(s, getdynlibext()), names));
skip_testsuite(~hasBinary, 'no C toolchain: FMU exported source-only, cannot load');

d2 = struct();
d2.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmu)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'fwv_yf', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.25; d2.duration = 1.0; d2.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r.status, 'done');
yf = evalin('base', 'fwv_yf'); yf = yf(:)';
evalin('base', 'clear(''fwv_yf'')');
% The FMU's baked table reproduces the simulation exactly.
n = min(numel(ys), numel(yf));
assert_istrue(max(abs(ys(1:n) - yf(1:n))) < 1e-12);
rmfile(modelFile);
evalin('base', 'clear(''nflow_fw_simin'')');
%=============================================================================
