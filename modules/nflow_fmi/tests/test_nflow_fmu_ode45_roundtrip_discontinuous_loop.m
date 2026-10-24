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
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
loopm = struct();
loopm.solver = 'ode45';
loopm.blocks = {...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 2)), ...
  struct('id', 'sat', 'type', 'saturation', 'inputs', 1, 'outputs', 1, ...
    'params', struct('UpperLimit', 1, 'LowerLimit', -1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1))};
loopm.connections = {...
  struct('from', 'ig', 'to', 'sat', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sat', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0)};
loopm.sampleTime = 0.1; loopm.duration = 3.0; loopm.runtime = 3.0; loopm.variables = struct();

dsim = loopm;
dsim.blocks{end + 1} = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
  'params', struct('VariableName', 'rt_ysimL', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
dsim.connections{end + 1} = struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0);
r = jsondecode(__nflow_simulate__(jsonencode(dsim)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'rt_ysimL'); ysim = ysim(:);
evalin('base', 'clear(''rt_ysimL'')');

dexp = loopm;
dexp.blocks{end + 1} = struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
  'params', struct('GotoTag', 'y', 'isExternalPort', true));
dexp.connections{end + 1} = struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0);
modelFileL = [tempname(), '.nflow'];
fid = fopen(modelFileL, 'wt'); fwrite(fid, jsonencode(dexp)); fclose(fid);
fmuL = NFlow.exportfmu(modelFileL, tempdir());
coL = tempname();
if isdir(coL)
  rmdir(coL, 's');
end
namesL = unzip(fmuL, coL);
hasBinaryL = any(cellfun(@(s) endsWith(s, getdynlibext()), namesL));
skip_testsuite(~hasBinaryL, 'no C toolchain: FMU exported source-only, cannot load');

d5 = struct();
d5.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuL)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_yfmuL', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d5.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d5.sampleTime = 0.1; d5.duration = 3.0; d5.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d5)));
assert_isequal(r.status, 'done');
yfmu = evalin('base', 'rt_yfmuL'); yfmu = yfmu(:);
evalin('base', 'clear(''rt_yfmuL'')');

nn = min(numel(ysim), numel(yfmu));
assert_istrue(nn >= 2);
assert_istrue(max(abs(ysim(1:nn) - yfmu(1:nn))) < 1e-9);
assert_istrue(abs(yfmu(end) - exp(-2)) < 1e-5);
rmfile(modelFileL);
%=============================================================================
