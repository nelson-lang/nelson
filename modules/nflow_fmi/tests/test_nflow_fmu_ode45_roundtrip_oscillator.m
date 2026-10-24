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
osc = struct();
osc.solver = 'ode45';
osc.blocks = {...
  struct('id', 'i1', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'i2', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1)), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
osc.connections = {...
  struct('from', 'i2', 'to', 'i1', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i1', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'i2', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'i1', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
osc.sampleTime = 0.05; osc.duration = 3.0; osc.variables = struct();

modelFileO = [tempname(), '.nflow'];
fid = fopen(modelFileO, 'wt'); fwrite(fid, jsonencode(osc)); fclose(fid);
fmuO = NFlow.exportfmu(modelFileO, tempdir());
coO = tempname();
if isdir(coO)
  rmdir(coO, 's');
end
namesO = unzip(fmuO, coO);
hasBinaryO = any(cellfun(@(s) endsWith(s, getdynlibext()), namesO));
skip_testsuite(~hasBinaryO, 'no C toolchain: FMU exported source-only, cannot load');

d3 = struct();
d3.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuO)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_yosc', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d3.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.05; d3.duration = 3.0; d3.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r.status, 'done');
yo = evalin('base', 'rt_yosc'); yo = yo(:);
evalin('base', 'clear(''rt_yosc'')');
assert_istrue(numel(yo) >= 2);
assert_istrue(abs(yo(end) - cos(3.0)) < 1e-4);
mido = yo(21);
assert_istrue(abs(mido - cos(1.0)) < 1e-4);
rmfile(modelFileO);
%=============================================================================
