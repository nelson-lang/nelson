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
sat = struct();
sat.solver = 'ode45';
sat.blocks = {...
  struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'UpperSaturationLimit', 1)), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
sat.connections = {...
  struct('from', 'c', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
sat.sampleTime = 0.1; sat.duration = 1.0; sat.variables = struct();

modelFileS = [tempname(), '.nflow'];
fid = fopen(modelFileS, 'wt'); fwrite(fid, jsonencode(sat)); fclose(fid);
fmuS = NFlow.exportfmu(modelFileS, tempdir());
coS = tempname();
if isdir(coS)
  rmdir(coS, 's');
end
namesS = unzip(fmuS, coS);
hasBinaryS = any(cellfun(@(s) endsWith(s, getdynlibext()), namesS));
skip_testsuite(~hasBinaryS, 'no C toolchain: FMU exported source-only, cannot load');

d4 = struct();
d4.blocks = {...
  struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuS)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_ysat', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d4.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d4.sampleTime = 0.1; d4.duration = 1.0; d4.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d4)));
assert_isequal(r.status, 'done');
ysat = evalin('base', 'rt_ysat'); ysat = ysat(:);
evalin('base', 'clear(''rt_ysat'')');
assert_istrue(abs(ysat(end) - 1.0) < 1e-9);
assert_istrue(max(ysat) <= 1.0 + 1e-9);
assert_istrue(abs(ysat(3) - 0.4) < 1e-6);
rmfile(modelFileS);
%=============================================================================
