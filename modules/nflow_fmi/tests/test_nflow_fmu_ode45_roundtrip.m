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
% Variable-step codegen: a model integrated with the adaptive
% ode45 (Dormand-Prince 5(4)) solver exports to an FMU whose generated C carries
% the embedded adaptive stepper. Compiling and co-simulating that FMU must match
% the closed-form solution -- an algorithm-independent check that the emitted
% stepper is correct, not merely that it compiles. The exported FMU is only
% loadable when a C toolchain built its binary, so the test skips otherwise.
%
% x' = -x, x(0) = 1  ->  x(t) = e^{-t}. Integrator (IC=1) with -1 feedback.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
%=============================================================================
d = struct();
d.solver = 'ode45';
d.blocks = {...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1)), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'ig', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 1.0; d.variables = struct();

modelFile = [tempname(), '.nflow'];
fid = fopen(modelFile, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
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
    'params', struct('VariableName', 'rt_y45', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d2.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 1.0; d2.variables = struct();

r = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r.status, 'done');
y = evalin('base', 'rt_y45'); y = y(:);
evalin('base', 'clear(''rt_y45'')');
assert_istrue(numel(y) >= 2);
assert_istrue(abs(y(end) - exp(-1)) < 1e-5);
mid = y(6);
assert_istrue(abs(mid - exp(-0.5)) < 1e-5);
rmfile(modelFile);
%=============================================================================
