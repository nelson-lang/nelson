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
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');

% PID under the ode4 fixed-step RK4 solver: exporting the model runs the unified
% RK4 codegen seam: the PID integral is a continuous state, so co-simulating the
% exported FMU must match a direct ode4 simulation of the same model -- a
% numerical check that the PID RK4 codegen is correct, through the compiled
% binary. D is 0 here on purpose: the derivative term is a DISCRETE term
% evaluated at the co-simulation's own step, so keeping it would compare two
% different integration schemes rather than the integral seam this test is for.
% The derivative term has its own test (test_nflow_pid_derivative_under_solver).
dpidRef = struct();
dpidRef.blocks = {...
  struct('id', 'r', 'type', 'ramp', 'inputs', 0, 'outputs', 1, 'params', struct('slope', 2)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'dv', 'type', 'pid', 'inputs', 1, 'outputs', 1, ...
    'params', struct('P', 2, 'I', 3, 'D', 0)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'rt_pref', 'SaveFormat', 'Array', ...
    'Decimation', 1, 'MaxDataPoints', 'inf'))};
dpidRef.connections = {...
  struct('from', 'r', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'dv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'dv', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dpidRef.sampleTime = 0.1; dpidRef.duration = 1.0; dpidRef.solver = 'ode4';
dpidRef.variables = struct();
rp = jsondecode(__nflow_simulate__(jsonencode(dpidRef)));
assert_isequal(rp.status, 'done');
pref = evalin('base', 'rt_pref'); pref = pref(:);
evalin('base', 'clear(''rt_pref'')');

% Same model with an external output, exported to an FMU.
dpidExp = dpidRef;
dpidExp.blocks{4} = struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
  'params', struct('GotoTag', 'y', 'isExternalPort', true));
dpidExp.connections{3} = struct('from', 'dv', 'to', 'out', 'fromIndex', 0, 'toIndex', 0);
modelFileP = [tempname(), '.nflow'];
fid = fopen(modelFileP, 'wt'); fwrite(fid, jsonencode(dpidExp)); fclose(fid);
fmuP = NFlow.exportfmu(modelFileP, tempdir());
coP = tempname();
if isdir(coP)
  rmdir(coP, 's');
end
namesP = unzip(fmuP, coP);
hasBinaryP = any(cellfun(@(s) endsWith(s, getdynlibext()), namesP));
if hasBinaryP
  dpidCo = struct();
  dpidCo.blocks = {...
    struct('id', 'f1', 'type', 'fmu', 'inputs', 0, 'outputs', 1, 'params', struct('path', fmuP)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
      'params', struct('VariableName', 'rt_pfmu', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  dpidCo.connections = {struct('from', 'f1', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
  dpidCo.sampleTime = 0.1; dpidCo.duration = 1.0; dpidCo.variables = struct();
  rc = jsondecode(__nflow_simulate__(jsonencode(dpidCo)));
  assert_isequal(rc.status, 'done');
  pfmu = evalin('base', 'rt_pfmu'); pfmu = pfmu(:);
  evalin('base', 'clear(''rt_pfmu'')');
  % The exported FMU (compiled RK4 codegen with PID in the seam) matches the
  % direct ode4 simulation at the shared samples.
  n = min(numel(pref), numel(pfmu));
  assert_istrue(n >= 1);
  assert_istrue(max(abs(pref(1:n) - pfmu(1:n))) < 1e-6);
end
%=============================================================================
