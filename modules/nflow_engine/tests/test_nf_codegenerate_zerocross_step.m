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
% <--WEIGHT 64-->
%=============================================================================
% step declares the simulator's t - Time surface too: with Time off the
% sample grid the solvers stop exactly at the jump instead of integrating
% through it (y(2.0) = 2.0 - 0.37 exactly, not grid-quantized).
dstp = struct();
dstp.blocks = {...
  struct('id', 'st', 'type', 'step', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Time', 0.37)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfzs_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
dstp.connections = {...
  struct('from', 'st', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
dstp.sampleTime = 0.1;
dstp.duration = 2.0;
dstp.solver = 'ode4';
dstp.variables = struct();
rs = jsondecode(__nflow_simulate__(jsonencode(dstp)));
assert_isequal(rs.status, 'done');
ys = evalin('base', 'nfzs_y');
% ~2 - 0.37 up to the RK4 stage residual at the crossing step; the exact
% value is whatever the simulator computes; the compiled comparison below
% verifies the exact generated behavior.
assert_istrue(abs(ys(end) - (2.0 - 0.37)) < 0.02);
evalin('base', 'clear(''nfzs_y'')');
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(dstp, 'nfzs_num', 'C', {'y'}, {'nfzs_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(dstp, 'nfzs_num', 'rust', {'y'}, {'nfzs_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
