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
% A bounded integrator contributes its x-limit surfaces: min(t, 1.35) under
% ode4 (x' = 1, upper limit 1.35).
db = struct();
db.blocks = {...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'LowerSaturationLimit', -10, 'UpperSaturationLimit', 1.35)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfzb_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
db.connections = {...
  struct('from', 'one', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
db.sampleTime = 0.1;
db.duration = 2.0;
db.solver = 'ode4';
db.variables = struct();
rb = jsondecode(__nflow_simulate__(jsonencode(db)));
assert_isequal(rb.status, 'done');
yb = evalin('base', 'nfzb_y');
assert_istrue(abs(yb(11) - 1.0) < 1e-9); % t = 1.0
assert_istrue(abs(yb(end) - 1.35) < 1e-9); % held at the limit
evalin('base', 'clear(''nfzb_y'')');
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(db, 'nfzb_num', 'C', {'y'}, {'nfzb_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(db, 'nfzb_num', 'rust', {'y'}, {'nfzb_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
