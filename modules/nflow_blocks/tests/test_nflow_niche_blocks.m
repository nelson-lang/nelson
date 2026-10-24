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
%=============================================================================
% Niche blocks: width (element count of a signal), signalConversion (identity
% pass-through), enumeratedConstant (fixed enumeration value). Compact form.
%=============================================================================
% width: a vector constant of width N -> scalar N.
for N = [1 3 5]
  v = (1:N) * 10;
  vn = ['w', int2str(N)];
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', v)), struct('id', 'wd', 'type', 'width', 'inputs', 1, 'outputs', 1, 'params', struct()), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', vn, 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'c', 'to', 'wd', 'fromIndex', 0, 'toIndex', 0), struct('from', 'wd', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', vn); y = y(:)';
  assert_isequal(y(1), N);
end
%=============================================================================
% signalConversion: identity pass-through (scalar and vector element 1).
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 5)), struct('id', 's', 'type', 'signalConversion', 'inputs', 1, 'outputs', 1, 'params', struct()), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'sc', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'c', 'to', 's', 'fromIndex', 0, 'toIndex', 0), struct('from', 's', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
res = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(res.status, 'done');
sc = evalin('base', 'sc'); sc = sc(:)';
assert_isequal(sc(1), 5);
%=============================================================================
% enumeratedConstant: outputs Value at every step.
d = struct();
d.blocks = {struct('id', 'e', 'type', 'enumeratedConstant', 'inputs', 0, 'outputs', 1, 'params', struct('EnumClass', 'Color', 'Value', 7)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'en', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {struct('from', 'e', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
res = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(res.status, 'done');
en = evalin('base', 'en'); en = en(:)';
assert_isequal(en(1), 7);
%=============================================================================
