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
% coulombViscousFriction (niche): y = Gain*u + Offset*sign(u). Viscous slope
% plus a signed Coulomb offset; y(0) = 0 since sign(0) = 0. Row = {input, Gain,
% Offset, expected}. Compact (low-whitespace) form on purpose.
%=============================================================================
cases = {{2, 2, 3, 7}, {-2, 2, 3, -7}, {0, 2, 3, 0}, {5, 1, 0, 5}, ...
  {-1, 0, 4, -4}, {3, 0.5, 2, 3.5}};
for k = 1:numel(cases)
  c = cases{k}; u = c{1}; g = c{2}; o = c{3}; expected = c{4};
  vn = ['cvf_out', int2str(k)];
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', u)), struct('id', 'f', 'type', 'coulombViscousFriction', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', g, 'Offset', o)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', vn, 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {struct('from', 'c', 'to', 'f', 'fromIndex', 0, 'toIndex', 0), struct('from', 'f', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  y = evalin('base', vn); y = y(:)';
  assert_isequal(y(1), expected);
end
%=============================================================================
