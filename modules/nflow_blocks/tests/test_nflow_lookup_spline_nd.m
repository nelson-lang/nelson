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
% lookup2D / lookupND separable tensor-product spline (Cubic / Akima). A table
% T(i,...) = bp1(i)^3 (independent of the other axes) is reproduced exactly by
% the not-a-knot cubic spline along dim 1, so the multi-D spline returns 2.5^3 =
% 15.625 at (2.5, ...). Compact (low-whitespace) form.
%=============================================================================
bp1 = [0 1 2 3 4]; bp2 = [0 1 2 3]; R = numel(bp1); C = numel(bp2);
T = zeros(1, R * C);
for j = 1:C
  for i = 1:R
    T(i + (j - 1) * R) = bp1(i) ^ 3;
  end
end
% lookup2D cubic spline at (2.5, 1.5) and at breakpoints.
cases2 = {{2.5, 1.5, 15.625}, {3.0, 2.0, 27}, {1.5, 0.5, 3.375}};
for k = 1:numel(cases2)
  c = cases2{k}; u1 = c{1}; u2 = c{2}; exq = c{3}; vn = sprintf('s2_%d', k);
  evalin('base', ['clear(''' vn ''')']);
  d = struct();
  d.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', u1)), struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', u2)), struct('id', 'l', 'type', 'lookup2D', 'inputs', 2, 'outputs', 1, 'params', struct('BreakpointsForDimension1', bp1, 'BreakpointsForDimension2', bp2, 'Table', T, 'InterpMethod', 'Cubic spline', 'ExtrapMethod', 'Cubic spline')), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', vn, 'SaveFormat', 'Array'))};
  d.connections = {struct('from', 'a', 'to', 'l', 'fromIndex', 0, 'toIndex', 0), struct('from', 'b', 'to', 'l', 'fromIndex', 0, 'toIndex', 1), struct('from', 'l', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d))); assert_isequal(r.status, 'done');
  y = evalin('base', vn); assert_istrue(abs(y(1) - exq) < 1e-6);
end
% lookupND (N=3) cubic spline; table independent of dims 2 and 3.
bp3 = [0 1]; D = numel(bp3); T3 = zeros(1, R * C * D);
for kk = 1:D
  for j = 1:C
    for i = 1:R
      T3(i + (j - 1) * R + (kk - 1) * R * C) = bp1(i) ^ 3;
    end
  end
end
evalin('base', 'clear(''s3'')');
d3 = struct();
d3.blocks = {struct('id', 'a', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2.5)), struct('id', 'b', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1.5)), struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 0.5)), struct('id', 'l', 'type', 'lookupND', 'inputs', 3, 'outputs', 1, 'params', struct('NumberOfTableDimensions', 3, 'BreakpointsForDimension1', bp1, 'BreakpointsForDimension2', bp2, 'BreakpointsForDimension3', bp3, 'Table', T3, 'InterpMethod', 'Cubic spline', 'ExtrapMethod', 'Cubic spline')), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 's3', 'SaveFormat', 'Array'))};
d3.connections = {struct('from', 'a', 'to', 'l', 'fromIndex', 0, 'toIndex', 0), struct('from', 'b', 'to', 'l', 'fromIndex', 0, 'toIndex', 1), struct('from', 'c', 'to', 'l', 'fromIndex', 0, 'toIndex', 2), struct('from', 'l', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d3.sampleTime = 0.1; d3.duration = 0.1; d3.solver = 'discrete'; d3.variables = struct();
r3 = jsondecode(__nflow_simulate__(jsonencode(d3))); assert_isequal(r3.status, 'done');
s3 = evalin('base', 's3'); assert_istrue(abs(s3(1) - 15.625) < 1e-6);
%=============================================================================
