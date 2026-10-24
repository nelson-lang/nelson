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
% lookup1D: Cubic spline (not-a-knot) interpolation + out-of-range
% diagnostics. The not-a-knot spline reproduces a cubic exactly, so the spline
% through y = x^3 on [0..4] returns x^3 at any point and its cubic extrapolation.
% Compact (low-whitespace) form on purpose.
%=============================================================================
bp = [0 1 2 3 4]; tb = [0 1 8 27 64];
% Row: {u, InterpMethod, ExtrapMethod, expected}. Akima passes through the
% breakpoints exactly and reproduces linear data; here it is checked at a
% breakpoint (u=2 -> 8) and against a linear table further below.
cases = {{2.5, 'Cubic spline', 'Clip', 15.625}, {1.5, 'Cubic spline', 'Clip', 3.375}, {5.0, 'Cubic spline', 'Cubic spline', 125}, {2.5, 'Linear point-slope', 'Linear', 17.5}, {2.0, 'Akima spline', 'Clip', 8}, {3.0, 'Akima spline', 'Clip', 27}};
for k = 1:numel(cases)
  c = cases{k}; u = c{1}; interp = c{2}; extrap = c{3}; exq = c{4};
  vn = sprintf('sp%d', k); evalin('base', ['clear(''' vn ''')']);
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', u)), struct('id', 'l', 'type', 'lookup1D', 'inputs', 1, 'outputs', 1, 'params', struct('BreakpointsForDimension1', bp, 'Table', tb, 'InterpMethod', interp, 'ExtrapMethod', extrap)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', vn, 'SaveFormat', 'Array'))};
  d.connections = {struct('from', 'c', 'to', 'l', 'fromIndex', 0, 'toIndex', 0), struct('from', 'l', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d))); assert_isequal(r.status, 'done');
  y = evalin('base', vn); assert_istrue(abs(y(1) - exq) < 1e-6);
end
% Akima reproduces a linear table exactly.
evalin('base', 'clear(''akl'')');
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2.5)), struct('id', 'l', 'type', 'lookup1D', 'inputs', 1, 'outputs', 1, 'params', struct('BreakpointsForDimension1', [0 1 2 3 4], 'Table', [0 2 4 6 8], 'InterpMethod', 'Akima spline', 'ExtrapMethod', 'Akima spline')), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'akl', 'SaveFormat', 'Array'))};
d.connections = {struct('from', 'c', 'to', 'l', 'fromIndex', 0, 'toIndex', 0), struct('from', 'l', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d))); assert_isequal(r.status, 'done');
akl = evalin('base', 'akl'); assert_istrue(abs(akl(1) - 5) < 1e-9);
%=============================================================================
% Out-of-range diagnostic: Error aborts the run naming the block; None stays
% silent (clipped); an in-range Error does not fire.
%=============================================================================
dg = {{5.0, 'Error', 'error'}, {5.0, 'None', 'done'}, {5.0, 'Warning', 'done'}, {2.0, 'Error', 'done'}};
for k = 1:numel(dg)
  c = dg{k}; u = c{1}; act = c{2}; st = c{3};
  d = struct();
  d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', u)), struct('id', 'l', 'type', 'lookup1D', 'inputs', 1, 'outputs', 1, 'params', struct('BreakpointsForDimension1', bp, 'Table', tb, 'InterpMethod', 'Linear point-slope', 'ExtrapMethod', 'Clip', 'DiagnosticForOutOfRangeInput', act)), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'dgo', 'SaveFormat', 'Array'))};
  d.connections = {struct('from', 'c', 'to', 'l', 'fromIndex', 0, 'toIndex', 0), struct('from', 'l', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1; d.duration = 0.1; d.solver = 'discrete'; d.variables = struct();
  r = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(r.status, st);
end
%=============================================================================
