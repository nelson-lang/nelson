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
% prelookup / interpolationPrelookup / lookupDynamic. Written in a
% compact (low-whitespace) form.
%=============================================================================
% Case A: prelookup(bp=[0 1 2 3 4]) at u=2.5 gives k=2 on its first port and
% f=0.5 on its second; feeding both to interpolationPrelookup(Table=
% [0 1 4 9 16]) gives 4 + 0.5*(9-4) = 6.5, matching a lookup1D at the same
% point. The pair used to pass the index and the fraction through ONE two-wide
% wire, which no other block reads that way.
evalin('base', 'clear(''y'')');
d = struct();
d.blocks = {struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2.5)), struct('id', 'pl', 'type', 'prelookup', 'inputs', 1, 'outputs', 2, 'params', struct('BreakpointsForDimension1', [0 1 2 3 4])), struct('id', 'ip', 'type', 'interpolationPrelookup', 'inputs', 2, 'outputs', 1, 'params', struct('Table', [0 1 4 9 16])), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'y', 'SaveFormat', 'Array'))};
d.connections = {struct('from', 'c', 'to', 'pl', 'fromIndex', 0, 'toIndex', 0), struct('from', 'pl', 'to', 'ip', 'fromIndex', 0, 'toIndex', 0), struct('from', 'pl', 'to', 'ip', 'fromIndex', 1, 'toIndex', 1), struct('from', 'ip', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.duration = 0.2; d.solver = 'discrete'; d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'y'); y = y(:)';
assert_istrue(abs(y(1) - 6.5) < 1e-9);
evalin('base', 'clear(''y'')');

% Case B: lookupDynamic with x=2.5, xdat=[0 1 2 3 4], ydat=[0 1 4 9 16] -> 6.5.
evalin('base', 'clear(''y'')');
d2 = struct();
d2.blocks = {struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 2.5)), struct('id', 'xd', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [0 1 2 3 4])), struct('id', 'yd', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [0 1 4 9 16])), struct('id', 'ld', 'type', 'lookupDynamic', 'inputs', 3, 'outputs', 1, 'params', struct()), struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'y', 'SaveFormat', 'Array'))};
d2.connections = {struct('from', 'x', 'to', 'ld', 'fromIndex', 0, 'toIndex', 0), struct('from', 'xd', 'to', 'ld', 'fromIndex', 0, 'toIndex', 1), struct('from', 'yd', 'to', 'ld', 'fromIndex', 0, 'toIndex', 2), struct('from', 'ld', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d2.sampleTime = 0.1; d2.duration = 0.2; d2.solver = 'discrete'; d2.variables = struct();
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'y'); y2 = y2(:)';
assert_istrue(abs(y2(1) - 6.5) < 1e-9);
evalin('base', 'clear(''y'')');

% Case C: clip above range. prelookup at u=9 -> [k=3, f=1]; interp -> 9 + (16-9) = 16.
evalin('base', 'clear(''y'')');
d3 = d;
d3.blocks{1}.params.Value = 9;
r3 = jsondecode(__nflow_simulate__(jsonencode(d3)));
assert_isequal(r3.status, 'done');
y3 = evalin('base', 'y'); y3 = y3(:)';
assert_istrue(abs(y3(1) - 16) < 1e-9);
evalin('base', 'clear(''y'')');
%=============================================================================
