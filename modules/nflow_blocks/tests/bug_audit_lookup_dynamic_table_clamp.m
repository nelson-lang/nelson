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
% <-- Short Description -->
% lookupDynamic (Lookup Table Dynamic) indexed the table port (port 2) with
% the breakpoint indices 0..n-1 (n = breakpoint width) and read sigAt(table, i)
% with no width check. A saved model wiring a table vector NARROWER than the
% breakpoint vector made it read past the table region (out-of-bounds / garbage,
% no crash guarantee). The fix clamps the table index to the table width, so a
% mismatched model now yields a defined, in-range (clamped) output. This test
% builds such a mismatched model natively via __nflow_simulate__ and asserts the
% clamped value; before the fix the same inputs read OOB table slots (2026-08
% audit fix).
%=============================================================================
% x = 3.5 with xdat = [0 1 2 3 4] (width n = 5) selects interpolation interval
% i = 3 (between xdat(4)=3 and xdat(5)=4). ydat = [10 20 30] has width 3, so the
% table indices 3 and 4 are past the table region. The fix clamps both to the
% last table element (30), giving out = 30 + 0.5 * (30 - 30) = 30. Before the
% fix, sigAt(ydat, 3) and sigAt(ydat, 4) read past the 3-slot table (garbage).
evalin('base', 'clear(''y'')');
d = struct();
d.blocks = {...
  struct('id', 'x', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 3.5)), ...
  struct('id', 'xd', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [0 1 2 3 4])), ...
  struct('id', 'yd', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', [10 20 30])), ...
  struct('id', 'ld', 'type', 'lookupDynamic', 'inputs', 3, 'outputs', 1, 'params', struct()), ...
  struct('id', 'w', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, 'params', struct('VariableName', 'y', 'SaveFormat', 'Array'))};
d.connections = {...
  struct('from', 'x', 'to', 'ld', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'xd', 'to', 'ld', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'yd', 'to', 'ld', 'fromIndex', 0, 'toIndex', 2), ...
  struct('from', 'ld', 'to', 'w', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 0.2;
d.solver = 'discrete';
d.variables = struct();
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
y = evalin('base', 'y');
y = y(:)';
% The clamped output must be finite (a defined value, not OOB garbage / NaN)...
assert_istrue(all(isfinite(y)));
% ... equal to the last table element (clamp target)...
assert_istrue(abs(y(1) - 30) < 1e-9);
% ... and within the table range [min, max] = [10, 30].
assert_istrue(y(1) >= 10 - 1e-9 && y(1) <= 30 + 1e-9);
evalin('base', 'clear(''y'')');
%=============================================================================
% Second mismatched case: clip-above path also indexes the table by n-1 = 4,
% again past a width-3 table. x = 10 (>= xdat(end)) -> lookupTableAt(ydat, 4)
% clamps to ydat(3) = 30.
evalin('base', 'clear(''y'')');
d2 = d;
d2.blocks{1}.params.Value = 10;
r2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(r2.status, 'done');
y2 = evalin('base', 'y');
y2 = y2(:)';
assert_istrue(all(isfinite(y2)));
assert_istrue(abs(y2(1) - 30) < 1e-9);
evalin('base', 'clear(''y'')');
%=============================================================================
