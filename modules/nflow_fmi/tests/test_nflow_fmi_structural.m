%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% FMI 3.0 structural parameters: array dimensions bound to a
% <Dimension valueReference> resize when the bound structural parameter is
% overridden (applied in Configuration Mode before initialization).
% The StateSpace example declares m/n/r structural parameters and binds the
% u (input, m) and y (output, r) array sizes to them.
%=============================================================================
fmu = [modulepath('nflow_fmi', 'root'), '/examples/StateSpace.fmu'];
skip_testsuite(~isfile(fmu), ['StateSpace.fmu not found: ', fmu]);
%=============================================================================
% Inspection surfaces the dimension bindings and the structural parameters
% (with their integer default sizes as startValue).
j = jsondecode(__nflow_fmi_inspect_json__(fmu));
assert_isequal(j.success, true);
vars = j.variables;
if ~iscell(vars)
  vars = num2cell(vars);
end
names = {};
for i = 1:numel(vars)
  names{end + 1} = vars{i}.name;
end
iu = find(strcmp(names, 'u'));
ir = find(strcmp(names, 'r'));
assert_istrue(~isempty(iu) && ~isempty(ir));
assert_isequal(vars{ir}.causality, 'structuralParameter');
assert_isequal(vars{ir}.startValue, 3);
% u has one dimension bound to the VR of the structural parameter m.
assert_istrue(isfield(vars{iu}, 'dimensions'));
du = vars{iu}.dimensions;
if iscell(du)
  du = du{1};
else
  du = du(1);
end
assert_isequal(du.start, 3);
assert_istrue(isfield(du, 'vr'));
blk = fmuToBlock(fmu, 'Name', 'StateSpaceFmu');
assert_isequal(blk.defaultParams.inputWidths, 3);
assert_isequal(blk.defaultParams.outputWidths, 3);
%=============================================================================
% Default sizes: u is a width-3 input port, y a width-3 output port. This
% also exercises array-valued FMU INPUT ports end to end (one value
% reference carrying three elements).
base = struct('name', 'StateSpaceFmu', 'sampleTime', 0.05, 'duration', 0.5, ...
  'solver', 'discrete');
cst = struct('id', 'c1', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
  'params', struct('Value', [1, 2, 3]));
fmub = struct('id', 'fmu1', 'type', 'fmu', 'inputs', 1, 'outputs', 1, ...
  'params', struct('path', fmu, 'outputWidths', 3));
scope = struct('id', 'sc1', 'type', 'scope', 'inputs', 1, 'outputs', 0, 'params', struct());
base.blocks = {cst, fmub, scope};
base.connections = {...
  struct('from', 'c1', 'fromIndex', 0, 'to', 'fmu1', 'toIndex', 0), ...
  struct('from', 'fmu1', 'fromIndex', 0, 'to', 'sc1', 'toIndex', 0)};
res = jsondecode(__nflow_simulate__(jsonencode(base)));
assert_isequal(res.status, 'done');
assert_isequal(res.signalDims.fmu1, [3, 1]);
y3 = res.signals.fmu1;
assert_istrue(all(isfinite(y3(:))));
%=============================================================================
% Overriding the structural parameter r resizes the y output port to 2
% (the FMU is reconfigured in Configuration Mode; the declared outputWidths
% matches the effective size).
d2 = base;
d2.blocks{2}.params.outputWidths = 2;
d2.blocks{2}.params.parameterValues = struct('r', 2);
res2 = jsondecode(__nflow_simulate__(jsonencode(d2)));
assert_isequal(res2.status, 'done');
assert_isequal(res2.signalDims.fmu1, [2, 1]);
y2 = res2.signals.fmu1;
assert_istrue(all(isfinite(y2(:))));
%=============================================================================
