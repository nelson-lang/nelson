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
% <--TIMEOUT 600-->
%=============================================================================
% Enable gate under ode4 (RK4 discrete co-sim): a CONTINUOUS integrator inside
% an enable subsystem now lowers onto the packed RK4 state instead of forcing
% the Euler fallback. While the gate is closed the member's packed derivative
% is forced to zero (freeze), and the boundary hold latches once per major step
% (the report pass) while tracking the live member value when enabled. Model:
% constant 1 -> [enable: integrator] -> out, gate = clock < 0.575. The gate
% edge sits BETWEEN samples (0.575, not on the 0.05 grid) so the enabled sample
% set is unambiguous: the integrator ramps x = t through t = 0.55 then holds.
% (A gate driven by a fast CONTINUOUS signal crossing mid-RK-step carries an
% O(dt) edge difference vs the simulator -- a discretisation gap that vanishes
% as dt shrinks, like the acausal ZOH gap -- so this leg uses a clock gate.)
%=============================================================================
innerI = struct();
innerI.blocks = {...
  struct('id', 'in1', 'type', 'labelSource', 'inputs', 0, 'outputs', 1, ...
    'params', struct('label', 'in1')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, 'params', struct()), ...
  struct('id', 'out1', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('label', 'out1'))};
innerI.connections = {...
  struct('from', 'in1', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out1', 'fromIndex', 0, 'toIndex', 0)};
innerI.externalInputs = {struct('id', 'in1', 'name', 'in1')};
innerI.externalOutputs = {struct('id', 'out1', 'name', 'out1')};
di = struct();
di.blocks = {...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'ctl', 'type', 'compareToConstant', 'inputs', 1, 'outputs', 1, ...
    'params', struct('relop', 'lt', 'const', 0.575)), ...
  struct('id', 'sub', 'type', 'subsystem', 'inputs', 2, 'outputs', 1, ...
    'params', struct('subsystem', innerI, 'controlPort', struct('kind', 'enable'))), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfei_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
di.connections = {...
  struct('from', 'one', 'to', 'sub', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'clk', 'to', 'ctl', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ctl', 'to', 'sub', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'sub', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'sub', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
di.sampleTime = 0.05;
di.duration = 1.0;
di.solver = 'ode4';
di.variables = struct();
r5 = jsondecode(__nflow_simulate__(jsonencode(di)));
assert_isequal(r5.status, 'done');
yi = evalin('base', 'nfei_y');
assert_istrue(yi(11) > 0.49); % ramped while enabled (t ~ 0.5)
% The gate is a level: it is sampled once per major step, so the step during
% which it was still open ([0.55, 0.60]) is integrated WHOLE and the state
% freezes at 0.60. It used to be read at RK4 stage time instead, which kept a
% fraction of that step - between one sixth and five sixths of it - and left the
% state somewhere in between, wherever the change happened to fall among the
% stages.
assert_istrue(abs(yi(13) - 0.60) < 1e-9);
assert_istrue(abs(yi(end) - yi(13)) < 1e-9); % frozen once disabled
evalin('base', 'clear(''nfei_y'')');
% Structural: the integrator joins the packed RK4 state and its derivative is
% zeroed when the enable gate is closed.
fi = [tempname(), '.nflow'];
[~, fistem] = fileparts(fi);
fid = fopen(fi, 'wt');
fwrite(fid, jsonencode(di));
fclose(fid);
nflow_codegenerate(fi, tempdir(), false, 'C');
srcI = fileread([tempdir(), fistem, '/model.c']);
assert_istrue(isempty(strfind(srcI, 'unknown:')));
assert_istrue(~isempty(strfind(srcI, '__nflow_rhs'))); % on the RK4 path, not Euler
assert_istrue(~isempty(strfind(srcI, '__xdot[0] = 0.0;'))); % freeze mask
nflow_codegenerate(fi, tempdir(), false, 'rust');
srcIR = fileread([tempdir(), fistem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcIR, '__xdot[0] = 0.0;')));
rmfile(fi);
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(di, 'nfei_num', 'C', {'y'}, {'nfei_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(di, 'nfei_num', 'rust', {'y'}, {'nfei_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
