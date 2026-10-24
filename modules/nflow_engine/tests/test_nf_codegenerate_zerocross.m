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
% Zero-crossing lowering: under ode4/ode45 the generated stepper
% now localizes each surface sign change by bisection with fixed-RK4 trial
% steps (the simulator's localizeRoot: 60 iterations / 1e-12, the state left
% at the last probe, the time at the midpoint) and restarts the advance at
% the crossing. Surfaces: bounded integrator (x-min / x-max), switch
% (control - Threshold), saturation and deadZone (u - lower / u - upper).
%
% Discriminating model: y = integral of switch(t >= 0.35 ? 1 : 0). The exact
% trajectory is max(0, t - 0.35); an RK4 step straddling the flip without
% localization lands visibly off (stage-weighted average), so this test is
% RED before the lowering and GREEN after.
%=============================================================================
d = struct();
d.blocks = {...
  struct('id', 'clk', 'type', 'clock', 'inputs', 0, 'outputs', 1, 'params', struct()), ...
  struct('id', 'one', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'zer', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 0)), ...
  struct('id', 'sw', 'type', 'switch', 'inputs', 3, 'outputs', 1, ...
    'params', struct('Threshold', 0.35, 'Condition', 'ge')), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfzc_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'one', 'to', 'sw', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'clk', 'to', 'sw', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'zer', 'to', 'sw', 'fromIndex', 0, 'toIndex', 2), ...
  struct('from', 'sw', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1;
d.duration = 1.0;
d.solver = 'ode4';
d.variables = struct();
%=============================================================================
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
ysim = evalin('base', 'nfzc_y');
% Exact trajectory max(0, t - 0.35) at the samples (the simulator localizes
% the flip; rows start at t = 0).
assert_istrue(abs(ysim(4)) < 1e-9); % t = 0.3
assert_istrue(abs(ysim(5) - 0.05) < 1e-9); % t = 0.4
assert_istrue(abs(ysim(11) - 0.65) < 1e-9); % t = 1.0
evalin('base', 'clear(''nfzc_y'')');
%=============================================================================
f = [tempname(), '.nflow'];
[~, fstem] = fileparts(f);
fid = fopen(f, 'wt');
fwrite(fid, jsonencode(d));
fclose(fid);
nflow_codegenerate(f, tempdir(), false, 'C');
src = fileread([tempdir(), fstem, '/model.c']);
assert_istrue(isempty(strfind(src, 'unknown:')));
% The crossing machinery: extended rhs, surface writes, sub-RK4 bisection.
assert_istrue(~isempty(strfind(src, 'double* __g')));
assert_istrue(~isempty(strfind(src, '__nflow_rk4sub')));
assert_istrue(~isempty(strfind(src, '__g[0] = ')));
nflow_codegenerate(f, tempdir(), false, 'rust');
srcR = fileread([tempdir(), fstem, '/src/lib.rs']);
assert_istrue(~isempty(strfind(srcR, '__g: Option<&mut [f64]>')));
assert_istrue(~isempty(strfind(srcR, '__nflow_rk4sub')));
rmfile(f);
%=============================================================================
% Compiled numeric equivalence vs the simulator (both targets, ode4): the
% generated trajectory must track the localized flip, not the straddled RK4.
if __nflow_codegen_toolchain__('C')
  nflow_codegen_check_numeric(d, 'nfzc_num', 'C', {'y'}, {'nfzc_y'}, 5e-6, 1e-9);
  if __nflow_codegen_toolchain__('rust')
    nflow_codegen_check_numeric(d, 'nfzc_num', 'rust', {'y'}, {'nfzc_y'}, 5e-6, 1e-9);
  end
end
%=============================================================================
