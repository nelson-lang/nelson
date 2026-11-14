%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 12-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Code generation of a comparator-driven PWM switched converter: a sawtooth
% carrier and a relational comparator (carrier < duty) gate the ideal high-side
% switch of a buck island. The generated fixed-step program (C and Rust) must
% emit the comparator and the switch branch and, when a C toolchain is present,
% compile, run and average the output to Vout = D * Vin = 4.8 V. (The generated
% program integrates the island through its embedded fixed-step solver and gates
% the switch per step, so it is compared to its own physical steady state, not
% sample-by-sample to the event-aware IDAS reference.)
%=============================================================================
d = struct();
d.blocks = { ...
  struct('id', 'carrier', 'type', 'repeatingSequenceInterpolated', 'inputs', 0, ...
    'outputs', 1, 'params', struct('TimeValues', [0, 1e-4], 'OutValues', [0, 1])), ...
  struct('id', 'duty', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
    'params', struct('Value', 0.4)), ...
  struct('id', 'pwm', 'type', 'relationalOperator', 'inputs', 2, 'outputs', 1, ...
    'params', struct('Operator', 'lt')), ...
  struct('id', 'Vin', 'type', 'ConstantVoltage', 'params', struct('V', 12)), ...
  struct('id', 'SW', 'type', 'IdealSwitch', 'params', struct()), ...
  struct('id', 'D1', 'type', 'IdealDiode', 'params', struct('Ron', 0.001, 'Goff', 1e-6, 'Vknee', 0)), ...
  struct('id', 'L1', 'type', 'Inductor', 'params', struct('L', 0.001, 'i0', 0)), ...
  struct('id', 'C1', 'type', 'Capacitor', 'params', struct('C', 1e-4, 'v0', 0)), ...
  struct('id', 'R1', 'type', 'Resistor', 'params', struct('R', 5)), ...
  struct('id', 'VS', 'type', 'VoltageSensor', 'params', struct()), ...
  struct('id', 'G', 'type', 'Ground', 'params', struct()), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'vout', 'isExternalPort', true)), ...
  struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfpwm_y', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = { ...
  struct('from', 'carrier', 'to', 'pwm', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'duty', 'to', 'pwm', 'fromIndex', 0, 'toIndex', 1), ...
  struct('from', 'pwm', 'to', 'SW', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'VS', 'to', 'y', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'VS', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
d.physicalWires = { ...
  struct('pins', {{'Vin.p', 'SW.p'}}), ...
  struct('pins', {{'SW.n', 'D1.n', 'L1.p'}}), ...
  struct('pins', {{'L1.n', 'C1.p', 'R1.p', 'VS.p'}}), ...
  struct('pins', {{'Vin.n', 'D1.p', 'C1.n', 'R1.n', 'VS.n', 'G.p'}})};
d.sampleTime = 2e-6;
d.duration = 0.01;
d.variables = struct();
d.solver = 'ode4';
dj = NFlow.internal.acausalExpand(jsonencode(d));

% --- Generation: both targets emit the comparator and the switch branch ---
model = [tempdir(), 'nfpwmbuck.nflow'];
fid = fopen(model, 'wt'); fwrite(fid, dj); fclose(fid);
nflow_codegenerate(model, tempdir(), false, 'C');
cs = fileread([tempdir(), 'nfpwmbuck/model.c']);
assert_istrue(~contains(cs, 'unknown:'));               % every block is lowered
assert_istrue(contains(cs, '> 0.5'));                   % switch branch gate
nflow_codegenerate(model, tempdir(), false, 'rust');
assert_istrue(isfile([tempdir(), 'nfpwmbuck/src/lib.rs']));

% --- Numeric: compile, run, average the regulated output (C toolchain only) ---
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'No C toolchain for code generation.');
crate = [tempdir(), 'nfpwmbuck'];
if isdir(crate); try; rmdir(crate, 's'); catch; end; end
nflow_codegenerate(model, tempdir(), true, 'C', struct('build', true, 'buildConfig', 'Release'));
exe = [crate, '/build/Release/nflow_run'];
if ispc(); exe = [exe, '.exe']; end
assert_istrue(isfile(exe));
csv = [tempdir(), 'nfpwmbuck_C.csv'];
[st, out] = system(['"', exe, '" -t ', num2str(d.duration), ' -o "', csv, '"']);
assert_isequal(st, 0, out);
lines = strsplit(strtrim(fileread(csv)), sprintf('\n'));
nr = numel(lines) - 1;
v = zeros(nr, 1);
for i = 1:nr
  vv = strsplit(strtrim(lines{i + 1}), ',');
  v(i) = str2double(vv{2});
end
tail = v(round(2 * nr / 3):end);
% Averaged step-down law Vout = D * Vin = 0.4 * 12 = 4.8 V.
assert_istrue(abs(mean(tail) - 4.8) < 0.3);
assert_istrue(~any(isnan(v)));
assert_istrue(min(v) > -0.05);
assert_istrue(max(v) < 14.5);
rmfile(model);
%=============================================================================
