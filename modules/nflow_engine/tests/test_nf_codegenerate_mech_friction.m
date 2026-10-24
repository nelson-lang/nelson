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
% A mechanicalTranslationalIsland with regularised Coulomb friction
% (F = -Fc*tanh(v/vEps)) exercises the tanh() nonlinearity on the island path.
% A single mass launched at v0 = 1 decays to rest under friction; a speed
% sensor exposes v as an external output for the numeric harness.
%
% Regression: the generated Rust crate is `#![no_std]` + `libm`, where the
% std `f64::tanh` inherent method does not exist. The Rust island emitter must
% therefore route tanh through `libm::tanh(...)`, never the method form
% `(v).tanh()`, which fails to compile (E0599: no method named `tanh`). Both
% backends are compiled and run by nflow_codegen_check_numeric, so a
% non-compiling emit fails this test rather than shipping silently.
%=============================================================================
skip_testsuite(~__nflow_codegen_toolchain__('C'), 'C toolchain (cmake) not available');
%=============================================================================
[hasC, hasRust] = __nflow_codegen_support__('mechanicalTranslationalIsland');
assert_istrue(hasC);
assert_istrue(hasRust);
%=============================================================================
comps = {...
  struct('kind', 'mass', 'node', 1, 'm', 1.0, 's0', 0.0, 'v0', 1.0), ...
  struct('kind', 'friction', 'node', 1, 'Fc', 2.0, 'vEps', 0.05), ...
  struct('kind', 'speedSensor', 'node', 1, 'outPort', 0)};
d = struct();
d.blocks = {...
  struct('id', 'mi', 'type', 'mechanicalTranslationalIsland', 'inputs', 0, 'outputs', 1, ...
    'params', struct('nodes', 2, 'components', {comps})), ...
  struct('id', 'sv', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'sv', 'isExternalPort', true)), ...
  struct('id', 'wv', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
    'params', struct('VariableName', 'nfmi_v', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
d.connections = {...
  struct('from', 'mi', 'to', 'sv', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'mi', 'to', 'wv', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.01;
d.duration = 2.0;
d.solver = 'ode4';
d.variables = struct();
%=============================================================================
% The interpreter integrates the friction island to rest (physical sanity).
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
v = evalin('base', 'nfmi_v');
evalin('base', 'clear(''nfmi_v'')');
assert_isequal(v(1), 1.0); % launched at v0 = 1
assert_istrue(all(isfinite(v)));
assert_istrue(abs(v(end)) < 1e-3); % friction brings it to rest
%=============================================================================
% C backend: generated code compiles, runs, and matches the interpreter.
[~, uniqueTag] = fileparts(tempname());
stem = ['nfmi_fric_', uniqueTag];
nflow_codegen_check_numeric(d, stem, 'C', {'sv'}, {'nfmi_v'}, 5e-6, 1e-6);
srcC = fileread([tempdir(), stem, '/model.c']);
assert_istrue(~isempty(strfind(srcC, 'tanh(')));
%=============================================================================
% Rust backend: the same, and tanh must be emitted through libm (no_std), not
% as the std method form. Building the crate is what catches the regression.
if __nflow_codegen_toolchain__('rust')
  nflow_codegen_check_numeric(d, stem, 'rust', {'sv'}, {'nfmi_v'}, 5e-6, 1e-6);
  srcR = fileread([tempdir(), stem, '/src/lib.rs']);
  assert_istrue(~isempty(strfind(srcR, 'libm::tanh(')));
  assert_istrue(isempty(strfind(srcR, ').tanh()')));
end
if isdir([tempdir(), stem])
  rmdir([tempdir(), stem], 's');
end
%=============================================================================
