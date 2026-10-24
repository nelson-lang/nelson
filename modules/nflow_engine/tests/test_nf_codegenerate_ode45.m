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
% Unified codegen 5.3 (b): a model integrated with the variable-step ode45
% (Dormand-Prince 5(4)) solver generates C and Rust with an embedded adaptive
% stepper over the shared __nflow_rhs, instead of being rejected. Structural
% checks only (no compiler); numeric validation is the FMU round-trip test.
%=============================================================================
% x' = -x, x(0) = 1  ->  e^{-t}. Integrator (IC=1) with -1 feedback.
d = struct();
d.solver = 'ode45';
d.blocks = {...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 1)), ...
  struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, 'params', struct('Gain', -1)), ...
  struct('id', 'out', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
d.connections = {...
  struct('from', 'ig', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'g', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'out', 'fromIndex', 0, 'toIndex', 0)};
d.sampleTime = 0.1; d.runtime = 1.0; d.variables = struct();
model = [tempname(), '.nflow'];
[~, modelstem] = fileparts(model);
fid = fopen(model, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);
%=============================================================================
% C generation succeeds and emits the Dormand-Prince adaptive stepper (not the
% fixed RK4 weights) over the shared rhs. nflow_codegenerate writes into
% <destination>/<modelStem>/model.c.
outDir = tempname();
if isdir(outDir)
  rmdir(outDir, 's');
end
mkdir(outDir);
nflow_codegenerate(model, outDir, false, 'C');
cfile = [outDir, filesep, modelstem, filesep, 'model.c'];
assert_istrue(isfile(cfile));
txt = fileread(cfile);
% Shared rhs is present (reused by the adaptive stepper).
assert_istrue(~isempty(strfind(txt, '__nflow_rhs')));
% Dormand-Prince tableau marker (5th-order b coefficient) + adaptive controls.
assert_istrue(~isempty(strfind(txt, '35.0/384')));
assert_istrue(~isempty(strfind(txt, '__att'))); % attempt / reject loop
assert_istrue(~isempty(strfind(txt, 'pow('))); % step-size control
% It must NOT be the fixed RK4 emission (dt/6 combination).
assert_istrue(isempty(strfind(txt, '(dt / 6.0)')));
%=============================================================================
% Rust emits the same embedded adaptive Dormand-Prince stepper (V2).
outDirR = tempname();
if isdir(outDirR)
  rmdir(outDirR, 's');
end
mkdir(outDirR);
nflow_codegenerate(model, outDirR, false, 'rust');
rfile = [outDirR, filesep, modelstem, filesep, 'src', filesep, 'lib.rs'];
assert_istrue(isfile(rfile));
rtxt = fileread(rfile);
assert_istrue(~isempty(strfind(rtxt, '__nflow_rhs')));
assert_istrue(~isempty(strfind(rtxt, '35.0/384.0'))); % Dormand-Prince tableau
assert_istrue(~isempty(strfind(rtxt, 'libm::pow('))); % adaptive step control (no_std math)
assert_istrue(~isempty(strfind(rtxt, 'libm::sqrt('))); % RMS error norm
assert_istrue(isempty(strfind(rtxt, '(dt / 6.0)'))); % not the fixed RK4
%=============================================================================
rmfile(model);
%=============================================================================
