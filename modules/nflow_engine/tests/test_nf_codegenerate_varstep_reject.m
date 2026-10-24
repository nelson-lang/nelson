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
% The generators lower the fixed-step schemes (ode1/ode4 -> RK4, discrete) and,
% for the C target, the adaptive ode45 (see test_nf_codegenerate_ode45). The
% CVODES and IDAS/DAE backends have NO embedded lowering, so a model selecting
% one would silently fall through to the fixed-step emission -- byte-different
% from the variable-step simulation. Code generation must reject them explicitly
% for both C and Rust. This rejection is purely structural.
%=============================================================================
function writeModel(f, solver)
  json = ['{', ...
    '"version":"1.0.0","format":"nflow-2","name":"SolverReject",', ...
    '"solver":"', solver, '","sampleTime":0.1,"runtime":1.0,', ...
    '"blocks":[', ...
    '{"id":"ig","type":"integrator","x":100,"y":100,"rotation":0,', ...
    '"inputs":1,"outputs":1,"params":{"InitialCondition":1}},', ...
    '{"id":"sc","type":"scope","x":300,"y":100,"rotation":0,"params":{}}', ...
    '],', ...
    '"connections":[{"from":"ig","fromPort":0,"to":"sc","toPort":0}]', ...
    '}'];
  fid = fopen(f, 'wt');
  fwrite(fid, json);
  fclose(fid);
end
%=============================================================================
model = [tempname(), '.nflow'];
[~, modelstem] = fileparts(model);
%=============================================================================
% Every CVODES / DAE solver name is rejected, for both C and Rust targets.
variableSolvers = {'variableStiff', 'variableNonstiff', ...
  'cvodesBdf', 'cvodesAdams', 'dae', 'idas'};
for k = 1:numel(variableSolvers)
  s = variableSolvers{k};
  writeModel(model, s);
  expected = _(['Code generation is not supported for the variable-step solver ''', s, ...
    ''': it has no embedded lowering, so the generated fixed-step code would ', ...
    'differ from the simulation. Select the fixed-step ''ode4'' (RK4) / ''ode1'' or ', ...
    'the adaptive ''ode45'' solver for code generation, or simulate this model.']);
  cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''C'')'];
  assert_checkerror(cmd, expected);
  cmd = ['nflow_codegenerate(''', model, ''', tempdir(), false, ''rust'')'];
  assert_checkerror(cmd, expected);
end
%=============================================================================
% The fixed-step solvers still generate without error (no over-rejection).
% nflow_codegenerate writes into <destination>/<modelStem>/model.c.
cstem = [tempdir(), modelstem, filesep, 'model.c'];
writeModel(model, 'ode4');
if isfile(cstem)
  rmfile(cstem);
end
nflow_codegenerate(model, tempdir(), false, 'C');
assert_istrue(isfile(cstem));
writeModel(model, 'ode1');
rmfile(cstem);
nflow_codegenerate(model, tempdir(), false, 'C');
assert_istrue(isfile(cstem));
%=============================================================================
rmfile(model);
%=============================================================================
