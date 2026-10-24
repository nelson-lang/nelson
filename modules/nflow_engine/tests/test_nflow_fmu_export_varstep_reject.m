%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 64-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% FMU export uses the same generateC path as the code generator, which lowers
% ode1/ode4 (RK4) and the adaptive ode45 (Dormand-Prince). The CVODES and
% IDAS/DAE backends have NO embedded lowering, so a model selecting one would
% produce an FMU that integrates fixed-step -- byte-different from the simulation.
% NFlow.exportfmu must reject those explicitly. An ode45 model, by contrast, now
% exports (see test_nflow_fmu_ode45_roundtrip for the numeric round-trip).
%=============================================================================
function f = writeModel(work, solver)
  d = struct();
  d.solver = solver;
  d.blocks = {...
    struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
      'params', struct('InitialCondition', 1)), ...
    struct('id', 'g', 'type', 'gain', 'inputs', 1, 'outputs', 1, ...
      'params', struct('Gain', -1)), ...
    struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
      'params', struct('GotoTag', 'y', 'isExternalPort', true))};
  d.connections = {...
    struct('from', 'ig', 'to', 'g', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'g', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
  d.sampleTime = 0.1;
  d.duration = 1.0;
  d.variables = struct();
  f = [work, filesep, 'M.nflow'];
  fid = fopen(f, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
end
%=============================================================================
work = tempname();
mkdir(work);
cleaner = onCleanup(@() rmdir(work, 's')); %#ok<NASGU>
%=============================================================================
% Every CVODES / DAE solver name is rejected before any codegen / compile.
variableSolvers = {'variableStiff', 'variableNonstiff', ...
  'cvodesBdf', 'cvodesAdams', 'dae', 'idas'};
for k = 1:numel(variableSolvers)
  s = variableSolvers{k};
  f = writeModel(work, s);
  msg = '';
  try
    NFlow.exportfmu(f, [work, filesep, 'out']);
  catch e
    msg = e.message;
  end
  assert_istrue(~isempty(strfind(msg, 'variable-step solver')));
  assert_istrue(~isempty(strfind(msg, s)));
end
%=============================================================================
% An ode45 model must NOT hit the reject guard (it now exports). It may still
% fail later if no C toolchain is present -- but never with the reject message.
f = writeModel(work, 'ode45');
msg = '';
try
  NFlow.exportfmu(f, [work, filesep, 'out45']);
catch e
  msg = e.message;
end
assert_istrue(isempty(strfind(msg, 'variable-step solver')));
%=============================================================================
