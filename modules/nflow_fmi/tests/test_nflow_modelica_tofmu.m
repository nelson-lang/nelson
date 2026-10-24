%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% OpenModelica bridge: modelicaToFmu. Where a capable OpenModelica
% is present it compiles a Modelica model to an FMU, caches it, and the FMU
% imports through nflow's FMI path and simulates to the analytical response.
% Where OpenModelica is absent/incapable it asserts the guard blocks the build.
%=============================================================================
% Argument validation does not need OpenModelica.
raised = false;
try
  modelicaToFmu();
catch
  raised = true;
end
assert_istrue(raised);
%=============================================================================
info = modelicaInfo();
%=============================================================================
src = sprintf('model FO\n  output Real x(start = 1.0);\nequation\n  der(x) = -x;\nend FO;\n');
%=============================================================================
if ~info.capable
  % Guard path: the build is blocked with a clear, typed error.
  msg = '';
  id = '';
  try
    modelicaToFmu(src);
  catch e
    msg = e.message;
    id = e.identifier;
  end
  assert_isequal(id, 'Nelson:nflow_fmi:modelicaUnavailable');
  assert_istrue(~isempty(msg));
else
  % Happy path: compile, cache, import and simulate.
  fmu = modelicaToFmu(src); % model name parsed as FO
  assert_istrue(isfile(fmu));
  assert_istrue(endsWith(fmu, 'FO.fmu'));

  fmu2 = modelicaToFmu(src); % cached -> same path
  assert_isequal(fmu, fmu2);

  fi = fmiInfo(fmu);
  assert_isequal(fi.fmiVersion, '2.0');
  assert_istrue(fi.coSimulation);
  ix = find(strcmp(fi.names, 'x'), 1);
  assert_istrue(~isempty(ix));
  assert_isequal(fi.causalities{ix}, 'output'); % 'output' -> a wired port

  % nflow simulates the OpenModelica FMU: x(t) = e^{-t}.
  model = ['{"blocks":[', ...
    '{"id":"m","type":"fmu","inputs":0,"outputs":1,"params":{"path":"', strrep(fmu, '\', '/'), '"}},', ...
    '{"id":"tw","type":"toWorkspace","inputs":1,"outputs":0,"params":{"VariableName":"y"}}],', ...
    '"connections":[{"from":"m","to":"tw","fromIndex":0,"toIndex":0}],', ...
    '"runtime":1.0,"sampleTime":0.05}'];
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, model); fclose(fid);
  out = sim(p);
  y = out.logsout(1).values;
  assert_istrue(abs(y(1) - 1.0) < 1e-6);
  assert_istrue(abs(y(end) - exp(-1)) < 2e-2); % FMU's own CS step -> loose tol

  % A malformed model reports a typed compile failure.
  bad = sprintf('model Bad\n  Real x\nequation\n  der(x) = -x;\nend Bad;\n'); % missing ';'
  badId = '';
  try
    modelicaToFmu(bad, 'Bad');
  catch e2
    badId = e2.identifier;
  end
  assert_isequal(badId, 'Nelson:nflow_fmi:modelicaCompileFailed');
end
%=============================================================================
