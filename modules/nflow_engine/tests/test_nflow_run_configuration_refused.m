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
% Regression: a run configuration the engine could not honour was quietly
% replaced by one it could. A solver it does not have fell through to the
% fixed-step loop, and a step or duration of zero (or less) was clamped - a
% sampleTime of 0 became 1e-6, so a 0.3 s run silently produced 300001 samples.
% The engine now names the field that is wrong.
%=============================================================================
function msg = local_run(d)
  model = [tempname(), '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, jsonencode(d));
  fclose(fid);
  msg = '';
  try
    sim(model);
  catch e
    msg = e.message;
  end
  rmfile(model);
end
function d = local_doc()
  d = struct();
  d.version = '1.0.0';
  d.format = 'nflow-2';
  d.name = 'run_configuration';
  d.runtime = 0.3;
  d.sampleTime = 0.1;
  d.blocks = {...
    struct('id', 'k', 'type', 'constant', 'x', 40, 'y', 100, 'rotation', 0, ...
      'params', struct('Value', 1)), ...
    struct('id', 'i', 'type', 'integrator', 'x', 240, 'y', 100, 'rotation', 0, ...
      'params', struct('InitialCondition', 0)), ...
    struct('id', 'tw', 'type', 'toWorkspace', 'x', 440, 'y', 100, 'rotation', 0, ...
      'params', struct('VariableName', 'y_cfg', 'SaveFormat', 'Array', ...
      'Decimation', 1, 'MaxDataPoints', 'inf'))};
  d.connections = {...
    struct('from', 'k', 'to', 'i', 'fromIndex', 0, 'toIndex', 0), ...
    struct('from', 'i', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0)};
end
%=============================================================================
% A solver the engine does not have is named back, with the ones it does have.
d = local_doc();
d.solver = 'no_such_solver';
msg = local_run(d);
asserts.istrue(~isempty(strfind(msg, 'no_such_solver')));
asserts.istrue(~isempty(strfind(msg, 'ode4')));
%=============================================================================
% A step of zero or less: refused instead of clamped to 1e-6.
d = local_doc();
d.sampleTime = 0;
asserts.istrue(~isempty(strfind(local_run(d), 'sampleTime')));
d.sampleTime = -0.1;
asserts.istrue(~isempty(strfind(local_run(d), 'sampleTime')));
%=============================================================================
% A duration of zero or less, likewise.
d = local_doc();
d.runtime = 0;
asserts.istrue(~isempty(strfind(local_run(d), 'greater than zero')));
d.runtime = -5;
asserts.istrue(~isempty(strfind(local_run(d), 'greater than zero')));
%=============================================================================
% The configuration as written runs, and every solver the message lists is
% accepted: a run that asks for one of them is not turned away.
asserts.istrue(isempty(local_run(local_doc())));
evalin('base', 'clear(''y_cfg'')');
for s = {'discrete', 'ode1', 'ode4', 'ode45'}
  d = local_doc();
  d.solver = s{1};
  asserts.istrue(isempty(local_run(d)));
  evalin('base', 'clear(''y_cfg'')');
end
%=============================================================================
