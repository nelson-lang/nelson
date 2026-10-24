%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = sim(model, varargin)
  % sim  Run an nflow simulation of a model.
  %
  %   out = sim(model)
  %   out = sim(model, 'StopTime', T)
  %   out = sim(model, 'StopTime', T, 'SampleTime', dt)
  %
  %   model is a loaded system (name or handle) or the path to a .nflow file.
  %   'StopTime' and 'SampleTime' override the values stored in the model.
  %
  %   out is a structure holding the simulation results: one field per
  %   To Workspace block (its VariableName), plus 'tout' (the time vector).
  %   From Workspace blocks read their signal from the base workspace, and
  %   To Workspace blocks also write their result there, as during an
  %   interactive run.
  %
  %   If the model carries a top-level 'stopFcn' field (a text blob of Nelson
  %   commands), it runs in the base workspace once
  %   the run stops, with the result exposed as 'out'. It fires only from
  %   sim() (never on model load) and runs even when no output is requested,
  %   e.g. set stopFcn to 'NFlow.plotScopes(out)' to pop the scopes up.
  narginchk(1, inf);
  nargoutchk(0, 1);

  options = sim_parse_options(varargin);
  documentJson = sim_model_json(model);
  documentJson = prepare_model_json(documentJson);
  % Decoded only to read scalar defaults and to map To Workspace outputs; the
  % blocks/connections are NOT re-encoded from here. jsondecode homogenizes a
  % heterogeneous / nested-subsystem block array and collapses a subsystem's
  % inner diagram, so the model's raw JSON is passed straight to the engine and
  % only scalar run overrides (sample time, duration, solver, tolerances,
  % variables) are sent alongside as a small object the engine overlays.
  document = jsondecode(documentJson);

  overrides = struct();
  sampleTime = sim_field(document, 'sampleTime', 0.01);
  duration = sim_field(document, 'runtime', 10);
  if isfield(options, 'StopTime')
    duration = options.StopTime;
  end
  if isfield(options, 'SampleTime')
    sampleTime = options.SampleTime;
  end
  overrides.sampleTime = sampleTime;
  overrides.duration = duration;
  overrides.variables = sim_variables(document);

  % Forward the solver configuration stored in the model (a variable-step /
  % ode1 / ode4 name plus optional tolerances), overridable per call. Without
  % this sim() always ran the default fixed-step engine, ignoring the model's
  % Solver setting. Absent or 'discrete' keeps the fixed-step path.
  solver = sim_field(document, 'solver', '');
  if isfield(options, 'Solver')
    solver = options.Solver;
  end
  if (ischar(solver) || isstring(solver)) && ~isempty(char(solver)) ...
      && ~strcmp(char(solver), 'discrete')
    overrides.solver = char(solver);
    overrides = sim_forward_tol(overrides, document, options, 'relTol', 'RelTol');
    overrides = sim_forward_tol(overrides, document, options, 'absTol', 'AbsTol');
    overrides = sim_forward_tol(overrides, document, options, 'maxStep', 'MaxStep');
  end

  resultJson = __nflow_simulate__(documentJson, jsonencode(overrides));
  result = jsondecode(resultJson);
  if ~isfield(result, 'status') || ~strcmp(result.status, 'done')
    reason = 'unknown error';
    if isfield(result, 'reason')
      reason = result.reason;
    end
    error('Nelson:nflow_engine:simFailed', 'Simulation of ''%s'' failed: %s', ...
      sim_model_label(model, document), reason);
  end

  % A StopFcn callback (a text blob of Nelson commands stored in the model,
  % executed in the base workspace) runs once the results are ready, even when
  % the caller asked for no output, so the output must be built whenever a
  % callback is present.
  stopFcn = sim_field(document, 'stopFcn', '');
  hasStopFcn = (ischar(stopFcn) || (isstring(stopFcn) && isscalar(stopFcn))) ...
    && ~isempty(strtrim(char(stopFcn)));
  out = [];
  if nargout >= 1 || hasStopFcn
    out = NFlow.internal.buildSimOutput(result, document);
  end
  if nargout >= 1
    varargout{1} = out;
  end
  if hasStopFcn
    NFlow.internal.runStopFcn(char(stopFcn), out);
  end
end
%=============================================================================
function options = sim_parse_options(args)
  options = struct();
  if mod(numel(args), 2) ~= 0
    error('Nelson:nflow_engine:simArgs:simExpectsNameValuePairsAfter', 'sim expects name/value pairs after the model.');
  end
  for k = 1:2:numel(args)
    name = args{k};
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error('Nelson:nflow_engine:simArgs:parameterNamesStrings', 'Parameter names must be strings.');
    end
    name = char(name);
    value = args{k + 1};
    switch name
      case {'StopTime', 'SampleTime', 'RelTol', 'AbsTol', 'MaxStep'}
        if (ischar(value) || isstring(value))
          value = str2double(char(value));
        end
        if ~(isnumeric(value) && isscalar(value) && isfinite(value))
          error('Nelson:nflow_engine:simArgs', '%s must be a finite numeric scalar.', name);
        end
        options.(name) = double(value);
      case 'Solver'
        if ~(ischar(value) || (isstring(value) && isscalar(value)))
          error('Nelson:nflow_engine:simArgs:solverAString', '''Solver'' must be a string.');
        end
        options.Solver = char(value);
      otherwise
        warning('Nelson:nflow_engine:simIgnoredParameter', ...
          'sim: parameter ''%s'' is not supported and is ignored.', name);
    end
  end
end
%=============================================================================
function documentJson = sim_model_json(model)
  % A loaded model (handle or name) is snapshotted to a temp file; otherwise
  % the argument is treated as a .nflow file path.
  if isnumeric(model) && isscalar(model)
    documentJson = sim_snapshot(model);
    return
  end
  if ~(ischar(model) || (isstring(model) && isscalar(model)))
    error('Nelson:nflow_engine:simArgs:modelANameAHandleA', 'model must be a name, a handle or a .nflow file path.');
  end
  name = char(model);
  if bdIsLoaded(name)
    documentJson = sim_snapshot(name);
    return
  end
  filename = name;
  if ~isfile(filename)
    if ~endsWith(lower(filename), '.nflow')
      filename = [filename, '.nflow'];
    end
  end
  if ~isfile(filename)
    error('Nelson:nflow_engine:modelNotFound', ...
      'Model ''%s'' is not a loaded system nor an existing .nflow file.', name);
  end
  documentJson = fileread(filename);
end
%=============================================================================
function documentJson = sim_snapshot(model)
  target = [tempname(), '.nflow'];
  cleanup = onCleanup(@() sim_delete_snapshot(target));
  save_system(model, target);
  documentJson = fileread(target);
end
%=============================================================================
function sim_delete_snapshot(target)
  if isfile(target)
    delete(target);
  end
end
%=============================================================================
function variables = sim_variables(document)
  % The on-disk 'variables' field is a text blob of 'name = value' lines; keep
  % only the numeric-scalar assignments. A sim-input object (struct) passes
  % through unchanged.
  variables = struct();
  if ~isfield(document, 'variables')
    return
  end
  raw = document.variables;
  if isstruct(raw)
    variables = raw;
    return
  end
  if ~(ischar(raw) || isstring(raw))
    return
  end
  text = char(raw);
  lines = strsplit(text, {sprintf('\n'), ';'});
  for k = 1:numel(lines)
    tokens = regexp(strtrim(lines{k}), '^([A-Za-z_]\w*)\s*=\s*(.+)$', 'tokens', 'once');
    if numel(tokens) == 2
      value = str2double(tokens{2});
      if ~isnan(value)
        variables.(tokens{1}) = value;
      end
    end
  end
end
%=============================================================================
function value = sim_field(s, name, defaultValue)
  if isfield(s, name)
    value = s.(name);
  else
    value = defaultValue;
  end
end
%=============================================================================
function request = sim_forward_tol(request, document, options, field, optName)
  v = [];
  if isfield(document, field) && isnumeric(document.(field))
    v = document.(field);
  end
  if isfield(options, optName)
    v = options.(optName);
  end
  if ~isempty(v) && isnumeric(v) && isscalar(v) && isfinite(v) && v > 0
    request.(field) = double(v);
  end
end
%=============================================================================
function label = sim_model_label(model, document)
  if isfield(document, 'name') && ~isempty(document.name)
    label = char(document.name);
  elseif ischar(model) || (isstring(model) && isscalar(model))
    label = char(model);
  else
    label = 'model';
  end
end
%=============================================================================
