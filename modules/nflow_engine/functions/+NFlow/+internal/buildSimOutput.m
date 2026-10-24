%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% out = NFlow.internal.buildSimOutput(result, document)
%
% Assemble the sim() output struct from a decoded engine result and the decoded
% model: out.tout, one field per To Workspace VariableName (already written to
% the base workspace by the engine), and out.logsout (one {name,time,values}
% entry per logged signal). Shared by sim() and the GUI Run StopFcn hook so both
% expose an identical 'out'.
%=============================================================================
function out = buildSimOutput(result, document)
  narginchk(2, 2);
  out = struct();
  out.tout = sim_time_vector(result);
  logs = struct('name', {}, 'time', {}, 'values', {});
  blocks = sim_field(document, 'blocks', {});
  for k = 1:numel(blocks)
    b = sim_cell_or_struct(blocks, k);
    if ~isfield(b, 'type') || ~strcmp(b.type, 'toWorkspace')
      continue
    end
    variableName = 'simout';
    if isfield(b, 'params') && isfield(b.params, 'VariableName')
      variableName = char(b.params.VariableName);
    end
    if ~isvar('base', variableName)
      continue
    end
    value = evalin('base', variableName);
    out.(variableName) = value; % also exposed directly as out.<VariableName>
    logs(end + 1) = sim_tw_entry(variableName, value, out.tout); %#ok<AGROW>
  end
  logs = sim_append_scopes(logs, result);
  out.logsout = logs;
end
%=============================================================================
function entry = sim_tw_entry(name, value, tout)
  entry = struct('name', name, 'time', tout(:), 'values', []);
  if isstruct(value)
    if isfield(value, 'time') && ~isempty(value.time)
      entry.time = value.time(:);
    end
    if isfield(value, 'signals') && isfield(value.signals, 'values')
      entry.values = value.signals.values;
    end
  elseif isnumeric(value)
    entry.values = value;
  end
end
%=============================================================================
function logs = sim_append_scopes(logs, result)
  if ~isfield(result, 'scopes')
    return
  end
  scopes = result.scopes;
  for k = 1:numel(scopes)
    s = sim_cell_or_struct(scopes, k);
    v = sim_scope_values(s);
    if isempty(v)
      continue % scope logged nothing
    end
    entry = struct('name', '', 'time', [], 'values', v);
    if isfield(s, 'id')
      entry.name = char(s.id);
    end
    if isfield(s, 'times')
      entry.time = s.times(:);
    end
    logs(end + 1) = entry; %#ok<AGROW>
  end
end
%=============================================================================
function v = sim_scope_values(s)
  % The engine emits scope series as [nSignals][nSamples]; expose them as a
  % [nSamples x nSignals] matrix (columns = signals), robust to jsondecode
  % returning either a cell of vectors or a numeric matrix.
  v = [];
  if ~isfield(s, 'series')
    return
  end
  ser = s.series;
  if iscell(ser)
    cols = cell(1, numel(ser));
    for j = 1:numel(ser)
      cols{j} = sim_series_column(ser{j});
    end
    lens = cellfun(@numel, cols);
    if ~isempty(cols) && all(lens == lens(1))
      v = zeros(lens(1), numel(cols));
      for j = 1:numel(cols)
        v(:, j) = cols{j};
      end
    end
  elseif isnumeric(ser)
    if isvector(ser)
      v = ser(:);
    else
      v = ser.'; % rows (signals) -> columns
    end
  end
  if ~isnumeric(v)
    v = []; % never surface a non-numeric (malformed decode)
  end
end
%=============================================================================
function col = sim_series_column(value)
  if isnumeric(value)
    col = value(:);
    return
  end
  if ~iscell(value)
    col = [];
    return
  end
  col = zeros(numel(value), 1);
  for k = 1:numel(value)
    item = value{k};
    if isnumeric(item) && isscalar(item)
      col(k) = item;
    else
      col(k) = NaN;
    end
  end
end
%=============================================================================
function t = sim_time_vector(result)
  t = [];
  if isfield(result, 'scopes')
    scopes = result.scopes;
    for k = 1:numel(scopes)
      s = sim_cell_or_struct(scopes, k);
      if isfield(s, 'times') && ~isempty(s.times)
        t = s.times(:);
        return
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
function b = sim_cell_or_struct(container, k)
  if iscell(container)
    b = container{k};
  else
    b = container(k);
  end
end
%=============================================================================
