%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Builds the full property model of a single graphics object for the property
% inspector. Shared by the Qt, webview and wasm backends. It is only evaluated
% when the inspector selection changes (never on the idle refresh poll), so it
% may enumerate every visible property, but it never ships bulk array data:
% large or structured values are summarised to a short description string.
% Value display text comes from the shared __graphics_value_string__ so the
% light per-poll refresh (__graphics_inspector_values__) stays identical.
%=============================================================================
function model = __graphics_inspector_model__(object)
  names = properties(object);
  values = get(object);
  settable = set(object);
  writableNames = fieldnames(settable);
  entries = cell(1, numel(names));
  for k = 1:numel(names)
    name = names{k};
    raw = values.(name);
    writable = any(strcmp(name, writableNames));
    choices = {};
    if writable && isfield(settable, name)
      choices = settable.(name);
      if ~iscell(choices)
        choices = {};
      end
    end
    entries{k} = local_describe(name, raw, writable, choices);
  end
  model = struct('handle', local_handle(object), ...
    'type', __graphics_value_string__(get(object, 'Type')), ...
    'label', local_label(object, values), ...
    'properties', {entries});
end
%=============================================================================
function entry = local_describe(name, raw, writable, choices)
  raw = local_coerce(raw);
  kind = 'info';
  value = [];
  if ~isempty(choices)
    kind = 'enum';
    value = __graphics_value_string__(raw);
  elseif local_is_color(name, raw)
    kind = 'color';
    value = raw;
  elseif islogical(raw) && isscalar(raw)
    kind = 'bool';
    value = raw;
  elseif isnumeric(raw) && isscalar(raw) && isreal(raw)
    kind = 'number';
    value = double(raw);
  elseif isnumeric(raw) && isreal(raw) && isvector(raw) && numel(raw) <= 8
    kind = 'vector';
    value = double(raw(:)');
  elseif ischar(raw) || (isstring(raw) && isscalar(raw))
    kind = 'text';
    value = char(raw);
  end
  entry = struct('name', name, 'kind', kind, 'writable', logical(writable), ...
    'display', __graphics_value_string__(raw), 'value', value, 'choices', {choices});
end
%=============================================================================
function tf = local_is_color(name, raw)
  tf = false;
  if numel(name) >= 5 && endsWith(name, 'Color')
    if isnumeric(raw) && isequal(size(raw), [1, 3]) && isreal(raw)
      tf = true;
    elseif ischar(raw) || (isstring(raw) && isscalar(raw))
      tf = true;
    end
  end
end
%=============================================================================
function raw = local_coerce(raw)
  if strcmp(class(raw), 'nelson.lang.OnOffSwitchState')
    raw = char(raw);
  end
end
%=============================================================================
function h = local_handle(object)
  try
    h = __web_graphics_handle__(object, 'number');
  catch
    h = 0;
  end
end
%=============================================================================
function label = local_label(object, values)
  label = '';
  if isfield(values, 'DisplayName') && ~isempty(values.DisplayName)
    label = __graphics_value_string__(values.DisplayName);
  end
  if isempty(label) && isfield(values, 'Tag') && ~isempty(values.Tag)
    label = __graphics_value_string__(values.Tag);
  end
  if isempty(label)
    label = __graphics_value_string__(get(object, 'Type'));
  end
end
%=============================================================================
