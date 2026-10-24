%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Ensure the import-options object owns a per-variable VariableOptions struct
% array whose length matches the declared variables. Each entry mirrors the
% variable name and type and carries the per-variable reader settings that
% setvaropts can override.
%=============================================================================
function opts = __import_options_variable_options_init__(opts)
  names = opts.VariableNames;
  if isstring(names)
    names = cellstr(names);
  end
  nvars = numel(names);
  types = opts.VariableTypes;
  if isstring(types)
    types = cellstr(types);
  end
  existing = opts.VariableOptions;
  needRebuild = ~isstruct(existing) || numel(existing) ~= nvars;
  options = repmat(__import_options_default_variable_option__(), 1, nvars);
  for idx = 1:nvars
    options(idx).Name = names{idx};
    if idx <= numel(types)
      options(idx).Type = types{idx};
    end
    if ~needRebuild
      options(idx) = __import_options_merge_variable_option__(options(idx), existing(idx));
    end
  end
  opts.VariableOptions = options;
end
%=============================================================================
function s = __import_options_default_variable_option__()
  s = struct('Name', '', 'Type', 'char', 'FillValue', [], ...
    'InputFormat', '', 'WhitespaceRule', 'trim', 'EmptyFieldRule', 'auto');
end
%=============================================================================
function target = __import_options_merge_variable_option__(target, source)
  fields = fieldnames(source);
  for k = 1:numel(fields)
    target.(fields{k}) = source.(fields{k});
  end
end
%=============================================================================
