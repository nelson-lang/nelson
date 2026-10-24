%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [data, props] = applyVariableProperties(obj, data, oldNames, props)
  % Validates the per-variable metadata of a Properties struct against the
  % variables named oldNames, stores it as row cells and renames the data
  % fields by position.
  props = rowVariableProperties(props);
  n = length(oldNames);
  names = checkedVariableNames(obj, props.VariableNames);
  renamed = ~isequal(names(:), oldNames(:));
  if renamed || ~iscellstr(names)
    validateNames(obj, names, _('Variable names must be nonempty and distinct.'));
  end
  if length(names) ~= n
    error('Nelson:table:invalidVariableNames', _('VariableNames must have one name per variable.'));
  end
  checkPerVariableLength(props, n, 'VariableDescriptions');
  checkPerVariableLength(props, n, 'VariableUnits');
  checkPerVariableLength(props, n, 'VariableContinuity');
  if renamed
    newData = struct();
    for k = 1:n
      newData.(names{k}) = data.(oldNames{k});
    end
    data = newData;
  end
  props.VariableNames = names;
end
%=============================================================================
function props = rowVariableProperties(props)
  % Per-variable metadata is stored as a row cell, whatever the orientation
  % given; string arrays become cells of character vectors.
  fields = {'VariableNames', 'VariableDescriptions', 'VariableUnits', 'VariableContinuity'};
  for k = 1:length(fields)
    if isfield(props, fields{k})
      v = props.(fields{k});
      if isstring(v)
        props.(fields{k}) = reshape(cellstr(v), 1, []);
      elseif iscell(v) && size(v, 1) > 1
        props.(fields{k}) = reshape(v, 1, []);
      end
    end
  end
end
%=============================================================================
function checkPerVariableLength(props, n, name)
  if isfield(props, name) && ~isempty(props.(name)) && numel(props.(name)) ~= n
    error('Nelson:table:invalidVariableProperty', _('%s must have one element per variable.'), name);
  end
end
%=============================================================================
