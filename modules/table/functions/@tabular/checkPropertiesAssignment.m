%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = checkPropertiesAssignment(obj, current, value)
  % Checks the shape of a value assigned to Properties: a scalar structure
  % whose fields are known properties. Missing fields keep their current
  % value.
  if ~isstruct(value) || ~isscalar(value)
    error('Nelson:table:invalidPropertiesAssignment', _('Properties must be a scalar structure.'));
  end
  names = fieldnames(value);
  if isequal(names, fieldnames(current))
    return
  end
  known = knownPropertyNames(obj);
  merged = current;
  for k = 1:length(names)
    if ~isfield(current, names{k}) && ~any(strcmp(names{k}, known))
      error('Nelson:table:unknownProperty', _('Unknown table property: %s.'), names{k});
    end
    merged.(names{k}) = value.(names{k});
  end
  if isfield(value, 'RowNames') && ~isempty(value.RowNames)
    merged.RowNames = checkedRowNames(obj, value.RowNames);
  end
  if isfield(value, 'DimensionNames')
    merged.DimensionNames = checkedDimensionNames(obj, value.DimensionNames);
  end
  value = merged;
end
%=============================================================================
function names = knownPropertyNames(obj)
  % Natively built tables carry only some of these fields.
  names = {'DimensionNames', 'Description', 'UserData', 'VariableNames', 'VariableTypes', ...
    'VariableDescriptions', 'VariableUnits', 'VariableContinuity', 'CustomProperties'};
  if istimetable(obj)
    names = [names, {'RowTimes', 'StartTime', 'SampleRate', 'TimeStep', 'Events'}];
  else
    names = [names, {'RowNames'}];
  end
end
%=============================================================================
