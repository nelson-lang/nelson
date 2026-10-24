%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __writetable_json__(T, filename, varargin)
  % JSON backend of writetable and writetimetable (called by the builtins).
  % Each table row is written as a JSON object, keys are the variable names.
  options = parseOptions(varargin);
  [keys, values] = tableEntries(T, options.WriteRowNames);
  for k = 1:numel(values)
    values{k} = jsonTableWriteColumn(values{k}, options.PreserveInfAndNaN);
  end
  __write_json_table__(convertStringsToChars(filename), keys, values, ...
    options.PrettyPrint, options.PreserveInfAndNaN);
end
%=============================================================================
function options = parseOptions(nameValues)
  options.WriteRowNames = false;
  options.PrettyPrint = true;
  options.PreserveInfAndNaN = true;
  if mod(numel(nameValues), 2) ~= 0
    error('Nelson:spreadsheet:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  supported = {'FileType', 'WriteRowNames', 'PrettyPrint', 'PreserveInfAndNaN', 'DateLocale'};
  for k = 1:2:numel(nameValues)
    match = strcmpi(supported, convertStringsToChars(nameValues{k}));
    if ~any(match)
      error('Nelson:spreadsheet:propertyNotSupported', 'Property name not supported.');
    end
    name = supported{match};
    value = nameValues{k + 1};
    switch name
      case {'WriteRowNames', 'PrettyPrint', 'PreserveInfAndNaN'}
        if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
          error('Nelson:spreadsheet:logicalScalarExpected', 'Logical scalar expected.');
        end
        options.(name) = logical(value);
      otherwise
        % FileType is 'json' here; DateLocale is accepted, dates are written in English.
    end
  end
end
%=============================================================================
function [keys, values] = tableEntries(T, writeRowNames)
  keys = T.Properties.VariableNames;
  values = cell(1, numel(keys));
  for k = 1:numel(keys)
    values{k} = T.(keys{k});
  end
  if istimetable(T)
    rowKey = T.Properties.DimensionNames{1};
    rowValues = T.Properties.RowTimes;
  elseif writeRowNames && ~isempty(T.Properties.RowNames)
    rowKey = T.Properties.DimensionNames{1};
    rowValues = T.Properties.RowNames(:);
  else
    return
  end
  keys = [{rowKey}, keys];
  values = [{rowValues}, values];
end
%=============================================================================
