%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function args = jsonTableArgs(nameValues)
  % Parses the JSON name-value arguments of readtable, readtimetable and
  % detectImportOptions. Import options properties are validated by
  % JSONImportOptions; TableNodeName, TextType and FileType are handled here.
  if mod(numel(nameValues), 2) ~= 0
    error('Nelson:spreadsheet:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  args.TableNodeName = '';
  args.TextType = 'string';
  args.Set = {};
  optionArgs = {};
  supported = {'FileType', 'TableNodeName', 'TextType', 'ParsingMode', 'VariableSelectors', ...
    'TableSelector', 'RowNamesSelector', 'VariableUnitsSelector', 'VariableDescriptionsSelector', ...
    'RepeatedNodeRule', 'AllowComments', 'AllowInfAndNaN', 'AllowTrailingCommas', ...
    'MissingRule', 'ImportErrorRule', 'VariableNamingRule'};
  for k = 1:2:numel(nameValues)
    name = nameValues{k};
    if ~((ischar(name) && isrow(name)) || (isstring(name) && isscalar(name)))
      error('Nelson:spreadsheet:invalidParameterName', 'Invalid parameter name. The parameter name must be a nonempty string or character array.');
    end
    match = strcmpi(supported, char(name));
    if ~any(match)
      error('Nelson:spreadsheet:propertyNotSupported', 'Property name not supported.');
    end
    name = supported{match};
    value = nameValues{k + 1};
    switch name
      case 'FileType'
        if ~strcmpi(value, 'json')
          error('Nelson:spreadsheet:fileTypeNotSupported', 'FileType not supported.');
        end
      case 'TableNodeName'
        args.TableNodeName = char(value);
      case 'TextType'
        args.TextType = lower(char(value));
        if ~any(strcmp(args.TextType, {'char', 'string'}))
          error('Nelson:spreadsheet:textTypeNotSupported', 'TextType not supported.');
        end
      otherwise
        optionArgs = [optionArgs, {name, value}];
        args.Set{end + 1} = name;
    end
  end
  args.Options = nelson.io.json.JSONImportOptions(optionArgs{:});
end
%=============================================================================
