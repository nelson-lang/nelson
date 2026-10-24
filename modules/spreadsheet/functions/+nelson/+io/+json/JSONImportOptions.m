%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef JSONImportOptions
  properties
    VariableNames
    VariableNamingRule
    VariableTypes
    SelectedVariableNames
    VariableOptions
    VariableSelectors
    RowNamesSelector
    TableSelector
    VariableDescriptionsSelector
    VariableUnitsSelector
    ImportErrorRule
    MissingRule
    RepeatedNodeRule
    AllowComments
    AllowInfAndNaN
    AllowTrailingCommas
  end

  methods
    function obj = JSONImportOptions(varargin)
      obj = initializeDefaults(obj, 1);
      if mod(nargin, 2) ~= 0
        error('Nelson:spreadsheet:nameValueExpected', 'Name-value arguments expected.');
      end
      names = varargin(1:2:end);
      values = varargin(2:2:end);
      for k = 1:numel(names)
        name = resolvePropertyName(obj, names{k});
        if strcmp(name, 'NumVariables')
          obj = initializeDefaults(obj, validateNumVariables(obj, values{k}));
        end
      end
      for k = 1:numel(names)
        name = resolvePropertyName(obj, names{k});
        if ~strcmp(name, 'NumVariables')
          obj = setOption(obj, name, values{k});
        end
      end
    end
  end

  methods (Access = private)
    function obj = initializeDefaults(obj, nvars)
      obj = resizeVariables(obj, nvars);
      obj.VariableNamingRule = 'preserve';
      obj.VariableOptions = {};
      obj.VariableSelectors = {};
      obj.RowNamesSelector = '';
      obj.TableSelector = '';
      obj.VariableDescriptionsSelector = '';
      obj.VariableUnitsSelector = '';
      obj.ImportErrorRule = 'fill';
      obj.MissingRule = 'fill';
      obj.RepeatedNodeRule = 'addcol';
      obj.AllowComments = true;
      obj.AllowInfAndNaN = true;
      obj.AllowTrailingCommas = true;
    end

    function obj = resizeVariables(obj, nvars)
      names = cell(1, nvars);
      for k = 1:nvars
        names{k} = ['Var', num2str(k)];
      end
      obj.VariableNames = names;
      obj.SelectedVariableNames = names;
      obj.VariableTypes = repmat({'char'}, 1, nvars);
    end

    function obj = setOption(obj, name, value)
      switch name
        case 'ParsingMode'
          mode = validateChoice(obj, name, value, {'lenient', 'strict'});
          flag = strcmp(mode, 'lenient');
          obj.AllowComments = flag;
          obj.AllowInfAndNaN = flag;
          obj.AllowTrailingCommas = flag;
        case 'VariableNames'
          names = toCellstrRow(obj, value);
          if numel(names) ~= numel(obj.VariableNames)
            obj = resizeVariables(obj, numel(names));
          end
          obj.VariableNames = names;
          obj.SelectedVariableNames = names;
        case 'VariableTypes'
          types = toCellstrRow(obj, value);
          if numel(types) ~= numel(obj.VariableNames)
            error('Nelson:spreadsheet:variableTypesMismatch', '%s must match the number of variables.', 'VariableTypes');
          end
          obj.VariableTypes = types;
        case 'SelectedVariableNames'
          obj.SelectedVariableNames = selectedNames(obj, value);
        case 'VariableSelectors'
          selectors = toCellstrRow(obj, value);
          if numel(selectors) ~= numel(obj.VariableNames)
            obj = resizeVariables(obj, numel(selectors));
          end
          obj.VariableSelectors = selectors;
        case {'RowNamesSelector', 'TableSelector', 'VariableDescriptionsSelector', 'VariableUnitsSelector'}
          obj.(name) = toText(obj, value);
        case {'ImportErrorRule', 'MissingRule'}
          obj.(name) = validateChoice(obj, name, value, {'fill', 'error', 'omitrow', 'omitvar'});
        case 'RepeatedNodeRule'
          obj.(name) = validateChoice(obj, name, value, {'addcol', 'ignore', 'error'});
        case 'VariableNamingRule'
          obj.(name) = validateChoice(obj, name, value, {'preserve', 'modify'});
        case {'AllowComments', 'AllowInfAndNaN', 'AllowTrailingCommas'}
          if ~(islogical(value) || isnumeric(value)) || ~isscalar(value)
            error('Nelson:spreadsheet:logicalScalarExpected', 'Logical scalar expected.');
          end
          obj.(name) = logical(value);
        otherwise
          obj.(name) = value;
      end
    end

    function canonical = resolvePropertyName(obj, name)
      name = toText(obj, name);
      supported = [{'NumVariables', 'ParsingMode'}, properties(obj)'];
      matches = strcmpi(supported, name);
      if sum(matches) ~= 1
        error('Nelson:spreadsheet:unknownNameValue', 'Invalid name-value argument name.');
      end
      canonical = supported{find(matches, 1)};
    end

    function nvars = validateNumVariables(obj, value)
      if ~isnumeric(value) || ~isscalar(value) || value < 0 || value ~= fix(value)
        error('Nelson:spreadsheet:invalidNumVariables', '%s must be a nonnegative integer scalar.', 'NumVariables');
      end
      nvars = double(value);
    end

    function value = validateChoice(obj, name, value, choices)
      value = lower(toText(obj, value));
      if ~any(strcmp(choices, value))
        error('Nelson:spreadsheet:invalidOptionValue', 'Invalid value for %s.', name);
      end
    end

    function names = selectedNames(obj, value)
      if isnumeric(value)
        names = obj.VariableNames(value);
      else
        names = toCellstrRow(obj, value);
      end
      if ~all(ismember(names, obj.VariableNames))
        error('Nelson:spreadsheet:unknownVariableName', 'Selected variable names must be variable names.');
      end
    end

    function value = toText(obj, value)
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      if ~ischar(value) || (~isempty(value) && ~isrow(value))
        error('Nelson:spreadsheet:stringScalarExpected', 'String scalar or character vector expected.');
      end
    end

    function names = toCellstrRow(obj, names)
      if isstring(names)
        names = cellstr(names);
      elseif ischar(names)
        names = {names};
      end
      if ~iscellstr(names)
        error('Nelson:spreadsheet:cellStringExpected', 'Cell array of character vectors expected.');
      end
      names = names(:)';
    end
  end
end
%=============================================================================
