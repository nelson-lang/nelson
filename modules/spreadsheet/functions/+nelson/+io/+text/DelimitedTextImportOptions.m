%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef DelimitedTextImportOptions
  properties
    SelectedVariableNames
    VariableNames
    VariableOptions
    VariableTypes
    ImportErrorRule
    MissingRule
    PreserveVariableNames
    VariableNamingRule
    DataLines
    VariableNamesLine
    RowNamesColumn
    VariableUnitsLine
    VariableDescriptionsLine
    ExtraColumnsRule
    EmptyLineRule
    Whitespace
    LineEnding
    CommentStyle
    Encoding
    ConsecutiveDelimitersRule
    LeadingDelimitersRule
    TrailingDelimitersRule
    Delimiter
  end

  methods
    function obj = DelimitedTextImportOptions(varargin)
      obj = initializeDefaults(obj, 1);
      if nargin == 0
        return
      end
      if mod(nargin, 2) ~= 0
        error('Nelson:spreadsheet:nameValueExpected', 'Name-value arguments expected.');
      end
      k = 1;
      while k <= nargin
        name = normalizeName(obj, varargin{k});
        value = varargin{k + 1};
        if strcmpi(name, 'NumVariables')
          obj = initializeDefaults(obj, validateNumVariables(obj, value));
        else
          name = resolvePropertyName(obj, name);
          obj.(name) = normalizePropertyValue(obj, name, value);
          if strcmp(name, 'VariableNames')
            obj.SelectedVariableNames = obj.VariableNames;
            obj = ensureVariableTypes(obj, length(obj.VariableNames));
          elseif strcmp(name, 'VariableTypes')
            obj.VariableTypes = toCellstrRow(obj, value);
          elseif strcmp(name, 'SelectedVariableNames')
            obj.SelectedVariableNames = toCellstrRow(obj, value);
          end
        end
        k = k + 2;
      end
      obj = synchronizeVariables(obj);
    end
  end

  methods (Access = private)
    function obj = initializeDefaults(obj, nvars)
      names = cell(1, nvars);
      for k = 1:nvars
        names{k} = ['Var', num2str(k)];
      end
      obj.SelectedVariableNames = names;
      obj.VariableNames = names;
      obj.VariableOptions = {};
      obj.VariableTypes = repmat({'char'}, 1, nvars);
      obj.ImportErrorRule = 'fill';
      obj.MissingRule = 'fill';
      obj.PreserveVariableNames = false;
      obj.VariableNamingRule = 'modify';
      obj.DataLines = [1 Inf];
      obj.VariableNamesLine = 0;
      obj.RowNamesColumn = 0;
      obj.VariableUnitsLine = 0;
      obj.VariableDescriptionsLine = 0;
      obj.ExtraColumnsRule = 'addvars';
      obj.EmptyLineRule = 'skip';
      obj.Whitespace = sprintf('\b\t ');
      obj.LineEnding = {'\n', '\r', '\r\n'};
      obj.CommentStyle = {};
      obj.Encoding = '';
      obj.ConsecutiveDelimitersRule = 'split';
      obj.LeadingDelimitersRule = 'keep';
      obj.TrailingDelimitersRule = 'keep';
      obj.Delimiter = {','};
    end

    function name = normalizeName(obj, name)
      if isstring(name)
        if ~isscalar(name)
          error('Nelson:spreadsheet:stringScalarExpected', 'String scalar or character vector expected.');
        end
        name = char(name);
      end
      if ~ischar(name) || ~isrow(name)
        error('Nelson:spreadsheet:stringScalarExpected', 'String scalar or character vector expected.');
      end
    end

    function canonical = resolvePropertyName(obj, name)
      supported = [{'NumVariables'}, properties(obj)'];
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

    function value = normalizePropertyValue(obj, name, value)
      switch name
        case {'VariableNames', 'SelectedVariableNames', 'VariableTypes', 'Delimiter', ...
            'LineEnding', 'CommentStyle'}
          value = toCellstrRow(obj, value);
        case {'ImportErrorRule', 'MissingRule', 'VariableNamingRule', 'ExtraColumnsRule', ...
            'EmptyLineRule', 'Whitespace', 'Encoding', 'ConsecutiveDelimitersRule', ...
            'LeadingDelimitersRule', 'TrailingDelimitersRule'}
          value = normalizeName(obj, value);
        case {'DataLines'}
          if ~isnumeric(value) || size(value, 2) ~= 2
            error('Nelson:spreadsheet:invalidDataLines', '%s must be a numeric array with two columns.', 'DataLines');
          end
        case {'VariableNamesLine', 'RowNamesColumn', 'VariableUnitsLine', ...
            'VariableDescriptionsLine'}
          if ~isnumeric(value) || ~isscalar(value)
            error('Nelson:spreadsheet:numericScalarExpected', 'Numeric scalar expected.');
          end
        case 'PreserveVariableNames'
          if ~islogical(value) || ~isscalar(value)
            error('Nelson:spreadsheet:logicalScalarExpected', 'Logical scalar expected.');
          end
      end
    end

    function names = toCellstrRow(obj, names)
      if isempty(names)
        names = {};
      elseif isstring(names)
        names = cellstr(names);
      elseif ischar(names)
        names = {names};
      end
      if ~iscellstr(names)
        error('Nelson:spreadsheet:cellStringExpected', 'Cell array of character vectors expected.');
      end
      names = names(:)';
    end

    function obj = synchronizeVariables(obj)
      obj.VariableNames = toCellstrRow(obj, obj.VariableNames);
      obj.SelectedVariableNames = toCellstrRow(obj, obj.SelectedVariableNames);
      obj.VariableTypes = toCellstrRow(obj, obj.VariableTypes);
      obj = ensureVariableTypes(obj, length(obj.VariableNames));
      if isempty(obj.SelectedVariableNames)
        obj.SelectedVariableNames = obj.VariableNames;
      end
    end

    function obj = ensureVariableTypes(obj, nvars)
      if isempty(obj.VariableTypes)
        obj.VariableTypes = repmat({'char'}, 1, nvars);
      elseif length(obj.VariableTypes) == 1 && nvars > 1
        obj.VariableTypes = repmat(obj.VariableTypes, 1, nvars);
      elseif length(obj.VariableTypes) ~= nvars
        error('Nelson:spreadsheet:variableTypesMismatch', '%s must match the number of variables.', 'VariableTypes');
      end
    end
  end
end
%=============================================================================
