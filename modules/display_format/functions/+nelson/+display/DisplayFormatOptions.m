%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef DisplayFormatOptions
  properties (Dependent)
    NumericFormat
    LineSpacing
    TruncateMatrices
  end

  properties (Hidden)
    NumericFormatValue = "short"
    LineSpacingValue = "loose"
    TruncateMatricesValue = "on"
  end

  methods
    function obj = DisplayFormatOptions(varargin)
      if nargin == 0
        currentFormat = format();
        obj.NumericFormatValue = currentFormat.NumericFormatValue;
        obj.LineSpacingValue = currentFormat.LineSpacingValue;
        obj.TruncateMatricesValue = currentFormat.TruncateMatricesValue;
        return
      end
      if mod(nargin, 2) ~= 0
        error('Nelson:display_format:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:nargin
        name = varargin{k};
        if isstring(name)
          if ~isscalar(name)
            error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
          end
          name = char(name);
        end
        if ~ischar(name) || ~isrow(name)
          error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
        end

        supportedNames = {'NumericFormat'; 'LineSpacing'; 'TruncateMatrices'};
        nameMatches = startsWith(upper(supportedNames), upper(name));
        if sum(nameMatches) ~= 1
          error('Nelson:display_format:invalidNameValueArgumentName', 'Invalid name-value argument name.');
        end
        nameIndex = find(nameMatches);

        switch supportedNames{nameIndex(1)}
          case 'NumericFormat'
            value = varargin{k + 1};
            if isstring(value)
              if ~isscalar(value)
                error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
              end
              value = char(value);
            end
            if ~ischar(value) || ~isrow(value)
              error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
            end
            supportedValues = {'short'; 'long'; 'shortE'; 'longE'; 'shortG'; 'longG'; ...
              'shortEng'; 'longEng'; '+'; 'bank'; 'hex'; 'rational'};
            valueMatches = strcmpi(supportedValues, value);
            if sum(valueMatches) ~= 1
              error('Nelson:display_format:invalidValueNumericformat', 'Invalid value for "%s".', 'NumericFormat');
            end
            valueIndex = find(valueMatches);
            obj.NumericFormatValue = string(supportedValues{valueIndex(1)});
          case 'LineSpacing'
            value = varargin{k + 1};
            if isstring(value)
              if ~isscalar(value)
                error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
              end
              value = char(value);
            end
            if ~ischar(value) || ~isrow(value)
              error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
            end
            supportedValues = {'compact'; 'loose'};
            valueMatches = strcmpi(supportedValues, value);
            if sum(valueMatches) ~= 1
              error('Nelson:display_format:invalidValueLinespacing', 'Invalid value for "%s".', 'LineSpacing');
            end
            valueIndex = find(valueMatches);
            obj.LineSpacingValue = string(supportedValues{valueIndex(1)});
          case 'TruncateMatrices'
            value = varargin{k + 1};
            if isstring(value)
              if ~isscalar(value)
                error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
              end
              value = char(value);
            end
            if ~ischar(value) || ~isrow(value)
              error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
            end
            supportedValues = {'on'; 'off'};
            valueMatches = strcmpi(supportedValues, value);
            if sum(valueMatches) ~= 1
              error('Nelson:display_format:invalidValueTruncatematrices', 'Invalid value for "%s".', 'TruncateMatrices');
            end
            valueIndex = find(valueMatches);
            obj.TruncateMatricesValue = string(supportedValues{valueIndex(1)});
        end
      end
    end

    function value = get.NumericFormat(obj)
      value = obj.NumericFormatValue;
    end

    function obj = set.NumericFormat(obj, value)
      if isstring(value)
        if ~isscalar(value)
          error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
        end
        value = char(value);
      end
      if ~ischar(value) || ~isrow(value)
        error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
      end
      supportedValues = {'short'; 'long'; 'shortE'; 'longE'; 'shortG'; 'longG'; ...
        'shortEng'; 'longEng'; '+'; 'bank'; 'hex'; 'rational'};
      valueMatches = strcmp(supportedValues, value);
      if sum(valueMatches) ~= 1
        error('Nelson:display_format:invalidValueNumericformat', 'Invalid value for "%s".', 'NumericFormat');
      end
      valueIndex = find(valueMatches);
      obj.NumericFormatValue = string(supportedValues{valueIndex(1)});
    end

    function value = get.LineSpacing(obj)
      value = obj.LineSpacingValue;
    end

    function obj = set.LineSpacing(obj, value)
      if isstring(value)
        if ~isscalar(value)
          error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
        end
        value = char(value);
      end
      if ~ischar(value) || ~isrow(value)
        error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
      end
      supportedValues = {'compact'; 'loose'};
      valueMatches = strcmp(supportedValues, value);
      if sum(valueMatches) ~= 1
        error('Nelson:display_format:invalidValueLinespacing', 'Invalid value for "%s".', 'LineSpacing');
      end
      valueIndex = find(valueMatches);
      obj.LineSpacingValue = string(supportedValues{valueIndex(1)});
    end

    function value = get.TruncateMatrices(obj)
      value = obj.TruncateMatricesValue;
    end

    function obj = set.TruncateMatrices(obj, value)
      if isstring(value)
        if ~isscalar(value)
          error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
        end
        value = char(value);
      end
      if ~ischar(value) || ~isrow(value)
        error('Nelson:display_format:stringScalarCharacterVectorExpected', 'String scalar or character vector expected.');
      end
      supportedValues = {'on'; 'off'};
      valueMatches = strcmp(supportedValues, value);
      if sum(valueMatches) ~= 1
        error('Nelson:display_format:invalidValueTruncatematrices', 'Invalid value for "%s".', 'TruncateMatrices');
      end
      valueIndex = find(valueMatches);
      obj.TruncateMatricesValue = string(supportedValues{valueIndex(1)});
    end

    function disp(obj)
      narginchk(1, 1);
      if ~isscalar(obj)
        fprintf('  %s array\n', class(obj));
        return
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      fprintf('    NumericFormat: "%s"\n', char(obj.NumericFormat));
      fprintf('      LineSpacing: "%s"\n', char(obj.LineSpacing));
      fprintf(' TruncateMatrices: "%s"\n', char(obj.TruncateMatrices));
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
    end

    function display(obj, name)
      narginchk(1, 2);
      if nargin == 1
        name = inputname(1);
      end
      if isempty(name)
        name = 'ans';
      end
      fmt = format();
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      fprintf('%s =\n', name);
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      fprintf('  DisplayFormatOptions with properties:\n');
      if ~isscalar(obj)
        fprintf('  %s array\n', class(obj));
        return
      end
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
      fprintf('    NumericFormat: "%s"\n', char(obj.NumericFormat));
      fprintf('      LineSpacing: "%s"\n', char(obj.LineSpacing));
      fprintf(' TruncateMatrices: "%s"\n', char(obj.TruncateMatrices));
      if strcmp(fmt.LineSpacing, 'loose')
        fprintf('\n');
      end
    end
  end
end
%=============================================================================
