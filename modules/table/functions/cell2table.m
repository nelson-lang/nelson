%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cell2table(varargin)
  narginchk(1, inf);
  nargoutchk(0, 1);
  C = varargin{1};
  if ~iscell(C) || ~ismatrix(C)
    error('Nelson:table:invalidType:cA2DCellArray', _('C must be a 2-D cell array.'));
  end
  [options, hasVariableNames] = parseOptions(varargin(2:end));
  nCols = size(C, 2);
  if size(C, 1) == 0
    values = repmat({[]}, 1, nCols);
  else
    values = cell(1, nCols);
    for k = 1:nCols
      values{k} = concatenateColumn(C(:, k));
    end
  end
  if ~hasVariableNames
    prefix = inputname(1);
    if isempty(prefix)
      prefix = 'Var';
    end
    names = cell(1, nCols);
    for k = 1:nCols
      names{k} = sprintf('%s%d', prefix, k);
    end
    options = [{'VariableNames', names}, options];
  end
  if nCols == 0
    options = [{'Size', [size(C, 1), 0], 'VariableTypes', {}}, options];
  end
  varargout{1} = table(values{:}, options{:});
end
%=============================================================================
function column = concatenateColumn(column)
  % A column of numeric, logical or string elements becomes one array (it
  % stays a cell when they do not concatenate). So does a column of datetime,
  % duration, calendarDuration, categorical... elements.
  firstValue = column{1};
  if isnumeric(firstValue) || islogical(firstValue)
    try
      column = cell2mat(column);
    catch
    end
  elseif all(cellfun(@isstring, column))
    column = string(column);
  elseif isobject(firstValue)
    try
      column = vertcat(column{:});
    catch
    end
  end
end
%=============================================================================
function [options, hasVariableNames] = parseOptions(args)
  if mod(length(args), 2) ~= 0
    error('Nelson:table:nameValuePair:nameValueArgumentsComePairs', _('Name-value arguments must come in pairs.'));
  end
  optionNames = {'VariableNames', 'RowNames', 'DimensionNames'};
  options = args;
  hasVariableNames = false;
  for k = 1:2:length(args)
    name = args{k};
    if ~(ischar(name) && isrow(name)) && ~(isstring(name) && isscalar(name))
      error('Nelson:table:invalidType:parameterNameACharacterVectorString', _('Parameter name must be a character vector or string scalar.'));
    end
    idx = find(strcmpi(char(name), optionNames), 1);
    if isempty(idx)
      error('Nelson:table:unknownParameter', _('Unknown parameter name.'));
    end
    options{k} = optionNames{idx};
    hasVariableNames = hasVariableNames || (idx == 1);
  end
end
%=============================================================================
