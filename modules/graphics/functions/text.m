%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = text(varargin)
  % h = text(x, y, label)
  % h = text(x, y, z, label)
  % h = text(ax, x, y, label)
  % h = text(ax, x, y, z, label)
  % h = text(PropertyName, PropertyValue, ...)
  % h = text(ax, PropertyName, PropertyValue, ...)

  narginchk(0, 10000);
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  parent = [];
  if (~isempty(inputArguments) && isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    parent = inputArguments{1};
    inputArguments = inputArguments(2:end);
  end

  % text() and text(PropertyName, PropertyValue, ...) take the position from
  % the 'Position' property instead of the positional x, y and z arguments.
  if (isempty(inputArguments) || ischar(inputArguments{1}))
    [parent, inputArguments] = extractParentProperty(parent, inputArguments);
    H = __text__('Parent', parent, inputArguments{:});
    if (nargout == 1)
      varargout{1} = H;
    end
    return
  end

  narginchk(3, 10000);
  if isempty(parent)
    parent = gca();
  end

  if isnumeric(inputArguments{1})
    x = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    error('Nelson:text:invalidInputArgument', 'Invalid input argument: numeric value expected.')
  end
  if isnumeric(inputArguments{1})
    y = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    error('Nelson:text:invalidInputArgument', 'Invalid input argument: numeric value expected.')
  end
  if isnumeric(inputArguments{1})
    z = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    z = zeros(size(x));
  end

  if (length(x) ~= length(y))
    error('Nelson:text:invalidVectorsXAnd', 'Vectors x and %s must be the same length.', 'y');
  end
  if (length(x) ~= length(z))
    error('Nelson:text:invalidVectorsXAnd', 'Vectors x and %s must be the same length.', 'z');
  end

  if numel(inputArguments) == 0
    error('Nelson:text:invalidLastArgument', 'last argument must be a string or cell of strings.')
  end
  if ischar(inputArguments{1})
    labelArray = charToLabel(inputArguments{1}, length(x));
  elseif iscell(inputArguments{1})
    labelArray = cellToLabel(inputArguments{1}, length(x));
  elseif isstring(inputArguments{1})
    labelArray = stringToLabel(inputArguments{1}, length(x));
  else
    error('Nelson:text:invalidLastArgument', 'last argument must be a string or cell of strings.')
  end
  inputArguments = inputArguments(2:end);
  [parent, inputArguments] = extractParentProperty(parent, inputArguments);
  H = [];
  for (i = 1:numel(x))
    H = [H, __text__('Parent', parent, 'Position', [x(i), y(i), z(i)], 'String', labelArray{i}, inputArguments{1:end})];
  end
  if (nargout == 1)
    varargout{1} = H;
  end
end
%=============================================================================
function [parent, inputArguments] = extractParentProperty(parent, inputArguments)
  if (mod(numel(inputArguments), 2) ~= 0)
    error('Nelson:graphics:pvPairsExpected', 'Property/value pairs expected.');
  end
  parentIndex = [];
  for k = 1:2:numel(inputArguments)
    if (ischar(inputArguments{k}) && strcmpi(inputArguments{k}, 'Parent'))
      parentIndex = k;
    end
  end
  if ~isempty(parentIndex)
    parent = inputArguments{parentIndex + 1};
    inputArguments(parentIndex:parentIndex + 1) = [];
  elseif isempty(parent)
    parent = gca();
  end
end
%=============================================================================
function labelArray = charToLabel(value, len)
  label = value;
  labelArray = repmat({label}, [len, 1]);
end
%=============================================================================
function labelArray = stringToLabel(value, len)
  if isscalar(value)
    labelArray = repmat({stringScalarToChar(value)}, [len, 1]);
  elseif len == 1
    labelArray = {stringArrayToText(value)};
  elseif numel(value) == len
    labelArray = cell(len, 1);
    for idx = 1:len
      labelArray{idx} = stringScalarToChar(value(idx));
    end
  else
    error('Nelson:text:invalidNumberOfText', 'Number of text labels must match the number of positions.');
  end
end
%=============================================================================
function labelArray = cellToLabel(value, len)
  if len == 1
    labelArray = {cellElementsToText(value)};
  elseif numel(value) == len
    labelArray = cell(len, 1);
    for idx = 1:len
      labelArray{idx} = labelValueToChar(value{idx});
    end
  elseif numel(value) == 1
    labelArray = repmat({labelValueToChar(value{1})}, [len, 1]);
  else
    error('Nelson:text:invalidNumberOfText', 'Number of text labels must match the number of positions.');
  end
end
%=============================================================================
function label = cellElementsToText(value)
  label = '';
  for idx = 1:numel(value)
    line = labelValueToChar(value{idx});
    if idx == 1
      label = line;
    else
      label = [label, char(10), line];
    end
  end
end
%=============================================================================
function label = labelValueToChar(value)
  if ischar(value)
    label = value;
  elseif isstring(value)
    if isscalar(value)
      label = stringScalarToChar(value);
    else
      label = stringArrayToText(value);
    end
  elseif iscell(value)
    label = cellElementsToText(value);
  elseif isnumeric(value)
    label = sprintf('%g', value);
  else
    error('Nelson:text:invalidTextLabel', 'Text label must be a string, a cell of strings, or a numeric value.');
  end
end
%=============================================================================
function label = stringArrayToText(value)
  label = '';
  for idx = 1:numel(value)
    line = stringScalarToChar(value(idx));
    if idx == 1
      label = line;
    else
      label = [label, char(10), line];
    end
  end
end
%=============================================================================
function label = stringScalarToChar(value)
  if ismissing(value)
    label = '';
  else
    label = char(value);
  end
end
%=============================================================================
