%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function setProperties(obj, numArgs, varargin)
  % Set property values from constructor arguments.
  %   setProperties(obj, numArgs, name1, value1, ...)
  %   setProperties(obj, numArgs, arg1, ..., argN, valueOnlyName1, ...)
  % numArgs is the number of arguments supplied by the constructor (nargin).
  % Any arguments beyond numArgs are the names of the properties that may be
  % set by value only, in order, before the name-value pairs.
  narginchk(2, Inf);
  nargoutchk(0, 0);
  if ~isscalar(numArgs) || ~isnumeric(numArgs) || numArgs ~= fix(numArgs) || numArgs < 0
    error('Nelson:handle:setProperties:invalidCount', ...
      _('Wrong value for argument #%d: non-negative integer scalar expected.'), 2);
  end
  totalArgs = length(varargin);
  if numArgs > totalArgs
    error('Nelson:handle:setProperties:invalidCount', ...
      _('Wrong value for argument #%d: value cannot exceed the number of supplied property arguments.'), 2);
  end
  userArgs = varargin(1:numArgs);
  valueOnlyNames = varargin(numArgs + 1:end);
  valueOnlyNames = checkValueOnlyNames(valueOnlyNames);
  valueOnlyCount = numel(valueOnlyNames);
  k = 1;
  usedValueOnly = 0;
  argCount = numel(userArgs);
  % Leading value-only arguments: consumed by position until a property name
  % is reached, which switches the remaining arguments to name-value pairs.
  while k <= argCount
    current = userArgs{k};
    if isPropertyName(obj, current)
      break;
    end
    usedValueOnly = usedValueOnly + 1;
    if usedValueOnly > valueOnlyCount
      error('Nelson:handle:setProperties:tooManyValueOnly', ...
        _('Too many value-only inputs: at most %d expected, %d supplied. A common cause is a misspelled property name.'), ...
        valueOnlyCount, usedValueOnly);
    end
    obj.(valueOnlyNames{usedValueOnly}) = current;
    k = k + 1;
  end
  % Name-value pairs for the remaining arguments.
  if mod(argCount - k + 1, 2) ~= 0
    error('Nelson:handle:setProperties:nameValuePairs', ...
      _('Property arguments must be name-value pairs.'));
  end
  while k <= argCount
    name = userArgs{k};
    if isStringScalar(name)
      name = char(name);
    end
    if ~(ischar(name) && isrow(name))
      error('Nelson:handle:setProperties:invalidName', ...
        _('Wrong type for argument #%d: string expected.'), k + 2);
    end
    obj.(name) = userArgs{k + 1};
    k = k + 2;
  end
end
%=============================================================================
function tf = isStringScalar(value)
  tf = isstring(value) && isscalar(value);
end
%=============================================================================
function tf = isPropertyName(obj, value)
  tf = false;
  if (ischar(value) && isrow(value)) || isStringScalar(value)
    tf = isprop(obj, char(value));
  end
end
%=============================================================================
function names = checkValueOnlyNames(names)
  for k = 1:numel(names)
    name = names{k};
    if isStringScalar(name)
      name = char(name);
      names{k} = name;
    end
    if ~(ischar(name) && isrow(name))
      error('Nelson:handle:setProperties:invalidName', ...
        _('Value-only property name #%d must be a string.'), k);
    end
  end
end
%=============================================================================
