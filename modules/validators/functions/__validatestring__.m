%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function matched = __validatestring__(str, validStrings, varargin)
  narginchk(2, 5);
  nargoutchk(0, 1);
  [funcName, varName, argIndex] = localParseContext(varargin{:});
  strValue = localTextScalar(str, 'str');
  validWasString = isstring(validStrings);
  choices = localTextList(validStrings, 'validStrings');
  if isempty(choices)
    localError(funcName, 'unrecognizedStringChoice', ...
      localExpectedMessage(choices, strValue, varName, argIndex));
  end
  lowerInput = lower(strValue);
  exact = [];
  for k = 1:numel(choices)
    if strcmp(lowerInput, lower(choices{k}))
      exact = k;
      break
    end
  end
  if ~isempty(exact)
    matched = localReturnValue(choices{exact}, validWasString);
    return
  end
  matches = [];
  for k = 1:numel(choices)
    candidate = lower(choices{k});
    if length(lowerInput) <= length(candidate) && strcmp(lowerInput, candidate(1:length(lowerInput)))
      matches(end + 1) = k;
    end
  end
  if isempty(matches)
    localError(funcName, 'unrecognizedStringChoice', ...
      localExpectedMessage(choices, strValue, varName, argIndex));
  end
  if numel(matches) > 1
    if ~localSubstringChain(choices(matches))
      localError(funcName, 'ambiguousStringChoice', ...
        localAmbiguousMessage(choices(matches), strValue, varName, argIndex));
    end
    lengths = zeros(1, numel(matches));
    for k = 1:numel(matches)
      lengths(k) = length(choices{matches(k)});
    end
    [~, pos] = min(lengths);
    matches = matches(pos);
  end
  matched = localReturnValue(choices{matches(1)}, validWasString);
end
%=============================================================================
function [funcName, varName, argIndex] = localParseContext(varargin)
  funcName = '';
  varName = '';
  argIndex = [];
  if nargin == 0
    return
  elseif nargin == 1
    if isnumeric(varargin{1})
      argIndex = localPositiveInteger(varargin{1}, 'argIdx');
    else
      funcName = localTextScalar(varargin{1}, 'funcName');
    end
  elseif nargin == 2
    funcName = localTextScalar(varargin{1}, 'funcName');
    varName = localTextScalar(varargin{2}, 'varName');
  elseif nargin == 3
    funcName = localTextScalar(varargin{1}, 'funcName');
    varName = localTextScalar(varargin{2}, 'varName');
    argIndex = localPositiveInteger(varargin{3}, 'argIdx');
  else
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
end
%=============================================================================
function value = localTextScalar(value, name)
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if ~(ischar(value) && isrow(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', name);
  end
end
%=============================================================================
function values = localTextList(values, name)
  if isstring(values)
    values = cellstr(values);
  end
  if ischar(values)
    values = cellstr(values);
  end
  if ~iscellstr(values)
    error('Nelson:validators:mustBeTextArray', '%s must be a string array or cell array of character vectors.', name);
  end
  values = values(:)';
end
%=============================================================================
function value = localPositiveInteger(value, name)
  if ~(isnumeric(value) && isscalar(value) && isfinite(value) && value >= 1 && fix(value) == value)
    error('Nelson:validators:mustBePositiveInteger', '%s must be a positive integer.', name);
  end
  value = double(value);
end
%=============================================================================
function matched = localReturnValue(value, validWasString)
  if validWasString
    matched = string(value);
  else
    matched = value;
  end
end
%=============================================================================
function tf = localSubstringChain(values)
  tf = true;
  for i = 1:numel(values)
    for j = 1:numel(values)
      if i ~= j
        a = lower(values{i});
        b = lower(values{j});
        shorter = a;
        longer = b;
        if length(a) > length(b)
          shorter = b;
          longer = a;
        end
        if length(shorter) > length(longer) || ~strcmp(shorter, longer(1:length(shorter)))
          tf = false;
          return
        end
      end
    end
  end
end
%=============================================================================
function msg = localExpectedMessage(choices, inputValue, varName, argIndex)
  msg = localPrefix(varName, argIndex);
  msg = [msg, 'to match one of these values:', newline(), newline(), localChoices(choices), ...
    newline(), newline(), 'The input', ', ''', inputValue, ''', ', ...
    'did not match any of the valid values.'];
end
%=============================================================================
function msg = localAmbiguousMessage(choices, inputValue, varName, argIndex)
  msg = localPrefix(varName, argIndex);
  msg = [msg, 'to match one of these values:', newline(), newline(), localChoices(choices), ...
    newline(), newline(), 'The input', ', ''', inputValue, ''', ', ...
    'matched more than one valid value.'];
end
%=============================================================================
function msg = localPrefix(varName, argIndex)
  if ~isempty(argIndex) && ~isempty(varName)
    msg = sprintf('Expected input number %d, %s,', argIndex, varName);
  elseif ~isempty(argIndex)
    msg = sprintf('Expected input number %d', argIndex);
  elseif ~isempty(varName)
    msg = sprintf('Expected input %s', varName);
  else
    msg = 'Expected input';
  end
  msg = [msg, ' '];
end
%=============================================================================
function txt = localChoices(choices)
  txt = '';
  for k = 1:numel(choices)
    if k > 1
      txt = [txt, ', '];
    end
    txt = [txt, '''', choices{k}, ''''];
  end
end
%=============================================================================
function localError(funcName, reason, msg)
  id = ['Nelson:validators:', reason];
  if ~isempty(funcName)
    id = ['Nelson:', funcName, ':', reason];
  end
  error(id, msg);
end
%=============================================================================
