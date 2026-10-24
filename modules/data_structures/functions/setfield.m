%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = setfield(s, varargin)
  if ~(isstruct(s) || (iscell(s) && isempty(s)) || isempty(s))
    error('Nelson:data_structures:invalidInputStructExpected', 'Invalid Input struct expected.');
  end
  if length(varargin) < 2
    error('Nelson:data_structures:wrongNumbersInputArguments', 'Wrong numbers of input arguments.');
  end
  % setfield(s, 'field1', ..., 'fieldN', value): intermediate arguments are a
  % chain of field names, the last argument is the value to assign. A cell
  % argument holds the subscripts of an indexing step:
  % setfield(s, {i}, 'field', {k}, value) assigns s(i).field(k) = value.
  fieldnames = varargin(1:end - 1);
  hasSubscripts = false;
  for i = 1:length(fieldnames)
    fieldname = fieldnames{i};
    if iscell(fieldname)
      hasSubscripts = true;
      continue;
    end
    if isstring(fieldname) && isscalar(fieldname)
      fieldname = convertStringsToChars(fieldname);
      fieldnames{i} = fieldname;
    end
    if ~ischar(fieldname)
      error('Nelson:data_structures:inputStringCharacterArray', 'Input should be a string or character array.');
    end
  end
  fieldvalue = varargin{end};
  if hasSubscripts
    f = setSubsChain(s, fieldnames, fieldvalue);
  else
    f = setFieldChain(s, fieldnames, fieldvalue);
  end
end
%=============================================================================
function s = setFieldChain(s, fieldnames, fieldvalue)
  fieldname = fieldnames{1};
  if length(fieldnames) == 1
    s.(fieldname) = fieldvalue;
  else
    if isstruct(s) && isfield(s, fieldname)
      sub = s.(fieldname);
    else
      sub = struct();
    end
    s.(fieldname) = setFieldChain(sub, fieldnames(2:end), fieldvalue);
  end
end
%=============================================================================
function s = setSubsChain(s, steps, fieldvalue)
  % steps mixes field names and cells of subscripts; each step is applied to
  % the value selected by the previous ones, growing it when it does not exist.
  step = steps{1};
  rest = steps(2:end);
  if iscell(step)
    if isempty(rest)
      s(step{:}) = fieldvalue;
    else
      try
        sub = s(step{:});
      catch
        sub = [];
      end
      s(step{:}) = setSubsChain(sub, rest, fieldvalue);
    end
  else
    if isempty(rest)
      s.(step) = fieldvalue;
    else
      if isstruct(s) && isscalar(s) && isfield(s, step)
        sub = s.(step);
      else
        sub = [];
      end
      s.(step) = setSubsChain(sub, rest, fieldvalue);
    end
  end
end
%=============================================================================
