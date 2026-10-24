%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = eventtableResolveVariable(field, value, data, variableNames, rowTimes)
  % Name of the event variable designated by value (a name or an index),
  % [] when value is [].
  if isnumeric(value) && isempty(value)
    name = [];
    return
  end
  if isstring(value) && isscalar(value)
    value = char(value);
  end
  if isnumeric(value) && isscalar(value) && value == fix(value) && value >= 1
    if value > length(variableNames)
      error('Nelson:eventtable:varIndexOutOfRange', 'Variable index exceeds table dimensions.');
    end
    name = variableNames{value};
  elseif ischar(value) && isrow(value)
    name = value;
  else
    error('Nelson:eventtable:invalidVarName', 'A table variable name subscript must be a character vector, string array, or cell array of character vectors, with each name in the subscript containing one or more characters.');
  end
  if ~any(strcmp(name, variableNames))
    error('Nelson:eventtable:unrecognizedVariable', sprintf('Unrecognized table variable name ''%s''.', name));
  end
  eventtableCheckVariableType(field, data.(name), rowTimes);
end
%=============================================================================
