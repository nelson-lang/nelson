%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function idx = tableResolveVariableSubscript(names, subs, vartypeId, vartypeMessage)
  % Variable positions (row vector) of a name, index or logical subscript;
  % a vartype subscript raises the caller specific error.
  if strcmp(class(subs), 'vartype')
    error(vartypeId, vartypeMessage);
  end
  if ~(isnumeric(subs) || islogical(subs) || ischar(subs) || isstring(subs) || iscellstr(subs))
    error('Nelson:table:invalidVarSubscript', _('A table variable subscript must be a numeric array containing real positive integers, a logical array, a character vector, a string array, or a cell array of character vectors.'));
  end
  if (ischar(subs) && isempty(subs)) || (isstring(subs) && any(strlength(subs(:)) == 0))
    error('Nelson:table:invalidVarName', _('A table variable name subscript must be a character vector, string array, or cell array of character vectors, with each name in the subscript containing one or more characters.'));
  end
  if isempty(subs)
    idx = zeros(1, 0);
  else
    idx = double(reshape(tableResolveVariables(names, subs), 1, []));
  end
end
%=============================================================================
