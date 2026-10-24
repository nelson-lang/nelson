%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, ok] = str2num(s)
  narginchk(1, 1);
  if isstring(s) && isscalar(s)
    s = char(s);
  end
  if ~ischar(s)
    error('Nelson:string:inputCharacterVectorStringScalar', 'Input must be a character vector or string scalar.');
  end
  x = [];
  ok = false;
  if isempty(s)
    return
  end
  if ~isrow(s)
    % the rows of a character matrix are separate rows of the result
    s = strjoin(cellstr(s), ';');
  end
  % carriage returns and NUL characters are whitespace
  s = strrep(s, char(13), char(10));
  s = strrep(s, char(0), ' ');
  try
    x = eval(['[', s, ']']);
  catch
    x = [];
    return
  end
  % text and cell results are not numbers
  if ischar(x) || isstring(x) || iscell(x)
    x = [];
  else
    ok = true;
  end
end
%=============================================================================
