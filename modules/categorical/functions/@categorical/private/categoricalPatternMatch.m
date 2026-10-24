%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = categoricalPatternMatch(fun, functionName, str, pat, varargin)
  % Pattern matching on category names: each category is tested once,
  % then the result is gathered through the codes (undefined elements give false).
  if ~iscategorical(str) || iscategorical(pat)
    error('Nelson:UndefinedFunction:incorrectArgumentType', _('Check for incorrect argument data type or missing argument in call to function ''%s''.'), functionName);
  end
  names = str.categoryNames;
  if isempty(names)
    categoryTF = false(0, 1);
  else
    categoryTF = fun(string(names), pat, varargin{:});
  end
  lookup = [false; categoryTF(:)];
  tf = reshape(lookup(str.codes + 1), size(str.codes));
end
%=============================================================================
