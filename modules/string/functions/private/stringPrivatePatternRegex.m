%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function expr = stringPrivatePatternRegex(value)
  if isa(value, 'pattern')
    expr = value.Expression;
  elseif ischar(value)
    expr = regexptranslate('escape', value);
  elseif isstring(value) && isscalar(value)
    expr = regexptranslate('escape', char(value));
  else
    error('Nelson:string:patternStringScalarCharacterVector', 'Pattern, string scalar, or character vector expected.');
  end
end
%=============================================================================
