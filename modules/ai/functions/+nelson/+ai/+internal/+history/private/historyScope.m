%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The scope a surface stores its conversation under: the Qt dock and web pane share
% one ('main'); other surfaces (e.g. NFlow) keep their own. Id sanitized for a file.
function scope = historyScope(surfaceId)
  s = regexprep(lower(strtrim(char(surfaceId))), '[^a-z0-9_-]', '_');
  if isempty(s) || strcmp(s, 'qt') || strcmp(s, 'web')
    scope = 'main';
  else
    scope = s;
  end
end
%=============================================================================
