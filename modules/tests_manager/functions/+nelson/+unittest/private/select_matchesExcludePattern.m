%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = select_matchesExcludePattern(value, pattern)
  tf = false;
  if ~isempty(pattern)
    tf = contains(value, pattern);
  end
end
