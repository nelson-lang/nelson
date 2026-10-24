%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function name = graphicsSelectName(names, k)
  if numel(names) == 1
    name = names{1};
  else
    name = names{k};
  end
end
%=============================================================================
