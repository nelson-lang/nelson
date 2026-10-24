%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isValidShrinkfacesIndices(indices, vertexCount)
  tf = isempty(indices) || (all(indices >= 1) && all(fix(indices) == indices) ...
    && all(indices <= vertexCount));
end
%=============================================================================
