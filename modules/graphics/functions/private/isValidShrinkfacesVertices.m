%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isValidShrinkfacesVertices(vertices)
  tf = isnumeric(vertices) && isreal(vertices) && ismatrix(vertices) && size(vertices, 2) >= 2;
end
%=============================================================================
