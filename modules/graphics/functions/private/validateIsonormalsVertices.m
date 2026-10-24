%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateIsonormalsVertices(vertices)
  if ~isnumeric(vertices) || ~isreal(vertices) || ~ismatrix(vertices) || size(vertices, 2) ~= 3
    error('Nelson:validateIsonormalsVertices:invalidVertices', 'Vertices must be a real numeric N-by-3 matrix.');
  end
end
%=============================================================================
