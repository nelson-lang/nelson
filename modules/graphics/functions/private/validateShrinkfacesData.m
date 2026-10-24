%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function validateShrinkfacesData(faces, vertices)
  if ~isValidShrinkfacesFaces(faces)
    error('Nelson:graphics:mustBeRealMatrix', '%s must be a real numeric matrix.', 'Faces');
  end
  if ~isValidShrinkfacesVertices(vertices)
    error('Nelson:validateShrinkfacesData:invalidVertices', 'Vertices must be a real numeric matrix with at least two columns.');
  end
  valid = faces(isfinite(faces));
  if ~isValidShrinkfacesIndices(valid, size(vertices, 1))
    error('Nelson:validateShrinkfacesData:facesContainInvalidVertex', 'Faces contain invalid vertex indices.');
  end
end
%=============================================================================
