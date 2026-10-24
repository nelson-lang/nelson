%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = shrinkfaces(varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  [faces, vertices, sf, patchHandle] = parseShrinkfacesInputs(varargin);
  [newFaces, newVertices] = shrinkFaceData(faces, vertices, sf);
  if nargout == 0
    if ~isempty(patchHandle)
      patchHandle.Faces = newFaces;
      patchHandle.Vertices = newVertices;
    end
  elseif nargout == 1
    varargout{1} = struct('faces', newFaces, 'vertices', newVertices);
  else
    varargout{1} = newFaces;
    varargout{2} = newVertices;
  end
end
%=============================================================================
