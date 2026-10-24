%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isonormals(varargin)
  narginchk(2, 6);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [x, y, z, V, vertices, patchHandle, negate] = parseIsonormalsInputs(args);
  normals = computeIsonormals(x, y, z, V, vertices, negate);
  if nargout == 0
    if isempty(patchHandle)
      error('Nelson:isonormals:patchHandleExpectedWhen', 'Patch handle expected when no output argument is requested.');
    end
    patchHandle.VertexNormals = normals;
    patchHandle.VertexNormalsMode = 'manual';
  else
    varargout{1} = normals;
  end
end
%=============================================================================
