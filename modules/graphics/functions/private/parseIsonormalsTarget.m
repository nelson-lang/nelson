%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [vertices, patchHandle] = parseIsonormalsTarget(value)
  patchHandle = [];
  if isscalar(value) && isgraphics(value, 'patch')
    patchHandle = value;
    vertices = value.Vertices;
  else
    vertices = value;
  end
end
%=============================================================================
