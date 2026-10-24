%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function V = validateIsosurfaceVolume(V)
  if ~isnumeric(V) || ~isreal(V) || ndims(V) ~= 3
    error('Nelson:isosurface:invalidVolumeType', 'Volume data must be a real numeric 3-D array.');
  end
  V = double(V);
end
%=============================================================================
