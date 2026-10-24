%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z, V, isovalue, colors] = parseDefaultIsosurfaceData(args)
  isovalue = [];
  colors = [];
  switch numel(args)
    case 1
      V = validateIsosurfaceVolume(args{1});
      [x, y, z] = defaultGridVectors(V);
    case 2
      V = validateIsosurfaceVolume(args{1});
      [x, y, z] = defaultGridVectors(V);
      [isovalue, colors] = parseLevelOrColors(args{2}, V);
    case 3
      V = validateIsosurfaceVolume(args{1});
      [x, y, z] = defaultGridVectors(V);
      isovalue = validateIsosurfaceLevel(args{2});
      colors = validateIsosurfaceColors(args{3}, V);
    otherwise
      error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
end
%=============================================================================
function [x, y, z] = defaultGridVectors(V)
  x = 1:size(V, 2);
  y = 1:size(V, 1);
  z = 1:size(V, 3);
end
%=============================================================================
