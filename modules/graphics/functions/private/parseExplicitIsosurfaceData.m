%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z, V, isovalue, colors] = parseExplicitIsosurfaceData(args)
  isovalue = [];
  colors = [];
  switch numel(args)
    case 4
      V = validateIsosurfaceVolume(args{4});
      [x, y, z] = validateIsosurfaceGrid(args{1}, args{2}, args{3}, V);
    case 5
      V = validateIsosurfaceVolume(args{4});
      [x, y, z] = validateIsosurfaceGrid(args{1}, args{2}, args{3}, V);
      [isovalue, colors] = parseLevelOrColors(args{5}, V);
    case 6
      V = validateIsosurfaceVolume(args{4});
      [x, y, z] = validateIsosurfaceGrid(args{1}, args{2}, args{3}, V);
      isovalue = validateIsosurfaceLevel(args{5});
      colors = validateIsosurfaceColors(args{6}, V);
  end
end
%=============================================================================
