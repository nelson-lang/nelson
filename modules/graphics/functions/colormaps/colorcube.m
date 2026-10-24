%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = colorcube(varargin)
  narginchk(0, 1);
  m = requestedColorCount(varargin{:});
  if m < 8
    map = gray(m);
    return
  end
  redGreenSteps = fix(m ^ (1 / 3) + eps);
  extraColors = m - redGreenSteps ^ 3;
  if extraColors == 0 && redGreenSteps > 2
    blueSteps = redGreenSteps - 1;
  else
    blueSteps = redGreenSteps;
  end
  map = colorCubeGrid(redGreenSteps, blueSteps);
  map = removeRepeatedAndDarkEdges(map);
  map = [map; primaryAndGrayRamps(m - size(map, 1) - 1)];
end
%=============================================================================
function map = colorCubeGrid(redGreenSteps, blueSteps)
  rgValues = linspace(0, 1, redGreenSteps);
  bValues = linspace(0, 1, blueSteps);
  %#ok<NLS0004>
  [red, green, blue] = meshgrid(rgValues, rgValues, bValues);
  map = [red(:), green(:), blue(:)];
end
%=============================================================================
function map = removeRepeatedAndDarkEdges(map)
  movement = sum(abs(diff(map.').'), 2);
  map = map(movement ~= 0, :);
  pairSums = [sum(map(:, [1 2]), 2), sum(map(:, [2 3]), 2), sum(map(:, [1 3]), 2)];
  map = map(min(pairSums, [], 2) ~= 0, :);
end
%=============================================================================
function map = primaryAndGrayRamps(remainingColors)
  primarySteps = floor(remainingColors / 4);
  graySteps = remainingColors - 3 * primarySteps;
  primaryRamp = linspace(1 / primarySteps, 1, primarySteps).';
  zeroRamp = zeros(primarySteps, 1);
  grayRamp = linspace(1 / graySteps, 1, graySteps).';
  map = [primaryRamp, zeroRamp, zeroRamp;
    zeroRamp, primaryRamp, zeroRamp;
    zeroRamp, zeroRamp, primaryRamp;
    0, 0, 0;
    grayRamp, grayRamp, grayRamp];
end
%=============================================================================
