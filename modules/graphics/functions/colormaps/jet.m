%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = jet(varargin)
  narginchk(0, 1);
  colorCount = requestedColorCount(varargin{:});
  map = zeros(colorCount, 3);
  if colorCount == 0
    return
  end

  bandLength = ceil(colorCount / 4);
  band = jetBandValues(bandLength);
  middleStart = ceil(bandLength / 2) - double(mod(colorCount, 4) == 1);
  positions = middleStart + (1:numel(band)).';

  map = setJetBand(map, positions + bandLength, 1, band);
  map = setJetBand(map, positions, 2, band);
  map = setJetBand(map, positions - bandLength, 3, band);
end
%=============================================================================
function values = jetBandValues(bandLength)
  rising = (1:bandLength).' / bandLength;
  plateau = ones(max(bandLength - 1, 0), 1);
  values = [rising; plateau; flipud(rising)];
end
%=============================================================================
function map = setJetBand(map, rows, channel, values)
  validRows = rows >= 1 & rows <= size(map, 1);
  if ~any(validRows)
    return
  end
  map(rows(validRows), channel) = values(validRows);
end
%=============================================================================
