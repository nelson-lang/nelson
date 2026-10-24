%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = ip_resize3_nearest(volume, outputSize)
  rowIndex = ip_resize3_nearest_index(size(volume, 1), outputSize(1));
  columnIndex = ip_resize3_nearest_index(size(volume, 2), outputSize(2));
  planeIndex = ip_resize3_nearest_index(size(volume, 3), outputSize(3));
  out = volume(rowIndex, columnIndex, planeIndex);
end
%=============================================================================
function idx = ip_resize3_nearest_index(inputLength, outputLength)
  coord = ((1:outputLength) - 0.5) * inputLength / outputLength + 0.5;
  idx = min(max(round(coord), 1), inputLength);
end
%=============================================================================
