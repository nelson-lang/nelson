%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function boundaries = ip_boundaries_from_perimeter(perim, labels, numLabels)
  boundaries = cell(numLabels, 1);
  [rows, cols] = find(perim);
  for k = 1:numLabels
    selected = labels(sub2ind(size(labels), rows, cols)) == k;
    boundaries{k} = [rows(selected), cols(selected)];
  end
end
%=============================================================================
