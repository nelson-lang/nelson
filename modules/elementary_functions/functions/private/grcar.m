%=============================================================================
% Copyright (c) 2019-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = grcar(n, upperBand, className)
  if isempty(upperBand)
    activeUpperBand = 3;
  else
    activeUpperBand = upperBand;
  end
  rowIndex = (1:n).';
  columnIndex = 1:n;
  R = cast(columnIndex >= rowIndex & columnIndex <= rowIndex + activeUpperBand, className);
  R(2:n + 1:n * n) = -ones(className);
end
%=============================================================================
