%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function column = tableDefaultColumn(sample, nRows)
  % nRows rows of the default value of the class of sample (what growing an
  % array of that class fills in: 0, false, <missing>, NaT, <undefined>, an
  % empty cell, ...), as wide as sample.
  column = sample([], :);
  if nRows > 0
    column(nRows + 1, :) = sample(1, :);
    column = column(1:nRows, :);
  end
end
%=============================================================================
