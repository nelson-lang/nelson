%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function column = timetableMissingRows(sample, nRows)
  % nRows rows of missing values of the class of sample, as wide as sample:
  % NaN, NaT, <missing>, <undefined>, an empty character vector in a cell,
  % and 0 or false for the types without missing value.
  sz = size(sample);
  sz(1) = nRows;
  if isfloat(sample)
    column = NaN(sz, class(sample));
  elseif isinteger(sample)
    column = zeros(sz, class(sample));
  elseif islogical(sample)
    column = false(sz);
  elseif iscell(sample)
    column = repmat({''}, sz);
  elseif isa(sample, 'duration')
    column = seconds(NaN(sz));
    column.Format = sample.Format;
  else
    column = sample([], :);
    column(1:nRows, :) = missing;
  end
end
%=============================================================================
