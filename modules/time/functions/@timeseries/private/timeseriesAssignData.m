%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = timeseriesAssignData(data, rows, value, isTimeFirst)
  dim = timeseriesSampleDimension(data, isTimeFirst);
  subs = repmat({':'}, 1, ndims(data));
  subs{dim} = rows;
  data(subs{:}) = value;
end
%=============================================================================
