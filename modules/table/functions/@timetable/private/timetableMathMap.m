%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = timetableMathMap(A, fun, functionName, varargin)
  T = table.applyMapWithArgs(timetable2table(A, 'ConvertRowTimes', false), fun, functionName, varargin{:});
  if height(T) == height(A)
    R = timetable.fromTableAndTimes(T, A.Properties.RowTimes, A.Properties);
  else
    R = T;
  end
end
%=============================================================================
