%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = timetableMathReduce(A, fun, functionName, varargin)
  T = table.applyReduce(timetable2table(A, 'ConvertRowTimes', false), fun, functionName, varargin{:});
  if isDimTwo(varargin) && height(T) == height(A)
    R = timetable.fromTableAndTimes(T, A.Properties.RowTimes, A.Properties);
  else
    R = T;
  end
end
%=============================================================================
function tf = isDimTwo(args)
  tf = ~isempty(args) && isnumeric(args{1}) && isscalar(args{1}) && args{1} == 2;
end
%=============================================================================
