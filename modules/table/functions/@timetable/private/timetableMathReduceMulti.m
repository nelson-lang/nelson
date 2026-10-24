%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = timetableMathReduceMulti(A, fun, functionName, requestedOutputs, varargin)
  tmp = cell(1, requestedOutputs);
  [tmp{:}] = table.applyReduceMulti(timetable2table(A, 'ConvertRowTimes', false), ...
    fun, functionName, requestedOutputs, varargin{:});
  for k = 1:requestedOutputs
    if isDimTwo(varargin) && height(tmp{k}) == height(A)
      varargout{k} = timetable.fromTableAndTimes(tmp{k}, A.Properties.RowTimes, A.Properties);
    else
      varargout{k} = tmp{k};
    end
  end
end
%=============================================================================
function tf = isDimTwo(args)
  tf = ~isempty(args) && isnumeric(args{1}) && isscalar(args{1}) && args{1} == 2;
end
%=============================================================================
