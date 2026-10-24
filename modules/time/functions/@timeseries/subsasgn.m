%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = subsasgn(obj, s, value)
  if strcmp(s(1).type, '()')
    if length(s) ~= 1
      error('Nelson:time:unsupportedTimeseriesAssignment', 'Unsupported %s.', 'timeseries assignment');
    end
    rows = timeseriesResolveRows(obj, s(1).subs{1});
    obj.Data_ = timeseriesAssignData(obj.Data_, rows, value.Data, obj.IsTimeFirst_);
    obj.Time_(rows) = value.Time(:);
    if ~isempty(obj.Quality_) && ~isempty(value.Quality)
      obj.Quality_(rows) = value.Quality(:);
    end
    obj = timeseriesRefreshMetadata(obj);
    timeseriesValidate(obj);
    return
  end
  if strcmp(s(1).type, '.')
    obj = timeseriesDotAssign(obj, s(1).subs, value);
    return
  end
  error('Nelson:time:invalidTimeseriesAssignment', 'Invalid %s.', 'timeseries assignment');
end
%=============================================================================
