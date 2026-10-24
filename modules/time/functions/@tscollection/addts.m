%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = addts(obj, ts, name)
  if ~istimeseries(ts)
    error('Nelson:time:inputTimeseriesObject', 'Input must be a timeseries object.');
  end
  if nargin < 3 || isempty(name)
    name = ts.Name;
  end
  if isempty(name)
    name = ['series', int2str(length(fieldnames(obj.Members_)) + 1)];
  end
  name = tscollectionValidName(timeseriesText(name, 'Timeseries name'));
  if isempty(obj.Time)
    obj.Time_ = ts.Time;
  elseif ~isequal(obj.Time, ts.Time)
    ts = resample(ts, obj.Time);
  end
  obj.Members_.(name) = ts;
  obj = tscollectionRefreshMetadata(obj);
end
%=============================================================================
