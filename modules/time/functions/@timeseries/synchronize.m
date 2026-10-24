%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = synchronize(varargin)
  if nargin < 2
    error('Nelson:time:leastTwoTimeseriesObjectsRequired', 'At least two timeseries objects are required.');
  end
  method = '';
  if ischar(varargin{end}) || isstring(varargin{end})
    method = char(varargin{end});
    varargin(end) = [];
  end
  time = [];
  for k = 1:length(varargin)
    if ~istimeseries(varargin{k})
      error('Nelson:time:allInputsTimeseriesObjects', 'All inputs must be timeseries objects.');
    end
    time = union(time(:), varargin{k}.Time(:));
  end
  for k = 1:length(varargin)
    if isempty(method)
      varargout{k} = resample(varargin{k}, time);
    else
      varargout{k} = resample(varargin{k}, time, method);
    end
  end
end
%=============================================================================
