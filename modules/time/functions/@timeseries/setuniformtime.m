%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = setuniformtime(obj, varargin)
  startTime = 0;
  interval = 1;
  len = obj.Length;
  for k = 1:2:length(varargin)
    name = lower(timeseriesText(varargin{k}, 'Property name'));
    switch name
      case {'starttime', 'start'}
        startTime = varargin{k + 1};
      case {'interval', 'increment', 'timestep'}
        interval = varargin{k + 1};
      case 'length'
        len = varargin{k + 1};
      case 'endtime'
        len = floor((varargin{k + 1} - startTime) ./ interval) + 1;
      otherwise
        error('Nelson:time:unsupportedUniformTimeOption', 'Unsupported %s.', 'uniform time option');
    end
  end
  obj.Time = startTime + (0:len - 1).' .* interval;
end
%=============================================================================
