%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = addsample(obj, varargin)
  data = [];
  time = [];
  quality = [];
  for k = 1:2:length(varargin)
    name = lower(timeseriesText(varargin{k}, 'Property name'));
    switch name
      case 'data'
        data = varargin{k + 1};
      case 'time'
        time = varargin{k + 1};
      case 'quality'
        quality = varargin{k + 1};
      otherwise
        error('Nelson:time:unsupportedSampleProperty', 'Unsupported %s.', 'sample property');
    end
  end
  if isempty(time) || isempty(data)
    error('Nelson:time:sampleDataTimeRequired', 'Sample data and time are required.');
  end
  obj = append(obj, timeseries(data, time, quality, 'IsTimeFirst', obj.IsTimeFirst));
end
%=============================================================================
