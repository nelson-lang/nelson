%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = append(obj, varargin)
  for k = 1:length(varargin)
    other = varargin{k};
    if ~istimeseries(other)
      error('Nelson:time:allInputsTimeseriesObjects', 'All inputs must be timeseries objects.');
    end
    obj.Data_ = cat(timeseriesSampleDimension(obj.Data_, obj.IsTimeFirst_), obj.Data_, other.Data);
    obj.Time_ = [obj.Time_; other.Time(:)];
    if ~isempty(obj.Quality) || ~isempty(other.Quality)
      leftQuality = obj.Quality(:);
      rightQuality = other.Quality(:);
      if isempty(leftQuality)
        leftQuality = NaN(obj.Length - other.Length, 1);
      end
      if isempty(rightQuality)
        rightQuality = NaN(other.Length, 1);
      end
      obj.Quality_ = [leftQuality; rightQuality];
    end
    obj.Events = [obj.Events(:); other.Events(:)];
  end
  obj = timeseriesRefreshMetadata(obj);
  timeseriesValidate(obj);
end
%=============================================================================
