%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = resample(obj, newTime, method)
  if nargin < 3 || isempty(method)
    method = getinterpmethod(obj);
  end
  newTime = timeseriesNormalizeTime(newTime);
  obj.Data_ = timeseriesInterpolateData(obj, newTime, method);
  if ~isempty(obj.Quality)
    obj.Quality_ = interp1(obj.Time, double(obj.Quality(:)), newTime, 'nearest', 'extrap');
  end
  obj.Time_ = newTime(:);
  obj = timeseriesRefreshMetadata(obj);
  timeseriesValidate(obj);
end
%=============================================================================
