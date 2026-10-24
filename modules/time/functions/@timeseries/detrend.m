%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = detrend(obj, varargin)
  mode = 'linear';
  if nargin > 1
    mode = char(varargin{1});
  end
  y = timeseriesDataAsColumns(obj);
  if isempty(y)
    return
  end
  if strcmpi(mode, 'constant')
    obj.Data_ = y - mean(y, 1);
  else
    x = obj.Time(:);
    x = [x, ones(length(x), 1)];
    coeff = x \ y;
    obj.Data_ = y - x * coeff;
  end
end
%=============================================================================
