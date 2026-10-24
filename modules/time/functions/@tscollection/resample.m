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
  if nargin < 3
    method = '';
  end
  obj.Time_ = timeseriesNormalizeTime(newTime);
  names = fieldnames(obj.Members_);
  for k = 1:length(names)
    if isempty(method)
      obj.Members_.(names{k}) = resample(obj.Members_.(names{k}), obj.Time_);
    else
      obj.Members_.(names{k}) = resample(obj.Members_.(names{k}), obj.Time_, method);
    end
  end
  obj = tscollectionRefreshMetadata(obj);
end
%=============================================================================
