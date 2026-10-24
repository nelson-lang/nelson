%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = timeseriesDataAsColumns(obj)
  data = obj.Data_;
  if isempty(data)
    y = [];
    return
  end
  if isvector(data)
    y = data(:);
    return
  end
  if obj.IsTimeFirst_
    y = reshape(data, obj.Length, []);
  else
    order = [ndims(data), 1:ndims(data) - 1];
    data = permute(data, order);
    y = reshape(data, obj.Length, []);
  end
end
%=============================================================================
