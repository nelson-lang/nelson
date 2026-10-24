%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = timeseriesSubset(obj, selector)
  rows = timeseriesResolveRows(obj, selector);
  out = obj;
  out.Data_ = timeseriesSelectData(obj.Data_, rows, obj.IsTimeFirst_);
  out.Time_ = obj.Time_(rows);
  if ~isempty(obj.Quality_)
    out.Quality_ = obj.Quality_(rows);
  end
  out = timeseriesRefreshMetadata(out);
end
%=============================================================================
