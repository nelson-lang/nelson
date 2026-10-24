%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isequalwithequalnans(obj, varargin)
  tf = true;
  for k = 1:length(varargin)
    other = varargin{k};
    tf = tf && istimeseries(other) ...
      && isequaln(obj.Data, other.Data) ...
      && isequaln(obj.Time, other.Time) ...
      && isequaln(obj.Quality, other.Quality);
  end
end
%=============================================================================
