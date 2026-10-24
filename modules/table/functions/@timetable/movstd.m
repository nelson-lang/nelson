%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [R, M] = movstd(A, varargin)
  R = timetableMathMap(A, @movstd, 'movstd', varargin{:});
  if nargout > 1
    M = movmean(A, varargin{:});
  end
end
