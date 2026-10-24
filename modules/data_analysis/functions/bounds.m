%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [smallest, largest] = bounds(A, varargin)
  narginchk(1, 3);
  nargoutchk(0, 2);
  limits = minmaxPair(A, varargin{:});
  smallest = limits.smallest;
  largest = limits.largest;
end
%=============================================================================
function limits = minmaxPair(A, varargin)
  if nargin == 1
    limits.smallest = min(A);
    limits.largest = max(A);
    return
  end
  limits.smallest = min(A, [], varargin{:});
  limits.largest = max(A, [], varargin{:});
end
%=============================================================================
