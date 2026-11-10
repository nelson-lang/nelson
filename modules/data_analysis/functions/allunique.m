%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = allunique(A, varargin)
  % true when no value (or row with 'rows') appears twice; NaN values are all distinct
  narginchk(1, 2);
  if nargin == 2
    tf = numunique(A, varargin{:}) == size(A, 1);
  else
    tf = numunique(A) == numel(A);
  end
end
%=============================================================================
