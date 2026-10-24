%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function n = numel(A, varargin)
  if nargin == 1
    n = numel(A.codes);
  else
    n = numel(A.codes, varargin{:});
  end
end
%=============================================================================
