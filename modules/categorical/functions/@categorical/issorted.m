%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = issorted(A, varargin)
  sortable = A.codes;
  sortable(sortable == 0) = Inf;
  tf = issorted(sortable, varargin{:});
end
%=============================================================================
