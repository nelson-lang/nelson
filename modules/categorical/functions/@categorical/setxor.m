%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ib] = setxor(A, B, varargin)
  [CA, ia] = setdiff(A, B, varargin{:});
  [CB, ib] = setdiff(B, A, varargin{:});
  C = [CA(:); CB(:)];
end
%=============================================================================
