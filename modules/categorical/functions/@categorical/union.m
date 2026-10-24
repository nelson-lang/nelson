%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ib] = union(A, B, varargin)
  if ~isa(B, 'categorical')
    B = categorical(B);
  end
  AB = [A(:); B(:)];
  [C, i] = unique(AB, varargin{:});
  ia = i(i <= numel(A));
  ib = i(i > numel(A)) - numel(A);
end
%=============================================================================
