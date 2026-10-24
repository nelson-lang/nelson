%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [C, ia, ib] = intersect(A, B, varargin)
  [codesA, codesB, ~, options] = tableSetInputs(A, B, varargin);
  [~, ia, ib] = intersect(codesA, codesB, 'rows', options{:});
  ia = reshape(ia, [], 1);
  ib = reshape(ib, [], 1);
  C = A(ia, :);
end
%=============================================================================
