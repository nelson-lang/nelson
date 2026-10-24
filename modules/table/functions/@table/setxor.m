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
  [codesA, codesB, B, options] = tableSetInputs(A, B, varargin);
  [codesC, ia, ib] = setxor(codesA, codesB, 'rows', options{:});
  ia = reshape(ia, [], 1);
  ib = reshape(ib, [], 1);
  C = tableMergeSetRows(A, B, ia, ib, [codesA(ia, :); codesB(ib, :)], codesC);
end
%=============================================================================
