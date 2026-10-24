%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [tf, loc] = ismember(A, B, varargin)
  % Rows are compared by value (row names are ignored); loc is the lowest
  % matching row of B.
  [codesA, codesB, ~, options] = tableSetInputs(A, B, varargin);
  [tf, loc] = ismember(codesA, codesB, 'rows', options{:});
  tf = reshape(tf, [], 1);
  loc = reshape(loc, [], 1);
end
%=============================================================================
