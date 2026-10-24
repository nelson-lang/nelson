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
  if ~isa(B, 'categorical')
    B = categorical(B);
  end
  [a, b] = categorical.reconcile(A, B, false);
  tf = false(size(a));
  loc = zeros(size(a));
  for k = 1:numel(a)
    if a(k) ~= 0
      idx = find(b == a(k), 1);
      if ~isempty(idx)
        tf(k) = true;
        loc(k) = idx;
      end
    end
  end
  tf(a == 0) = false;
end
%=============================================================================
