%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [V, method, windowSize, sd] = parseSmooth3Inputs(args)
  V = args{1};
  if ~(isnumeric(V) || islogical(V)) || ~isreal(V) || ndims(V) ~= 3
    error('Nelson:parseSmooth3Inputs:invalidV', 'V must be a real numeric or logical 3-D array.');
  end
  method = 'box';
  windowSize = [3 3 3];
  sd = 0.65;
  if numel(args) >= 2
    method = validateSmooth3Method(args{2});
  end
  if numel(args) >= 3
    windowSize = validateSmooth3WindowSize(args{3});
  end
  if numel(args) >= 4
    sd = validateSmooth3StandardDeviation(args{4});
  end
end
%=============================================================================
