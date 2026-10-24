%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [b, a] = sos2tf(sos)
  narginchk(1, 1);
  nargoutchk(0, 2);
  if isempty(sos)
    b = 1;
    a = 1;
    return;
  end
  if ~ismatrix(sos) || size(sos, 2) ~= 6
    error('Nelson:signal:invalidSOSMatrix', 'SOS matrix must have six columns.');
  end
  b = 1;
  a = 1;
  for row = 1:size(sos, 1)
    b = conv(b, sos(row, 1:3));
    a = conv(a, sos(row, 4:6));
  end
end
%=============================================================================
