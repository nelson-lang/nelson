%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = angle(Z)
  if islogical(Z) || ischar(Z)
    Z = double(Z);
  end
  % adding zero turns a negative zero into a positive one: angle(-0) is 0
  if issparse(Z)
    R = sparse(atan2(full(imag(Z)), full(real(Z)) + 0) + 0);
  else
    R = atan2(imag(Z), real(Z) + 0) + 0;
  end
end
%=============================================================================
