%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function v = nonzeros(A)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if (~isnumeric(A) && ~islogical(A) && ~ischar(A))
    error('Nelson:nonzeros:invalidInput', 'A numeric, logical, or character array expected.');
  end
  [~, ~, v] = find(A);
  v = v(:);
end
%=============================================================================
