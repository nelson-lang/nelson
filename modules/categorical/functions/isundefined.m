%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isundefined(A)
  narginchk(1, 1);
  if isa(A, 'categorical')
    tf = __categorical_codes_isundefined__(A.codes);
  else
    tf = false(size(A));
  end
end
%=============================================================================
