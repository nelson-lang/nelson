%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = bitset(A, pos, val)
  narginchk(2, 3);
  if nargin < 3 || ischar(val) || isstring(val)
    % A char/string third argument is an assumed-type hint, not a value.
    val = 1;
  end
  cls = class(A);
  mask = bitshift(cast(1, cls), pos - 1);
  cleared = bitxor(A, bitand(A, mask));
  seted = bitor(A, mask);
  setSelect = cast(val ~= 0, cls);
  clearSelect = cast(val == 0, cls);
  r = seted .* setSelect + cleared .* clearSelect;
end
%=============================================================================
