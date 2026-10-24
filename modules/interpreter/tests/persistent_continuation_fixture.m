%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = persistent_continuation_fixture()
  persistent aa bb cc ...
      dd ee ff
  if isempty(aa)
    aa = 1;
    bb = 2;
    cc = 3;
    dd = 4;
    ee = 5;
    ff = 6;
  end
  r = [aa, bb, cc, dd, ee, ff];
end
%=============================================================================
