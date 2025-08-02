%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function fun1()
  persistent toto
  if isempty(toto)
    toto = 0;
  end
  disp(['fun1 toto ', int2str(toto)]);
  toto = toto + 1;
end
