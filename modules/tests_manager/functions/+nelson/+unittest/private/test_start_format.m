%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function fmtStart = test_start_format(nbTotalTests)
  lenCharsNbTests = length(int2str(nbTotalTests));
  if lenCharsNbTests < 2
    lenCharsNbTests = lenCharsNbTests + 1;
  end
  fmtStart = ['  ', '%0', int2str(lenCharsNbTests), 'd/', ...
    '%0', int2str(lenCharsNbTests), 'd - %s'];
end
%=============================================================================
