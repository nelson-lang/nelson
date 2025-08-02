%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

function r = nfilename2(a, b)
  r = titi(a,b) + 2;
  nfilename('fullpath');
  
function x = titi(a, b)
  x = a - b;
  nfilename('fullpathext');
