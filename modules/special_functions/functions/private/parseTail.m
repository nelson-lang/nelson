%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function upper = parseTail(tail, identifier)
  % true for 'upper', false for 'lower'
  if ~((ischar(tail) || isstring(tail)) && any(strcmpi(char(tail), {'lower', 'upper'})))
    error(identifier, 'Tail must be ''lower'' or ''upper''.');
  end
  upper = strcmpi(char(tail), 'upper');
end
%=============================================================================
