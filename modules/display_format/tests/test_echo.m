%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('echo'), 1);
asserts.isequal(nargout('echo'), 1);
%=============================================================================
initialState = echo();
try
  echo('off');
  asserts.isequal(echo(), 'off');
  echo('on');
  asserts.isequal(echo(), 'on');
  echo(initialState);
catch ex
  echo(initialState);
  rethrow(ex);
end
%=============================================================================
