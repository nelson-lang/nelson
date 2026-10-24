%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('savenh5'), 1);
asserts.isequal(nargout('savenh5'), 0);
%=============================================================================
filename = [tempname(), '.nh5'];
try
  A = [1 2; 3 4];
  B = 'Nelson';
  savenh5(filename, 'A', 'B');
  asserts.istrue(isfile(filename));
  loaded = loadnh5(filename);
  asserts.isequal(loaded.A, A);
  asserts.isequal(loaded.B, B);
  C = eye(2);
  savenh5(filename, 'C', '-append');
  loaded = loadnh5(filename);
  asserts.isequal(loaded.C, C);
  rmfile(filename);
catch ex
  if isfile(filename)
    rmfile(filename);
  end
  rethrow(ex);
end
%=============================================================================
