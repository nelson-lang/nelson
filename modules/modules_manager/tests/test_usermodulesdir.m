%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('usermodulesdir'),0);
assert_isequal(nargout('usermodulesdir'), 1);
%=============================================================================
P = getenv('NELSON_EXTERNAL_MODULES_PATH');
if isdir(P)
  REF = p;
else
  V = version('-number');
  REF = [userdir(), 'Nelson/', sprintf('%d.%d.%d',V(1), V(2), V(3)), '/modules/'];
end
R = usermodulesdir();
assert_isequal(R, REF);
%=============================================================================
