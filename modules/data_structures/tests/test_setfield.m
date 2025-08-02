%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('setfield'), -2);
assert_isequal(nargout('setfield'), 1);
%=============================================================================
R = {};
R = setfield(R, 'toto', 3);
REF = struct('toto', 3);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, 'titi', 4);
REF = struct('toto', 3, 'titi', 4);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
R = setfield(R, "titi", 4);
REF = struct('toto', 3, 'titi', 4);
assert_isequal(R, REF);
%=============================================================================
R = struct('toto', 3);
assert_checkerror('R = setfield(R, 19, 4);', _('Input should be a string or character array.'));
%=============================================================================
