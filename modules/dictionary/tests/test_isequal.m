%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Jack", "John", "Jess"];
ages = [50, 35, 68]; 
gym = dictionary(names, ages);
gym2 = dictionary(gym);
ages2 = [50, 35, 66]; 
gym3 = dictionary(names, ages2);
%=============================================================================
assert_istrue(isequal(gym, gym));
assert_isequal(gym, gym);
assert_istrue(isequal(gym, gym2));
assert_isequal(gym, gym2);
assert_isfalse(isequal(gym, gym3));
%=============================================================================

