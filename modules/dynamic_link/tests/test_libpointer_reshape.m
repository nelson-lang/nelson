%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@libpointer/reshape'), 3);
assert_isequal(nargout('@libpointer/reshape'), 0);
%=============================================================================
a = libpointer('double', 3);
assert_checkerror('a.reshape(4, 4)', _('Only numericPtr can be reshaped.'));
%=============================================================================
a = libpointer('doublePtr', eye(2, 2));
a.reshape(3, 3);
REF = [1     1     0;
0     0     0;
0     0     0];
assert_isequal(get(a, 'Value'), REF);
%=============================================================================
