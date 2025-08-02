%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@libpointer/plus'), 2);
assert_isequal(nargout('@libpointer/plus'), 1);
%=============================================================================
x = [1 2 3 4 5];
xPtr = libpointer('doublePtr', x);
y = xPtr + 1;
y.reshape(1, 4);
assert_isequal(y.Value, [2 3 4 5]);
%=============================================================================
x = [1 2 3 4 5];
xPtr = libpointer('doublePtr', x);
y = xPtr + 4;
y.reshape(1, 1);
assert_isequal(y.Value, [5]);
%=============================================================================
k = libpointer();
assert_checkerror('y2 = k + 1',_('The datatype and size of the value must be defined.'));
%=============================================================================
x = [1 2 3 4 5];
xPtr = libpointer('doublePtr', x);
assert_checkerror('y = xPtr + 10', _('Offset must not be greater than the size of the pointer.'));
%=============================================================================
xPtr = libpointer('double', 3);
y2 = xPtr + 1;
y2 = xPtr.plus(1);
%=============================================================================
