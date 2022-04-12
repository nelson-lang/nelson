%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = 133.3;
xPtr = libpointer('doublePtr', x);
R = [xPtr; xPtr];
assert_isequal(size(R),  [2 1]);
assert_isequal(class(R), 'libpointer');
%=============================================================================
