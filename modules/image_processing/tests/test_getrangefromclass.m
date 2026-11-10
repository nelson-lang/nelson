%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(getrangefromclass(1), [0 1]);
assert_isequal(getrangefromclass(uint8(1)), [0 255]);
assert_isequal(getrangefromclass(int16(1)), [-32768 32767]);
assert_isequal(getrangefromclass(true), [0 1]);
assert_isequal(getrangefromclass(single(1)), [0 1]);
assert_isequal(getrangefromclass(uint32(1)), [0 4294967295]);
assert_isequal(class(getrangefromclass(uint8(1))), 'double');
assert_checkerror('getrangefromclass(''a'')', _('I must be a numeric or logical array.'));
%=============================================================================
