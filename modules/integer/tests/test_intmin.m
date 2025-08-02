%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('intmin'), -1);
assert_isequal(nargout('intmin'), -1);
%=============================================================================
assert_isequal(intmin(), intmin('int32'));
assert_isequal(intmin('int32'), int32(-2147483648));
assert_isequal(intmin('int8'), int8(-128));
assert_isequal(intmin('uint8'), uint8(0));
assert_isequal(intmin('int16'), int16(-32768));
assert_isequal(intmin('uint16'), uint16(0));
assert_isequal(intmin('uint32'), uint32(-2147483648));
assert_isequal(intmin('int64'), int64(-9223372036854775808));
assert_isequal(intmin('uint64'), uint64(0));
%=============================================================================
assert_checkerror('intmin(''uint64'', 3)', _('Wrong number of input arguments.'));
assert_checkerror('intmin(3)', _('Wrong type for argument #1: string expected.'));
assert_checkerror('intmin(''t'')', _('The name of an integer class expected.'));
assert_checkerror('[a, b] = intmin()', _('Wrong number of output arguments.'));
%=============================================================================
