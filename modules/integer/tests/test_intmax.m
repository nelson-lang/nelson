%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('intmax'), -1);
assert_isequal(nargout('intmax'), -1);
%=============================================================================
assert_isequal(intmax(), intmax('int32'));
assert_isequal(intmax('int32'), int32(2147483647));
assert_isequal(intmax('int8'), int8(127));
assert_isequal(intmax('uint8'), uint8(255));
assert_isequal(intmax('int16'), int16(32767));
assert_isequal(intmax('uint16'), uint16(65535));
assert_isequal(intmax('uint32'), uint32(4294967295));
assert_isequal(intmax('int64'), int64(9223372036854775807i64));
assert_isequal(intmax('uint64'), uint64(18446744073709551615u64));
%=============================================================================
assert_checkerror('intmax(''uint64'', 3)', _('Wrong number of input arguments.'));
assert_checkerror('intmax(3)', _('Wrong type for argument #1: string expected.'));
assert_checkerror('intmax(''t'')', _('The name of an integer class expected.'));
assert_checkerror('[a, b] = intmax()', _('Wrong number of output arguments.'));
%=============================================================================
