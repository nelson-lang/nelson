%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('intmax'), 2);
asserts.isequal(nargout('intmax'), 1);
%=============================================================================
asserts.isequal(intmax(), intmax('int32'));
asserts.isequal(intmax('int32'), int32(2147483647));
asserts.isequal(intmax('int8'), int8(127));
asserts.isequal(intmax('uint8'), uint8(255));
asserts.isequal(intmax('int16'), int16(32767));
asserts.isequal(intmax('uint16'), uint16(65535));
asserts.isequal(intmax('uint32'), uint32(4294967295));
asserts.isequal(intmax('int64'), int64(9223372036854775807i64));
asserts.isequal(intmax('uint64'), uint64(18446744073709551615u64));
%=============================================================================
msg = _('With two inputs, the first one must be ''like''.');
asserts.checkerror('intmax(''uint64'', 3)', msg, 'Nelson:constants:firstArgNotLike');
msg = _('Class name must be an integer class such as ''int8'' or ''uint64''.');
asserts.checkerror('intmax(3)', msg, 'Nelson:constants:integerClassExpected');
asserts.checkerror('intmax(''t'')', msg, 'Nelson:constants:integerClassExpected');
asserts.checkerror('[a, b] = intmax()', _('Too many output arguments.'));
%=============================================================================
