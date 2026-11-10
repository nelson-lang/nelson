%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('intmin'), 2);
asserts.isequal(nargout('intmin'), 1);
%=============================================================================
asserts.isequal(intmin(), intmin('int32'));
asserts.isequal(intmin('int32'), int32(-2147483648));
asserts.isequal(intmin('int8'), int8(-128));
asserts.isequal(intmin('uint8'), uint8(0));
asserts.isequal(intmin('int16'), int16(-32768));
asserts.isequal(intmin('uint16'), uint16(0));
asserts.isequal(intmin('uint32'), uint32(-2147483648));
asserts.isequal(intmin('int64'), int64(-9223372036854775808));
asserts.isequal(intmin('uint64'), uint64(0));
%=============================================================================
msg = _('With two inputs, the first one must be ''like''.');
asserts.checkerror('intmin(''uint64'', 3)', msg, 'Nelson:constants:firstArgNotLike');
msg = _('Class name must be an integer class such as ''int8'' or ''uint64''.');
asserts.checkerror('intmin(3)', msg, 'Nelson:constants:integerClassExpected');
asserts.checkerror('intmin(''t'')', msg, 'Nelson:constants:integerClassExpected');
asserts.checkerror('[a, b] = intmin()', _('Too many output arguments.'));
%=============================================================================
