%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% intmax and intmin take the class of the 'like' value
assert_isequal(intmax('like', int8(1)), int8(127));
assert_isequal(intmin('like', uint16(5)), uint16(0));
assert_isequal(intmax('like', uint64([1 2])), intmax('uint64'));
assert_isequal(intmin('LIKE', int64(1)), intmin('int64'));
assert_isequal(intmax("uint8"), uint8(255));
%=============================================================================
msg = _('The value after ''like'' must be an integer array.');
assert_checkerror('intmax(''like'', 1)', msg, 'Nelson:constants:integerPrototypeExpected');
assert_checkerror('intmin(''like'', true)', msg, 'Nelson:constants:integerPrototypeExpected');
msg = _('Class name must be an integer class such as ''int8'' or ''uint64''.');
assert_checkerror('intmax(''double'')', msg, 'Nelson:constants:integerClassExpected');
msg = _('With two inputs, the first one must be ''like''.');
assert_checkerror('intmin(''foo'', int8(1))', msg, 'Nelson:constants:firstArgNotLike');
%=============================================================================
