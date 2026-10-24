%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
previous = history('saveafter');
assert_isequal(class(previous), 'double');
%=============================================================================
history('saveafter', 5);
assert_isequal(history('saveafter'), 5);
%=============================================================================
history('saveafter', 0);
assert_isequal(history('saveafter'), 0);
%=============================================================================
assert_checkerror('history(''saveafter'', -1)', _('Argument #2 must be nonnegative.'));
assert_checkerror('history(''saveafter'', 1.5)', _('Argument #2 must be an integer.'));
assert_checkerror('history(''saveafter'', ''a'')', _('Wrong type for argument #2: double expected.'));
assert_checkerror('history(''saveafter'', [1 2])', _('Wrong size for argument #2: scalar expected.'));
%=============================================================================
% 'get' with a non-double second argument reports argument #2, not #1.
assert_checkerror('history(''get'', true)', _('Wrong type for argument #2: double expected.'));
%=============================================================================
history('saveafter', previous);
assert_isequal(history('saveafter'), previous);
%=============================================================================
