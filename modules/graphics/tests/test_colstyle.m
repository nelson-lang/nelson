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
[l, c, m, msg] = colstyle('r:x');
assert_isequal(l, ':');
assert_isequal(c, 'r');
assert_isequal(m, 'x');
assert_isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle("r:x");
assert_isequal(l, ':');
assert_isequal(c, 'r');
assert_isequal(m, 'x');
assert_isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle ('+')
assert_isequal(l, '');
assert_isequal(c, '');
assert_isequal(m, '+');
assert_isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle ('')
assert_isequal(l, '');
assert_isequal(c, '');
assert_isequal(m, '');
assert_isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle ('4')
REF.message = _('Invalid LineSpec string.');
REF.Identifier = 'Nelson:colstyle:InvalidLinespec';
assert_isequal(l, '');
assert_isequal(c, '');
assert_isequal(m, '');
assert_isequal(msg, REF);
%=============================================================================
[l, c, m, msg] = colstyle ('4', 'plot')
REF.message = _('Invalid LineSpec string.');
REF.Identifier = 'Nelson:colstyle:InvalidLinespec';
assert_isequal(l, '');
assert_isequal(c, '');
assert_isequal(m, '');
assert_isequal(msg, REF);
%=============================================================================
[l, c, m, msg] = colstyle ('*', 'plot')
REF = [];
assert_isequal(l, 'none');
assert_isequal(c, '');
assert_isequal(m, '*');
assert_isequal(msg, REF);
%=============================================================================
