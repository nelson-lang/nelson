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
asserts.isequal(l, ':');
asserts.isequal(c, 'r');
asserts.isequal(m, 'x');
asserts.isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle("r:x");
asserts.isequal(l, ':');
asserts.isequal(c, 'r');
asserts.isequal(m, 'x');
asserts.isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle('+')
asserts.isequal(l, '');
asserts.isequal(c, '');
asserts.isequal(m, '+');
asserts.isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle('')
asserts.isequal(l, '');
asserts.isequal(c, '');
asserts.isequal(m, '');
asserts.isequal(msg, []);
%=============================================================================
[l, c, m, msg] = colstyle('4')
REF.message = _('Invalid LineSpec string.');
REF.Identifier = 'Nelson:colstyle:InvalidLinespec';
asserts.isequal(l, '');
asserts.isequal(c, '');
asserts.isequal(m, '');
asserts.isequal(msg, REF);
%=============================================================================
[l, c, m, msg] = colstyle('4', 'plot')
REF.message = _('Invalid LineSpec string.');
REF.Identifier = 'Nelson:colstyle:InvalidLinespec';
asserts.isequal(l, '');
asserts.isequal(c, '');
asserts.isequal(m, '');
asserts.isequal(msg, REF);
%=============================================================================
[l, c, m, msg] = colstyle('*', 'plot')
REF = [];
asserts.isequal(l, 'none');
asserts.isequal(c, '');
asserts.isequal(m, '*');
asserts.isequal(msg, REF);
%=============================================================================
