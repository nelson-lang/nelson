%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
t = tempname();
asserts.istrue(startsWith(t, tempdir()));
%=============================================================================
d = tempname();
mkdir(d, 's');
t = tempname(d);
asserts.istrue(startsWith(t, d));
%=============================================================================
d = tempname();
mkdir(d, 's');
t = tempname(string(d));
asserts.istrue(startsWith(t, d));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 1), char(10), _('Value must be a character vector or string scalar.')];
asserts.checkerror('t = tempname(1);', msg)
%=============================================================================
