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
assert_istrue(startsWith(t, tempdir()));
%=============================================================================
d = [tempdir(), 'path1'];
mkdir(d,'s');
t = tempname(d);
assert_istrue(startsWith(t, d));
%=============================================================================
d = [tempdir(), 'path1'];
mkdir(d,'s');
t = tempname(string(d));
assert_istrue(startsWith(t, d));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 1), char(10),  _('Value must be a character vector or string scalar.')];
assert_checkerror('t = tempname(1);', msg)
%=============================================================================
