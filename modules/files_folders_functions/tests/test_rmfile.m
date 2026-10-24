%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.txt'];
filewrite(filename, 'Nelson');
[status, msg] = rmfile(filename);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isfile(filename));
%=============================================================================
[status, msg] = rmfile(filename);
asserts.isfalse(status);
asserts.isequal(msg, _('an existing file expected.'));
%=============================================================================
