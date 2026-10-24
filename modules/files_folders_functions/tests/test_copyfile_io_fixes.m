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
% Bug 8: with two outputs, copying a non-existent source must NOT throw.
% It must return status = false (logical) and a non-empty message.
src = [tempname(), '_does_not_exist.tmp'];
dst = [tempname(), '.tmp'];
[status, msg] = copyfile(src, dst);
asserts.isfalse(status);
asserts.isequal(class(status), 'logical');
asserts.istrue(ischar(msg));
asserts.isfalse(isempty(msg));
%=============================================================================
% A successful copy must still report status = true and an empty message.
src = [tempname(), '.txt'];
dst = [tempname(), '.txt'];
filewrite(src, 'Nelson');
[status, msg] = copyfile(src, dst);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isfile(dst));
rmfile(src);
rmfile(dst);
%=============================================================================
% With 0 or 1 output, a missing source still raises an error.
src = [tempname(), '_missing_zzz.tmp'];
dst = [tempname(), '.tmp'];
cmd = ['copyfile(''', src, ''', ''', dst, ''');'];
asserts.checkerror(cmd, _('Directory source does not exist.'));
%=============================================================================
