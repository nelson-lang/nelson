%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('nativecharset'), 1);
assert_isequal(nargout('nativecharset'), 1);
%=============================================================================
windows_1251 = [modulepath('characters_encoding', 'tests'), '/olaf_Windows-1251.txt'];
F = fopen(windows_1251, 'rt');
R = fread(F,'uint8');
fclose(F);
ce = nativecharset(R');
assert_istrue(iscell(ce));
assert_istrue(iscellstr(ce));
assert_istrue(any(contains(ce, 'windows-1251')));
%=============================================================================
