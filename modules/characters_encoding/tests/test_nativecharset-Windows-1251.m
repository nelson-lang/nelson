%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_if_reduced_characters_encoding_backend();
%=============================================================================
asserts.isequal(nargin('nativecharset'), 1);
asserts.isequal(nargout('nativecharset'), 1);
%=============================================================================
windows_1251 = [modulepath('characters_encoding', 'tests'), '/olaf_Windows-1251.txt'];
F = fopen(windows_1251, 'rt');
R = fread(F, 'uint8');
fclose(F);
ce = nativecharset(R');
asserts.istrue(iscell(ce));
asserts.istrue(iscellstr(ce));
asserts.istrue(any(contains(ce, 'windows-1251')));
%=============================================================================
