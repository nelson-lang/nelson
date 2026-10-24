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
big5 = [modulepath('characters_encoding', 'tests'), '/yan_BIG-5.txt'];
F = fopen(big5, 'rt');
R = fread(F, 'uint8');
fclose(F);
ce = nativecharset(R');
asserts.istrue(iscell(ce));
asserts.istrue(iscellstr(ce));
asserts.istrue(strcmp(ce{1}, 'Big5'));
%=============================================================================
