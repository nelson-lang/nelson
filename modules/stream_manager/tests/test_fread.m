%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fread'), 3);
assert_isequal(nargout('fread'), 1);
%=============================================================================
filename = [modulepath('characters_encoding', 'tests'), '/olaf_Windows-1251.txt'];
F = fopen(filename, 'rt', 'n', 'windows-1251');
[R, S] = fread(F, '*char');
fclose(F);
assert_isequal(S, 29106);
P = strfind(R, 'P.S. Когато преглеждах първата');
assert_isequal(P, 14964);
%=============================================================================
