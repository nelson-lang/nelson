%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('nativecharset'), 1);
assert_isequal(nargout('nativecharset'), 1);
%=============================================================================
big5 = [modulepath('characters_encoding', 'tests'), '/yan_BIG-5.txt'];
F = fopen(big5, 'rt');
R = fread(F,'uint8');
fclose(F);
ce = nativecharset(R');
assert_istrue(iscell(ce));
assert_istrue(iscellstr(ce));
assert_istrue(strcmp(ce{1}, 'Big5'));
%=============================================================================
