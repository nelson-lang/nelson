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
iso8859_1 = [modulepath('characters_encoding', 'tests'), '/die_ISO-8859-1.txt'];
F = fopen(iso8859_1, 'rt');
R = fread(F,'uint8');
fclose(F);
ce = nativecharset(R');
assert_istrue(iscell(ce));
assert_istrue(iscellstr(ce));
assert_istrue(any(contains(ce, 'ISO-8859-1')))
%=============================================================================
pt = [modulepath('characters_encoding', 'tests'), '/portugal_ISO-8859-1.txt'];
F = fopen(pt, 'rt');
R = fread(F,'uint8');
fclose(F);
ce = nativecharset(R');
assert_istrue(iscell(ce));
assert_istrue(iscellstr(ce));
assert_istrue(any(contains(ce, 'ISO-8859-1')))
%=============================================================================
