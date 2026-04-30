%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
longStringSource = ['"', repmat('a', 1, 300), '"'];
longString = eval(longStringSource);
assert_isequal(strlength(longString), 300);
%=============================================================================
longCharacterArraySource = ['''', repmat('b', 1, 300), ''''];
longCharacterArray = eval(longCharacterArraySource);
assert_isequal(strlength(longCharacterArray), 300);
%=============================================================================
