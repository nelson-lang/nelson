%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(isstrprop('a1 ', 'alpha'), [true false false]);
asserts.isequal(isstrprop('a1 ', 'digit'), [false true false]);
asserts.isequal(isstrprop('a1 ', 'alphanum'), [true true false]);
R = isstrprop(["a1", "B!"], "alpha", "ForceCellOutput", true);
asserts.istrue(iscell(R));
asserts.isequal(R{1}, [true false]);
asserts.isequal(R{2}, [true false]);
%=============================================================================
