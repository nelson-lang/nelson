%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'low'}, {'low', 'high'}, {'low', 'high'}, 'Ordinal', true);
B = categorical({'low'});
asserts.istrue(isordinal(A));
asserts.isfalse(isordinal(B));
asserts.isfalse(isordinal(1));
%=============================================================================
