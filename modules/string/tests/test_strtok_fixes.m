%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a cell input keeps its shape
asserts.isequal(strtok({'a b', 'c d'}), {'a', 'c'});
[t, r] = strtok({'a b', 'c d'});
asserts.isequal(t, {'a', 'c'});
asserts.isequal(r, {' b', ' d'});
asserts.isequal(size(strtok({})), [0 0]);
asserts.isequal(size(strtok(cell(0, 2))), [0 2]);
[t, r] = strtok({});
asserts.isequal(size(t), [0 0]);
asserts.isequal(size(r), [0 0]);
%=============================================================================
