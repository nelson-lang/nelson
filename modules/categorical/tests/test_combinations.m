%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = combinations(categorical({'x', 'y'}), categorical({'u', 'v', 'w'}));
asserts.isequal(size(T), [6 2]);
T = combinations([1; 2], {'a', 'b'});
asserts.isequal(size(T), [4 2]);
%=============================================================================
