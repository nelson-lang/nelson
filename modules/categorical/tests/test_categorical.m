%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'b', 'a', '', 'b'});
asserts.isequal(class(A), 'categorical');
asserts.isequal(categories(A), {'a'; 'b'});
asserts.isequal(isundefined(A), [false false true false]);
asserts.isequal(cellstr(A), {'b', 'a', '<undefined>', 'b'});
%=============================================================================
