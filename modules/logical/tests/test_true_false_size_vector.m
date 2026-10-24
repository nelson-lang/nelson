%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = zeros(2, 3);
R = true(size(A));
asserts.isequal(size(R), [2 3]);
asserts.isequal(class(R), 'logical');
asserts.istrue(all(R(:)));
%=============================================================================
R = false(size(A));
asserts.isequal(size(R), [2 3]);
asserts.isequal(class(R), 'logical');
asserts.isfalse(any(R(:)));
%=============================================================================
R = true([1 4 2]);
asserts.isequal(size(R), [1 4 2]);
asserts.istrue(all(R(:)));
%=============================================================================
R = false([0 3]);
asserts.isequal(size(R), [0 3]);
asserts.istrue(isempty(R));
%=============================================================================
R = true(size(A), 'like', false);
asserts.isequal(size(R), [2 3]);
asserts.isequal(class(R), 'logical');
asserts.istrue(all(R(:)));
%=============================================================================
asserts.isequal(true([]), true(0, 0));
asserts.checkerror('false([1; 2])', sprintf(_('%s must be a row vector.'), 'Argument #1'));
%=============================================================================
