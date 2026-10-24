%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Implicit expansion (broadcasting) for the two-argument forms of max and min.
%=============================================================================
% row vs column
R = max([1 5 3], [4; 2]);
REF = [4 5 4; 2 5 3];
asserts.isequal(R, REF);
%=============================================================================
R = min([1 5 3], [4; 2]);
REF = [1 4 3; 1 2 2];
asserts.isequal(R, REF);
%=============================================================================
% matrix vs row
R = max([1 2 3; 4 5 6], [10 1 100]);
REF = [10 2 100; 10 5 100];
asserts.isequal(R, REF);
%=============================================================================
R = min([1 2 3; 4 5 6], [10 1 100]);
REF = [1 1 3; 4 1 6];
asserts.isequal(R, REF);
%=============================================================================
% matrix vs column
R = max([1 2 3; 4 5 6], [5; 3]);
REF = [5 5 5; 4 5 6];
asserts.isequal(R, REF);
%=============================================================================
R = min([1 2 3; 4 5 6], [5; 3]);
REF = [1 2 3; 3 3 3];
asserts.isequal(R, REF);
%=============================================================================
% column vs row (two vectors of unequal orientation)
R = max([1; 2; 3], [10 20]);
REF = [10 20; 10 20; 10 20];
asserts.isequal(R, REF);
%=============================================================================
% integer class is preserved
R = max(int8([100 -100]), int8([50; -50]));
REF = int8([100 50; 100 -50]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'int8');
%=============================================================================
% single class is preserved
R = min(single([1 5 3]), single([4; 2]));
REF = single([1 4 3; 1 2 2]);
asserts.isequal(R, REF);
asserts.isequal(class(R), 'single');
%=============================================================================
% NaN is ignored by default (omitnan) unless both operands are NaN
R = max([1 NaN 3], [NaN; 2]);
REF = [1 NaN 3; 2 2 3];
asserts.isequal(R, REF);
%=============================================================================
R = min([1 NaN 3], [NaN; 2]);
REF = [1 NaN 3; 1 2 2];
asserts.isequal(R, REF);
%=============================================================================
% scalar and same-size operands keep working
asserts.isequal(max([1 2 3], 2), [2 2 3]);
asserts.isequal(max([1 2 3], [3 1 2]), [3 2 3]);
%=============================================================================
