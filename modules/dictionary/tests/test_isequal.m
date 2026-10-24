%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Jack", "John", "Jess"];
ages = [50, 35, 68];
gym = dictionary(names, ages);
gym2 = dictionary(gym);
ages2 = [50, 35, 66];
gym3 = dictionary(names, ages2);
%=============================================================================
asserts.istrue(isequal(gym, gym));
asserts.isequal(gym, gym);
asserts.istrue(isequal(gym, gym2));
asserts.isequal(gym, gym2);
asserts.isfalse(isequal(gym, gym3));
%=============================================================================
d1 = dictionary([1 2], [3 4]);
d2 = dictionary([2 1], [4 3]);
asserts.istrue(isequal(d1, d2));
%=============================================================================
d1 = dictionary([1 1 2], [10 11 20]);
d2 = dictionary([2 1], [20 11]);
asserts.istrue(isequal(d1, d2));
%=============================================================================
d1 = dictionary(string({'a', 'b'}), {1, [2 3]});
d2 = dictionary(string({'b', 'a'}), {[2 3], 1});
asserts.istrue(isequal(d1, d2));
d2.allValues = {{[2 3]}, {1}};
asserts.istrue(isequal(d1, d2));
%=============================================================================
