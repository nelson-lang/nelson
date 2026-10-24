%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = remove(d, 2);
REF = dictionary([1 3], ["Unicycle" "Tricycle"]);
asserts.isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = remove(d, 4);
asserts.isequal(R, d);
%=============================================================================
d = dictionary(string({'a', 'b', 'c'}), {1, [2 3], {4, 5}});
R = remove(d, string({'a', 'c'}));
asserts.isfalse(isKey(R, "a"));
asserts.istrue(isKey(R, "b"));
asserts.isfalse(isKey(R, "c"));
asserts.isequal(R{'b'}, [2 3]);
asserts.isequal(keys(R), "b");
%=============================================================================
