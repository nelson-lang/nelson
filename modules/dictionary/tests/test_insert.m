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
R = insert(d, 4, "Car");
REF = dictionary([1 2 3 4], ["Unicycle" "Bicycle" "Tricycle" "Car"]);
asserts.isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = {1 2 3};
d = dictionary(wheels, names);
R = insert(d, {2 4}, ["Motorcycle" "Car"], 'Overwrite', true)
REF = dictionary({1 2 3 4}, ["Unicycle" "Motorcycle" "Tricycle" "Car"]);
asserts.isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = {1 2 3};
d = dictionary(wheels, names);
d = insert(d, {2 4}, ["Motorcycle" "Car"], 'Overwrite', false)
REF = dictionary({1 2 3 4}, ["Unicycle" "Bicycle" "Tricycle" "Car"]);
asserts.isequal(d, REF);
%=============================================================================
d = dictionary(string({'a', 'b'}), {1, [2 3]});
R = insert(d, string({'b', 'c'}), {99, {4, 5}}, 'Overwrite', false);
asserts.isequal(R{'b'}, [2 3]);
asserts.isequal(R{'c'}, {4, 5});
R = insert(d, string({'b', 'c'}), {99, {6, 7}}, 'Overwrite', true);
asserts.isequal(R{'b'}, 99);
asserts.isequal(R{'c'}, {6, 7});
%=============================================================================

