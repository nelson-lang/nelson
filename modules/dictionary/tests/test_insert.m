%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
assert_isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = {1 2 3};
d = dictionary(wheels,names);
R = insert(d,{2 4},["Motorcycle" "Car"], 'Overwrite', true)
REF = dictionary({1 2 3 4}, ["Unicycle" "Motorcycle" "Tricycle" "Car"]);
assert_isequal(R, REF);
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = {1 2 3};
d = dictionary(wheels,names);
d = insert(d,{2 4},["Motorcycle" "Car"], 'Overwrite', false)
%=============================================================================


