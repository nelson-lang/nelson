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
asserts.istrue(isKey(d, 1));
asserts.isfalse(isKey(d, '1'));
%=============================================================================
R = isKey(d, [1 2 3 4 5]);
REF = [true, true, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
d = dictionary(string({'abc'}), 1);
asserts.istrue(isKey(d, 'abc'));
asserts.isfalse(isKey(d, 'def'));
%=============================================================================
names = ["Unicycle" "Bicycle" "Tricycle"];
wheels = [1 2 3];
d = dictionary(wheels, names);
R = isKey(d, ["1" "2" "3" "4" "5"]);
REF = [true, true, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
names = {"Unicycle" "Bicycle" "Tricycle"};
wheels = {1 2 3};
d = dictionary(wheels, names);
R = isKey(d, {"1" "2" "3" "4" "5"});
REF = [false, false, false, false, false];
asserts.isequal(R, REF);
%=============================================================================
names = {"Unicycle" "Bicycle" "Tricycle"};
wheels = {1 2 3};
d = dictionary(wheels, names);
msg = sprintf(_("Unable to use '%s' as key for dictionary with '%s' key type."), 'string', 'cell');
asserts.checkerror('R = isKey(d, ["1" "2" "3" "4" "5"]);', msg);
%=============================================================================
