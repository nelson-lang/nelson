%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Age = 36;
Height = 71;
Weight = 76;
BloodPressure = 44;
T = table(Age, Height, Weight, BloodPressure);
R = cos(T);
assert_isequal(Age, 36);
assert_isequal(Height, 71);
assert_isequal(Weight, 76);
assert_isequal(BloodPressure, 44);
REF = table(cos(Age), cos(Height), cos(Weight), cos(BloodPressure), 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith'};
Age = 36;
Height = 71;
Weight = 76;
BloodPressure = 44;
T = table(LastName, Age, Height, Weight, BloodPressure);
msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), 'cos', 'cell');
assert_checkerror('cos(T)', msg);
%=============================================================================
functions = {@abs;
@acos;
@acosh;
@acot;
@acotd;
@acoth;
@acsc;
@acscd;
@acsch;
@asec;
@asecd;
@asech;
@asin;
@asind;
@asinh;
@atan;
@atand;
@atanh;
@ceil;
@cosd;
@cosh;
@cospi;
@cot;
@cotd;
@coth;
@csc;
@cscd;
@csch;
@exp;
@fix;
@floor;
@log;
@log10;
@log1p;
@log2;
@nextpow2;
@round;
@sec;
@secd;
@sech;
@sin;
@sind;
@sinh;
@sinpi;
@sqrt;
@tan;
@tand;
@tanh;
@var;
@acosd};

Age = 36;
Height = 71;
Weight = 76;
BloodPressure = 44;
T = table(Age, Height, Weight, BloodPressure);

for fun = functions'
  R = fun{1}(T);
  REF = table(fun{1}(Age), fun{1}(Height), fun{1}(Weight), fun{1}(BloodPressure), 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
  assert_isequal(R, REF)
end