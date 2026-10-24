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
asserts.isequal(Age, 36);
asserts.isequal(Height, 71);
asserts.isequal(Weight, 76);
asserts.isequal(BloodPressure, 44);
REF = table(cos(Age), cos(Height), cos(Weight), cos(BloodPressure), 'VariableNames', {'Age', 'Height', 'Weight', 'BloodPressure'});
assert_table_numeric_isapprox(R, REF);
%=============================================================================
LastName = {'Smith'};
Age = 36;
Height = 71;
Weight = 76;
BloodPressure = 44;
T = table(LastName, Age, Height, Weight, BloodPressure);
msg = sprintf(_('Undefined function ''%s'' for input arguments of type ''%s''.'), 'cos', 'cell');
asserts.checkerror('cos(T)', msg);
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
  assert_table_numeric_isapprox(R, REF)
end
%=============================================================================
function assert_table_numeric_isapprox(computed, expected)
  asserts.isequal(size(computed), size(expected));
  asserts.isequal(computed.Properties.VariableNames, expected.Properties.VariableNames);
  asserts.isequal(computed.Properties.VariableTypes, expected.Properties.VariableTypes);
  asserts.isapprox(table2array(computed), table2array(expected), 1e-12);
end
