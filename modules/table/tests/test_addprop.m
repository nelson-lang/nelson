%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], 'VariableNames', {'A'});
T = addprop(T, 'Source', 'table');
T.Properties.CustomProperties.Source = 'unit-test';
asserts.isequal(T.Properties.CustomProperties.Source, 'unit-test');
%=============================================================================
T = addprop(T, "Quality", "table");
T.Properties.CustomProperties.Quality = 'checked';
asserts.isequal(T.Properties.CustomProperties.Quality, 'checked');
%=============================================================================
T = addprop(T, {'LowerLimit', 'UpperLimit'}, 'variable');
T.Properties.CustomProperties.LowerLimit = [0];
T.Properties.CustomProperties.UpperLimit = [10];
asserts.isequal(T.Properties.CustomProperties.LowerLimit, [0]);
asserts.isequal(T.Properties.CustomProperties.UpperLimit, [10]);
%=============================================================================
U = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
U = addprop(U, ["Instrument", "Precision", "SourceFile"], ...
  ["variable", "variable", "table"]);
U.Properties.CustomProperties.Instrument = ["clock", "sensor", "meter"];
U.Properties.CustomProperties.Precision = [0.1, 0.2, 0.3];
U.Properties.CustomProperties.SourceFile = "unit-test.csv";
asserts.isequal(U.Properties.CustomProperties.Instrument, ["clock", "sensor", "meter"]);
asserts.isequal(U.Properties.CustomProperties.Precision, [0.1, 0.2, 0.3]);
asserts.isequal(U.Properties.CustomProperties.SourceFile, "unit-test.csv");
asserts.checkerror('addprop(T, ''Source'', ''table'');', 'Custom property already exists.');
asserts.checkerror('addprop(T, ''InvalidType'', ''other'');', ...
  'Property type must be ''table'' or ''variable''.');
%=============================================================================
