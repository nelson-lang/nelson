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
T = rmprop(T, 'Source');
asserts.isfalse(isfield(T.Properties.CustomProperties, 'Source'));
%=============================================================================
T = addprop(T, {'LowerLimit', 'UpperLimit'}, 'variable');
T.Properties.CustomProperties.LowerLimit = [0];
T.Properties.CustomProperties.UpperLimit = [10];
T = rmprop(T, "LowerLimit");
asserts.isfalse(isfield(T.Properties.CustomProperties, 'LowerLimit'));
asserts.istrue(isfield(T.Properties.CustomProperties, 'UpperLimit'));
T = rmprop(T, {'UpperLimit'});
asserts.isfalse(isfield(T.Properties.CustomProperties, 'UpperLimit'));
asserts.checkerror('rmprop(T, ''Missing'');', 'Custom property does not exist.');
%=============================================================================
U = table([1; 2], 'VariableNames', {'A'});
asserts.checkerror('rmprop(U, ''Missing'');', 'Custom property does not exist.');
%=============================================================================
U = addprop(U, ["Instrument", "Precision"], ["variable", "variable"]);
U.Properties.CustomProperties.Instrument = {'sensor'};
U.Properties.CustomProperties.Precision = [0.1];
U = rmprop(U, ["Instrument", "Precision"]);
asserts.isfalse(isfield(U.Properties.CustomProperties, 'Instrument'));
asserts.isfalse(isfield(U.Properties.CustomProperties, 'Precision'));
asserts.istrue(isfield(U.Properties, 'VariableNames'));
%=============================================================================
