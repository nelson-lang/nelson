%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_1.csv'];
R = readcell(csv_filename);
REF = {'Row', 'Age', 'Height', 'Weight', 'BloodPressure_1', 'BloodPressure_2';
'Smith', [ 38], [    71], [   176], [            124], [             93];
'Johnson', [ 43], [    69], [   163], [            109], [             77];
'Williams', [ 38], [    64], [   131], [            125], [             83];
'Jones', [ 40], [    67], [   133], [            117], [             75]
'Brown', [ 49], [    64], [   119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_2.csv'];
R = readcell(csv_filename);
REF = {'Row'     , 'Age'      , 'Height'   , 'Weight'   , 'BloodPressure_1', 'BloodPressure_2';
'# test'  , string(NaN), string(NaN), string(NaN), string(NaN), string(NaN);
'Smith'   , [       38], [       71], [      176], [            124], [             93];
'Johnson' , [       43], [       69], [      163], [            109], [             77];
'Williams', [       38], [       64], [      131], [            125], [             83];
'Jones'   , [       40], [       67], [      133], [            117], [             75];
'Brown'   , [       49], [       64], [      119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_3.csv'];
R = readcell(csv_filename);
REF = {'Smith' , [       38], [       71], [      176], [            124], [             93];
'Johnson' , [       43], [       69], [      163], [            109], [             77];
'Williams', [       38], [       64], [      131], [            125], [             83];
'Jones'   , [       40], [       67], [      133], [            117], [             75];
'Brown'   , [       49], [       64], [      119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_1.csv'];
options = detectImportOptions(csv_filename);
R = readcell(csv_filename, options);
REF = {'Smith' , [       38], [       71], [      176], [            124], [             93];
'Johnson' , [       43], [       69], [      163], [            109], [             77];
'Williams', [       38], [       64], [      131], [            125], [             83];
'Jones'   , [       40], [       67], [      133], [            117], [             75];
'Brown'   , [       49], [       64], [      119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_2.csv'];
options = detectImportOptions(csv_filename);
R = readcell(csv_filename, options);
REF = {'Smith' , [       38], [       71], [      176], [            124], [             93];
'Johnson' , [       43], [       69], [      163], [            109], [             77];
'Williams', [       38], [       64], [      131], [            125], [             83];
'Jones'   , [       40], [       67], [      133], [            117], [             75];
'Brown'   , [       49], [       64], [      119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readcell_3.csv'];
options = detectImportOptions(csv_filename);
R = readcell(csv_filename, options);
REF = {'Smith' , [       38], [       71], [      176], [            124], [             93];
'Johnson' , [       43], [       69], [      163], [            109], [             77];
'Williams', [       38], [       64], [      131], [            125], [             83];
'Jones'   , [       40], [       67], [      133], [            117], [             75];
'Brown'   , [       49], [       64], [      119], [            122], [             80]};
assert_isequal(R, REF);
%=============================================================================
