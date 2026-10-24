%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--CLI MODE-->
previousWorkers = maxNumCompThreads(1);
cleanupWorkers = onCleanup(@() maxNumCompThreads(previousWorkers)); %#ok<NLS0004>
output = evalc('status = test_run([nelsonroot(), ''/modules/string/tests/test_strfind.m'']);');
asserts.istrue(status);
asserts.istrue(contains(output, 'test_strfind'));

output = evalc('status = test_run({''string''}, ''test_strfind'');');
asserts.istrue(status);
asserts.istrue(contains(output, 'test_strfind'));

txt = evalc('test_run(''string'', ''test_strfind'');');
asserts.istrue(contains(txt, '01/01 - test_strfind'));
asserts.isfalse(contains(txt, '02/'));

txt = evalc('test_run(''string'', ''test_strfind.m'');');
asserts.istrue(contains(txt, '01/01 - test_strfind'));
asserts.isfalse(contains(txt, '02/'));
