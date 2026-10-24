%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
base = [[tempdir(), ''], 'test_unittest_parsetags_batch_', createGUID()];
firstFile = [base, '_first.m'];
secondFile = [base, '_second.m'];
bomFile = [base, '_bom.m'];
missingFile = [base, '_missing.m'];
%#ok<NLS0004>
cleanupFirst = onCleanup(@() evalc(removeCommand(firstFile)));
%#ok<NLS0004>
cleanupSecond = onCleanup(@() evalc(removeCommand(secondFile)));
%#ok<NLS0004>
cleanupBom = onCleanup(@() evalc(removeCommand(bomFile)));

filewrite(firstFile, {'% <--GUI MODE-->'; 'asserts.istrue(true);'});
filewrite(secondFile, {'% <--MPI MODE-->'; '% <--REUSE PROCESS-->'; ...
  '% <--WEIGHT 8-->'; '% <--TIMEOUT 200-->'; 'asserts.istrue(true);'});
bomContent = strjoin({'% <--REUSE PROCESS-->', '% <--WEIGHT 4-->', ...
  'asserts.istrue(true);'}, char([13 10]));
bomBytes = [uint8([239 187 191]), uint8(bomContent)];
fid = fopen(bomFile, 'wb');
fwrite(fid, bomBytes, 'uint8');
fclose(fid);

scalar = __nelson_unittest_parsetags__(firstFile); %#ok<NLS0004>
batch = __nelson_unittest_parsetags__({firstFile, secondFile, bomFile}); %#ok<NLS0004>
asserts.isequal(size(batch), [1, 3]);
asserts.isequal(batch(1), scalar);
asserts.istrue(batch(1).gui_mode);
asserts.istrue(batch(1).with_display);
asserts.isfalse(batch(1).cli_mode);
asserts.istrue(batch(2).mpi_mode);
asserts.istrue(batch(2).cli_mode);
asserts.istrue(batch(2).reuse_process);
asserts.isequal(batch(2).weight, 8);
asserts.isequal(batch(2).timeout, 200);
asserts.isequal(batch(1).timeout, 0);
asserts.istrue(batch(3).reuse_process);
asserts.isequal(batch(3).weight, 4);
asserts.isequal(batch(3).timeout, 0);

message = '';
try
  __nelson_unittest_parsetags__({firstFile, missingFile}); %#ok<NLS0004>
catch exception
  message = exception.message;
end
asserts.istrue(contains(message, 'File 2'));
asserts.istrue(contains(message, missingFile));

suite = nelson.unittest.discover(...
  {firstFile, secondFile, bomFile}, 'Kind', 'test');
asserts.isequal(suite.count, 3);
asserts.istrue(any(strcmp(suite.cases(1).tags, 'gui_mode')));
asserts.istrue(any(strcmp(suite.cases(2).tags, 'mpi_mode')));
asserts.istrue(any(strcmp(suite.cases(2).tags, 'reuse_process')));
asserts.isequal(suite.cases(2).weight, 8);
asserts.isequal(suite.cases(2).timeout, 200);
asserts.isfalse(any(strcmp(suite.cases(2).tags, 'timeout')));
asserts.istrue(any(strcmp(suite.cases(3).tags, 'reuse_process')));
asserts.isequal(suite.cases(3).weight, 4);
run_removeFile(firstFile);
run_removeFile(secondFile);
run_removeFile(bomFile);
%=============================================================================
