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
guiFile = [[tempdir(), ''], 'test_gui_', createGUID(), '.m'];
mpiFile = [[tempdir(), ''], 'test_mpi_', createGUID(), '.m'];
seqFile = [[tempdir(), ''], 'test_seq_', createGUID(), '.m'];
benchFile = [[tempdir(), ''], 'bench_perf_', createGUID(), '.m'];
cleanupGui = onCleanup(@() evalc(removeCommand(guiFile))); %#ok<NLS0004>
cleanupMpi = onCleanup(@() evalc(removeCommand(mpiFile))); %#ok<NLS0004>
cleanupSeq = onCleanup(@() evalc(removeCommand(seqFile))); %#ok<NLS0004>
cleanupBench = onCleanup(@() evalc(removeCommand(benchFile))); %#ok<NLS0004>

filewrite(guiFile, {'% <--GUI MODE-->'; '% <--WITH DISPLAY-->'; 'asserts.istrue(true);'});
filewrite(mpiFile, {'% <--MPI MODE-->'; 'asserts.istrue(true);'});
filewrite(seqFile, {'% <--SEQUENTIAL TEST REQUIRED-->'; 'asserts.istrue(true);'});
filewrite(benchFile, {'asserts.istrue(true);'});

suite = nelson.unittest.discover({guiFile; mpiFile; seqFile; benchFile}, 'Kind', 'all');
testPlan = nelson.unittest.plan(suite, 'Workers', 4);

asserts.istrue(any(strcmp(testPlan.cases(1).resources, 'with_display')));
asserts.istrue(any(strcmp(testPlan.cases(2).resources, 'mpi_mode')));
asserts.istrue(any(strcmp(testPlan.cases(3).tags, 'sequential_test_required')));
asserts.istrue(any(strcmp(testPlan.groups.gui, testPlan.cases(1).id)));
asserts.istrue(any(strcmp(testPlan.groups.mpi, testPlan.cases(2).id)));
asserts.istrue(any(strcmp(testPlan.groups.sequential, testPlan.cases(3).id)));
asserts.istrue(any(strcmp(testPlan.groups.bench, testPlan.cases(4).id)));
run_removeFile(guiFile);
run_removeFile(mpiFile);
run_removeFile(seqFile);
run_removeFile(benchFile);
%=============================================================================
