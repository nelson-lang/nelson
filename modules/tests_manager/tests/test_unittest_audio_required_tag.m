%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: a test in a non-audio module can require the audio module (its
% file-I/O builtins) with the <--AUDIO REQUIRED--> tag, without also requiring a
% physical audio device. Without the tag, the audio module stays disabled
% (--no-audio) in the test child, so audiowrite/audioread are unavailable.
%=============================================================================
base = [[tempdir(), ''], 'test_unittest_audio_required_tag_', createGUID()];
taggedFile = [base, '_tagged.m'];
plainFile = [base, '_plain.m'];
%#ok<NLS0004>
cleanupTagged = onCleanup(@() evalc(['if isfile(''', taggedFile, '''), rmfile(''', taggedFile, '''), end']));
%#ok<NLS0004>
cleanupPlain = onCleanup(@() evalc(['if isfile(''', plainFile, '''), rmfile(''', plainFile, '''), end']));

filewrite(taggedFile, {'% <--AUDIO REQUIRED-->'; 'assert_istrue(exist(''audiowrite'') == 5);'});
filewrite(plainFile, {'assert_istrue(true);'});

% The tag is parsed and does not imply a device requirement.
tagged = __nelson_unittest_parsetags__(taggedFile);
asserts.istrue(tagged.audio_required);
asserts.isfalse(tagged.audio_input_required);
asserts.isfalse(tagged.audio_output_required);

plain = __nelson_unittest_parsetags__(plainFile);
asserts.isfalse(plain.audio_required);

% An AUDIO REQUIRED test keeps the audio module enabled: no --no-audio is added.
runtimeOptions = struct('Timeout', [], 'LauncherGrace', 0, 'WorkerPoolInline', false);
taggedCase = initialize_test_case(taggedFile, 'example.othermodule', runtimeOptions);
asserts.isfalse(should_disable_audio(taggedCase));
taggedArgs = test_case_process_user_arguments(taggedCase);
asserts.isfalse(contains(taggedArgs, '--no-audio'));

% A plain test in a non-audio module keeps the historical behavior: audio off.
plainCase = initialize_test_case(plainFile, 'example.othermodule', runtimeOptions);
asserts.istrue(should_disable_audio(plainCase));
plainArgs = test_case_process_user_arguments(plainCase);
asserts.istrue(contains(plainArgs, '--no-audio'));

% A resource-bearing test must run in its own process, never pooled.
asserts.isfalse(can_pool_test_case_resources(taggedCase.options));
asserts.istrue(can_pool_test_case_resources(plainCase.options));
%=============================================================================
