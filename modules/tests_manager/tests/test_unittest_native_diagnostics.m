%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
skip_if_no_native_unittest_runner();
%=============================================================================
removeCommand = @(filename)['if isfile(''', filename, '''), ', ...
  'rmfile(''', filename, '''), end'];
previousForceNativeRunner = getenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER');
cleanupForceNativeRunner = onCleanup(...
  @() setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', previousForceNativeRunner)); %#ok<NLS0004>
setenv('NELSON_UNITTEST_FORCE_NATIVE_RUNNER', 'TRUE');
base = [[tempdir(), ''], 'test_unittest_native_diagnostics_', createGUID()];
passFile = [base, '_test_pass.m'];
timeoutFile = [base, '_test_timeout.m'];
cleanupPass = onCleanup(@() evalc(removeCommand(passFile))); %#ok<NLS0004>
cleanupTimeout = onCleanup(@() evalc(removeCommand(timeoutFile))); %#ok<NLS0004>

filewrite(passFile, {'asserts.istrue(true);'});
suite = nelson.unittest.discover(passFile, 'Kind', 'test');
results = nelson.unittest.run(suite, 'Workers', 1);
diagnostics = results.cases(1).diagnostics;
asserts.isequal(results.cases(1).outcome, 'passed');
asserts.isequal(diagnostics.kind, 'test');
asserts.isequal(diagnostics.index, 1);
asserts.isequal(diagnostics.reason, 'payload');
asserts.istrue(diagnostics.pid > 0);
asserts.istrue(diagnostics.timeout > 0);
asserts.istrue(contains(diagnostics.process_policy, 'windows') || ...
  contains(diagnostics.process_policy, 'posix'));
asserts.istrue(contains(diagnostics.metadata, passFile));
asserts.istrue(contains(diagnostics.job_id, 'test_1'));
asserts.istrue(contains(diagnostics.executable, 'nelson-cli'));
asserts.istrue(contains(diagnostics.process_arguments, '--file'));

filewrite(timeoutFile, {'pause(5);'});
suite = nelson.unittest.discover(timeoutFile, 'Kind', 'test');
results = nelson.unittest.run(suite, 'Workers', 1, 'Timeout', 1);
diagnostics = results.cases(1).diagnostics;
asserts.isequal(results.cases(1).outcome, 'timeout');
asserts.isequal(diagnostics.reason, 'timeout');
asserts.isequal(diagnostics.timeout, 1);
asserts.istrue(diagnostics.pid > 0);
asserts.istrue(contains(diagnostics.process_policy, 'windows') || ...
  contains(diagnostics.process_policy, 'posix'));

payload = decode_runfile_payload('{ invalid json');
asserts.isequal(payload.r, 1);
asserts.isfalse(isempty(payload.msg));
payloads = extractRunfilePayloads(['__NELSON_UNITTEST_RESULT__{ invalid json', newline]);
asserts.isequal(length(payloads), 1);
asserts.isequal(payloads(1).r, 1);
asserts.isfalse(isempty(payloads(1).msg));

accentedMsg = native2unicode(...
  uint8([195 169 99 104 101 99 32 97 99 99 101 110 116 117 195 169]), ...
  'UTF-8');
accentedStdout = native2unicode(...
  uint8([115 111 114 116 105 101 32 97 99 99 101 110 116 117 195 169 101]), ...
  'UTF-8');
accentedPayload = struct('r', 1, 'msg', accentedMsg, ...
  'stdout', accentedStdout, 'timing', 0);
encodedPayload = encode_runfile_payload(accentedPayload);
encodedJson = jsonencode(encodedPayload);
asserts.isfalse(contains(encodedJson, accentedMsg(1)));
decodedPayload = decode_runfile_payload(encodedJson);
asserts.isequal(decodedPayload.msg, accentedPayload.msg);
asserts.isequal(decodedPayload.stdout, accentedPayload.stdout);

invalidOutputFile = [[tempdir(), ''], 'test_unittest_invalid_output_', createGUID(), '.json'];
cleanupInvalidOutput = onCleanup(@() evalc(removeCommand(invalidOutputFile))); %#ok<NLS0004>
filewrite(invalidOutputFile, '{ invalid json');
[outputPayload, outputStatus] = read_runfile_output_file(invalidOutputFile); %#ok<NLS0004>
asserts.isfalse(outputStatus);
asserts.isequal(outputPayload.r, 1);
asserts.isfalse(isempty(outputPayload.msg));
asserts.istrue(contains(outputPayload.msg, 'Invalid runner JSON file'));
run_removeFile(invalidOutputFile);

invalidUtf8OutputFile = [[tempdir(), ''], ...
  'test_unittest_invalid_utf8_output_', createGUID(), '.json'];
cleanupInvalidUtf8Output = onCleanup(@() evalc(removeCommand(invalidUtf8OutputFile))); %#ok<NLS0004>
fid = fopen(invalidUtf8OutputFile, 'wb');
fwrite(fid, uint8([255 254 0 123]));
fclose(fid);
[outputPayload, outputStatus] = read_runfile_output_file(invalidUtf8OutputFile); %#ok<NLS0004>
asserts.isfalse(outputStatus);
asserts.isequal(outputPayload.r, 1);
asserts.istrue(contains(outputPayload.msg, 'not valid UTF-8'));
asserts.isfalse(contains(outputPayload.msg, 'UTF8_ERROR'));
run_removeFile(invalidUtf8OutputFile);
%=============================================================================
